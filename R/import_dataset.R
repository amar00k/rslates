




makeVarnameFromFilename <- function(filename, max.length = 15) {
  name.parts <- sub("\\..*$", "", basename(filename)) %>%
    strsplit(split = "[^a-zA-Z0-9]") %>%
    pluck(1)

  w <- which(cumsum(nchar(name.parts)) + 0:(length(name.parts)-1) <= max.length)
  if (length(w) != 0) {
    dataset.name <- name.parts[w] %>% paste(collapse="_")
  } else {
    dataset.name <- substr(fileinfo$name, 1, max.length)
  }
}


fileInfo <- function(filepath, filename = basename(filepath), origin = "local") {
  list(origin = origin,
       name = filename,
       size = file.size(filepath),
       extension = sub(".*\\.(.*$)", "\\1", filename),
       type = sub(".*\\.(.*$)", "\\1", filename),
       datapath = filepath
  )
}


detectTabularFormat <- function(filename, nlines = 50, comment.char = "") {
  lines <- readLines(filename, nlines) %>%
    keep(~!is.na(.))
  separators <- c("Space"=" ", "Tab"="\t", "Comma"=",", "Semicolon"=";")
  comment.chars <- c("#", "%", "$")
  quotation.marks <- c("Single Quote"="'", "Double Quote"='"')

  # for each separator get the unique counts in lines
  # then discard separators that never show up
  # and order by separators containing the least amount of different
  # number of occurrences per line
  lines.2 <- lines[ 2:length(lines) ]
  format <- map(quotation.marks, function(q) {
    map(separators, function(sep) {
      gsub(sprintf("%s.*?%s", q, q), "!", lines.2) %>%
        map(~sum(strsplit(.x, split="")[[1]] == sep)) %>%
        unlist %>%
        unique
    }) %>%
      keep(~length(.x) == 1 && .x != 0) %>%
      unlist
  }) %>%
    discard(is.null)

  if (length(format) == 0) {
    format <- list(sep = NULL, quote = NULL)
  } else if (length(format) == 1) {
    format <- list(sep = names(format[[1]]), quote = names(format))
  } else {
    format <- list(sep = names(format[[1]]), quote = "\"'")
  }

  return(format)
}










importDatasetModal <- function(id) {
  slatesMultiPageModal(id, function(input, output, session) {
    ns <- session$ns

    slate.options <- slateOptions(
      height = "60vh",
      use.card = FALSE,
      inputs.style = "flowing"
    )

    slate.server <- slateServer(
      "preview_slate",
      blueprint = slateBlueprint(),
      slate.options = slate.options
    )


    # get the file info structure from currently loaded file
    fileinfo <- reactive({
      import.from <- input$import_from
      file.data <- input$file_input

      if (is.null(import.from))
        return(NULL)

      if (import.from == "local" && !is.null(file.data)) {
        fileInfo(filename = file.data$name, filepath = file.data$datapath)
      } else {
        return(NULL)
      }
    })


    # get the importer blueprint object matching the currently selected importer name
    blueprint <- reactive({
      req(importer <- input$importer)

      blueprint.filename <- file.path(
        getOption("rslates.blueprints")$directory,
        getOption("rslates.importers")[[ importer ]]$blueprint
      )

      loadBlueprint(blueprint.filename)
    })


    # update the slate server when blueprint changes
    observeEvent(blueprint(), {
      req(
        fileinfo <- fileinfo(),
        blueprint <- blueprint()
      )

      # setup input values for importer blueprint
      input.values <- list(
        name = makeVarnameFromFilename(fileinfo$name)
      )

      blueprint$inputs %<>% assignInputValues(input.values)

      # update the slate
      slate.server$updateBlueprint(blueprint)

      # update file information
      slate.server$import.values$fileinfo <- fileinfo
    })


    # get parameter autodetection for currently loaded dataset and importer
    autodetection <- reactive({
      req(
        fileinfo <- fileinfo(),
        importer <- input$importer
      )

      if (importer == "tabular")
        return(detectTabularFormat(fileinfo$datapath))
      else
        return(NULL)
    })


    # render file summary table
    output$file_summary <- renderTable({
      req(fileinfo <- fileinfo())

      x <- fileinfo[ c("name", "size", "extension") ]

      data.frame(Property = names(x), Value = unlist(x))
    }, colnames = FALSE, rownames = FALSE)


    # update importer select input choices based on file format
    observeEvent(fileinfo(), {
      req(fileinfo <- fileinfo())

      importers <- getImporters(fileinfo$extension) %>%
        map_chr("name") %>%
        set_names(names(.), .)

      updateSelectInput(session, "importer", choices = importers)
    })


    # render the text description of the currently selected importer
    output$importer_description <- renderText({
      req(importer <- input$importer)

      getOption("rslates.importers")[[ importer ]]$description
      # importers <- getOption("rslates.importers")$datatypes
      # w <- match(importer.name, importers %>% map("name"))
      #
      # importers[[ w ]]$description
    })


    # file type details
    output$datatype_summary <- renderTable({
      req(autodetection <- autodetection())

      autodetection %<>% keep(~!is.null(.))

      req(length(autodetection) > 0)

      data.frame(Property = names(autodetection),
                 Value = unlist(autodetection))
    }, colnames = FALSE, rownames = FALSE)


    # render the dataset summary (right side of page 1)
    output$dataset_summary <- renderUI({
      fileinfo <- fileinfo()

      if (is.null(fileinfo)) {
        div(class = "d-flex h-100",
            id = ns("panel_no_data"),
            div(
              class = "text-muted text-left m-auto w-75",
              tags$p("Please import a dataset. Supported file types include: "),
              #tags$br(),
              tags$div(
                class = "text-left",
                tags$li("Tabular data: csv, tsv, txt"),
                tags$li("Spreadsheet data: xls, xlsx, odf"),
                tags$li("Raw Text: txt"),
                tags$li("R Objects: RData, rda, rdf"),
                tags$li("Serialized objects: json, yaml, yml")
              )
            )
        )
      } else {
        div(class = "d-flex flex-column h-100",
            id = ns("panel_data_info"),
            tableOutput(ns("file_summary")),
            tagAppendAttributes(
              selectInput(ns("importer"), label = "Import As", choices = NULL),
              class = "mb-0"
            ),
            tags$i(textOutput(ns("importer_description")))
            #tags$br(),
            #tableOutput(ns("datatype_summary"))
        )
      }
    })


    pages <- list(
      page1 = function() {
        tags$div(
          class = "row px-4",
          style = "height: 60vh;",
          tags$div(
            class = "col-md-5",
            shinyWidgets::radioGroupButtons(
              ns("import_from"),
              label = "Import from",
              choices = c("Local File"="local", "URL"="url", "Built-in Dataset"="builtin")),
            conditionalPanel(
              condition = sprintf("input['%s'] === 'local'", ns("import_from")),
              helpText("Import a dataset from a file on your computer."),
              slatesFileInput(
                ns("file_input"),
                label = "File",
                accept = paste0(".", getValidExtensions())
              )
            )
          ),
          tags$div(
            class = "col-md-7",
            class = "h-100",
            uiOutput(ns("dataset_summary"), class = "h-100")
          )
        )
      },
      page2 = function() {
        tags$div(
          style = "min-height: 60vh;",
          slateUI(ns("preview_slate"), slate.options = slate.options)
        )
      }
    )


    validators <- list(
      page1 = function() {
        import.from <- input$import_from
        fileinfo <- fileinfo()

        if (is.null(import.from))
          return(FALSE)

        if (import.from %in% c("local", "url")) {
          return(!is.null(fileinfo))
        } else {
          return(FALSE)
        }
      },
      page2 = function() {
        data <- slate.server$export.data()

        !is.null(data) && length(data) == 1 && names(data)[1] != ""
      }
    )


    submit <- function() {
      data <- slate$export.data

      dataset <- list(
        name = names(data)[1],
        type = "tabular",
        fileinfo = fileinfo(),
        data = data[[1]],
        slate.data = slate.server$slate.data()
      )

      return(list(dataset = dataset))
    }


    list(
      pages = pages,
      validators = validators,
      submit = submit
    )
  }, default.size = "xl")
}









#
# # The import dataset modal dialog
# importDatasetModal <- function(id, session) {
#   ID <- function(x) paste0(id, "_", x)
#   ns <- session$ns
#   input <- session$input
#   output <- session$output
#
#   observers <- list()
#
#   slate.options <- slateOptions(
#     height = "60vh",
#     use.card = FALSE,
#     inputs.style = "flowing"
#   )
#
#   slate.server <- slateServer(
#     ID("preview_slate"),
#     blueprint = getOption("rslates.import.blueprints")[[ "CSV Import" ]],
#     slate.options = slate.options
#   )
#
#   file.info <- reactive({
#     import.from <- input[[ ID("import_from") ]]
#     file.data <- input[[ ID("file_input") ]]
#
#     if (is.null(import.from))
#       return(NULL)
#
#     if (import.from == "local" && !is.null(file.data)) {
#       list(origin = "local",
#            name = file.data$name,
#            size = file.data$size,
#            extension = sub(".*\\.(.*$)", "\\1", file.data$name),
#            type = file.data$type,
#            datapath = file.data$datapath
#       )
#     } else {
#       return(NULL)
#     }
#   })
#
#
#   autodetection.info <- reactive({
#     req(
#       file.info <- file.info(),
#       datatype <- input[[ ID("datatype") ]]
#     )
#
#     if (datatype == "Tabular")
#       return(detectTabularFormat(file.info$datapath))
#     else
#       return(list())
#   })
#
#
#   blueprint <- reactive({
#     req(file.info <- file.info())
#
#     # if blablala
#
#   })
#
#
#     # # get the importer blueprint
#     # blueprint.name <- getFileTypes()$tabular$module
#     # blueprint <- getOption("rslates.import.blueprints")[[ blueprint.name ]]
#     #
#     # # generate a proposed dataset name
#     # name.parts <- sub("\\..*$", "", file.info$name) %>%
#     #   strsplit(split = "[^a-zA-Z0-9]") %>%
#     #   pluck(1)
#     #
#     # idx <- nchar(name.parts) %>% { which(cumsum(.) <= 15) }
#     # if (length(idx) != 0) {
#     #   dataset.name <- name.parts[idx] %>% paste(collapse="_")
#     # } else {
#     #   dataset.name <- substr(fileinfo$name, 1, 15)
#     # }
#     #
#     # # setup the blueprint initial values
#     # if (file.info$extension == "csv") {
#     #   auto <- autodetection.info()
#     #
#     #   auto$name <- dataset.name
#     #
#     #   blueprint$inputs %<>% assignInputValues(auto)
#     # }
#     #
#     # dlog(blueprint)
#     #
#     # # update the slate
#     # slate.server$updateBlueprint(blueprint)
#     # slate.server$import.values$fileinfo <- file.info
#   #})
#
#
#   isFileSupported <- reactive({
#     file.info <- file.info()
#
#     if (is.null(file.info))
#       return(FALSE)
#
#     file.info$extension %in% getValidExtensions()
#   })
#
#
#   # file summary table
#   output[[ ID("file_summary") ]] <- renderTable({
#     dlog()
#     req(file.info <- file.info())
#
#     x <- file.info[ c("name", "size", "extension") ]
#
#     data.frame(Property = names(x), Value = unlist(x))
#   }, colnames = FALSE, rownames = FALSE)
#
#
#   # select file format
#   observeEvent(file.info(), {
#     dlog()
#     req(file.info <- file.info())
#
#     importers <- getImporters(file.info$extension) %>% map_chr("name")
#
#     updateSelectInput(session, ID("datatype"), choices = importers)
#   })
#
#
#   # file type details
#   output[[ ID("datatype_summary") ]] <- renderTable({
#     dlog()
#     req(autodetection.info <- autodetection.info())
#
#     x <- autodetection.info
#
#     data.frame(Property = names(x), Value = unlist(x))
#   }, colnames = FALSE, rownames = FALSE)
#
#
#   output[[ ID("dataset_summary") ]] <- renderUI({
#     file.info <- file.info()
#
#     if (is.null(file.info)) {
#       div(class = "d-flex h-100",
#           id = ns(ID("panel_no_data")),
#           div(
#             class = "text-muted text-center m-auto w-50",
#             "Please import a dataset. Click here for a list of supported file types."
#           )
#       )
#     } else {
#       div(class = "d-flex flex-column h-100",
#           id = ns(ID("panel_data_info")),
#           tableOutput(ns(ID("file_summary"))),
#           selectInput(ns(ID("datatype")), label = "Import Type", choices = NULL),
#           tableOutput(ns(ID("datatype_summary")))
#       )
#     }
#   })
#
#   # # summary table
#   # output[[ ID("data_summary") ]] <- renderUI({
#   #   file.info <- file.info()
#   #
#   #   if (is.null(file.info)) {
#   #     return(
#   #       div(class = "d-flex h-100",
#   #           div(
#   #             class = "text-muted text-center m-auto w-50",
#   #             "Please import a dataset. Click here for a list of supported file types."
#   #           )
#   #       )
#   #     )
#   #   }
#   #
#   #   # make the styled tables for file infos
#   #   makeTable <- function(df,
#   #                         widths = list(.rownames = 150,
#   #                                       Value = 250,
#   #                                       Status = 50)) {
#   #     theme <- autoReactableTheme(
#   #       options = list(headerStyle = list(display = "none"))
#   #     )
#   #
#   #     reactable::reactable(
#   #       df,
#   #       sortable = FALSE,
#   #       outlined = FALSE,
#   #       borderless = TRUE,
#   #       compact = TRUE,
#   #       theme = theme,
#   #       columns = list(
#   #         .rownames = reactable::colDef(
#   #           minWidth = widths$row.names,
#   #           align = "right",
#   #           style = list(fontWeight = 400)),
#   #         Value = reactable::colDef(
#   #           minWidth = widths$Value,
#   #           style = list(fontWeight = 300),
#   #           name = ""),
#   #         Status = reactable::colDef(
#   #           minWidth = widths$Status,
#   #           name = "Status",
#   #           cell = function(status) {
#   #             switch(
#   #               status,
#   #               "ok" = tags$div(class = "text-success", icon("check")),
#   #               "question" = tags$div(class = "text-primary", icon("question")),
#   #               "error" = tags$div(class = "text-danger", icon("times"))
#   #             )
#   #           })
#   #       )
#   #     ) %>% tagList
#   #   }
#   #
#   #   # get extension and file type
#   #   ext <- file.info$extension
#   #   file.type <- getFileType(ext)
#   #
#   #   if (file.type != "") {
#   #     type.description <- paste0(file.type, " (", ext, ")")
#   #     type.status <- "ok"
#   #   } else {
#   #     type.description <- paste("Unsupported file extension:", ext)
#   #     type.status <- "error"
#   #   }
#   #
#   #   file.df <- data.frame(
#   #     row.names = c("Filename:", "Size:", "File Type:"),
#   #     Value = c(file.info$name,
#   #               utils:::format.object_size(file.info$size, units = "auto"),
#   #               type.description),
#   #     Status = c("ok", "ok", type.status)
#   #   )
#   #
#   #   file.reactable <- makeTable(file.df)
#   #   details.reactable <- NULL
#   #
#   #   # Auto-detection results
#   #   if (file.type == "Tabular") {
#   #     format <- detectTabularFormat(file.info$datapath)
#   #
#   #     details.df <- data.frame(
#   #       row.names = c("Separator:", "Quote Type:"),
#   #       Value = c(if (!is.null(format$sep)) format$sep else "Undetermined",
#   #                 if (!is.null(format$quote)) format$quote else "Undetermined"),
#   #       Status = c(if (!is.null(format$sep)) "ok" else "error",
#   #                  if (!is.null(format$quote)) "ok" else "question"))
#   #
#   #     details.reactable <- makeTable(details.df)
#   #   }
#   #
#   #   # finally make the ui
#   #   if (!is.null(details.reactable)) {
#   #     tagList(
#   #       file.reactable,
#   #       # tags$div(
#   #       #   class = "w-100 text-center",
#   #       #   tags$b(paste(type.description))
#   #       # ),
#   #       br(),
#   #       details.reactable
#   #     )
#   #   } else {
#   #     file.reactable
#   #   }
#   # })
#
#   pages.ui <- list(
#     function() {
#       tags$div(
#         class = "row px-4",
#         style = "height: 60vh;",
#         tags$div(
#           class = "col-md-5",
#           shinyWidgets::radioGroupButtons(
#             ns(ID("import_from")),
#             label = "Import from",
#             choices = c("Local File"="local", "URL"="url", "Built-in Dataset"="builtin")),
#           conditionalPanel(
#             condition = sprintf("input['%s'] === 'local'", ns(ID("import_from"))),
#             helpText("Import a dataset from a file on your computer."),
#             slatesFileInput(
#               ns(ID("file_input")),
#               label = "File",
#               accept = paste0(".", getValidExtensions())
#             )
#           )
#         ),
#         tags$div(
#           class = "col-md-7",
#           class = "h-100",
#           uiOutput(ns(ID("dataset_summary")), class = "h-100")
#         )
#       )
#     },
#     function() {
#       tags$div(
#         style = "min-height: 60vh;",
#         slateUI(ns(ID("preview_slate")), slate.options = slate.options)
#         #uiOutput(ns(ID("slate_ui")))
#       )
#     }
#   )
#
#   validators <- list(
#     function() {
#       import.from <- input[[ ID("import_from") ]]
#
#       if (is.null(import.from))
#         return(FALSE)
#
#       if (import.from %in% c("local", "url")) {
#         return(isFileSupported())
#       } else {
#         return(FALSE)
#       }
#     },
#     function() {
#       data <- slate.server$export.data()
#
#       !is.null(data) && length(data) == 1 && names(data)[1] != ""
#     }
#   )
#
#   submit.fun <- function() {
#     data <- slate.server$export.data()
#
#     dataset <- list(
#       name = names(data)[1],
#       type = "tabular",
#       file.info = file.info(),
#       data = data[[1]]
#       #import.blueprint =
#     )
#
#     return(list(dataset = dataset))
#   }
#
#   slatesModalMultiPage(id, session,
#                        pages.ui = pages.ui,
#                        submit.fun = submit.fun,
#                        validators = validators,
#                        default.size = "xl",
#                        observers = observers
#   )
# }
#
#










# file type information
getFileTypes <- function() {
  list(
    tabular = list(
      name = "Tabular",
      extensions = c("txt", "csv", "tsv"),
      module = "CSV Import"
    ),
    spreadsheet = list(
      name = "Spreadsheet",
      extensions = c("xlsx", "xls")
    )
  )
}

getFileType <- function(extension) {
  for (type in getFileTypes())
    if (extension %in% type$extensions)
      return(type$name)

  return("")
}

getValidExtensions <- function() {
  sapply(getFileTypes(), "[[", "extentions") %>% unlist()
}




detectTabularFormat <- function(filename, nlines = 50, comment.char = "") {
  lines <- readLines(filename, nlines)
  separators <- c("Space"=" ", "Tab"="\t", "Comma"=",", "Semicolon"=";")
  comment.chars <- c("#", "%", "$")
  quotation.marks <- c("Single Quote'=", "Double Quote"='"')

  # for each separator get the unique counts in lines
  # then discard separators that never show up
  # and order by separators containing the least amount of different
  # number of occurrences per line

  format <- lapply(quotation.marks, function(q) {
    sapply(separators, function(sep) {
      gsub(sprintf("(%s.*?)%s(.*?%s)", q, sep, q), "\\1!\\2", lines) %>%
        map(~sum(strsplit(.x, split="")[[1]] == sep)) %>%
        unlist %>%
        unique
    }) %>%
      keep(~length(.x) == 1 && .x != 0) %>%
      unlist
  }) %>%
    discard(is.null)

  if (length(format) == 0) {
    format <- list(sep = "", quote = "")
  } else if (length(format) == 1) {
    format <- list(sep = names(format[[1]]), quote = names(format))
  } else {
    format <- list(sep = names(format[[1]]), quote = "")
  }

  return(format)
}



# The import dataset modal dialog
importDatasetModal <- function(id, session) {
  ID <- function(x) paste0(id, "_", x)
  ns <- session$ns
  input <- session$input
  output <- session$output

  slate.server <- reactiveVal(NULL)


  # observe({
  #   req(data <- data())
  #
  #   server <- isolate(slate.server())
  #
  #   if (!is.null(server))
  #     server$destroy()
  #
  #   blueprint.name <- blueprintFromJSON(getFileTypes()$tabular$module)
  #   blueprint <- getOption("rslates.importer.blueprints")[[ blueprint.name ]]
  #   slate.options <- slateOptions()
  #   server <- slateServer(ID("slate"), blueprint, slate.options)
  # })


  data <- reactive({
    # import.from <- input[[ ID("import_from") ]]
    # file.data <- input[[ ID("file_input") ]]
    #
    # if (is.null(import.from))
    #   return(NULL)
    #
    # if (import.from == "local" && !is.null(file.data)) {
    #   list(origin = "local",
    #        name = file.data$name,
    #        size = file.data$size,
    #        extention = gsub(".*\\.(.*$)", "\\1", name)
    #        type = file.data$type,
    #        path = file.data$datapath
    #   )
    # } else {
    #   return(NULL)
    # }

    # test data
    list(origin = "local",
         name = "WHO COVID-19 global table data February 25th 2021 at 3.31.34 PM.csv",
         size = file.size("C:\\Users\\daniel\\Downloads\\WHO COVID-19 global table data February 25th 2021 at 3.31.34 PM.csv"),
         type = "application/vnd.ms-excel",
         extention = "csv",
         path = "C:\\Users\\daniel\\Downloads\\WHO COVID-19 global table data February 25th 2021 at 3.31.34 PM.csv"
    )

  })


  isFileSupported <- reactive({
    data <- data()

    if (is.null(data))
      return(FALSE)

    ext <- gsub(".*\\.(.*$)", "\\1", data$name)
    file.type <- getFileType(ext)

    !is.null(file.type)
  })


  output[[ ID("slate_ui") ]] <- renderUI({
    data <- data()

    if (is.null(data))
      return()

    blueprint <- blueprintFromJSON(getFileTypes()$tabular$module)

    slate.options <- slateOptions(
      height = "60vh",
      use.card = FALSE,
      inputs.style = "flowing"
    )

    slateUI(ns(ID("slate")), blueprint, slate.options)
  })


  # summary table
  output[[ ID("data_summary") ]] <- renderUI({
    data <- data()

    if (is.null(data)) {
      return(
        div(class = "d-flex h-100",
          div(
            class = "text-muted text-center m-auto w-50",
            "Please import a dataset. Click here for a list of supported file types."
          )
        )
      )
    }

    # make the styled tables for file infos
    makeTable <- function(df,
                          widths = list(.rownames = 150,
                                        Value = 250,
                                        Status = 50)) {
      theme <- autoReactableTheme(
        options = list(headerStyle = list(display = "none"))
      )

      reactable::reactable(
        df,
        sortable = FALSE,
        outlined = FALSE,
        borderless = TRUE,
        compact = TRUE,
        theme = theme,
        columns = list(
          .rownames = reactable::colDef(
            minWidth = widths$row.names,
            align = "right",
            style = list(fontWeight = 400)),
          Value = reactable::colDef(
            minWidth = widths$Value,
            style = list(fontWeight = 300),
            name = ""),
          Status = reactable::colDef(
            minWidth = widths$Status,
            name = "Status",
            cell = function(status) {
              switch(
                status,
                "ok" = tags$div(class = "text-success", icon("check")),
                "question" = tags$div(class = "text-primary", icon("question")),
                "error" = tags$div(class = "text-danger", icon("times"))
              )
            })
        )
      ) %>% tagList
    }

    # get extension and file type
    ext <- data$extention
    file.type <- getFileType(ext)

    if (file.type != "") {
      type.description <- paste0(file.type, " (", ext, ")")
      type.status <- "ok"
    } else {
      type.description <- paste("Unsupported file extension:", ext)
      type.status <- "error"
    }

    file.df <- data.frame(
      row.names = c("Filename:", "Size:", "File Type:"),
      Value = c(data$name,
                utils:::format.object_size(data$size, units = "auto"),
                type.description),
      Status = c("ok", "ok", type.status)
    )

    file.reactable <- makeTable(file.df)
    details.reactable <- NULL

    # Auto-detection results
    if (file.type == "Tabular") {
      format <- detectTabularFormat(data$path)

      details.df <- data.frame(
        row.names = c("Separator:", "Quote Type:"),
        Value = c(if (format$sep != "") format$sep else "Undetermined",
                  if (format$quote != "") format$quote else "Undetermined"),
        Status = c(if (format$sep != "") "ok" else "error",
                   if (format$quote != "") "ok" else "question"))

      details.reactable <- makeTable(details.df)
    }

    # finally make the ui
    if (!is.null(details.reactable)) {
      tagList(
        file.reactable,
        # tags$div(
        #   class = "w-100 text-center",
        #   tags$b(paste(type.description))
        # ),
        br(),
        details.reactable
      )
    } else {
      file.reactable
    }
  })


  pages.ui <- list(
    function() {
      tags$div(
        class = "row px-4",
        style = "height: 60vh;",
        tags$div(
          class = "col-md-5",
          shinyWidgets::radioGroupButtons(
            ns(ID("import_from")),
            label = "Import from",
            choices = c("Local File"="local", "URL"="url", "Built-in Dataset"="builtin")),
          conditionalPanel(
            condition = paste0("input[ '", ns(ID("import_from")), "'] === 'local'"),
            helpText("Import a dataset from a file on your computer."),
            slatesFileInput(ns(ID("file_input")), label = "File")
          )
        ),
        tags$div(
          class = "col-md-7",
          uiOutput(ns(ID("data_summary")), class = "h-100")
        )
      )
    },
    function() {
      tags$div(
        style = "min-height: 60vh;",
        uiOutput(ns(ID("slate_ui")))
      )
    }
  )

  validators <- list(
    function() {
      import.from <- input[[ ID("import_from") ]]

      if (is.null(import.from))
        return(FALSE)

      if (import.from %in% c("local", "url")) {
        return(isFileSupported())
      } else {
        return(FALSE)
      }
    },
    function() {
      TRUE
    }
  )

  submit.fun <- function() {
    list(data = data())
  }

  slatesModalMultiPage(id, session,
                       pages.ui = pages.ui,
                       submit.fun = submit.fun,
                       validators = validators,
                       default.size = "xl"
  )
}





#' Editor Panel UI Function
#'
#' @param id the panel id.
#'
#' @return The HTML representing the panel.
projectEditorUI <- function(id, project) {
  ns <- NS(id)

  code.ui <-
    fluidRow(
      tags$style(type='text/css', '#txt_out {white-space: pre-wrap;}'),
      tags$head(tags$style(paste0("#", ns("output_global"), "{ overflow-y:scroll; max-height: 400px; }"))),
      column(6,
             shinyAce::aceEditor(
               ns("ace_global"),
               mode = "r",
               autoComplete = "live",
               height = "400px",
               selectionId = "selection_global",
               value = "",
               placeholder = "Enter global scope code here...",
               hotkeys = list(
                 run_key = list(
                   win = "Ctrl-Shift-Enter",
                   mac = "CMD-ENTER|CMD-SHIFT-ENTER"
                 )
               )
             ),
             actionButton(ns("eval_global"), "Evaluate")
      ),
      column(6,
             verbatimTextOutput(ns("output_global"), placeholder = TRUE)
      )
    )

  # section.div <- function(...) {
  #   tags$div(
  #     style = "filter: drop-shadow(0px 18px 8px #00000011);",
  #     tags$div(
  #       class = "bg-light px-5 py-4",
  #       ...
  #     ),
  #     tags$div(
  #       class = "bg-light slanted-bottom-40-rev",
  #       style = "height: 50px;"
  #     )
  #   )
  # }

  header.section <- tags$div(
    style = "filter: drop-shadow(0px 18px 8px #00000011);",
    tags$div(
      class = "bg-light px-5 py-4",
      #uiOutput(ns("header_ui"))
      div(class = "slates-flow-3",
        textInput(ns("project_title"), label = "Title", value = project$title),
        textInput(ns("project_authors"), label = "Author(s)", value = project$authors),
        dateInput(ns("project_date"), label = "Date", value = project$data.created)
      )
    )
    # ),
    # tags$div(
    #   class = "bg-light slanted-bottom-40-rev",
    #   style = "height: 50px;"
    # )
  )

  data.section <- tags$div(
    style = "filter: drop-shadow(0px 18px 8px #00000011); position: relative;",
    tags$div(
      class = "bg-light container px-4 py-3",
      tags$a(class = "h4",
             `data-toggle` = "collapse",
             href = "#data_section_contents",
             "Project Data"
      ),
      div(
        class = "collapse show mt-3",
        id = "data_section_contents",
        div(
          class = "d-flex",
          actionButton(ns("add_dataset"),
                       label = "Import Dataset", icon = icon("file-import")),
          shinyjs::disabled(
            actionButton(ns("add_dataset"),
                         label = "Add Transformation", icon = icon("code-branch"),
                         class="ml-3"))
        ),
        tags$hr(),
        tags$div(
          tags$p(
            class = "text-muted",
            "No data. Use the buttons above to import a dataset!"),
        ),
        div(id = "datasets_begin"),
        div(id = "datasets_end")
      )
    )
  )

  analysis.section <- tags$div(
    style = "filter: drop-shadow(0px 18px 8px #00000011); position: relative;",
    class = "bg-light container px-4 py-3",
    tags$a(class = "h4",
           `data-toggle` = "collapse",
           href = "#analysis_section_contents",
           "Analysis"
    ),
    div(
      class = "collapse show mt-3",
      id = "analysis_section_contents",
      div(
        class = "d-flex",
        actionButton(ns("add_slate"),
                     label = "New Chunk", icon = icon("plus")),
      ),
      tags$hr(),
      tags$div(
        tags$p(
          class = "text-muted",
          "No chunks. Use the buttons above to create a new chunk!"
        ),
        div(id = "slates_begin"),
        div(id = "slates_end")
      )
    )
  )


  tags$div(
    verticalLayout(
      header.section,
      br(),
      data.section,
      br(),
      analysis.section
    )
  )
}


projectEditorServer <- function(id, project, session.data, global.options) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    modals <- list(
      #select.input = create_select_input_modal("select_input_modal", session),
      file.import = create_file_import_modal("file_import_modal", session)
    )

    #session.data$project.envir <- new.env()
    #global.envir <- reactiveVal(new.env())

    code.changed <- reactiveVal(FALSE)

    # list of slates
    datasets <- reactiveValues()
    slates <- reactiveValues()

    # keep track of the time of last modification
    # TODO: avoid setting it while initializing the module
    date.modified <- reactiveVal(project$date.modified)

    observe({
      shinyAce::updateAceEditor(session, "ace_global", theme = global.options$ace.theme)
    })

    # Source code section
    observe({
      input$ace_global

      code.changed(TRUE)
    })


    #
    # header section
    #
    # output$header_ui <- renderUI({
    #   verticalLayout(
    #     textInput(session$ns("project_title"), label = "Title", value = project$title),
    #     textInput(session$ns("project_authors"), label = "Author(s)", value = project$authors),
    #     dateInput(session$ns("project_date"), label = "Date", value = project$data.created)
    #   )
    # })



    # observe({
    #   code.changed
    #
    #   status <- "success"
    #   if (code.changed() == TRUE) {
    #     status <- "warning"
    #   }
    #
    #   shinydashboardPlus::updateBox("box_global", action = "update", options = list(status = status))
    # })

    output$output_global <- renderPrint({
      input$eval_global
      input$ace_global_run_key

      code.changed(FALSE)

      global.envir(new.env())

      eval(parse(text = isolate(input$ace_global)), env = global.envir())
    })


    #
    # Datasets
    #

    # Modal dialog to import a dataset
    import.modal <- importDatasetModal("import_dataset", session)


    # observe Add Slab buttons
    observeEvent(input$add_dataset, {
      blueprints <- session.data$data.blueprints

      import.modal$show(
        title = "Import Dataset",
        callback = function(data) {
          print(data)

        }
      )

      # modals$file.import$show(function(res) {
      #   print(res)
      #   #add_data_slate(blueprints[[ value ]], ask.title = FALSE)
      # })
    })

    # outputOptions(output, "output_global", suspendWhenHidden = FALSE)


    addDataset <- function(blueprint, ask.title = TRUE) {
      dataset.list <- reactiveValuesToList(datasets)
      id <- seq.uid("dataset")

      insertUI(selector = "#data_slates_end",
               where = "beforeBegin",
               ui = dataSlateUI(ns(id), blueprint))

      datasets[[ id ]] <- callModule(dataSlateServer, id, blueprint, global.envir, open.settings = TRUE)
    }




    #
    # Analysis
    #

    add.slate.modal <- slatesSelectModal(ns("add_slate_modal"), session)

    # observe add slate button
    observeEvent(input$add_slate, {
      dlog()

      blueprints <- session.data$blueprints

      dlog(names(blueprints))

      #modals$select.input$show("Select blueprint", names(blueprints), function(value) {
      add.slate.modal$show(
        label = "Select Blueprint:",
        choices = names(blueprints),
        callback = function(value) {
          addSlate(blueprints[[ value ]])
        })
    })


    addSlate <- function(blueprint) {
      slate.list <- reactiveValuesToList(slates)
      id <- seq.uid("slate")  # TODO: substitute this

      slate.options <- slateOptions(
        envir = session.data$project.envir,
        open.inputs = TRUE
      )

      server <- slateServer(
        id,
        blueprint,
        slate.options = slate.options,
        global.options = global.options
      )

      # if (!is.null(input.values)) {
      #   blueprint$input.list <- lapply(blueprint$input.list, function(x) {
      #     value <- input.values[[ x$name ]]
      #
      #     if (!is.null(value)) {
      #       x$value <- value
      #     }
      #
      #     return (x)
      #   })
      # }
      #

      insertUI(
        selector = "#slates_end",
        where = "beforeBegin",
        ui = slateUI(
          ns(id),
          blueprint,
          slate.options = slate.options
        )
      )

      slates[[ id ]] <- server
    }





    #
    #
    # #
    # # Slates
    # #
    # slates <- reactiveValues()
    #
    # add_slate <- function(blueprint, input.values = NULL, open.settings = TRUE) {
    #   slate_list <- reactiveValuesToList(slates)
    #   id <- paste0("Slate_", length(slate_list) + 1)
    #
    #   if (!is.null(input.values)) {
    #     blueprint$input.list <- lapply(blueprint$input.list, function(x) {
    #       value <- input.values[[ x$name ]]
    #
    #       if (!is.null(value)) {
    #         x$value <- value
    #       }
    #
    #       return (x)
    #     })
    #   }
    #
    #   insertUI(selector = "#slates_end",
    #            where = "beforeBegin",
    #            ui = editr_slateUI(ns(id), blueprint))
    #
    #   slates[[ id ]] <- callModule(editr_slateServer, id, blueprint, global.envir, open.settings)
    # }
    #
    # # observe add slate button
    # observeEvent(input$btn_add_slate, {
    #   blueprints <- session.data$slate.blueprints
    #   names(blueprints) <- sapply(blueprints, "[[", "title")
    #
    #   modals$select.input$show("Select blueprint", names(blueprints), function(value) {
    #     add_slate(blueprints[[ value ]], input.values = NULL, open.settings = TRUE)
    #   })
    # })
    #
    # # observe({
    # #   blueprint <- session.data$slate.blueprints$slate_scatterplot
    # #
    # #   isolate(add_slate(blueprint, open.settings = FALSE))
    # # })
    #
    # isolate({
    #   for (slate in project.slates) {
    #     print(paste("adding slate:", slate$blueprint.name))
    #
    #     blueprint <- session.data$slate.blueprints[[ slate$blueprint.name ]]
    #     add_slate(blueprint, slate$input.value, open.settings = FALSE)
    #   }
    # })
    #
    # # create and return the project object
    # project <- reactive({
    #   list(
    #     # a random string
    #     project.uid <- project.uid,
    #     project.name <- project.name
    #   )
    # })
    #
    # session.data$slates <- slates
    # session.data$project.name <- project.name
    # session.data$global.envir <- global.envir
    #
    # return (session.data)





    observe({
      input$ace_global

      date.modified(Sys.time())
    })


    project$state <- reactive({
      list(
        date.modified = date.modified(),
        global.code = input$ace_global
      )
    })


    reactive.project <- reactive({
      list(
        title = title,
        date.modified = date.modified(),
        global.code = input$ace_global
      )
    })


    return(project)
  })
}




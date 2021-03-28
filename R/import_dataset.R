

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
  sapply(getFileTypes(), "[[", "extensions") %>% unlist()
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
    format <- list(sep = NULL, quote = NULL)
  } else if (length(format) == 1) {
    format <- list(sep = names(format[[1]]), quote = names(format))
  } else {
    format <- list(sep = names(format[[1]]), quote = NULL)
  }

  return(format)
}



# The import dataset modal dialog
importDatasetModal <- function(id, session) {
  ID <- function(x) paste0(id, "_", x)
  ns <- session$ns
  input <- session$input
  output <- session$output

  observers <- list()

  slate.options <- slateOptions(
    height = "60vh",
    use.card = FALSE,
    inputs.style = "flowing"
  )

  slate.server <- slateServer(
    ID("preview_slate"),
    blueprint = getOption("rslates.importer.blueprints")[[ "CSV Import" ]],
    slate.options = slate.options
  )


  file.info <- reactive({
    import.from <- input[[ ID("import_from") ]]
    file.data <- input[[ ID("file_input") ]]

    if (is.null(import.from))
      return(NULL)

    if (import.from == "local" && !is.null(file.data)) {
      list(origin = "local",
           name = file.data$name,
           size = file.data$size,
           extension = sub(".*\\.(.*$)", "\\1", file.data$name),
           type = file.data$type,
           datapath = file.data$datapath
      )
    } else {
      return(NULL)
    }
  })


  autodetection.info <- reactive({
    req(file.info <- file.info())

    if (file.info$extension == "csv")
      return(detectTabularFormat(file.info$datapath))
    else
      return(NULL)
  })


  observeEvent(file.info(), {
    req(file.info <- file.info())

    dlog()

    # get the importer blueprint
    blueprint.name <- getFileTypes()$tabular$module
    blueprint <- getOption("rslates.importer.blueprints")[[ blueprint.name ]]

    # generate a proposed dataset name
    name.parts <- sub("\\..*$", "", file.info$name) %>%
      strsplit(split = "[^a-zA-Z0-9]") %>%
      pluck(1)

    idx <- nchar(name.parts) %>% { which(cumsum(.) <= 15) }
    if (length(idx) != 0) {
      dataset.name <- name.parts[idx] %>% paste(collapse="_")
    } else {
      dataset.name <- substr(fileinfo$name, 1, 15)
    }

    # setup the blueprint initial values
    if (file.info$extension == "csv") {
      auto <- autodetection.info()

      auto$name <- dataset.name

      blueprint$inputs %<>% assignInputValues(auto)
    }

    # update the slate
    slate.server$updateBlueprint(blueprint)
    slate.server$import.data$fileinfo <- file.info
  })


  isFileSupported <- reactive({
    file.info <- file.info()

    if (is.null(file.info))
      return(FALSE)

    ext <- gsub(".*\\.(.*$)", "\\1", file.info$name)
    file.type <- getFileType(ext)

    !is.null(file.type)
  })


  # summary table
  output[[ ID("data_summary") ]] <- renderUI({
    file.info <- file.info()

    if (is.null(file.info)) {
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
    ext <- file.info$extension
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
      Value = c(file.info$name,
                utils:::format.object_size(file.info$size, units = "auto"),
                type.description),
      Status = c("ok", "ok", type.status)
    )

    file.reactable <- makeTable(file.df)
    details.reactable <- NULL

    # Auto-detection results
    if (file.type == "Tabular") {
      format <- detectTabularFormat(file.info$datapath)

      details.df <- data.frame(
        row.names = c("Separator:", "Quote Type:"),
        Value = c(if (!is.null(format$sep)) format$sep else "Undetermined",
                  if (!is.null(format$quote)) format$quote else "Undetermined"),
        Status = c(if (!is.null(format$sep)) "ok" else "error",
                   if (!is.null(format$quote)) "ok" else "question"))

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
        slateUI(ns(ID("preview_slate")), slate.options = slate.options)
        #uiOutput(ns(ID("slate_ui")))
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
      data <- slate.server$export.data()

      !is.null(data) && length(data) == 1 && names(data)[1] != ""
    }
  )

  submit.fun <- function() {
    data <- slate.server$export.data()

    dataset <- list(
      name = names(data)[1],
      type = "tabular",
      file.info = file.info(),
      data = data[[1]]
    )

    return(list(dataset = dataset))
  }

  slatesModalMultiPage(id, session,
                       pages.ui = pages.ui,
                       submit.fun = submit.fun,
                       validators = validators,
                       default.size = "xl",
                       observers = observers
  )
}



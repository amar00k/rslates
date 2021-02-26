









# file type information
file.types <- list(
  tabular = list(
    name = "Tabular",
    extensions = c("txt", "csv", "tsv"),
    module = system.file("data_blueprints/CSV_Import.json", package = "rslates")
  ),
  spreadsheet = list(
    name = "Spreadsheet",
    extensions = c("xlsx", "xls")
  )
)

valid.extensions <- sapply(file.types, "[[", "extentions") %>% unlist()

getFileType <- function(extension) {
  for (type in file.types)
    if (extension %in% type$extensions)
      return(type$name)

  return(NULL)
}





tabular.helper <- function(filename) {
  separators <- c(" ", "\t", ",", ";")
  lines <- readLines(filename, n = 20)

  # comment character


  # separator
  tab <- sapply(lines, function(l) {
    chars <- strsplit(l, split="")[[1]]
    sapply(separators, function(s) {
      sum(chars == s)
    })
  })

  sep <- apply(tab, 1, unique)
  sep <- sep[ sapply(sep, length) == 1 ]
  sep <- sep[ sapply(sep, "!=", 0) ]

  # decimal


}


# The import dataset modal dialog
importDatasetModal <- function(id, session) {
  ID <- function(x) paste0(id, "_", x)
  ns <- session$ns
  input <- session$input
  output <- session$output

  data <- reactive({
    import.from <- input[[ ID("import_from") ]]
    file.data <- input[[ ID("file_input") ]]

    if (is.null(import.from))
      return(NULL)

    if (import.from == "local" && !is.null(file.data)) {
      list(origin = "local",
           name = file.data$name,
           size = file.data$size,
           type = file.data$type
      )
    } else {
      return(NULL)
    }

    # # test data
    # list(origin = "local",
    #      name = "Test long filename with unknown extension.rdf",
    #      size = 1234567,
    #      type = "blabla"
    # )

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

    blueprint <- blueprintFromJSON(file.types$tabular$module)

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

    # get extension and file type
    ext <- gsub(".*\\.(.*$)", "\\1", data$name)
    file.type <- getFileType(ext)

    if (!is.null(file.type)) {
      file.type <- paste0(file.type, " (", ext, ")")
      type.status <- "ok"
    } else {
      file.type <- paste("Unsupported file extension:", ext)
      type.status <- "error"
    }

    df <- data.frame(
      row.names = c("Filename:", "Size:", "File Type:"),
      Value = c(data$name, utils:::format.object_size(data$size, units = "auto"), file.type),
      Status = c("ok", "ok", type.status)
    )

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
          maxWidth = 100,
          align = "right",
          style = list(fontWeight = 400)),
        Value = reactable::colDef(
          minWidth = 200,
          style = list(fontWeight = 300),
          name = ""),
        Status = reactable::colDef(
          maxWidth = 50,
          name = "Status",
          cell = function(status) {
          switch(
            status,
            "ok" = tags$div(class = "text-success", icon("check")),
            "error" = tags$div(class = "text-danger", icon("times"))
          )
        })
      )
    ) %>% tagList
  })


  pages.ui <- list(
    function() {
      tags$div(
        class = "row px-4",
        style = "height: 40vh;",
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
      select.modal = slatesSelectModal(ns("select_modal"), session),
      file.import = create_file_import_modal("file_import_modal", session)
    )

    global.envir <- reactiveVal(new.env())
    code.changed <- reactiveVal(FALSE)

    # list of slates
    slates <- reactiveValues()
    datasets <- reactiveValues()

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


    addDataset <- function(blueprint, ask.title = TRUE) {
      dataset.list <- reactiveValuesToList(datasets)
      id <- seq.uid("dataset")

      insertUI(selector = "#data_slates_end",
               where = "beforeBegin",
               ui = dataSlateUI(ns(id), blueprint))

      datasets[[ id ]] <- callModule(dataSlateServer, id, blueprint, global.envir, open.settings = TRUE)
    }


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





    #
    # Slates
    #
    addSlate <- function(blueprint, input.values = NULL, open.settings = TRUE) {
      slate.list <- reactiveValuesToList(slates)
      id <- seq.uid("slate")

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

      insertUI(selector = "#slates_end",
               where = "beforeBegin",
               ui = slateUI(ns(id),
                            blueprint,
                            input.container = options()$rslates.input.container))


      slates[[ id ]] <- slateServer(
        id,
        blueprint,
        slate.options = list(
          envir = global.envir,
          open.settings = open.settings
        ),
        global.options = global.options
      )
    }


    # observe add slate button
    observeEvent(input$add_slate, {
      blueprints <- session.data$blueprints

      #modals$select.input$show("Select blueprint", names(blueprints), function(value) {
      modals$select.modal$show(
        label = "Select Blueprint:",
        choices = names(blueprints),
        callback = function(value) {
          addSlate(blueprints[[ value ]], input.values = NULL, open.settings = TRUE)
        })
    })





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




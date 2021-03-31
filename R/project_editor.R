







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
      div(class = "slates-flow slates-flow-3",
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
        # tags$div(
        #   tags$p(
        #     class = "text-muted",
        #     "No data. Use the buttons above to import a dataset!"),
        # ),
        uiOutput(ns("datasets_summary")),
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

    project.data <- reactiveValues(
      envir = new.env()
    )

    #global.envir <- reactiveVal(new.env())

    code.changed <- reactiveVal(FALSE)

    # dataset:
    #   - name
    #   - type ()
    #   - file.info
    #     * origin
    #     * name
    #     * size
    #     * extension
    #     * datapath
    #  - data
    datasets <- reactiveValues()

    # list of slates
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

    datasetSummary <- function(dataset) {
      data <- dataset$data

      if (class(data) == "data.frame") {
        paste0("data.frame (", nrow(data), " obs. of ", ncol(data), " variables)")
      } else {
        class(data)
      }
    }


    datasetCard <- function(dataset) {
      icn <- switch(
        dataset$type,
        "tabular" = {
          "file-csv"
        },
        "dice-d6"
      )

      status <- "primary"
      #bg <- paste0("bg-", status)

      # tags$div(
      #   class = "card dataset-card",
      #   tags$div(
      #     class = "card-header",
      #     icon(icn, class = "fa-2x"),
      #     dataset$name
      #   ),
      #   tags$div(
      #     class = "card-body",
      #     datasetSummary(dataset)
      #   )
      # )

      tags$div(
        class = "card dataset-card d-flex flex-row",
        tags$div(
          class = paste("dataset-icon card-img", paste0("text-", status)),
          icon(icn)
          #tags$span(class="badge badge-secondary", "URL")
        ),
        tags$div(
          class = "p-2",
          tags$div(
            class = "dataset-title",
            dataset$name
          ),
          tags$div(
            class = "dataset-summary",
            datasetSummary(dataset)
          )
        )
      )

    }


    output$datasets_summary <- renderUI({
      datasets <- reactiveValuesToList(datasets)

      if (length(datasets) == 0) {
        return(
          tags$p(
            class = "text-muted",
            "No data. Use the buttons above to import a dataset!"
          )
        )
      }

      dataset.tags <- tags$div(
        class = "d-flex flex-wrap justify-content-between",
        map(datasets, datasetCard)
      )

      # dataset.tags <- map(datasets, ~{
      #   type <- .x$file.info$extension
      #   name <- .x$name
      #
      #   tags$a(
      #     icon("file-csv"), name
      #   )
      # })

      envir.tags <- tags$p(
        class = "text-muted",
        map_chr(ls(project.data$envir), ~{
          paste0(., " (", class(project.data$envir[[.]]), ")")
        }) %>%
          paste(collapse = "; ")
      )

      tagList(
        dataset.tags,
        tags$hr(),
        envir.tags
      )
    })


    # output$output_global <- renderPrint({
    #   input$eval_global
    #   input$ace_global_run_key
    #
    #   code.changed(FALSE)
    #
    #   global.envir(new.env())
    #
    #   eval(parse(text = isolate(input$ace_global)), env = global.envir())
    # })


    #
    # Datasets
    #

    # Modal dialog to import a dataset
    import.modal <- importDatasetModal("import_dataset")


    # observe Add Slab buttons
    observeEvent(input$add_dataset, {
      blueprints <- session.data$data.blueprints

      import.modal$show(
        title = "Import Dataset",
        callback = function(dataset) {
          addDataset(dataset)
        }
      )
    })

    addDataset <- function(dataset) {
      if (is.null(dataset) || dataset$name == "")
        return()

      datasets[[ dataset$name ]] <- dataset

      project.data$envir[[ dataset$name ]] <- dataset$data

      # data <- dataset$data %>%
      #   keep(names(dataset$data) != "")

      # for (name in names(data)) {
      #   project.data$envir[[ name ]] <- data[[name]]
      # }
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
        envir = project.data$envir,
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


    #
    # TEST
    #
    covid.19 <- list(
      origin = "local",
      name = "WHO COVID-19 global table data February 25th 2021 at 3.31.34 PM.csv",
      size = file.size("C:\\Users\\daniel\\Downloads\\WHO COVID-19 global table data February 25th 2021 at 3.31.34 PM.csv"),
      extension = "csv",
      datapath = "C:\\Users\\daniel\\Downloads\\WHO COVID-19 global table data February 25th 2021 at 3.31.34 PM.csv"
    )

    tab1 <- list(
      origin = "local",
      name = "test_tabular_1.csv",
      size = file.size(system.file("test/test_tabular_1.csv", package = "rslates")),
      extension = "txt",
      datapath = system.file("test/test_tabular_1.csv", package = "rslates")
    )

    tab2 <- list(
      origin = "local",
      name = "test_tabular_2.txt",
      size = file.size(system.file("test/test_tabular_2.txt", package = "rslates")),
      extension = "txt",
      datapath = system.file("test/test_tabular_2.txt", package = "rslates")
    )

    isolate({
      addDataset(list(
        name = "who.covid.19",
        type = "tabular",
        file.info = covid.19,
        data = read.table(covid.19$datapath, sep = ",")
      ))

      addDataset(list(
        name = "iris",
        type = "tabular",
        file.info = tab1,
        data = read.table(tab1$datapath, sep = ",")
      ))

      addDataset(list(
        name = "mtcars",
        type = "tabular",
        file.info = tab2,
        data = read.table(tab2$datapath, sep = "\t")
      ))
    })

    return(project)
  })
}




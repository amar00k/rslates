

getValidExtensions <- function() {
  getOption("rslates.importers") %>%
    map("extensions") %>%
    unlist %>%
    unique
}


getImporters <- function(extensions = NULL) {
  importers <- getOption("rslates.importers")

  if (is.null(extensions))
    return(importers)

  keep(importers, ~any(extensions %in% .$extensions))
}

getImporterIcon <- function(type) {
  getOption("rslates.importers")[[ type ]]$icon
}




slateViewerModal <- function(id) {
  slatesModal_(id, function(input, output, session) {
    ns <- session$ns

    slate.options <- slateOptions(
      height = "60vh",
      view.inputs = FALSE,
      use.card = FALSE
    )

    slate.server <- slateServer(
      id = "slate",
      blueprint = slateBlueprint(),
      slate.options = slate.options
    )

    slate.ui <- tags$div(
      style = "height: 60vh;",
      slateUI(id = ns("slate"), slate.options = slate.options)
    )

    ui <- function(blueprint, input.values, import.values) {
      blueprint$inputs %<>% assignInputValues(input.values)

      slate.server$updateBlueprint(blueprint)

      for (name in names(import.values))
        slate.server$import.values[[ name ]] <- import.values[[ name ]]

      slate.ui
    }

    submit <- function() {
      return(NULL)
    }

    return(list(
      ui = ui,
      submit = submit
    ))
  }, default.size = "xl")
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


    extendedDatasetSummary <- function(dataset) {
      data <- dataset$data

      if (class(data) == "data.frame") {
        paste(as.character(summary(data)), collapse = "\n")
      } else {
        class(data)
      }
    }


    datasetCard <- function(dataset) {
      icn <- getImporterIcon(dataset$type)

      status <- "primary"

      id <- ns(paste0(dataset$name, "_card"))

      card <- tags$div(
        # title = dataset$name,
        # `data-toggle` = "popover",
        # `data-content` = extendedDatasetSummary(dataset),
        id = id,
        class = "card dataset-card d-flex flex-row",
        tags$div(
          class = paste("dataset-icon card-img", paste0("text-", status)),
          icon(icn)
          #tags$span(class="badge badge-secondary", "URL")
        ),
        tags$div(
          class = "p-2 w-100",
          tags$div(
            class = "dataset-toolbox float-right",
            icon("eye", class = "dataset-button"),
            icon("pen", class = "dataset-button")
          ),
          tags$div(
            class = "dataset-title",
            dataset$name,
          ),
          tags$div(
            class = "dataset-summary",
            datasetSummary(dataset)
          )
        )
      )

      # card <- shinyBS::popify(
      #   card,
      #   title = dataset$name,
      #   content = as.character(tags$p(datasetSummary(dataset)))
      # )

      return(card)
    }


    # render the datasets list
    # dataset.cards.changed <- reactiveVal(runif(1))
    output$datasets_summary <- renderUI({
      dlog()

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

      envir.tags <- tags$p(
        class = "text-muted",
        map_chr(ls(project.data$envir), ~{
          paste0(., " (", class(project.data$envir[[.]]), ")")
        }) %>%
          paste(collapse = "; ")
      )

      #isolate(dataset.cards.changed(runif(1)))

      tagList(
        dataset.tags,
        tags$hr(),
        envir.tags
      )
    })



    # observe(label = "popovers", {
    #   dlog()
    #
    #   dataset.cards.changed()
    #
    #   invalidateLater(1000)
    #
    #   shinyjs::runjs("$('[data-toggle=\"popover\"]').popover();")
    # })





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
    dataset.viewer.modal <- slateViewerModal("dataset_viewer")

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

      dlog()

      datasets[[ dataset$name ]] <- dataset

      project.data$envir[[ dataset$name ]] <- dataset$data

      shinyjs::onclick(paste0(dataset$name, "_card"), {
        dlog()

        dataset.viewer.modal$show(
          title = dataset$name,
          blueprint = dataset$slate.data$blueprint,
          input.values = dataset$slate.data$input.values,
          import.values = dataset$slate.data$import.values
        )
      })

      # data <- dataset$data %>%
      #   keep(names(dataset$data) != "")
      #
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

      #modals$select.input$show("Select blueprint", names(blueprints), function(value) {
      add.slate.modal$show(
        label = "Select Blueprint:",
        choices = session.data$blueprints,
        callback = function(value) {
          blueprint <- loadBlueprint(value)

          addSlate(blueprint)
        })
    })


    addSlate <- function(blueprint, view.inputs = TRUE) {
      slate.list <- reactiveValuesToList(slates)
      id <- seq.uid("slate")  # TODO: substitute this

      slate.options <- slateOptions(
        envir = project.data$envir,
        view.inputs = view.inputs,
      )

      server <- slateServer(
        id,
        blueprint = blueprint,
        slate.options = slate.options,
        global.options = global.options
      )

      insertUI(
        selector = "#slates_end",
        where = "beforeBegin",
        ui = slateUI(
          ns(id), slate.options = slate.options
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





    loadDatasetFromFile <- function(filename, blueprint, varname) {
      import.values <- list(fileinfo = fileInfo(filename))
      input.values <- list(name = varname)

      slate <- slateData(
        blueprint,
        input.values = input.values,
        import.values = import.values
      )

      data <- getSlateExportData(slate)

      dataset <- list(
        name = names(data)[1],
        type = "tabular",
        fileinfo = import.values$fileinfo,
        data = data[[1]],
        slate.data = slate
      )
    }


    #
    # test
    #
    blueprint.dir <- getOption("rslates.blueprints")$directory

    isolate({
      file1 = "C:\\Users\\daniel\\Downloads\\WHO COVID-19 global table data February 25th 2021 at 3.31.34 PM.csv"

      loadDatasetFromFile(
        filename = file1,
        blueprint = loadBlueprint(file.path(blueprint.dir, getOption("rslates.importers")$tabular$blueprint)),
        varname = makeVarnameFromFilename(file1)
      ) %>% addDataset

    })



    return(project)
  })
}




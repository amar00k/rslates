

capture.session.info <- function(width = 80) {
  # set console width
  opt.width <- options()$width
  options(width = width)

  res <- capture.output(sessionInfo())

  # restore console width
  options(width = opt.width)

  return(res)
}


#' Editor Panel UI Function
#'
#' @param id the panel id.
#'
#' @return The HTML representing the panel.
slates_editUI <- function(id, project.data) {
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

  tags$div(
    verticalLayout(
      tags$br(),

      tags$div(
        id = ns("header_anchor"),
        class = "project-section-anchor",
        `data-toggle` = "collapse",
        href = paste0("#", ns("header_section")),
        "Header"
      ),
      tags$div(id = ns("header_section"),
               class = "collapse show",
               tags$div(
                 class="flex-container baseline",
                 tags$div(class="editor-section-subheader", "Title"),
                 tags$div(class="editor-section-sep"),
                 tags$div(class="editor-section-contents", id="project-title", project.data$title)
               ),
               tags$div(
                 class="flex-container baseline",
                 tags$div(class="editor-section-subheader", "Author(s)"),
                 tags$div(class="editor-section-sep"),
                 tags$div(class="editor-section-contents", id="project-author", project.data$authors)
               ),
               tags$div(
                 class="flex-container baseline",
                 tags$div(class="editor-section-subheader", "Date"),
                 tags$div(class="editor-section-sep"),
                 tags$div(class="editor-section-contents", id="project-date", Sys.Date())
               ),
               tags$div(
                 class="flex-container",
                 tags$div(class="editor-section-subheader", ""),
                 tags$div(class="editor-section-sep"),
                 tags$div(class="editor-section-contents", actionLink(ns("edit_header"), "Edit Header"))
               )
      ),

      tags$br(),

      tags$div(
        id = ns("datasets_anchor"),
        class = "project-section-anchor",
        `data-toggle` = "collapse",
        href = paste0("#", ns("datasets_section")),
        "Datasets"
      ),
      tags$div(
        id = ns("datasets_section"),
        class = "collapse show",
        tags$div(
          class="flex-container",
          tags$div(class="editor-section-subheader", ""),
          tags$div(class="editor-section-sep"),
          tags$div(class="editor-section-contents",
                   div(id = "data_slates_begin"),
                   div(id = "data_slates_end")
          )
        ),
        tags$div(
          class="flex-container",
          tags$div(class="editor-section-subheader", ""),
          tags$div(class="editor-section-sep"),
          tags$div(class="editor-section-contents",
                   actionLink(ns("btn_add_dataset"), "Add Dataset"))
        )
      ),

      tags$br(),

      tags$div(
        id = ns("source_anchor"),
        class = "project-section-anchor collapsed",
        `data-toggle` = "collapse",
        href = paste0("#", ns("source_section")),
        HTML("Source&nbsp;Code")
      ),
      tags$div(
        id = ns("source_section"),
        class = "collapse",
        class="flex-container",
        tags$div(class="editor-section-subheader", ""),
        tags$div(class="editor-section-sep"),
        tags$div(
          class="editor-section-contents",
          code.ui
        )
      ),

      tags$br(),

      tags$div(
        id = ns("analysis_anchor"),
        class = "project-section-anchor",
        `data-toggle` = "collapse",
        href = paste0("#", ns("analysis_section")),
        "Analysis"
      ),
      tags$div(
        id = ns("analysis_section"),
        class = "collapse show",
        tags$div(
          class="flex-container",
          tags$div(class="editor-section-subheader", ""),
          tags$div(class="editor-section-sep"),
          tags$div(class="editor-section-contents",
                   div(id = "slates_begin"),
                   div(id = "slates_end")
          )
        ),
        tags$div(
          class="flex-container",
          tags$div(class="editor-section-subheader", ""),
          tags$div(class="editor-section-sep"),
          tags$div(class="editor-section-contents", actionLink(ns("btn_add_slate"), "Add Slate"))
        )
      ),

      tags$br(),

      tags$div(
        id = ns("sessioninfo_anchor"),
        class = "project-section-anchor",
        `data-toggle` = "collapse",
        href = paste0("#", ns("sessioninfo_section")),
        HTML("Session&nbsp;Info")
      ),
      tags$div(
        id = ns("sessioninfo_section"),
        class = "collapse show",
        class="flex-container",
        tags$div(class="editor-section-subheader", ""),
        tags$div(class="editor-section-sep"),
        tags$div(
          class="editor-section-contents",
          HTML(paste(capture.session.info(320), collapse="<br>"))
        )
      )
    )
  )
}


#' Editor Panel Server Function
#'
#' @param input
#' @param output
#' @param session
#' @param session.data
slates_editServer <- function(input, output, session,
                              project, session.data, global.options) {
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

  addDataset <- function(blueprint, ask.title = TRUE) {
    dataset.list <- reactiveValuesToList(datasets)
    id <- seq.uid("dataset")

    insertUI(selector = "#data_slates_end",
             where = "beforeBegin",
             ui = dataSlateUI(ns(id), blueprint))

    datasets[[ id ]] <- callModule(dataSlateServer, id, blueprint, global.envir, open.settings = TRUE)
  }

  # observe Add Slab buttons
  observeEvent(input$btn_add_dataset, {
    blueprints <- session.data$data.blueprints

    modals$file.import$show(function(res) {
      print(res)
      #add_data_slate(blueprints[[ value ]], ask.title = FALSE)
    })
  })


  # outputOptions(output, "output_global", suspendWhenHidden = FALSE)


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
  # project.data <- reactive({
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

    slates[[ id ]] <- callModule(slateServer, id,
                                 blueprint,
                                 slate.options = list(
                                   envir = global.envir,
                                   open.settings = open.settings
                                 ),
                                 global.options = global.options)
  }

  # observe add slate button
  observeEvent(input$btn_add_slate, {
    blueprints <- session.data$blueprints

    #modals$select.input$show("Select blueprint", names(blueprints), function(value) {
    modals$select.modal$show(
      label = "Select Blueprint:",
      choices = names(blueprints),
      callback = function(value) {
        addSlate(blueprints[[ value ]], input.values = NULL, open.settings = TRUE)
      })
  })




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
}






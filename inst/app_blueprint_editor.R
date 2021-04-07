








blueprintEditorApp <- function(blueprint.filename = NULL) {

  editorLoadBlueprint <- function(pathname) {
    if (is.null(pathname) || !file.exists(pathname)) {
      return(slateBlueprint(
        name = gsub("\\.json$", "", basename(pathname))
      ))
    }

    tryCatch({
      blueprintFromJSON(filename = pathname)
    },
    error = function(e) {
      blueprintFromJSON(filename = pathname, preprocess = FALSE)
    })
  }

  theme <- getOption("rslates.themes")$default

  section.div <- function(...) {
    tags$div(
      style = "filter: drop-shadow(0px 18px 8px #00000011);",
      tags$div(
        class = "bg-light px-5 py-4",
        ...
      ),
      tags$div(
        class = "bg-light slanted-bottom-40-rev",
        style = "height: 50px;"
      )
    )
  }

  ui <- slatesNavbarPage(
    title = "Blueprint Editor",
    theme = theme,
    header = tagList(
    ),
    tabs = list(
      tabPanel(
        title = "Blueprint Editor",
        section.div(
          tags$div(
            class = "d-flex align-items-center",
            actionButton("new_blueprint", "New Blueprint", icon = icon("new"), class = "mr-2"),
            actionButton("load_blueprint", "Load Blueprint", icon = icon("load")),
            # selectInput(
            #   "active_blueprint",
            #   label = "Active Blueprint",
            #   choices = c("New Blueprint", blueprint.list),
            #   selected = blueprint.filename
            # ),
            #actionButton("new_blueprint", "New Blueprint", class = "ml-2"),
            uiOutput("save_state", class = "ml-auto"),
            shinyjs::disabled(
              actionButton("save_blueprint", "Save Blueprint", icon = icon("save"), class = "ml-2")
            )
          ),
          tags$hr(),
          flowLayout(
            selectInput(
              "preview_inputs_style",
              label = "Input Panel Style",
              choices = list("tabset", "collapses", "flowing"),
              selected = "tabset"
            ),
            textInput("slate_height", "Slate Height", value = "520px"),
            checkboxGroupInput(
              "slate_options", "Card",
              choices = c("Use Card" = "use.card",
                          "Show Header" = "card.header"),
              selected = c("use.card", "card.header"))
          ),
          uiOutput("slate_imports"),
          shinyBS::bsTooltip("preview_inputs_style", title = "Style of the inputs panel.",
                             placement = "top"),
          shinyBS::bsTooltip("slate_height", title = "Height of the slate in any valid css unit.",
                             placement = "top"),
          uiOutput("slate_ui")
          #slateUI("slate")
        )
      )
    ),
    session.info = TRUE
  )


  server <- function(input, output, session) {
    global.options <- reactiveValues(ace.theme = getOption("rslates.themes-ace")$default)

    slate.options <- do.call(
      reactiveValues,
      slateOptions(view.inputs = TRUE, open.editor = TRUE)
    )

    observe({
      slate.options$inputs.style = input$preview_inputs_style
      slate.options$height = input$slate_height
      slate.options$use.card = "use.card" %in% input$slate_options
      slate.options$card.header = "card.header" %in% input$slate_options
    })


    # the blueprint
    # load the initial blueprint
    blueprint <- reactiveVal(
      editorLoadBlueprint(blueprint.filename)
    )

    if (!is.null(blueprint.filename))
      blueprint.path <- reactiveVal(dirname(blueprint.filename))
    else
      blueprint.path <- getwd()

    # Create the slate server
    slate <- slateServer(
      "the_slate",
      blueprint = isolate(blueprint()),
      slate.options = slate.options,
      global.options = global.options
    )


    #
    # Themeing
    #

    observeEvent(input$select_ace_theme, {
      global.options$ace.theme <- input$select_ace_theme
    })

    observeEvent(input$select_theme, {
      theme <- loadTheme(input$select_theme)
      session$setCurrentTheme(theme)
    })


    # Slate imports
    output$slate_imports <- renderUI({
      imports <- blueprint()$imports

      observers <- map(imports, ~{
        input.id <- paste0(.$name, "_import")
        name <- .$name

        observeEvent(input[[ input.id ]], {
          data <- input[[ input.id ]]

          slate$import.values[[ name ]] <- data
        })
      })

      map(imports, ~{
        fileInput(paste0(.$name, "_import"), label = .$name)
      })
    })


    # slate ui

    output$slate_ui <- renderUI({
      slateUI("the_slate", slate.options = slate.options)
    })

    #
    # New / Load / Save
    #

    observeEvent(input$new_blueprint, {
      blueprint(slateBlueprint())

      isolate(slate$updateBlueprint(blueprint()))
    })


    load.blueprint.modal <- slatesFileInputModal("load_modal", session)

    observeEvent(input$load_blueprint, {
      load.blueprint.modal$show(
        title = "Load Blueprint", label = "Select Blueprint",
        callback = function(file) {
          blueprint(editorLoadBlueprint(file$datapath))
          blueprint.path(dirname(file$datapath))

          isolate(slate$updateBlueprint(blueprint()))
        })
    })


    output$save_state <- renderUI({
      req(
        slate, blueprint(), slate$blueprint()
      )

      up.to.date <- names(blueprint()) %>%
        map_lgl(~identical(blueprint()[[.]], slate$blueprint()[[.]])) %>%
        all

      shinyjs::toggleState("save_blueprint", condition = !up.to.date)

      if (up.to.date)
        tags$span("All changes saved", style = "color: green;")
      else
        tags$i("Unsaved changes", style = "color: red;")
    })


    save.modal <- slatesModal(
      id = "save_modal",
      session = session,
      ui.fun = function(filename) {
        textInput(session$ns("save_filename"), label = "Filename", value = filename)
      },
      submit.fun <- function() {
        list(filename = session$input$save_filename)
      }
    )

    observeEvent(input$save_blueprint, {
      req(bp <- slate$blueprint())

      # TODO: more robust filename sanitizer
      filename <- paste0(gsub(" ", "_", bp$name), ".json")

      save.modal$show(title = "Save Blueprint",
                      size = "m",
                      filename = file.path(blueprint.path(), filename),
                      callback = function(filename) {
        pathname <- filename
        ext <- sub("^.*\\.(.*?)$", "\\1", filename)

        if (ext == "json")
          text <- blueprintToJSON(bp)
        else if (ext == "yaml")
          text <- blueprintToYAML(bp)
        else
          stop("Invalid filename.")

        writeLines(text, con = pathname)
        blueprint.path(dirname(pathname))

        blueprint(slate$blueprint())

        options(rslates.tag.list = unique(c(getOption("rslates.blueprint.tags"), bp$tags)))
      })
    })


  }

  if (getOption("rslates.run.themer") == TRUE)
    bslib::run_with_themer(shiny::shinyApp(ui, server))
  else
    shiny::shinyApp(ui, server)
}


initServerOptions()

blueprintEditorApp(blueprint.filename = getOption("rslates.bp.editor.blueprint.filename"))


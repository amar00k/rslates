








blueprintEditorApp <- function(blueprint.ini = slateBlueprint("Untitled"),
                               blueprint.dir = NULL) {

  editorLoadBlueprint <- function(blueprint.name) {
    path <- file.path(blueprint.dir, blueprint.list)

    blueprintFromJSON(filename = path)
  }

  if (!is.null(blueprint.dir))
    blueprint.list <- dir(blueprint.dir, pattern = "\\.json$")
  else
    blueprint.list <- list()

  theme <- getOption("rslates.default.theme")

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
            selectInput(
              "active_blueprint",
              label = "Active Blueprint",
              choices = blueprint.list %>% sub("\\.json$", "", .)
            ),
            actionButton("new_blueprint", "New Blueprint", class = "ml-2"),
            uiOutput("save_state", class = "ml-auto"),
            actionButton("save_blueprint", "Save Changes", icon = icon("save"), class = "ml-2")
          ),
          tags$hr(),
          flowLayout(
            selectInput(
              "preview_inputs_style",
              label = "Input Panel Style",
              choices = list("tabset", "collapses", "flowing"),
              selected = "flowing"
            ),
            textInput("slate_height", "Slate Height", value = "520px"),
            checkboxGroupInput(
              "slate_options", "Card",
              choices = c("Use Card" = "use.card",
                          "Show Header" = "card.header"),
              selected = c("use.card", "card.header"))
          ),
          shinyBS::bsTooltip("preview_inputs_style", title = "Style of the inputs panel."),
          shinyBS::bsTooltip("slate_height", title = "Height of the slate in any valid css unit."),
          uiOutput("slate_preview")
        )
      )
    ),
    session.info = TRUE
  )


  server <- function(input, output, session) {
    global.options <- reactiveValues(ace.theme = getOption("default.ace.theme"))

    slate.options <- reactive({
      slateOptions(
        open.inputs = TRUE,
        open.editor = TRUE,
        inputs.style = input$preview_inputs_style,
        height = input$slate_height,
        use.card = "use.card" %in% input$slate_options,
        card.header = "card.header" %in% input$slate_options
      )
    })


    #
    # Themeing
    #

    observeEvent(input$select_ace_theme, {
      global.options$ace.theme <- input$select_ace_theme
    })

    observeEvent(input$select_theme, {
      print("select_theme")

      theme <- loadTheme(input$select_theme)
      session$setCurrentTheme(theme)
    })

    #
    # Slate
    #

    blueprint <- reactiveVal(NULL)
    server <- reactiveVal(NULL)

    # observe active blueprint and create new server, destroying
    # the previous one in the process
    # also check for unsaved changes and so on
    observe({
      blueprint.name <- input$active_blueprint
      server <- isolate(server())

      if (!is.null(server)) {
        server$destroy()
      }

      blueprint <- editorLoadBlueprint(blueprint.name)

      server <- slateServer(
        "slate_preview",
        blueprint = blueprint,
        slate.options = slate.options(),
        global.options = global.options
      )

      isolate({
        blueprint(blueprint)
        server(server)
      })
    })


    output$slate_preview <- renderUI({
      req(blueprint())

      slateUI("slate_preview",
              blueprint = blueprint(),
              slate.options = slate.options())
    })


    output$save_state <- renderUI({
      req(server())

      if (is.null(server()))
        return(tags$div())

      # if (identical(blueprint(), server()$blueprint()))
        tags$span("All changes saved", style = "color: green;")
      # else
      #   tags$i("Unsaved changes", style = "color: red;")
    })


  }

  shiny::shinyApp(ui, server)
}

blueprintEditorApp(blueprint.ini = getOption("rslates.bp.editor.blueprint"),
                   blueprint.dir = getOption("rslates.bp.editor.blueprint.dir") )


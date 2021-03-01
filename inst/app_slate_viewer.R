













slateViewerApp <- function(blueprint = slateBlueprint("untitled")) {
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
    title = "Slates",
    theme = getOption("rslates.default.theme"),
    header = tagList(
    ),
    tabs = list(
      tabPanel(title = "Slate Preview", section.div(
        flowLayout(
          selectInput(
            "preview_inputs_style",
            label = "Input Panel Style",
            choices = list("tabset", "collapses", "flowing"),
            selected = "flowing"
          ),
          textInput("slate_height", "Slate Height", value = "480px"),
          checkboxGroupInput(
            "slate_options", "Card",
            choices = c("Use Card" = "use.card",
                        "Show Header" = "card.header"),
            selected = c("use.card", "card.header"))
        ),
        shinyBS::bsTooltip("preview_inputs_style", title = "Style of the inputs panel."),
        shinyBS::bsTooltip("slate_height", title = "Height of the slate in any valid css unit."),
        uiOutput("slate_preview")
      ))
    ),
    session.info = TRUE
  )


  server <- function(input, output, session) {
    global.options <- reactiveValues(ace.theme = getOption("default.ace.theme"))

    slate.options <- reactive({
      slateOptions(
        inputs.style = input$preview_inputs_style,
        height = input$slate_height,
        use.card = "use.card" %in% input$slate_options,
        card.header = "card.header" %in% input$slate_options
      )
    })

    observe({
      slate.server <- slateServer(
        "slate_preview",
        blueprint = blueprint,
        slate.options = slate.options(),
        global.options = global.options
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

    output$slate_preview <- renderUI({
      slateUI("slate_preview",
              blueprint = blueprint,
              slate.options = slate.options())
    })
  }

  shiny::shinyApp(ui, server)
}

slateViewerApp(getOption("rslates.viewer.blueprint"))


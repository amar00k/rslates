

slatePreviewApp <- function(blueprint, input.container = "tabset") {
  default.theme <- "solar"
  default.ace.theme <- "twilight"

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
    title = "Slate Preview",
    theme = getOption("rslates.default.theme"),
    header = tagList(
    ),
    tabs = list(
      tabPanel(title = "Slate", section.div(
        slateUI("slate_preview", blueprint = blueprint, input.container = input.container))
      )
    ),
    session.info = TRUE
  )

  server <- function(input, output, session) {
    global.options <- reactiveValues(ace.theme = default.ace.theme)

    slate <- slateServer(
      "slate_preview",
      blueprint = blueprint,
      slate.options = list(
        input.container = input.container,
        envir = reactiveVal(new.env()),
        open.settings = TRUE
      ),
      global.options = global.options
    )

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
  }

  shiny::shinyApp(ui, server)
}

input.container <- getOption("rslates.input.container")

slatePreviewApp(getOption("rslates.preview.blueprint"), input.container)




slatePreviewApp <- function(blueprint, input.container = "tabset") {
  default.theme <- "solar"
  default.ace.theme <- "twilight"

  ui <- fluidPage(
    shiny::bootstrapLib(bslib::bs_theme(bootswatch = default.theme, version = "4")),
    shiny::tags$link(rel = "stylesheet", type = "text/css", href = "slates.css"),
    thematic::thematic_shiny(),
    title = "Slate Preview",
    titlePanel("Slate Preview"),
    sidebarLayout(
      sidebarPanel = sidebarPanel(
        width = 2,
        tagList(
          selectInput("select_theme",
                      label = "Theme",
                      choices = bslib::bootswatch_themes(),
                      selected = default.theme),
          selectInput("select_ace_theme",
                      label = "Ace Editor Theme",
                      choices = shinyAce::getAceThemes(),
                      selected = default.ace.theme)
        )
      ),
      mainPanel = mainPanel(
        width = 10,
        slateUI("slate_preview", blueprint, input.container)
      )
    )
  )

  server <- function(input, output, session) {

    global.options <- reactiveValues(ace.theme = default.ace.theme)

    slate <- callModule(slateServer, "slate_preview", blueprint, global.options = global.options)

    observeEvent(input$select_ace_theme, {
      global.options$ace.theme <- input$select_ace_theme
    })

    observeEvent(input$select_theme, {
      theme <- getCurrentTheme()

      if (!is.null(theme) && bslib::theme_bootswatch(theme) != input$select_theme) {
        theme <- bslib::bs_theme_update(theme, bootswatch = input$select_theme, version = "4")
        session$setCurrentTheme(theme)
      }
    })
  }

  shiny::shinyApp(ui, server)
}

blueprint <- getOption("rslates.preview.blueprint")
input.container <- getOption("rslates.preview.input.container")

slatePreviewApp(blueprint, input.container)











projectEditorApp <- function(project = NULL) {
  default.theme <- "solar"
  default.ace.theme <- "twilight"

  if (is.null(project)) {
    project <- slatesProject("Untitled Project")
  }

  ui <- fluidPage(
    shinyjs::useShinyjs(),
    shiny::bootstrapLib(bslib::bs_theme(bootswatch = default.theme, version = "4")),
    thematic::thematic_shiny(), # recolors plots
    shiny::tags$link(rel = "stylesheet", type = "text/css", href = "slates.css"),
    title = "Slates Project Editor",
    titlePanel("Slates Project Editor"),
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
        slates_editUI("editor", project)
      )
    )
  )


  htmltools::htmlDependencies(ui) <- htmltools::htmlDependency(
    "font-awesome",
    "5.13.0", "www/shared/fontawesome", package = "shiny",
    stylesheet = c("css/all.min.css", "css/v4-shims.min.css"))


  server <- function(input, output, session) {
    global.options <- reactiveValues(ace.theme = default.ace.theme)
    session.data <- reactiveValues(
      blueprints = options()$rslates.blueprints
    )

    #
    # Themeing
    #

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

    #
    # Editor module
    #
    editor <- callModule(module = slates_editServer, id = "editor",
                         project, session.data, global.options)

  }

  shiny::shinyApp(ui, server)
}

project <- getOption("rslates.editor.project")

projectEditorApp(project)




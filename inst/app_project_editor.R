








projectEditorApp <- function(project = NULL) {
  if (is.null(project)) {
    project <- slatesProject("Untitled Project")
  }

  ui <- slatesNavbarPage(
    title = "Slates",
    theme = getOption("rslates.themes")$default,
    header = tagList(
    ),
    tabs = list(
      tabPanel(title = "Project Editor", div(
        class = "",
        projectEditorUI("editor", project)
      ))
    ),
    session.info = TRUE
  )


  server <- function(input, output, session) {
    global.options <- reactiveValues(
      ace.theme = getOption("rslates.themes-ace")$default,
      bslib.theme = getOption("rslates.themes")$default
    )

    session.data <- reactiveValues(
      blueprints = getOption("rslates.blueprints.list") %>% set_names(sub("\\..*?$", "", names(.))),
      importers = getOption("rslates.importers.list")
    )

    #
    # Themeing
    #

    observeEvent(input$select_ace_theme, {
      global.options$ace.theme <- input$select_ace_theme
    })

    observeEvent(input$select_theme, {
      dlog()

      theme <- loadTheme(input$select_theme)
      session$setCurrentTheme(theme)
    })

    #
    # Editor module
    #
    editor <- projectEditorServer(
      id = "editor",
      project, session.data, global.options
    )
  }

  if (options()$rslates.run.themer == TRUE)
    bslib::run_with_themer(shiny::shinyApp(ui, server))
  else
    shiny::shinyApp(ui, server)
}

initServerOptions()

project <- getOption("rslates.editor.project")

projectEditorApp(project)




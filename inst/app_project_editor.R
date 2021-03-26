








projectEditorApp <- function(project = NULL) {
  if (is.null(project)) {
    project <- slatesProject("Untitled Project")
  }

  # ui <- fluidPage(
  #   shinyjs::useShinyjs(),
  #   shiny::bootstrapLib(bslib::bs_theme(bootswatch = default.theme, version = "4")),
  #   thematic::thematic_shiny(), # recolors plots
  #   shiny::tags$link(rel = "stylesheet", type = "text/css", href = "slates.css"),
  #   title = "Slates Project Editor",
  #   titlePanel("Slates Project Editor"),
  #   sidebarLayout(
  #     sidebarPanel = sidebarPanel(
  #       width = 2,
  #       tagList(
  #         selectInput("select_theme",
  #                     label = "Theme",
  #                     choices = bslib::bootswatch_themes(),
  #                     selected = default.theme),
  #         selectInput("select_ace_theme",
  #                     label = "Ace Editor Theme",
  #                     choices = shinyAce::getAceThemes(),
  #                     selected = default.ace.theme)
  #       )
  #     ),
  #     mainPanel = mainPanel(
  #       width = 10,
  #       slates_editUI("editor", project)
  #     )
  #   )
  # )

  ui <- slatesNavbarPage(
    title = "Slates",
    theme = getOption("rslates.default.theme"),
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

  # htmltools::htmlDependencies(ui) <- htmltools::htmlDependency(
  #   "font-awesome",
  #   "5.13.0", "www/shared/fontawesome", package = "shiny",
  #   stylesheet = c("css/all.min.css", "css/v4-shims.min.css"))


  server <- function(input, output, session) {
    global.options <- reactiveValues(
      ace.theme = getOption("rslates.default.ace.theme"),
      bslib.theme = getOption("rslates.default.theme")
    )
    session.data <- reactiveValues(
      blueprints = getOption("rslates.blueprints"),
      importer.blueprints = getOption("rslates.importer.blueprints")#,
      #project.envir = new.env()
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

blueprint.dir <- getOption("rslates.blueprint.dir")
importer.blueprint.dir <- getOption("rslates.importer.blueprint.dir")

blueprints <- loadBlueprints(blueprint.dir, on.error = "skip")
importer.blueprints <- loadBlueprints(importer.blueprint.dir, on.error = "skip")

blueprint.tags <- c(blueprints, importer.blueprints) %>%
  map("tags") %>%
  unlist %>%
  unique

options(rslates.blueprints = blueprints)
options(rslates.importer.blueprints = loadBlueprints(importer.blueprint.dir, on.error = "skip"))
options(rslates.tag.list = blueprint.tags)

project <- getOption("rslates.editor.project")

projectEditorApp(project)




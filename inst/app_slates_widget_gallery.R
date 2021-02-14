



slatesWidgetGalleryApp <- function() {
  default.theme <- "solar"
  default.ace.theme <- "twilight"

  input.list <- list(
    slateInput("text", "character", default = "Some text",
               description = "A simple text input."),
    slateInput("expr", "expression", default = "rep(1:10, 2)",
               description = "An expression input. It has a slightly different appearance.
                              Try entering an invalid expression that doesn't parse."),
    slateInput("number", "numeric", default = "42",
               description = "A simple numeric input. Can be identified by its rounded borders."),
    slateInput("pair", "numeric2", default = c(0,1000),
               description = "A pair of numeric values. Good for xlim, ylim, etc..."),
    slateInput("numbers", "numeric4", default = c(0,1,2,3),
               description = "A numeric input that requires four values. Used for par(mar=c(...)) and others."),
    slateInput("true_or_false", "logical", default = TRUE,
               description = "A logical input using a selectInput."),
    slateInput("chosen_letter", "choices", choices = c(LETTERS), default = "S",
               description = "A single-choice select input."),
    slateInput("chosen_letters", "choices", choices = c(LETTERS), default = c("S", "L", "A", "T", "E", "S"),
               multiple = TRUE, description = "A multiple-choice select input."),
    slateInput("anything", "free-choices", choices = c("write", "something", "or choose this", "or this"),
               default = c("write", "something"),
               multiple = TRUE, description = "A multiple-choice select input that allows arbitrary entries.")
  ) %>% set_names(sapply(., "[[", "name"))


  ui <- fluidPage(
    shinyjs::useShinyjs(),
    shiny::bootstrapLib(bslib::bs_theme(bootswatch = default.theme, version = "4")),
    shiny::tags$link(rel = "stylesheet", type = "text/css", href = "slates.css"),
    thematic::thematic_shiny(),
    title = "Slates Widget Gallery",
    titlePanel("Slates Widget Gallery"),
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
        widgetGalleryUI(id = "gallery", input.list = input.list)
      )
    )
  )


  server <- function(input, output, session) {
    global.options <- reactiveValues(ace.theme = default.ace.theme)
    global.options$group.name.generator <- sequenceGenerator("group")

    #
    # Themeing
    #

    observeEvent(input$select_ace_theme, {
      global.options$ace.theme <- input$select_ace_theme
    })

    observeEvent(input$select_theme, {
      print("select_theme")

      theme <- getCurrentTheme()

      if (!is.null(theme) && bslib::theme_bootswatch(theme) != input$select_theme) {
        theme <- bslib::bs_theme_update(theme, bootswatch = input$select_theme, version = "4")
        session$setCurrentTheme(theme)
      }
    })

    mod <- callModule(widgetGalleryServer, id = "gallery", input.list, global.options)
  }

  if (options()$rslates.run.themer == TRUE)
    bslib::run_with_themer(shiny::shinyApp(ui, server))
  else
    shiny::shinyApp(ui, server)
}

slatesWidgetGalleryApp()







slatesWidgetGalleryApp <- function() {
  default.theme <- "solar"
  default.ace.theme <- "twilight"

  inputs <- list(
    slateInput("text", "character", default = "Some text",
               description = "A simple text input."),
    slateInput("expr", "expression", default = "rep(1:10, 2)",
               description = "An expression input. It has a slightly different appearance.
                              Try entering an invalid expression."),
    slateInput("number", "numeric", default = "42",
               description = "A simple numeric input. Can be identified by its rounded borders."),
    slateInput("vector4", "numeric4", default = c(0,1,2,3),
               description = "A numeric input that requires four values."),
    slateInput("true_or_false", "logical", default = TRUE,
               description = "A logical input."),
    slateInput("letter", "choices", choices = c(LETTERS), default = "S",
               description = "A single-choice select input."),
    slateInput("letters", "choices", choices = c(LETTERS), default = c("S", "L", "A", "T", "E", "S"),
               multiple = TRUE, description = "A multiple-choice select input."),
    slateInput("anything", "free-choices", choices = c("write", "something", "or choose this", "or this"),
               default = c("write", "something"),
               multiple = TRUE, description = "A multiple-choice select input that allows arbitrary entries.")
  ) %>% set_names(sapply(., "[[", "name"))

  widgetGalleryUI <- function(id = NULL) {
    if (is.null(id))
      ns <- identity
    else
      ns <- NS(id)

    inputs.ui <- tagList(
      createInputGroup(inputGroup(name = "inputs", layout = "flow-3", inputs = inputs, ns = ns))
    )

    outputs.ui <- tagList()

    fluidPage(
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
            selectInput(ns("select_theme"),
                        label = "Theme",
                        choices = bslib::bootswatch_themes(),
                        selected = default.theme),
            selectInput(ns("select_ace_theme"),
                        label = "Ace Editor Theme",
                        choices = shinyAce::getAceThemes(),
                        selected = default.ace.theme)
          )
        ),
        mainPanel = mainPanel(
          tabsetPanel(
            tabPanel(title = "Inputs", tags$div(class = "container p-3", inputs.ui)),
            tabPanel(title = "Outputs", tags$div(class = "container p-3", outputs.ui))
          )
        )
      )
    )
  }


  widgetGalleryServer <- function(input, output, session) {
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

    #
    # Handlers
    #

    # initialize input observers
    for (x in inputs) {
      input.handlers[[ x$input.type ]]$create.observer(session, x$id)
    }


  }

  if (options()$rslates.run.themer == TRUE)
    bslib::run_with_themer(shiny::shinyApp(widgetGalleryUI(), widgetGalleryServer))
  else
    shiny::shinyApp(widgetGalleryUI(), widgetGalleryServer)
}

slatesWidgetGalleryApp()



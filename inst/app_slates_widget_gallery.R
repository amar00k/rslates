



slatesWidgetGalleryApp <- function() {
  inputs <- list(
    slateInput("text", "character", default = "Some text", null = TRUE,
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
    slateInput("anything", "choices", choices = c("write", "something", "or choose this", "or this"),
               default = c("write", "something"), custom = TRUE,
               description = "A choice select input that allows an arbitrary value."),
    slateInput("multiple_anythings", "choices", choices = c("write", "something", "or choose this", "or this"),
               default = c("write", "something"), multiple = TRUE, custom = TRUE,
               description = "A multiple-choice select input that allows arbitrary entries."),
    slateInput("multi_input", type = "multi",
               default = "character",
               inputs = list(
                 character = list(default = "Life"),
                 expression = list(default = "1:10"),
                 choices = list(default = "B", choices = c("A", "B", "C"), quote = TRUE),
                 numeric = list(default = 42)
               )
    ),
    slateInput("multi_input_2", type = "multi",
               default = "character",
               allow.null = TRUE,
               inputs = list(
                 numeric = list(default = 1),
                 numeric2 = list(default = c(1,2)),
                 numeric4 = list(default = c(1,2,3,4))
               )
    )
  ) %>% set_names(sapply(., "[[", "name"))

  group <- slateGroup(name = "group", layout = "flow-3")

  inputs %<>% map(~list_modify(., parent = group$name))

  section.div <- function(...) {
    tags$div(
      tags$div(
        class = "bg-light px-5 pt-5 pb-2",
        ...
      ),
      tags$div(
        class = "bg-light slanted-bottom-40-rev",
        style = "height: 50px;"
      )
    )
  }

  ui <- slatesNavbarPage(
    title = "Slates Widget Gallery",
    theme = getOption("rslates.default.theme"),
    tabs = list(
      tabPanel(
        "Inputs",
        section.div(
          widgetGalleryInputsUI(id = "gallery", inputs, group)
        )
      )
    ),
    session.info = TRUE
  )


  server <- function(input, output, session) {
    global.options <- reactiveValues(
      ace.theme = getOption("rslates.default.ace.theme"),
      bslib.theme = getOption("rslates.default.theme")
    )
    global.options$group.name.generator <- sequenceGenerator("group")

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

    widgetGalleryServer("gallery", inputs, global.options)
  }

  if (options()$rslates.run.themer == TRUE)
    bslib::run_with_themer(shiny::shinyApp(ui, server))
  else
    shiny::shinyApp(ui, server)
}

slatesWidgetGalleryApp()



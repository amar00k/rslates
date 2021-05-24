
library(rslates)
library(shinyTree)
library(bslib)

initServerOptions()

if (is.null(getOption("rslates.viewer.blueprint"))) {
  blueprint <- slateBlueprint()
} else {
  blueprint <- getOption("rslates.viewer.blueprint")
}

theme <- getOption("rslates.themes")$default

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
  theme = theme,
  header = tagList(
  ),
  tabs = list(
    tabPanel(title = "Viewer", section.div(
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
      shinyBS::bsTooltip("preview_inputs_style", title = "Style of the inputs panel.",
                         placement = "top"),
      shinyBS::bsTooltip("slate_height", title = "Height of the slate in any valid css unit.",
                         placement = "top"),
      slateUI("slate")
    ))
  ),
  session.info = TRUE
)


server <- function(input, output, session) {
  global.options <- reactiveValues(ace.theme = getOption("rslates.themes-ace")$default)

  slate <- slateServer(
    "slate",
    blueprint = blueprint,
    slate.options = slateOptions(),
    global.options = global.options
  )

  shinyBS::addTooltip(session, "slate_options", "Hello")


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
    slate.options <- slateOptions(
      inputs.style = input$preview_inputs_style,
      height = input$slate_height,
      use.card = "use.card" %in% input$slate_options,
      card.header = "card.header" %in% input$slate_options
    )

    slateUI("slate_preview",
            blueprint = blueprint,
            slate.options = slate.options)
  })
}

shiny::shinyApp(ui, server)



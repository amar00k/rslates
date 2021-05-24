

library(rslates)
library(shinyTree)
library(bslib)

initServerOptions()

blueprint.filename = getOption("rslates.bp.editor.blueprint.filename")

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

if (!is.null(blueprint.filename)) {
  blueprint <- loadBlueprint(blueprint.filename)
} else {
  blueprint <- slateBlueprint()
}

preview.tab <- tabPanel(
  title = "Preview",
  class = "container-fluid pt-3",
  flowLayout(
    selectInput(
      "preview_inputs_style",
      label = "Input Panel Style",
      choices = list("tabset", "collapses", "flowing"),
      selected = "tabset"
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
)

ui <- slatesNavbarPage(
  title = "Blueprint Editor",
  #theme = bslib::bs_theme(version = 4),
  theme = theme,
  header = tags$div(
    class = "d-flex align-items-center bg-light px-5 py-4",
    actionButton("new_blueprint", "New Blueprint", icon = icon("new"), class = "mr-2"),
    actionButton("load_blueprint", "Load Blueprint", icon = icon("load")),
    uiOutput("save_state", class = "ml-auto"),
    shinyjs::disabled(
      actionButton("save_blueprint", "Save Blueprint", icon = icon("save"), class = "ml-2")
    )
  ),
  tabs = list(
    tabPanel(
      title = "Edit",
      #uiOutput("slate_imports"),
      section.div(
        blueprintEditorUI("editor", blueprint)
      )
    ),
    preview.tab
  ),
  session.info = TRUE
)


server <- function(input, output, session) {
  global.options <- reactiveValues(ace.theme = getOption("rslates.themes-ace")$default)

  # load the initial blueprint
  blueprint <- reactiveVal(blueprint)

  if (!is.null(blueprint.filename))
    blueprint.path <- reactiveVal(dirname(blueprint.filename))
  else
    blueprint.path <- getwd()

  slate.options <- do.call(
    reactiveValues,
    slateOptions(view.inputs = TRUE, open.editor = FALSE)
  )

  # Create the slate server
  slate <- slateServer(
    "slate",
    blueprint = blueprint,
    slate.options = slate.options,
    global.options = global.options
  )

  # create the blueprint editor server
  blueprint.editor <- blueprintEditorServer(
    "editor",
    blueprint = isolate(blueprint()),
    slate.server = slate,
    global.options = global.options
  )

  #
  # Themeing
  #

  observeEvent(input$select_ace_theme, {
    global.options$ace.theme <- input$select_ace_theme
  })

  observeEvent(input$select_theme, {
    theme <- loadTheme(input$select_theme)
    #session$setCurrentTheme()
    session$setCurrentTheme(theme)
  })


  # Slate imports
  output$slate_imports <- renderUI({
    imports <- blueprint()$imports

    observers <- map(imports, ~{
      input.id <- paste0(.$name, "_import")
      name <- .$name

      observeEvent(input[[ input.id ]], {
        data <- input[[ input.id ]]

        slate$import.values[[ name ]] <- data
      })
    })

    map(imports, ~{
      fileInput(paste0(.$name, "_import"), label = .$name)
    })
  })


  # slate ui

  observe({
    slate.options$inputs.style = input$preview_inputs_style
    slate.options$height = input$slate_height
    slate.options$use.card = "use.card" %in% input$slate_options
    slate.options$card.header = "card.header" %in% input$slate_options
  })

  # output$slate_ui <- renderUI({
  #   slateUI("the_slate", slate.options = slate.options)
  # })

  #
  # New / Load / Save
  #

  observeEvent(input$new_blueprint, {
    blueprint(slateBlueprint())

    isolate(slate$updateBlueprint(blueprint()))
  })


  load.blueprint.modal <- slatesFileInputModal("load_modal", session)

  observeEvent(input$load_blueprint, {
    load.blueprint.modal$show(
      title = "Load Blueprint", label = "Select Blueprint",
      callback = function(file) {
        blueprint(editorLoadBlueprint(file$datapath))
        blueprint.path(dirname(file$datapath))

        isolate(slate$updateBlueprint(blueprint()))
      })
  })


  output$save_state <- renderUI({
    # req(
    #   slate, blueprint(), slate$blueprint()
    # )
    #
    # up.to.date <- names(blueprint()) %>%
    #   map_lgl(~identical(blueprint()[[.]], slate$blueprint()[[.]])) %>%
    #   all
    #
    # shinyjs::toggleState("save_blueprint", condition = !up.to.date)
    #
    # if (up.to.date)
    #   tags$span("All changes saved", style = "color: green;")
    # else
    #   tags$i("Unsaved changes", style = "color: red;")
  })


  save.modal <- slatesModal(
    id = "save_modal",
    session = session,
    ui.fun = function(filename) {
      textInput(session$ns("save_filename"), label = "Filename", value = filename)
    },
    submit.fun <- function() {
      list(filename = session$input$save_filename)
    }
  )

  observeEvent(input$save_blueprint, {
    req(bp <- slate$blueprint())

    # TODO: more robust filename sanitizer
    filename <- paste0(gsub(" ", "_", bp$name), ".json")

    save.modal$show(title = "Save Blueprint",
                    size = "m",
                    filename = file.path(blueprint.path(), filename),
                    callback = function(filename) {
                      pathname <- filename
                      ext <- sub("^.*\\.(.*?)$", "\\1", filename)

                      if (ext == "json")
                        text <- blueprintToJSON(bp)
                      else if (ext == "yaml")
                        text <- blueprintToYAML(bp)
                      else
                        stop("Invalid filename.")

                      writeLines(text, con = pathname)
                      blueprint.path(dirname(pathname))

                      blueprint(slate$blueprint())

                      options(rslates.tag.list = unique(c(getOption("rslates.blueprint.tags"), bp$tags)))
                    })
  })

}


shiny::shinyApp(ui, server)




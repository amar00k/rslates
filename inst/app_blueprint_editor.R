








blueprintEditorApp <- function(blueprint.dir, blueprint.filename = NULL) {

  if (is.null(blueprint.filename) && is.null(blueprint.dir))
    stop("At least one of blueprint.filename or blueprint.dir must be specified.")

  editorLoadBlueprint <- function(blueprint.filename) {
    path <- file.path(blueprint.dir, blueprint.filename)

    if (!file.exists(path)) {
      return(slateBlueprint(
        name = gsub("_", " ", blueprint.filename) %>%
          gsub("\\.json$", "", .)
      ))
    }

    tryCatch({
      blueprintFromJSON(filename = path)
    },
    error = function(e) {
      blueprintFromJSON(filename = path, preprocess = FALSE)
    })
  }

  blueprint.list <- dir(blueprint.dir, pattern = "\\.json$")
  if (!is.null(blueprint.filename))
    blueprint.list <- unique(c(blueprint.list, blueprint.filename))

  theme <- getOption("rslates.default.theme")

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
    title = "Blueprint Editor",
    theme = theme,
    header = tagList(
    ),
    tabs = list(
      tabPanel(
        title = "Blueprint Editor",
        section.div(
          tags$div(
            class = "d-flex align-items-center",
            selectInput(
              "active_blueprint",
              label = "Active Blueprint",
              choices = c("New Blueprint", blueprint.list),
              selected = blueprint.filename
            ),
            #actionButton("new_blueprint", "New Blueprint", class = "ml-2"),
            uiOutput("save_state", class = "ml-auto"),
            shinyjs::disabled(
              actionButton("save_blueprint", "Save Blueprint", icon = icon("save"), class = "ml-2")
            )
          ),
          tags$hr(),
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
          uiOutput("slate_imports"),
          shinyBS::bsTooltip("preview_inputs_style", title = "Style of the inputs panel.",
                             placement = "top"),
          shinyBS::bsTooltip("slate_height", title = "Height of the slate in any valid css unit.",
                             placement = "top"),
          slateUI("slate")
        )
      )
    ),
    session.info = TRUE
  )


  server <- function(input, output, session) {
    global.options <- reactiveValues(ace.theme = getOption("default.ace.theme"))

    slate.options <- do.call(
      reactiveValues,
      slateOptions(open.inputs = TRUE, open.editor = TRUE)
    )

    observe({
      slate.options$inputs.style = input$preview_inputs_style
      slate.options$height = input$slate_height
      slate.options$use.card = "use.card" %in% input$slate_options
      slate.options$card.header = "card.header" %in% input$slate_options
    })


    # the blueprint
    # load the initial blueprint
    blueprint <- reactiveVal(
      editorLoadBlueprint(isolate(input$active_blueprint))
    )


    # Create the slate server
    slate <- slateServer(
      "slate",
      blueprint = isolate(blueprint()),
      slate.options = slate.options,
      global.options = global.options
    )


    # list of blueprints in directory
    # TODO: handle blueprint.dir is NULL
    blueprint.list <- reactiveVal(dir(blueprint.dir, pattern = "\\.json$"))

    # isolate(
    #   resetBlueprint(editorLoadBlueprint(blueprint.list()[1]))
    # )

    #
    # Themeing
    #

    observeEvent(input$select_ace_theme, {
      global.options$ace.theme <- input$select_ace_theme
    })

    observeEvent(input$select_theme, {
      theme <- loadTheme(input$select_theme)
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

          slate$import.data[[ name ]] <- data
        })
      })

      map(imports, ~{
        fileInput(paste0(.$name, "_import"), label = .$name)
      })
    })

    #
    # New / Load / Save
    #

    observeEvent(blueprint.list(), { #input$new_blueprint, {
      options <- c("New Blueprint", blueprint.list())

      selected <- input$active_blueprint

      updateSelectInput(
        session,
        "active_blueprint",
        choices = options,
        selected = selected
      )
    }, ignoreInit = TRUE)


    observe(label = "change.blueprint", {
      dlog()

      if (input$active_blueprint == "New Blueprint") {
        blueprint(slateBlueprint())
      } else {
        blueprint(editorLoadBlueprint(input$active_blueprint))
      }

      isolate(slate$updateBlueprint(blueprint()))
    })


    output$save_state <- renderUI({
      req(
        slate, blueprint(), slate$blueprint()
      )

      up.to.date <- names(blueprint()) %>%
        map_lgl(~identical(blueprint()[[.]], slate$blueprint()[[.]])) %>%
        all

      shinyjs::toggleState("save_blueprint", condition = !up.to.date)

      if (up.to.date)
        tags$span("All changes saved", style = "color: green;")
      else
        tags$i("Unsaved changes", style = "color: red;")
    })


    save.modal <- slatesModal(
      id = "save_modal",
      session = session,
      ui.fun = function(filename) {
        tagList(
          shinyjs::disabled(
            textInput(session$ns("save_filename"), label = "Filename", value = filename)
          ),
          tags$p(
            "Save changes to ", file.path(blueprint.dir, filename), "?",
            tags$b("Existing file will be overwritten!")
          )
        )
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
                      size = "l",
                      filename = filename,
                      callback = function(filename) {
        pathname <- file.path(blueprint.dir, filename)
        text <- blueprintToJSON(bp)

        writeLines(text, con = pathname)

        blueprint(slate$blueprint())
        blueprint.list(dir(blueprint.dir, pattern = "\\.json$"))

        options(rslates.tag.list = unique(c(getOption("rslates.tag.list"), bp$tags)))
      })
    })


  }

  if (getOption("rslates.run.themer") == TRUE)
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

blueprintEditorApp(blueprint.dir = getOption("rslates.bp.editor.blueprint.dir"),
                   blueprint.filename = getOption("rslates.bp.editor.blueprint.filename"))


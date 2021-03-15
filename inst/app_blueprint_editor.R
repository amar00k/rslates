








blueprintEditorApp <- function(blueprint.ini = slateBlueprint("Untitled"),
                               blueprint.dir = NULL) {

  editorLoadBlueprint <- function(blueprint.name) {
    path <- file.path(blueprint.dir, blueprint.name)
    blueprintFromJSON(filename = path)
  }

  if (!is.null(blueprint.dir))
    blueprint.list <- dir(blueprint.dir, pattern = "\\.json$")
  else
    blueprint.list <- list()

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
              choices = c("New Blueprint", blueprint.list %>% sub("\\.json$", "", .))
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
    blueprint <- reactiveVal()  # do.call(reactiveValues, slateBlueprint())


    # Create the slate server
    slate <- slateServer(
      "slate",
      #blueprint = isolate(blueprint()),
      slate.options = slate.options,
      global.options = global.options
    )


    # updateBlueprint <- function(new.blueprint) {
    #   # for (name in names(blueprint))
    #   #   blueprint[[ name ]] <- new.blueprint[[ name ]]
    # }


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

    #
    # New / Load / Save
    #

    observeEvent(blueprint.list(), { #input$new_blueprint, {
      options <- c("New Blueprint", blueprint.list() %>% sub("\\.json$", "", .))

      selected <- input$active_blueprint

      updateSelectInput(
        session,
        "active_blueprint",
        choices = options,
        selected = selected
      )
    })


    observe(label = "change.blueprint", {
      dlog()

      # if (!is.null(blueprint()) && blueprint()$name == input$active_blueprint)
      #   return()

      if (input$active_blueprint == "New Blueprint") {
        blueprint(slateBlueprint())
      } else {
        blueprint(editorLoadBlueprint(paste0(input$active_blueprint, ".json")))
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

  shiny::shinyApp(ui, server)
}

blueprintEditorApp(blueprint.ini = getOption("rslates.bp.editor.blueprint"),
                   blueprint.dir = getOption("rslates.bp.editor.blueprint.dir") )


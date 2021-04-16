








blueprintEditorApp <- function(blueprint.filename = NULL) {

  # editorLoadBlueprint <- function(pathname) {
  #   if (is.null(pathname) || !file.exists(pathname)) {
  #     return(slateBlueprint(
  #       name = gsub("\\.json$", "", basename(pathname))
  #     ))
  #   }
  #
  #   tryCatch({
  #     loadBlueprint(filename = pathname, format = "auto")
  #     #blueprintFromJSON(filename = pathname)
  #   },
  #   error = function(e) {
  #     warning(e)
  #     slateBlueprint()
  #     #blueprintFromJSON(filename = pathname, preprocess = FALSE)
  #   })
  # }

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

  blueprint <- loadBlueprint(blueprint.filename)


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
            actionButton("new_blueprint", "New Blueprint", icon = icon("new"), class = "mr-2"),
            actionButton("load_blueprint", "Load Blueprint", icon = icon("load")),
            uiOutput("save_state", class = "ml-auto"),
            shinyjs::disabled(
              actionButton("save_blueprint", "Save Blueprint", icon = icon("save"), class = "ml-2")
            )
          ),
          tags$hr(),
          uiOutput("slate_imports"),
          tags$hr(),
          blueprintEditor2UI("editor", blueprint)
        )
      )
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

    # create the blueprint editor server
    blueprint.editor <- blueprintEditor2Server(
      "editor",
      blueprint = isolate(blueprint()),
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

    output$slate_ui <- renderUI({
      slateUI("the_slate", slate.options = slate.options)
    })

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

  if (getOption("rslates.run.themer") == TRUE)
    bslib::run_with_themer(shiny::shinyApp(ui, server))
  else
    shiny::shinyApp(ui, server)
}


initServerOptions()

blueprintEditorApp(blueprint.filename = getOption("rslates.bp.editor.blueprint.filename"))


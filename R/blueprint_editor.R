

blueprintEditorUI  <- function(id, blueprint) {
  ns <- NS(id)

  metadata.tab <- tabPanel(
    title = "Metadata",
    tags$div(
      class = "container pt-3",
      tags$div(
        class = "slates-flow slates-flow-2",
        textInput(ns("blueprint_name"), "Name", value = blueprint$name),
        textInput(ns("blueprint_author"), "Author(s)", value = blueprint$author),
        selectInput(ns("blueprint_category"), "Category",
                    choices = "", selected = ""),
        selectizeInput(ns("blueprint_tags"), label = "Tags", multiple = TRUE,
                       choices = getOption("rslates.blueprint.tags"),
                       selected = blueprint$tags,
                       options = list(
                         delimiter = '',
                         create = "function(input) { return { value: input, text: input } }"
                       )
        )
      )
    )
  )

  code.tab <- tabPanel(
    title = "Source Code",
    tags$div(
      class = "container pt-3",
      shinyAce::aceEditor(
        ns("blueprint_source"),
        height = "350px",
        mode = "r",
        value = blueprint$source
      ),
      uiOutput(ns("blueprint_alerts"))
    )
  )

  debug.tab <- tabPanel(
    title = "Debug",
    tags$div(
      class = "container pt-3",
      tags$div(
        class = "slates-flow slates-flow-3",
        selectInput(ns("debug_type"), label = "Debug",
                    choices = c("Blueprint", "State")),
        selectInput(ns("debug_element"),
                    label = "Element", choices = "")
      ),
      verbatimTextOutput(ns("blueprint_debug")) %>%
        tagAppendAttributes(style = "overflow: auto; height: 250px;")
    )
  )

  export.tab <- tabPanel(
    title = "Export",
    tags$div(
      class = "container pt-3",
      radioButtons(ns("export_format"), label = "Format", choices = c("YAML", "JSON")),
      verbatimTextOutput(ns("export_output"))
    )
  )

  tags$div(
    id = ns("blueprint_editor"),
    class = "container pt-3",
    tabsetPanel(
      metadata.tab,
      code.tab,
      debug.tab,
      export.tab
    ),
    tags$hr()
  )
}



blueprintEditorServer <- function(id, blueprint, slate, global.options = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # make a copy of the slate blueprint
    if (class(blueprint) == "reactivevalues")
      blueprint <- reactiveVal(isolate(reactiveValuesToList(blueprint)))
    else
      blueprint <- reactiveVal(blueprint)

    errors <- reactiveVal(list())

    rebuildBlueprint <- function(source) {
      blueprint <- blueprint()

      error <- tryCatch({
        new.blueprint <- slateBlueprint(
          name = blueprint$name,
          author = blueprint$author,
          category = blueprint$category,
          tags = blueprint$tags,
          source = source
        )

        values <- isolate(blueprint$inputs) %>%
          map(~getHandler(.)$as.value(., session)) %>%
          keep(map_lgl(., ~length(.) != 0))

        new.blueprint$inputs %<>%
          modify_if(~.$name %in% names(values),
                    ~list_modify(., value = values[[ .$name ]]))

        for (name in names(blueprint)) {
          if (!identical(blueprint[[ name ]], new.blueprint[[ name ]])) {
            dlog("changing", name)

            blueprint[[ name ]] <- new.blueprint[[ name ]]
          }
        }

        blueprint(new.blueprint)

        return(list())
      },
      error = function(e) {
        list(e)
      })

      return(error)
    }


    # preprocesses the blueprint source, fills in the preprocessor
    # reactiveValues structure and updates the blueprint reactiveValues
    # inputs and outputs structures
    observeEvent(input$blueprint_source, {
      req(source <- input$blueprint_source)
      req(source != isolate(blueprint()$source))

      dlog(isolate(blueprint()$name))

      errors <- list()

      errors <- append(errors, rebuildBlueprint(source))

      isolate(errors(errors))
    })


    observe(label = "blueprint.metadata", {
      req(
        !is.null(input$blueprint_name),
        blueprint <- blueprint(),
        !all(identical(blueprint$name, input$blueprint_name),
             identical(blueprint$author, input$blueprint_author),
             identical(blueprint$category, input$blueprint_category),
             identical(blueprint$tags, input$blueprint_tags))
      )

      dlog()

      blueprint$name <- input$blueprint_name
      blueprint$author <- input$blueprint_author
      blueprint$category <- input$blueprint_category

      if (is.null(input$blueprint_tags))
        blueprint$tags <- list()
      else
        blueprint$tags <- input$blueprint_tags


      blueprint(blueprint)
    })


    # displays error alerts from the preprocessor
    output$blueprint_alerts <- renderUI({
      dlog("blueprint_alerts", length(errors()))

      map(errors(),
          ~tags$div(
            class = "alert alert-danger",
            .$message
          )
      ) %>% tagList()
    })


    # change the ace theme on aceEditors
    observeEvent(global.options$ace.theme, {
      shinyAce::updateAceEditor(session, "blueprint_source", theme = global.options$ace.theme)
    })


    observe({
      shinyjs::toggle("debug_blueprint", condition = (input$debug_type == "Blueprint"))
      shinyjs::toggle("debug_state", condition = (input$debug_type == "State"))
    })


    observe({
      req(input$debug_type)

      selected <- input$debug_blueprint_element

      if (input$debug_type == "Blueprint")
        choices <-  names(blueprint())
      else
        choices <- names(slate)

      dlog()

      updateSelectInput(session, "debug_element",
                        choices = choices, selected = selected)
    })

    output$blueprint_debug <- renderText({
      str.str <- function(x) capture.output(str(x)) %>% paste(collapse = "\n")

      if (input$debug_type == "Blueprint") {
        blueprint()[[ input$debug_element ]] %>% str.str
      } else {
        obj <- slate[[ input$debug_element ]]

        if ("reactivevalues" %in% class(obj))
          obj <- reactiveValuesToList(obj)
        else if ("reactive" %in% class(obj))
          obj <- obj()

        if (class(obj) == "environment")
          obj <- as.list(obj)

        str.str(obj)
      }
    })


    output$export_output <- renderText({
      if (input$export_format == "YAML")
        blueprintToYAML(blueprint())
      else
        blueprintToJSON(blueprint())
    })


    updateBlueprint <- function(new.blueprint) {
      dlog(new.blueprint$name)

      blueprint <- blueprint()

      for (name in names(blueprint))
        blueprint[[ name ]] <- new.blueprint[[ name ]]

      updateTextInput(session, "blueprint_name", value = blueprint$name)
      updateTextInput(session, "blueprint_author", value = blueprint$author)
      updateSelectInput(session, "blueprint_category", selected = blueprint$category)
      updateSelectizeInput(session, "blueprint_tags",
                           choices = getOption("rslates.blueprint.tags"),
                           selected = blueprint$tags)

      shinyAce::updateAceEditor(session, "blueprint_source", value = blueprint$source)

      blueprint(blueprint)
    }

    list(
      blueprint = blueprint,
      updateBlueprint = updateBlueprint
    )

  })
}


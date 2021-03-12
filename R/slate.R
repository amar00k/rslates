















#' Setup Slate Display and Behaviour Options
#'
#' @param envir the parent environment where the slate code is executed.
#' @param height height of the slate in any css unit (e.g. "500px"). If NULL
#'   the height of the slate is unrestricted.
#' @param use.card use a bs4 card as a container for the slate UI.
#' @param card.header show the card header. Only used if `use.card` is TRUE.
#' @param open.settings start with the input settings panel open.
#' @param inputs.style the style used for the inputs panel. `"tabset"` uses
#'   a tabsetPanel as a container for input pages. `"collapses"` uses bs4
#'   collapse panels. `"flowing"` displays all pages in a single vertical container.
#'
#' @return a list of options to be passed to `slateUI()` and `slateServer()`.
#' @export
slateOptions <- function(envir = new.env(),
                         height = NULL,
                         use.card = TRUE,
                         card.header = TRUE,
                         open.inputs = TRUE,
                         open.editor = FALSE,
                         inputs.style = c("tabset", "collapses", "flowing")) {
  # envir is a reactiveVal
  # if (!("reactiveVal" %in% class(envir)))
  #   envir <- reactiveVal(envir)

  return(list(
    envir = envir,
    height = height,
    use.card = use.card,
    card.header = card.header,
    open.inputs = open.inputs,
    open.editor = open.editor,
    inputs.style = inputs.style
  ))
}


blueprintEditorUI  <- function(id, blueprint) {
  ns <- NS(id)

  tags$div(
    id = ns("blueprint_editor"),
    class = "container pt-3",
    tags$div(
      class = "slates-flow-2",
      textInput(ns("blueprint_name"), "Name", value = blueprint$name),
      textInput(ns("blueprint_author"), "Author(s)", value = blueprint$author),
      selectInput(ns("blueprint_category"), "Category",
                  choices = "", selected = ""),
      selectizeInput(ns("blueprint_tags"), label = "Tags", multiple = TRUE,
                     choices = blueprint$tags,
                     selected = blueprint$tags,
                     options = list(
                       delimiter = '',
                       create = "function(input) { return { value: input, text: input } }"
                     )
      )
    ),
    shinyAce::aceEditor(
      ns("blueprint_source"),
      height = "250px",
      mode = "r",
      value = blueprint$source
    ),
    uiOutput(ns("blueprint_alerts")),
    tags$hr()
  )

}



blueprintEditorServer <- function(id, blueprint, global.options = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    errors <- reactiveVal(list())

    # preprocesses the blueprint source, fills in the preprocessor
    # reactiveValues structure and updates the blueprint reactiveValues
    # inputs and outputs structures
    observe(label = "source.changed", {
      dlog()

      req(source <- input$blueprint_source)

      errors <- list()
      values <- map(isolate(blueprint$inputs), ~getHandler(.)$as.value(., session))

      tryCatch({
        new.blueprint <- slateBlueprint(
          name = blueprint$name,
          author = blueprint$author,
          category = blueprint$category,
          tags = blueprint$tags,
          source = source
        )

        values <- keep(values, map_lgl(values, ~length(.) != 0))
        new.blueprint$inputs %<>%
          modify_if(~.$name %in% names(values),
                    ~list_modify(., value = values[[ .$name ]]))

        for (name in names(blueprint)) {
          if (!identical(blueprint[[ name ]], new.blueprint[[ name ]])) {
            dlog("changing", name)
            #print(new.blueprint[[ name ]])

            blueprint[[ name ]] <- new.blueprint[[ name ]]
          }
        }
      },
      error = function(e) {
        isolate(errors %<>% append(list(paste(toString(e))))) # e$message)))
      })

      isolate(errors(errors))
    })


    observe(label = "blueprint.metadata", {
      req(!is.null(input$blueprint_name))

      dlog()

      blueprint$name <- input$blueprint_name
      blueprint$author <- input$blueprint_author
      blueprint$category <- input$blueprint_category
      blueprint$tags <- input$blueprint_tags
    })


    # displays error alerts from the preprocessor
    output$blueprint_alerts <- renderUI({
      dlog("blueprint_alerts")

      map(errors(),
          ~tags$div(
            class = "alert alert-danger",
            . #$message
          )
      ) %>% tagList()
    })


    # change the ace theme on aceEditors
    observeEvent(global.options$ace.theme, {
      shinyAce::updateAceEditor(session, "blueprint_source", theme = global.options$ace.theme)
    })


    setBlueprint <- function(new.blueprint) {
      shinyAce::updateAceEditor(session, "blueprint_source", value = new.blueprint$source)
    }


    list(
      setBlueprint = setBlueprint
    )

  })
}





# labels for the "Edit Blueprint" menu item
edit.blueprint.labels <- list(
  open = tags$div(
    icon("screwdriver"), "Edit Blueprint",
    tags$div(
      class = "font-weight-light small",
      "Edit the slate blueprint source code.",
      tags$br(),
      tags$i("Warning: you may lose your current settings.")
    )
  ),
  close = tags$div(
    icon("times"), tags$b("Close Editor"),
    tags$div(
      class = "font-weight-light small",
      "Return to normal view with",
      "current settings."
    )
  )
)



slateUI <- function(id, blueprint, slate.options = slateOptions()) {
  ns <- NS(id)

  #height <- slate.options$height
  #if (is.null(height))
  height <- "520px"

  dropdown.ui <- tags$div(
    shinyWidgets::dropdownButton(
      inputId = ns("settings_button"), label = "", icon = icon("cog"),
      circle = FALSE, inline = FALSE, right = TRUE,
      actionLink(
        ns("slate_rename"),
        label = tags$div(icon("pencil"), "Rename")
      ),
      tags$hr(),
      tags$a(
        href = "javascript:void(0);",
        shinyWidgets::prettyToggle(
          inputId = ns("slate_view_inputs"),
          label_off = "View Inputs",
          label_on = "Hide Inputs",
          icon_off = icon("eye"),
          icon_on = icon("eye-slash"),
          value = TRUE,
          status_on = "default",
          status_off = "default",
          plain = TRUE
        )
      ),
      actionLink(
        ns("slate_reset"),
        label = tags$div(
          icon("undo"), "Reset Inputs",
          tags$div(
            class = "font-weight-light small",
            #style = "white-space: normal;",
            "Reset all inputs to their default values."
          )
        )
      ),
      tags$hr(),
      actionLink(
        ns("slate_export"),
        label = tags$div(
          icon("file-export"), "Export...",
          tags$div(
            class = "font-weight-light small",
            #style = "white-space: normal;",
            "Export this slate to R source, RMarkdown",
            tags$br(),
            "or HTML."
          )
        )
      ),
      tags$hr(),
      actionLink(
        ns("slate_edit"),
        label = edit.blueprint.labels$open
      )
    )
  )

  body.ui <- tags$div(
    #shinyjs::hidden(editor.ui),
    uiOutput(ns("blueprint_editor_ui")),
    tags$div(
      class = "d-flex mh-100 flex-row justify-content-between align-content-stretch",
      style = if (!is.null(height)) paste0("height: ", height, ";") else "",
      tags$div(
        class = "flex-fill"),
      tags$div(
        class = "col-6",
        style = "overflow: auto;",
        tags$div(
          class = "card-body",
          uiOutput(ns("outputs_panel"))
        )
      ),
      tags$div(
        class = "flex-fill"),
      tags$div(
        id = ns("slate_inputs_container"),
        class = "slate-inputs-container col-6 show",
        tags$div(
          class = "collapse show",
          style = "width: 100%",
          div(
            style = "height: 100%; overflow-y: auto; overflow-x: hidden;",
            uiOutput(ns("inputs_panel")),
          )
        )
      )
    )
  )


  if (slate.options$use.card) {
    if (slate.options$card.header == TRUE) {
      header.ui <- tags$div(
        class="card-header d-flex justify-content-between",
        textOutput(ns("title"), container = tags$p),
        tags$div(
          class = "ml-auto",
          dropdown.ui
        )
      )
    } else {
      header.ui <- NULL
    }

    slate.class <- "card slate"
    if (isolate(slate.options$open.editor) == TRUE)
      slate.class <- paste(slate.class, "editing border-warning")

    ui <- tags$div(
      id = ns("slate_div"),
      class = slate.class,
      header.ui,
      body.ui
    )
  } else {
    ui <- body.ui
  }

  tagList(ui)
}



slateServer <- function(id, blueprint = NULL, slate.options = NULL, global.options = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # the blueprint
    if (is.null(blueprint))
      blueprint <- do.call(reactiveValues, slateBlueprint())
    else if (class(blueprint) != "reactivevalues")
      blueprint <- do.call(reactiveValues, blueprint)

    # the options
    if (is.null(slate.options))
      slate.options <- do.call(reactiveValues, slateOptions())
    else if (class(slate.options) != "reactivevalues")
      slate.options <- do.call(reactiveValues, slate.options)

    # the global options
    if (is.null(global.options)) {
      global.options <- reactiveValues(
        ace.theme = getOption("rslates.default.ace.theme")
      )
    }

    ready <- uiReady(session)

    # the title
    slate.title <- reactiveVal(isolate(blueprint$name))

    # observers created by this slate
    # will be destroyed if the slate is removed
    observers <- reactiveValues()

    # the server for the blueprint editor
    # TODO: don't even create until user requests it
    blueprint.editor <- blueprintEditorServer(
      "editor",
      blueprint = blueprint,
      global.options = global.options
    )


    # provides the slate (not blueprint) values of all inputs
    input.values <- reactive({
      dlog()

      blueprint$inputs %>%
        map(~getHandler(.)$as.value(., session))
    })


    # provides the sources for all outputs (with variables substituted)
    # TODO: display errors somewhere...
    output.sources <- reactive({
      dlog()

      inputs <- blueprint$inputs
      outputs <- blueprint$outputs
      blocks <- blueprint$blocks
      values <- input.values()

      inputs <- assignInputValues(inputs, values)

      map(outputs, ~{
        new.source <- tryCatch({
          substituteVariables(.$source, blocks, inputs)
        },
        error = function(e) {
          print(e)

          return(.$source)
        })

        list_modify(., source = new.source)
      })
    })


    # Everything that needs to be done AFTER the UI has been created.
    # This observer will abort until the UI is fully loaded,
    # it will then run its contents and self destroy
    .init <- observe(label = "on.init", {
      req(ready())

      # view / hide inputs
      observers$view.inputs <- observeEvent(input$slate_view_inputs, {
        dlog()

        # close the menu
        shinyjs::click("settings_button")

        shinyjs::toggleElement("slate_inputs_container", condition = input$slate_view_inputs)
      }, ignoreInit = TRUE)

      # kill me
      .init$destroy()
    })


    # when the user clicks on edit blueprint
    observers$slate.edit <- observeEvent(input$slate_edit, {
      dlog()

      # close the menu
      shinyjs::click("settings_button")

      slate.options$open.editor <- !slate.options$open.editor
    })


    # when edit mode is turned on or off
    observers$open.editor <- observe(label = "open.editor", {
      dlog()

      if (slate.options$open.editor == TRUE) {
        updateActionLink(session,
                         "slate_edit",
                         label = as.character(edit.blueprint.labels$close))
      } else {
        updateActionLink(session,
                         "slate_edit",
                         label = as.character(edit.blueprint.labels$open))
      }

      shinyjs::toggleClass("slate_div",
                           class = "editing border-warning",
                           condition = slate.options$open.editor)

      shinyjs::toggleState("slate_rename", condition = !slate.options$open.editor)
      shinyjs::toggleState("slate_export", condition = !slate.options$open.editor)
    })


    # when the user clicks on reset inputs
    # we update all UI inputs
    observers$slate.reset <- observeEvent(input$slate_reset, {
      dlog()

      # close the menu
      shinyjs::click("settings_button")

      inputs <- blueprint$inputs %>%
        map(~list_modify(., value = .$default))

      for (x in inputs)
        getHandler(x)$update.ui(x, session)
    })


    # observe inputs and toggle the state of the
    # reset inputs button
    observers$reset.inputs <- observe(label = "reset.inputs", {
      values <- input.values()
      all.default <- all(map_lgl(blueprint$inputs, ~identical(values[[ .$name ]], .$default)))

      shinyjs::toggleState("slate_reset", condition = !all.default)
    })


    #
    # Outputs
    #

    # the title display
    output$title <- renderText({
      slate.title()
    })


    # displays inputs panel
    output$inputs_panel <- renderUI({
      dlog("inputs_panel")

      # remove all tooltips
      shinyjs::runjs("$('.tooltip').remove();")

      createInputLayout(
        pages = blueprint$pages,
        groups = blueprint$groups,
        inputs = blueprint$inputs,
        ns = ns,
        inputs.style = slate.options$inputs.style
      )
    })


    # displays the outputs panel
    output$outputs_panel <- renderUI({
      req(outputs <- blueprint$outputs)
      req(sources <- output.sources())

      dlog("outputs_panel")

      selected <- isolate(input$output_tabs)

      output.tabs <- unname(lapply(outputs, function(x) {
        stopifnot(x$type %in% names(output.handlers))

        tabPanel(
          title = x$name,
          tags$div(
            tags$br(),
            getHandler(x)$create.ui(ns(x$id), x$name)
          )
        )
      }))

      text <- map(sources, ~paste0("#-- ", .$name, "\n", .$source)) %>%
        paste(collapse = "\n\n")

      output.tabs <- append(output.tabs, list(
        tabPanel(
          title = "Source",
          tags$br(),
          shinyAce::aceEditor(ns("debug_source"),
                              value = text,
                              mode = "r",
                              height = "300px",
                              readOnly = TRUE,
                              showLineNumbers = TRUE,
                              theme = global.options$ace.theme)
        )
      ))

      output.tabs$id <- ns("output_tabs")
      output.tabs$type <- "pills"
      output.tabs$selected <- selected
      outputs.ui <- do.call(tabsetPanel, output.tabs)
    })


    # updates the source debug output when values or theme change
    observers$debug.source <- observe(label = "debug.source", {
      sources <- isolate(output.sources())

      text <-
        map(sources, ~paste0("#-- ", .$name, "\n", .$source)) %>%
        paste(collapse = "\n\n")

      shinyAce::updateAceEditor(
        session, editorId = "debug_source", value = text, theme = global.options$ace.theme
      )
    })

    #
    # output$debug_source <- renderText({
    #   map(output.sources(), ~paste0("#-- ", .$name, "\n", .$source)) %>%
    #     paste(collapse = "\n\n")
    # })


    # provides the environment where outputs will be evaluated
    slate.envir <- reactive({
      # inputs <- input.list()
      # env <- new.env(parent = slate.options$envir())
      #
      # env <- tryCatch({
      #   for (d in blueprint$datasets) {
      #     if (!is.null(d$source)) {
      #       src <- buildSource(d$source, input.list())
      #       env[[ d$name ]] <- eval(parse(text = src), envir = new.env(parent = env))
      #     }
      #   }
      #
      #   return(env)
      # },
      # error = function(e) {
      #   # ignore errors
      #   return(env)
      # })

      env <- new.env(parent = slate.options$envir)

      return(env)
    })


    # creates output handlers when output structure changes
    observers$output.handlers <- observe(label = "output.handlers", {
      dlog()

      for (x in blueprint$outputs) {
        getHandler(x)$create.output(
          x, session, output.sources, input.values, slate.envir
        )
      }
    })


    # handles the output observers
    # TODO: assign each output its own observer
    observers$outputs <- observe(label = "outputs", {
      dlog()

      global.options$ace.theme

      for (x in blueprint$outputs) {
        getHandler(x)$observer(
          x$id, session, output.sources, input.values, slate.envir, global.options
        )
      }
    })


    # handles the inputs observers
    # TODO: assign each input its own observer
    observers$inputs <- observe(label = "inputs", {
      dlog()

      for (x in blueprint$inputs) {
        getHandler(x)$observer(x, session)
      }
    })


    # when inputs change, update their BS4 tooltips
    observers$tooltips <- observe(label = "tooltips", {
      dlog()

      map(names(input), ~input[[ . ]])

      shinyjs::runjs("$('[data-toggle=\"tooltip\"]').tooltip();")
    })



    output$blueprint_editor_ui <- renderUI({
      if (slate.options$open.editor == FALSE)
        return(tagList())

      blueprint <- isolate(reactiveValuesToList(blueprint))
      blueprintEditorUI(ns("editor"), blueprint)
    })


    setBlueprint <- function(new.blueprint) {
      #for (name in names(blueprint))
      #  blueprint[[ name ]] <- new.blueprint[[ name ]]

      blueprint.editor$setBlueprint(new.blueprint)
    }


    # Cleanup function
    destroy <- function() {
      # remove inputs
      # https://www.r-bloggers.com/2020/02/shiny-add-removing-modules-dynamically/
      id <- session$ns("")
      for (x in ns(names(input))) {
        .subset2(input, "impl")$.values$remove(x)
      }

      # remove outputs
      for (x in ns(names(output.handlers))) {
        output[[ x ]] <- NULL
      }

      # remove observers
      observers %>%
        reactiveValuesToList %>%
        map(~.$destroy())
    }

    return(
      list(
        blueprint = blueprint,
        slate.options = slate.options,
        global.options = global.options,
        setBlueprint = setBlueprint,
        destroy = destroy
        #blueprint = blueprint,
        #inputs = input.list,
        #import.data = import.data,
      )
    )
  })
}







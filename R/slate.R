















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
                     choices = getOption("rslates.tag.list"),
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



blueprintEditorServer <- function(id, blueprint = slateBlueprint(), global.options = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # make a copy of the slate blueprint
    if (class(blueprint) == "reactiveValues")
      blueprint <- reactiveVal(reactiveValuesToList(blueprint))
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
        list(toString(e))
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
      req(length(errors()) > 0)

      dlog("blueprint_alerts", length(errors()))

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


    updateBlueprint <- function(new.blueprint) {
      dlog(new.blueprint$name)

      blueprint <- blueprint()

      for (name in names(blueprint))
        blueprint[[ name ]] <- new.blueprint[[ name ]]

      updateTextInput(session, "blueprint_name", value = blueprint$name)
      updateTextInput(session, "blueprint_author", value = blueprint$author)
      updateSelectInput(session, "blueprint_category", selected = blueprint$category)
      updateSelectizeInput(session, "blueprint_tags",
                           choices = getOption("rslates.tag.list"),
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


    updateBlueprint <- function(new.blueprint, update.editor = TRUE) {
      dlog()

      if (!identical(blueprint$outputs, new.blueprint$outputs)) {
        for (x in blueprint$outputs) {
          output[[ x$id ]] <- NULL
        }
      }

      for (name in names(blueprint))
        blueprint[[ name ]] <- new.blueprint[[ name ]]

      if (update.editor == TRUE)
        blueprint.editor$updateBlueprint(new.blueprint)
    }


    # the blueprint
    # if (is.null(blueprint))
    #   blueprint <- do.call(reactiveValues, slateBlueprint())
    # else if (class(blueprint) != "reactivevalues")
    #   blueprint <- do.call(reactiveValues, blueprint)


    #
    # Inititalization
    #

    # ready utility
    ready <- uiReady(session)

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

    # create the blueprint container
    if (is.null(blueprint)) {
      blueprint <- do.call(reactiveValues, slateBlueprint())
    } else if (class(blueprint) == "reactiveValues") {
      blueprint <- do.call(reactiveValues, reactiveValuesToList(blueprint))
    } else {
      blueprint <- do.call(reactiveValues, blueprint)
    }

    # create the server for the blueprint editor
    # TODO: don't even create until user requests it
    blueprint.editor <- blueprintEditorServer(
      "editor",
      blueprint = blueprint,
      global.options = global.options
    )

    # the title
    slate.title <- reactiveVal(isolate(blueprint$name))

    # observers created by this slate
    # will be destroyed if the slate is removed
    observers <- reactiveValues()


    # # Finish initializing by loading in the initial blueprint
    # isolate(
    #   resetBlueprint(blueprint.ini)
    # )


    # observe changes in editor
    observeEvent(blueprint.editor$blueprint(), label = "editor.change", {
      req(!identical(reactiveValuesToList(blueprint),
                     blueprint.editor$blueprint())
      )

      dlog()

      updateBlueprint(blueprint.editor$blueprint(), update.editor = FALSE)
    })


    # provides the slate (not blueprint) values of all inputs
    input.values <- reactive({
      dlog()

      inputs <- blueprint$inputs %>%
        map(~getHandler(.)$as.value(., session))

      dlog(inputs)

      return(inputs)
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


    # observe inputs and toggle the state of the reset inputs button
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
      dlog("inputs_panel", isolate(blueprint$name), names(blueprint$inputs))

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
      outputs <- blueprint$outputs

      req(length(outputs) > 0)

      dlog("outputs_panel", isolate(blueprint$name), names(blueprint$outputs))

      selected <- isolate(input$output_tabs)
      if (!is.null(selected) && !(selected %in% names(outputs)))
        selected <- names(outputs)[1]

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

      output.tabs <- append(output.tabs, list(
        tabPanel(
          title = "Source",
          tags$br(),
          uiOutput(ns("source_debug_ui"))
        )
      ))

      output.tabs$id <- ns("output_tabs")
      output.tabs$type <- "pills"
      output.tabs$selected <- selected
      outputs.ui <- do.call(tabsetPanel, output.tabs)
    })


    output$source_debug_ui <- renderUI({
      sources <- output.sources()

      text <- map(sources, ~paste0("#-- ", .$name, "\n", .$source)) %>%
        paste(collapse = "\n\n")

      shinyAce::aceEditor(ns("debug_source"),
                          value = text,
                          mode = "r",
                          height = "300px",
                          readOnly = TRUE,
                          showLineNumbers = TRUE,
                          theme = global.options$ace.theme)
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




    # Cleanup function
    destroy <- function() {
      # remove inputs
      # https://www.r-bloggers.com/2020/02/shiny-add-removing-modules-dynamically/
      id <- session$ns("")
      for (x in ns(names(input))) {
        .subset2(input, "impl")$.values$remove(x)
      }

      # remove outputs
      # for (x in ns(names(output.handlers))) {
      #   output[[ x ]] <- NULL
      # }

      # remove observers
      observers %>%
        reactiveValuesToList %>%
        map(~.$destroy())
    }

    return(
      list(
        blueprint = reactive(reactiveValuesToList(blueprint)),
        slate.options = slate.options,
        global.options = global.options,
        updateBlueprint = updateBlueprint,
        destroy = destroy
        #blueprint = blueprint,
        #inputs = input.list,
        #import.data = import.data,
      )
    )
  })
}







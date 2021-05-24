

#' @export
slateData <- function(blueprint = slateBlueprint(),
                      title = "Untitled",
                      input.values = list(),
                      import.values = list(),
                      export.values = list(),
                      envir = new.env()) {
  stopifnot(class(blueprint) == "slateBlueprint")

  input.values <-
    assignInputValues(blueprint$inputs, input.values) %>%
    map("value")

  x <- list(
    blueprint = blueprint,
    title = title,
    input.values = input.values,
    import.values = import.values,
    export.values = export.values,
    envir = envir
  )

  class(x) <- "slateData"

  return(x)
}


#' @export
print.slateData <- function(x) {
  cat("Blueprint:", x$blueprint$name, "\n")
  cat("Title:", x$title, "\n")
  cat(
    "Input values:",
    paste(names(x$input.values), x$input.values, sep = " = ", collapse = ", "),
    "\n"
  )
  cat("Imports:", names(x$import.values), "\n")
  cat("Exports:", names(x$export.values), "\n")
}


#' Get all user sources from a slate
#'
#' @description Get user source code for all parts of a slate in the form of a names list of
#'   strings.
#'
#' @param x the slate data structure
#'
#' @return a list with elements `imports`, `toplevel` and `outputs`.
#' @export
getSlateUserSources <- function(x) {
  stopifnot(class(x) == "slateData")

  outputs <- x$blueprint$outputs
  toplevel <- x$blueprint$toplevel
  substitutions <- x$blueprint$substitutions
  import.values <- x$import.values

  inputs <- assignInputValues(x$blueprint$inputs, x$input.values)

  sources <- list()

  # sources$outputs <-
  #   map(outputs, function(output) {
  #     output$text <- map(output$source, ~{
  #       text <- tryCatch({
  #         substituteVariables(.$text, substitutions, inputs)
  #       },
  #       error = function(e) {
  #         return(.$text)
  #       })
  #
  #       return(text)
  #     }) %>% paste(collapse = "\n")
  #
  #     return(output)
  #   })
  sources$outputs <-
    map(outputs, ~{
      .$text <- tryCatch({
        substituteVariables(.$source, substitutions, inputs)
      },
      error = function(e) {
        strsplit(.$source, split = "\n")[[1]] %>%
          paste("##", ., collapse = "\n") %>%
          paste(
            "## There was an error processing output source:",
            "##", ., sep = "\n"
          )
      })

      return(.)
    })

  sources$toplevel <-
    tryCatch({
      substituteVariables(toplevel, substitutions, inputs)
    },
    error = function(e) {
      return(toplevel)
    })

  sources$imports <-
    x$blueprint$imports %>%
    keep(map_lgl(., ~!is.null(import.values[[ .$name ]]))) %>%
    map_chr(~{
      import.handlers[[ .$type ]]$make.source(., import.values[[ .$name ]])
    })

  return(sources)
}


#' Get formatted user code from slate
#'
#' @param x the slate data structure
#' @param headers whether to include section headers as comments
#'
#' @return a single character string
#' @export
getSlateUserSource <- function(x, headers = TRUE) {
  stopifnot(class(x) == "slateData")

  sources <- getSlateUserSources(x)

  import.code <- sources$imports %>%
    paste(collapse = "\n")

  toplevel.code <- sources$toplevel

  output.code <- map(sources$outputs, ~paste0("#-- ", .$name, "\n", .$text)) %>%
    paste(collapse = "\n\n")

  paste(import.code, toplevel.code, output.code, sep = "\n\n")
}


getSlateUserEnvir <- function(x) {
  stopifnot(class(x) == "slateData")

  sources <- getSlateUserSources(x)
  envir <- new.env(x$envir)

  envir <- tryCatch({
    for (x in sources$imports) {
      eval(parse(text = x), envir = envir)
    }

    eval(parse(text = sources$toplevel), envir = envir)

    return(envir)
  },
  error = function(e) {
    warning("There was an error evaluating slate ", x$title)

    return(envir)
  })

  return(envir)
}



getSlateExportData <- function(x) {
  stopifnot(class(x) == "slateData")

  envir <- getSlateUserEnvir(x)

  varnames <- map(x$blueprint$exports, ~{
    if (!grepl("^\\.", .$out.name))
      .$out.name
    else if (.$out.name == ".title")
      x$title
    else
      as.character(x$input.values[[ sub("^\\.", "", .$out.name) ]])
  })

  map(x$blueprint$exports, ~{
    envir[[ .$var.name ]]
  }) %>%
    set_names(varnames)
}




slateStateFromServer <- function(server, json = FALSE) {
  state <- list(
    blueprint = server$blueprint(),
    slate.options = reactiveValuesToList(server$slate.options),
    input.values = server$input.values(),
    import.values = reactiveValuesToList(server$import.values),
    export.data = server$export.data()
  )
}



# slateState <- function(blueprint, input.values, import.value, export.data) {
#
# }




#' Setup Slate Display and Behaviour Options
#'
#' @param envir the parent environment where the slate code is executed.
#' @param height height of the slate in any css unit (e.g. "500px"). If NULL
#'   the height of the slate is unrestricted.
#' @param container container for the slate. Either one of "card", "none", or a
#'   function used to contruct the container UI.
#' @param use.card use a bs4 card as a container for the slate UI.
#' @param card.header show the card header. Only used if `use.card` is TRUE.
#' @param inputs.style the style used for the inputs panel. `"tabset"` uses
#'   a tabsetPanel as a container for input pages. `"collapses"` uses bs4
#'   collapse panels. `"flowing"` displays all pages in a single vertical container.
#' @param view.inputs
#' @param open.editor
#'
#' @return a list of options to be passed to `slateUI()` and `slateServer()`.
#' @export
slateOptions <- function(envir = new.env(),
                         height = NULL,
                         container = "card",
                         use.card = TRUE,
                         card.header = TRUE,
                         view.inputs = TRUE,
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
    view.inputs = view.inputs,
    open.editor = open.editor,
    inputs.style = inputs.style
  ))
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



slateUI <- function(id, slate.options = slateOptions()) {
  ns <- NS(id)

  height <- slate.options$height

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
          inputId = ns("view_inputs"),
          label_off = "View Inputs",
          label_on = "Hide Inputs",
          icon_off = icon("eye"),
          icon_on = icon("eye-slash"),
          value = slate.options$view.inputs,
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

  # body.ui <- tags$div(
  #   #shinyjs::hidden(editor.ui),
  #   uiOutput(ns("blueprint_editor_ui")),
  #   tags$div(
  #     class = "d-flex mh-100 flex-row justify-content-between align-content-stretch",
  #     style = if (!is.null(height)) paste0("height: ", height, ";") else "",
  #     tags$div(
  #       class = "flex-fill"),
  #     tags$div(
  #       class = "col-6",
  #       #style = "overflow: auto;",
  #       tags$div(
  #         class = "card-body h-100",
  #         uiOutput(ns("outputs_panel"), class = "h-100")
  #       )
  #     ),
  #     tags$div(
  #       class = "flex-fill"),
  #     tags$div(
  #       id = ns("slate_inputs_container"),
  #       class = "slate-inputs-container col-6 show",
  #       tags$div(
  #         class = "collapse show",
  #         style = "width: 100%",
  #         div(
  #           style = "height: 100%; overflow-y: auto; overflow-x: hidden;",
  #           uiOutput(ns("inputs_panel")),
  #         )
  #       )
  #     )
  #   )
  # )


  body.ui <- tags$div(
    uiOutput(ns("blueprint_editor_ui")),
    tags$div(
      class = "d-flex",
      style = if (!is.null(height)) paste0("height: ", height, ";") else "",
      div(
        id = "outputs_panel_div",
        class = "w-50 p-2",
        uiOutput(ns("outputs_panel"))
      ),
      div(
        id = "inputs_panel_div",
        class = "w-50 p-2",
        uiOutput(ns("inputs_panel"), style = "height: 100%; overflow-y: auto; overflow-x: hidden;")
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



slateServer <- function(id,
                        slate = slateData(),
                        blueprint = slateBlueprint(),
                        slate.options = NULL,
                        global.options = NULL,
                        input.values = NULL,
                        import.values = list(),
                        simulate = FALSE) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    updateBlueprint <- function(new.blueprint,
                                update.editor = TRUE) {
      dlog()

      if (!identical(blueprint$outputs, new.blueprint$outputs)) {
        for (x in blueprint$outputs) {
          output[[ x$id ]] <- NULL
        }
      }

      for (name in names(blueprint))
        blueprint[[ name ]] <- new.blueprint[[ name ]]
    }

    #
    # Inititalization
    #

    # ready utility
    ready <- uiReady(session)

    # the global options
    if (is.null(global.options)) {
      global.options <- reactiveValues(
        ace.theme = getOption("rslates.themes-ace")$default
      )
    }

    # the options
    if (is.null(slate.options))
      slate.options <- do.call(reactiveValues, slateOptions())
    else if (class(slate.options) != "reactivevalues")
      slate.options <- do.call(reactiveValues, slate.options)


    # slate <- do.call(reactiveValues, slate)
    # slate$blueprint <- do.call(reactiveValues, slate$blueprint)

    # create the blueprint container
    if ("reactiveVal" %in% class(blueprint))
      blueprint <- isolate(blueprint())

    if (class(blueprint) == "reactiveValues") {
      blueprint <- do.call(reactiveValues, reactiveValuesToList(blueprint))
    } else {
      blueprint <- do.call(reactiveValues, blueprint)
    }

    if (!is.null(input.values)) {
      blueprint$inputs %<>% assignInputValues(input.values)
    }

    # the title
    slate.title <- reactiveVal(isolate(blueprint$name))

    # import data: where the caller may "place" data to be imported by the slate
    import.values <- do.call(reactiveValues, import.values)

    # observers created by this slate
    # will be destroyed if the slate is removed
    observers <- reactiveValues()


    slate.data <- reactive({
      blueprint <- slateBlueprint(
        name = blueprint$name,
        author = blueprint$author,
        category = blueprint$category,
        tags = blueprint$tags,
        outputs = blueprint$outputs,
        inputs = blueprint$inputs,
        pages = blueprint$pages,
        groups = blueprint$groups,
        source = blueprint$source,
        preprocess = TRUE
      )

      slateData(blueprint = blueprint,
                title = slate.title(),
                input.values = input.values(),
                import.values = reactiveValuesToList(import.values),
                envir = slate.options$envir)
    })

    # # observe changes in editor
    # observeEvent(blueprint.editor$blueprint(), label = "editor.change", {
    #   req(!identical(reactiveValuesToList(blueprint),
    #                  blueprint.editor$blueprint())
    #   )
    #   dlog()
    #
    #   updateBlueprint(blueprint.editor$blueprint(), update.editor = FALSE)
    # })


    # provides the state of all inputs
    input.values <- reactive({
      dlog()

      if (simulate == FALSE) {
        map(blueprint$inputs, ~getHandler(.)$as.value(., session))
      } else {
        map(blueprint$inputs, ~getHandler(.)$as.value(., value = .$value))
      }
    })


    # provides the sources for all outputs (with variables substituted)
    # TODO: display errors somewhere...
    sources <- reactive({
      getSlateUserSources(slate.data())
    })


    # this is the main source output of the slate
    source.code <- reactive({
      getSlateUserSource(slate.data())
    })


    # provides the environment where outputs will be evaluated
    # imports are evaluated first, then toplevel source code,
    # and eventually user-provided slate code
    slate.envir <- reactive({
      getSlateUserEnvir(slate.data())
    })


    # export data: exposes exported variables to the caller so they
    # can be fetched
    export.data <- reactive({
      getSlateExportData(slate.data())
    })


    # Everything that needs to be done AFTER the UI has been created.
    # This observer will abort until the UI is fully loaded,
    # it will then run its contents and self destroy
    .init <- observe(label = "on.init", {
      req(ready())

      shinyjs::toggleElement("slate_inputs_container", condition = input$view_inputs)

      # view / hide inputs
      observers$view.inputs <- observeEvent(input$view_inputs, {
        dlog()

        # close the menu
        shinyjs::click("settings_button")

        shinyjs::toggleElement("slate_inputs_container", condition = input$view_inputs)
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
    # UI
    #

    # the title display
    output$title <- renderText({
      slate.title()
    })


    # output$body_ui <- renderUI({
    #   height <- slate.options$height
    #
    #   container <- function(outputs, inputs) {
    #     tags$div(
    #       class = "d-flex",
    #       style = if (!is.null(height)) paste0("height: ", height, ";") else "",
    #       outputs,
    #       inputs
    #     )
    #   }
    #
    #   if (isTruthy(input$view_inputs)) {
    #     container(
    #       uiOutput(ns("outputs_panel"), class = "p-2 w-50"),
    #       uiOutput(ns("inputs_panel"), class = "p-2 w-50")
    #     )
    #   } else {
    #     container(
    #       uiOutput(ns("outputs_panel"), class = "p-2 w-100"),
    #       NULL
    #     )
    #   }
    # })


    # displays inputs panel
    output$inputs_panel <- renderUI({
      req(input$view_inputs)

      dlog("inputs_panel", isolate(blueprint$name), names(blueprint$inputs))

      # remove all tooltips
      shinyjs::runjs("$('.tooltip').remove();")

      createInputLayout(
        pages = blueprint$pages,
        groups = blueprint$groups,
        inputs = blueprint$inputs,
        ns = ns,
        inputs.style = slate.options$inputs.style
      ) %>%
        div(class = "h-100", style = "overflow-x: hidden; overflow-y: auto;", .)
    })


    # displays the outputs panel
    output$outputs_panel <- renderUI({
      outputs <- blueprint$outputs

      req(length(outputs) > 0)

      dlog("outputs_panel", isolate(blueprint$name), names(blueprint$outputs))

      selected <- isolate(input$output_tabs)
      if (!is.null(selected) && !(selected %in% names(outputs)))
        selected <- names(outputs)[1]

      dlog(selected)

      output.tabs <- unname(lapply(outputs, function(x) {
        stopifnot(x$type %in% names(output.handlers))

        tabPanel(
          title = x$name,
          tags$div(
            tags$br(),
            getHandler(x)$createUI(x, session)
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

      outputs.ui <- do.call(tabsetPanel, output.tabs) %>%
        tagAppendAttributes(class = "h-100 d-flex flex-column")

      outputs.ui$children[[2]] %<>% tagAppendAttributes(class = "overflow-auto")

      outputs.ui %>% div(class = "h-100", .)
    })


    output$source_debug_ui <- renderUI({
      text <- source.code()

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
      sources <- isolate(sources())

      text <-
        map(sources$outputs, ~paste0("#-- ", .$name, "\n", .$text)) %>%
        paste(collapse = "\n\n")

      shinyAce::updateAceEditor(
        session, editorId = "debug_source", value = text, theme = global.options$ace.theme
      )
    })


    # creates output handlers when output structure changes
    observers$output.handlers <- observe(label = "output.handlers", {
      dlog()

      for (x in blueprint$outputs) {
        output[[ x$id ]] <- getHandler(x)$createRenderer(
          x, session, sources, input.values, slate.envir
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
          x$id, session, sources, input.values, slate.envir, global.options
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


    # # create the server for the blueprint editor
    # # TODO: don't even create until user requests it
    # blueprint.editor <- blueprintEditorServer(
    #   "editor",
    #   blueprint = blueprint,
    #   slate = list(
    #     slate.title = slate.title,
    #     import.values = import.values,
    #     input.values = input.values,
    #     sources = sources,
    #     import.sources = import.sources ,
    #     source.code = source.code,
    #     slate.envir = slate.envir,
    #     export.data = export.data,
    #     global.options = global.options,
    #     slate.options = slate.options
    #   ),
    #   global.options = global.options
    # )


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
        id = id,
        blueprint = reactive(reactiveValuesToList(blueprint)),
        import.values = import.values,
        export.data = export.data,
        input.values = input.values,
        slate.options = slate.options,
        global.options = global.options,
        updateBlueprint = updateBlueprint,
        slate.data = slate.data,
        destroy = destroy
        #blueprint = blueprint,
        #inputs = input.list
      )
    )
  })
}







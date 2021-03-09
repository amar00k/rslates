




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
                         open.settings = TRUE,
                         inputs.style = c("tabset", "collapses", "flowing")) {
  # envir is a reactiveVal
  if (!("reactiveVal" %in% class(envir)))
    envir <- reactiveVal(envir)

  return(list(
    envir = envir,
    height = height,
    use.card = use.card,
    card.header = card.header,
    open.settings = open.settings,
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
      tags$i("Warning: this will reset all inputs.")
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


  editor.ui <- tags$div(
    id = ns("blueprint_editor"),
    class = "container pt-3",
    shinyAce::aceEditor(
      ns("blueprint_source"),
      height = "250px",
      mode = "r",
      value = blueprint$source
    ),
    uiOutput(ns("blueprint_alerts")),
    tags$hr()
  )


  body.ui <- tags$div(
    shinyjs::hidden(editor.ui),
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
            style = "height: 100%; overflow: auto;",
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

    ui <- tags$div(
      id = ns("slate_div"),
      class = "card slate",
      header.ui,
      body.ui
    )
  } else {
    ui <- body.ui
  }

  tagList(ui)
}



slateServer <- function(id, blueprint.ini, slate.options = NULL, global.options = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    if (is.null(slate.options)) {
      slate.options <- reactiveValues(
        envir = new.env(),
        open.settings = TRUE
      )
    }

    if (is.null(global.options)) {
      global.options <- reactiveValues(
        ace.theme = getOption("rslates.default.ace.theme")
      )
    }

    ready <- uiReady(session)
    edit.mode <- reactiveVal(FALSE)

    source.output <- slateOutput("Source", type="source")


    # Everything that needs to be done AFTER the UI has been created
    init <- observe({
      req(ready())

      # view / hide inputs
      observeEvent(input$slate_view_inputs, {
        dlog()

        # close the menu
        shinyjs::click("settings_button")

        shinyjs::toggleElement("slate_inputs_container", condition = input$slate_view_inputs)
      }, ignoreInit = TRUE)

      init$destroy()
    })


    # click on edit blueprint
    observeEvent(input$slate_edit, {
      dlog()

      # close the menu
      shinyjs::click("settings_button")

      if (edit.mode() == FALSE) {
        edit.mode(TRUE)

        updateActionLink(session, "slate_edit",
          label = as.character(edit.blueprint.labels$close)
        )
      } else {
        edit.mode(FALSE)

        updateActionLink(session, "slate_edit",
          label = as.character(edit.blueprint.labels$open)
        )
      }
    })


    # set edit mode on or off
    observeEvent(edit.mode(), {
      dlog()

      shinyjs::toggleElement("blueprint_editor", condition = edit.mode())

      shinyjs::toggleClass("slate_div", class = "editing border-warning", condition = edit.mode())

      shinyjs::toggleState("slate_rename", condition = !edit.mode())
      shinyjs::toggleState("slate_export", condition = !edit.mode())
    })




    # Store blueprint
    blueprint <- reactiveValues()
    preprocessor <- reactiveValues(
      errors = list()
    )


    # provides the values of all inputs
    input.values <- reactive({
      dlog()

      blueprint$inputs %>%
        map(~getHandler(.)$as.value(., session))
    })


    # provides the sources for all outputs (with variables substituted)
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
          e <- paste0("Error substituting variables in output '",
                     .$name, "': ", e)

          isolate(preprocessor$errors %<>% append(list(e)))

          return(.$source)
        })

        list_modify(., source = new.source)
      })
    })


    # preprocesses the blueprint source, fills in the preprocessor
    # reactiveValues structure and updates the blueprint reactiveValues
    # inputs and outputs structures
    observe(label = "source", {
      dlog()

      source <- input$blueprint_source

      #preprocessSource(source)

      inputs <- isolate(blueprint$inputs)
      values <- isolate(input.values())

      preprocessor$errors <- list()

      tryCatch({
        # preprocess the source
        parsed <- preprocessSource(source)

        blueprint$title <- parsed$title
        blueprint$source <- source
        blueprint$pages <- parsed$pages
        blueprint$groups <- parsed$groups
        blueprint$blocks <- parsed$blocks

        # make slate inputs from the preprocessor input data and
        # restore input values but only if they "were" set to
        # the default value, otherwise we let them as they are
        inputs <- parsed$inputs # %>%
          # assignInputValues(values) %>%
          # modify_if(~!is.null(.$value) && identical(.$value, .$default),
          #           ~list_modify(., value = NULL))

        isolate(blueprint$inputs <- inputs)

        outputs <-
          parsed$outputs %>%
          append(list(source.output))

        isolate(blueprint$outputs <- outputs)
      },
      error = function(e) {
        isolate(preprocessor$errors %<>% append(list(e$message)))
      })
    })


    output$title <- renderText({
      blueprint$title
    })


    # displays error alerts from the preprocessor
    output$blueprint_alerts <- renderUI({
      dlog("blueprint_alerts")

      map(preprocessor$errors,
          ~tags$div(
            class = "alert alert-danger",
            . #$message
          )
      ) %>% tagList()
    })


    # displays the main inputs panel
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

      output.tabs$id <- ns("output_tabs")
      output.tabs$type <- "pills"
      output.tabs$selected <- selected
      outputs.ui <- do.call(tabsetPanel, output.tabs)
    })


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

      env <- new.env(parent = slate.options$envir())

      return(env)
    })


    # creates output handlers when output structure changes
    observe(label = "output.handlers", {
      dlog()

      for (x in blueprint$outputs) {
        getHandler(x)$create.output(
          x, session, output.sources, input.values, slate.envir
        )
      }
    })


    # handles the output observers
    observe(label = "outputs", {
      dlog()

      global.options$ace.theme

      for (x in blueprint$outputs) {
        getHandler(x)$observer(
          x$id, session, output.sources, input.values, slate.envir, global.options
        )
      }
    })


    # handles the inputs observers
    observe(label = "inputs", {
      dlog()

      for (x in blueprint$inputs) {
        getHandler(x)$observer(x, session)
      }
    })


    # when inputs change, update their BS4 tooltips
    observe({
      map(names(input), ~input[[ . ]])

      shinyjs::runjs("$('[data-toggle=\"tooltip\"]').tooltip();")
    })


    # list of observers to be destroyed on exit
    # observers <- list()


    # # extract groups from layout
    # group.list <- reactive({
    #   lapply(blueprint$input.layout$pages, "[[", "groups") %>%
    #     unlist(recursive = FALSE) %>%
    #     set_names(sapply(., "[[", "name"))
    # })


    # import.data <- reactiveValues()
    # for (x in blueprint$imports) {
    #   if (x$type == "file")
    #     import.data[[ x$name ]] <- x
    # }






    # for (g in groups) {
    #   if (g$condition != "") {
    #     lapply(input.list, "[[", "value")
    #
    #   }
    # }



    # change the ace theme on all aceEditors
    # except those that are part of outputs
    observeEvent(global.options$ace.theme, {
      shinyAce::updateAceEditor(session, "blueprint_source", theme = global.options$ace.theme)
    })




    load <- function(filename, format = c("auto", "txt", "json")) {
      blueprint <- loadBlueprint(filename, format)

      if (format == "txt") {
        shinyAce::updateAceEditor(session, "blueprint_source",
                                  value = blueprint$source)
      }
    }




    # Cleanup function
    destroy <- function() {
      # remove inputs
      # https://www.r-bloggers.com/2020/02/shiny-add-removing-modules-dynamically/
      id <- session$ns("")
      for (x in paste0(id, names(input))) {
        .subset2(input, "impl")$.values$remove(x)
      }

      # remove outputs
      for (x in paste0(id, names(output.handlers))) {
        output[[ x ]] <- NULL
      }

      # remove observers
      for (x in observers) {
        x$destroy()
      }
    }

    return(
      list(
        load = loadBlueprint,
        destroy = destroy
        #blueprint = blueprint,
        #inputs = input.list,
        #import.data = import.data,
      )
    )
  })
}

  # observers$edit <- observeEvent(input$menu_edit, {
  #   modal.edit$show(blueprint, callback = function(new.input.list, ...) {
  #     input.list(new.input.list)
  #   })
  # })

  # observers$remove <- observeEvent(input$slate_box$visible, {
  #   if (input$slate_box$visible == FALSE) {
  #     removeUI(selector = paste0('#', ns("slate_div")))
  #
  #     # remove inputs
  #     # https://www.r-bloggers.com/2020/02/shiny-add-removing-modules-dynamically/
  #     # remove_shiny_inputs <- function(id, .input) {
  #     #   invisible(
  #     #     lapply(grep(id, names(.input), value = TRUE), function(i) {
  #     #       .subset2(.input, "impl")$.values$remove(i)
  #     #     })
  #     #   )
  #     # }
  #
  #     # remove observers
  #     for (x in reactiveValuesToList(observers)) {
  #       x$destroy()
  #     }
  #
  #     #slates[[ slate$id ]] <- NULL
  #   }
  # })

  # # switch to the selected output
  # observe({
  #   sel <- selected.output()
  #
  #   for (x in blueprint$output) {
  #     shinyjs::removeClass(paste0(x$id, "_button"), "in")
  #   }
  #   shinyjs::addClass(paste0(sel, "_button"), "in")
  #
  #   updateTabsetPanel(session = session,
  #                     inputId = "output_tabs",
  #                     selected = blueprint$output[[ sel ]]$title)
  # })

#
#   # handle output buttons
#   for (x in blueprint$output) {
#     (function(id) {
#       shinyjs::onclick(paste0(id, "_button"), {
#         selected.output(id)
#       })
#     })(x$id)
#   }


  # initialize inputs
  # for (page in blueprint$inputs) {
  #   for (group in page$groups) {
  #     if (group$type == "conditional-group") {
  #       observers[[ group$id ]] <- observe({
  #         test <- eval(group$condition, envir = list2env(input.values()))
  #
  #         shinyjs::toggle(group$id, condition = test)
  #       })
  #     }
  #   }
  # }

  # # add tooltips as soon as ui is added
  # tooltip.observer <- observe({
  #   req(input$slate_box$visible)
  #
  #   for (x in blueprint$input.list) {
  #     shinyBS::addTooltip(session, ns(x$id),
  #                         title = paste0("<b>", x$type, "</b><br><br>", x$description),
  #                         placement = "right",
  #                         options = list(container = "body"))
  #   }
  #
  #   # destroy this observer
  #   tooltip.observer$destroy()
  # })
  #
  # if (open.settings == TRUE) {
  #   modal.edit$show(blueprint, callback = function(new.input.list, ...) {
  #     input.list(new.input.list)
  #   })
  # }










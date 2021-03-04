




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





slateUI <- function(id, blueprint, slate.options = slateOptions()) {
  ns <- NS(id)

  height <- slate.options$height

  body.ui <- tags$div(
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
        id = ns("slate_inputs_body"),
        class = "slate-inputs-container col-6 show",
        style = "overflow: auto;",
        tags$div(
          class = "collapse show",
          style = "width: 100%",
          uiOutput(ns("inputs_panel")),
          tags$div(
            #class = "container",
            tags$div(
              h5("Blueprint Settings"),
              shinyAce::aceEditor(ns("blueprint_source"),
                                  value = blueprint$source)
            )
          )
        )
      )
    )
  )

  if (slate.options$use.card) {
    if (slate.options$card.header == TRUE) {
      header.ui <- tags$div(
        class="card-header d-flex justify-content-between",
        tags$p(blueprint$title),
        actionButton(
          ns("btn_edit"),
          class = "ml-auto",
          icon = icon("cog"),
          `data-toggle` = "collapse",
          href = paste0("#", ns("slate_inputs_body")),
          label = "")
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

    ready <- uiReady(session)
    source.output <- slateOutput("Source", type="source")

    # blueprint.properties <- reactiveValues(
    #   title = blueprint$title
    # )

    # Store blueprint
    blueprint <- reactiveValues()


    input.values <- reactive({
      print("slate.R: update reactive input values")

      blueprint$inputs %>%
        map(~getHandler(.)$as.value(., session)) %>%
        as.character
    })

    # pages <- reactiveVal(blueprint$pages)
    # groups <- reactiveVal(blueprint$groups)
    # inputs <- reactiveVal(blueprint$inputs)
    # outputs <- reactiveVal(blueprint$outputs %>% map(~list_modify(., source = NULL)))

    # # note: this can be inferred from blueprint$source
    # output.sources <- reactiveVal(blueprint$outputs %>% map("source"))



    #
    # Update inputs and outputs when blueprint source changes
    #
    observe({
      print("slate.R: observe source")

      source <- input$blueprint_source
      inputs <- isolate(blueprint$inputs)
      values <- isolate(input.values())

      new.inputs <- tryCatch({
        preprocessInputs(source)$inputs %>% print %>%
          map(~do.call(slateInput, .))
      },
      error = function(e) {
        return(NULL)
      })

      if (!is.null(new.inputs) && !identical(new.inputs, inputs)) {
        print(new.inputs)

        # set values of new inputs so we retain non-default values in UI
        new.inputs %<>%
          assignInputValues(values) %>%
          modify_if(~!is.null(.$value) && .$value == .$default,
                    ~list_modify(., value = NULL))

        isolate(blueprint$inputs <- new.inputs)
      }
    }, priority = 10)



    # observe({
    #   pages <- isolate(pages())
    #   groups <- isolate(groups())
    #   inputs <- isolate(inputs())
    #   outputs <- isolate(outputs())
    #   output.sources <- isolate(output.sources())
    #
    #   source <- input$blueprint_source
    #
    #   try({
    #     # inputs
    #     new.inputs <- preprocessInputs(source) %>%
    #       map(~do.call(slateInput, .))
    #
    #     if (!identical(new.inputs, inputs)) {
    #       # reset old values
    #       values <- isolate(input.values()) %>% map("value")
    #
    #       new.inputs %<>% map(~{
    #         if (.x$name %in% values)
    #           .x$value <- values[[ .x$name ]]
    #         else
    #           .x$value <- .x$default
    #
    #         .x
    #       })
    #
    #       inputs(new.inputs)
    #     }
    #   })
    #
    #   # outputs
    #   try({
    #     new.outputs.data <-
    #       preprocessSections(source) %>%
    #       map(~do.call(slateOutput, .)) %>%
    #       append(list(source.output))
    #
    #     new.outputs <- new.outputs.data %>%
    #       map(~list_modify(., source = NULL))
    #
    #     new.sources <- new.outputs.data %>%
    #       map("source")
    #
    #     if (!identical(new.outputs, outputs)) {
    #       outputs(new.outputs)
    #     }
    #
    #     if (!identical(new.sources, output.sources)) {
    #       output.sources(new.sources)
    #     }
    #   })
    # })


    output$inputs_panel <- renderUI({
      print("slate.R: rendering input layout.")

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


    output$outputs_panel <- renderUI({
      req(outputs <- blueprint$outputs)

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
      outputs.ui <- do.call(tabsetPanel, output.tabs)
    })


    # # create output handlers
    # observe({
    #   for (x in blueprint$outputs) {
    #     getHandler(x)$create.output(
    #       x, session, output.sources, input.values, slate.envir
    #     )
    #   }
    # })
    #
    #
    # # observe outputs
    # observe({
    #   global.options$ace.theme
    #
    #   for (x in blueprint$outputs) {
    #     getHandler(x)$observer(
    #       x$id, session, output.sources, blueprint$inputs, slate.envir, global.options
    #     )
    #   }
    # })
    #

    # observe inputs
    observe({
      print("slate.R: observe inputs")

      for (x in blueprint$inputs) {
        getHandler(x)$observer(x, session)
      }
    })


    observe({
      map(names(input), ~input[[ . ]])

      # shinyjs::runjs("$('[data-toggle=\"tooltip\"]').tooltip('hide');")
      # shinyjs::runjs("$('[data-toggle=\"tooltip\"]').tooltip('dispose');")
      shinyjs::runjs("$('[data-toggle=\"tooltip\"]').tooltip();")
    })


    # list of observers to be destroyed on exit
    observers <- list()

    # slate reactives
    if (is.null(global.options)) {
      global.options <- reactiveValues(
        ace.theme = "ambiance"
      )
    }

    if (is.null(slate.options)) {
      slate.options <- reactiveValues(
        envir = new.env(),
        open.settings = TRUE
      )
    }


    # extract groups from layout
    group.list <- reactive({
      lapply(blueprint$input.layout$pages, "[[", "groups") %>%
        unlist(recursive = FALSE) %>%
        set_names(sapply(., "[[", "name"))
    })


    # import.data <- reactiveValues()
    # for (x in blueprint$imports) {
    #   if (x$type == "file")
    #     import.data[[ x$name ]] <- x
    # }


    # run exec code in slate environment
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




    # for (g in groups) {
    #   if (g$condition != "") {
    #     lapply(input.list, "[[", "value")
    #
    #   }
    # }



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










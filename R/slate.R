



slateUI <- function(id,
                    blueprint,
                    container.fun = function(...) tags$div(id = ns("slate_div"), class = "card slate", ...),
                    input.container = "tabset") {
  ns <- NS(id)

  # create input panels
  inputs.ui <- createInputLayout(
    ns = ns,
    pages = blueprint$input.layout$pages,
    container = input.container
  )

  blueprint$outputs[[ length(blueprint$outputs) + 1 ]] <- slateOutput("Source", type="source")

  # create output pages defined in slate
  output.tabs <- unname(lapply(blueprint$outputs, function(x) {
    stopifnot(x$type %in% names(output.handlers))

    tabPanel(
      title = x$name,
      tags$div(
        tags$br(),
        output.handlers[[ x$type ]]$create.ui(ns(x$id), x$name)
      )
    )
  }))
  output.tabs$id <- ns("output_tabs")
  output.tabs$type <- "pills"
  outputs.ui <- do.call(tabsetPanel, output.tabs)


  ui <- tags$div(
    id = ns("slate_div"),
    class = "card slate",
    tags$div(
      class="card-header d-flex justify-content-between",
      tags$p(blueprint$title),
      actionButton(ns("btn_edit"),
                   class = "ml-auto",
                   icon = icon("cog"),
                   `data-toggle` = "collapse",
                   href = paste0("#", ns("slate_inputs_body")),
                   label = "")
    ),
    tags$div(
      class = "d-flex mh-100 flex-row justify-content-between align-content-stretch",
      tags$div(class = "flex-fill"),
      tags$div(class = "col-6",
               tags$div(class = "card-body",
                        style = "",
                        outputs.ui
               )
      ),
      tags$div(class = "flex-fill"),
      tags$div(
        id = ns("slate_inputs_body"),
        class = "slate-inputs-container col-6 show",
        tags$div(
          class = "collapse show",
          style = "width: 100%",
          inputs.ui
        )
      )
    )
  )

  tagList(ui)
}

slateServer <- function(id, blueprint, slate.options = NULL, global.options = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ui.ready <- uiReady(session)

    # append the source code outputs
    blueprint$outputs[[ length(blueprint$outputs) + 1 ]] <- slateOutput("Source", type="source")

    # modal.edit <- slate_modal(ns, "edit_modal", input, output, session, blueprint, global.envir)

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

    # extract inputs from layout and update values
    input.list <- reactive({
      req(ui.ready())

      inputs <- lapply(getInputs(blueprint), function(x) {
        x$value <- getHandler(x)$get.value(x, session)
        x$source <- getHandler(x)$get.source(x, session)

        return(x)
      })

      # if (length(blueprint$imports) > 0) {
      #   imports <- lapply(blueprint$imports, function(x) {
      #     x$data <- import.data[[ x$name ]]$data
      #     x$value <- dataset.handlers[[ x$type ]]$get.value(x, session)
      #     return(x)
      #   })
      #
      #   inputs <- append(inputs, imports)
      # }

      #names(inputs) <- sapply(inputs, "[[", "name")

      return(inputs)
    })


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


    # run datasets code in slate environment
    slate.envir <- reactive({
      inputs <- input.list()
      env <- new.env(parent = slate.options$envir())

      env <- tryCatch({
        for (d in blueprint$datasets) {
          if (!is.null(d$source)) {
            src <- buildSource(d$source, input.list())
            env[[ d$name ]] <- eval(parse(text = src), envir = new.env(parent = env))
          }
        }

        return(env)
      },
      error = function(e) {
        # ignore errors
        return(env)
      })

      return(env)
    })


    # initialize outputs
    for (x in blueprint$outputs) {
      getHandler(x)$create.output(x, session, blueprint, input.list, slate.envir)
    }


    # observe outputs
    observe({
      global.options$ace.theme

      for (x in blueprint$outputs) {
        # run observer
        getHandler(x)$observer(x$id, session, blueprint, input.list, slate.envir, global.options)
      }
    })


    # observe inputs
    observe({
      for (x in getInputs(blueprint)) {
        getHandler(x)$observer(x, session)
      }
    })


    # for (g in groups) {
    #   if (g$condition != "") {
    #     lapply(input.list, "[[", "value")
    #
    #   }
    # }


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
      list(blueprint = blueprint,
           inputs = input.list,
           #import.data = import.data,
           destroy = destroy)
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





























slate_container_dashboardPlus <- function(ns, blueprint, output.panel, output.buttons) {
  div(id = ns("slate_div"),
      shinydashboardPlus::box(id = ns("slate_box"),
                              title = tagList(blueprint$title),
                              icon = icon("chart-bar"),
                              collapsible = TRUE,
                              closable = TRUE,
                              dropdownMenu = shinydashboardPlus::boxDropdown(
                                shinydashboardPlus::boxDropdownItem("Configure", inputId = ns("menu_edit")),
                                shinydashboardPlus::boxDropdownItem("Rename"),
                                shinydashboardPlus::dropdownDivider(),
                                shinydashboardPlus::boxDropdownItem("Export"),
                                shinydashboardPlus::dropdownDivider(),
                                shinydashboardPlus::boxDropdownItem("Delete")
                              ),
                              #input.tabset,
                              #tags$div(style="height: 120px; display: block;",
                              output.panel,
                              #),
                              tags$hr(),
                              output.buttons
      )
  )
}

slate_container_bs4Dash <- function(ns, blueprint, output.panel, output.buttons) {
  #div(id = ns("slate_div"),
      bs4Dash::box(id = ns("slate_box"),
                   title = tagList(blueprint$title),
                   #icon = icon("chart-bar"),
                   collapsible = TRUE,
                   closable = TRUE,
                   dropdownMenu = bs4Dash::boxDropdown(
                     bs4Dash::boxDropdownItem("Configure", inputId = ns("menu_edit")),
                     bs4Dash::boxDropdownItem("Rename"),
                     bs4Dash::dropdownDivider(),
                     bs4Dash::boxDropdownItem("Export"),
                     bs4Dash::dropdownDivider(),
                     bs4Dash::boxDropdownItem("Delete")
                   ),
                   #input.tabset,
                   #tags$div(style="height: 120px; display: block;",
                   output.panel,
                   #),
                   tags$hr(),
                   output.buttons
      )

}


slate_container <- slate_container_dashboardPlus





# modal slate
slate_modal <- function(ns, id, input, output, session, blueprint, global.envir) {
  .callback <- NULL

  observers <- list()

  input.list <- reactive({
    lapply(blueprint$input.list, function(x) {
      val <- input.handlers[[ x$type ]]$get.value(x$id, input)

      if (!is.null(val)) {
        if (x$type == "file") {
          x$value <- val$datapath
        } else {
          x$value <- val
        }
      }

      return(x)
    })
  })

  # input.values <- reactive({
  #   lapply(blueprint$input.list, "[[", "value")
  # })

  slate <- reactive({
    blueprint$input.list <- input.list()

    blueprint$outputs <- lapply(blueprint$outputs, function(x) {
      x$id <- paste0(x$id, "_preview")
      return(x)
    })
    names(blueprint$outputs) <- sapply(blueprint$outputs, "[[", "id")

    return(blueprint)
  })

  dataset <- reactive({
    if (length(slate()$datasets) == 0)
      return (NULL)

    src <- slate()$datasets[[1]]$source(input.list())

    tryCatch({
      dat <- eval(parse(text = src), envir = new.env())
    },
    error = function(e) {
      print(e)
    },
    finally = {
      dat <- head(mtcars)
    })

    return (dat)
  })

  # call output observers
  observe({
    for (x in slate()$outputs) {
      handler <- output.handlers[[ x$type ]]$observer(x$id, session, slate(), input.list, global.envir())
      observers[[ x$id ]] <- handler
    }
  })

  # create output panels
  for (x in slate()$outputs) {
    handlers <- output.handlers[[ x$type ]]$create.outputs(x$id, slate(), input.list, global.envir, input)
    for (id in names(handlers)) {
      output[[ x$id ]] <- handlers[[ id ]]
    }
  }

  observeEvent(input$button_ok, {
    removeModal(session)

    if (!is.null(.callback)) {
      .callback(input.list(), dataset())
    }
  })

  show <- function(slate.ini, callback) {
    ns <- session$ns

    .callback <<- callback

    # create inputs defined in slate
    input.tabs <- create_slate_inputs(ns, slate())

    if (slate()$children[[1]]$main == TRUE) {
      input.main <- input.tabs[[1]]
      input.tabs[[1]] <- NULL
      input.tabset <- do.call(slatesCompactTabsetPanel, unname(input.tabs))
      input.ui <- tagList(input.main, input.tabset)
    } else {
      input.ui <- do.call(slatesCompactTabsetPanel, unname(input.tabs))
    }

    # create output pages defined in slate
    output.tabs <- lapply(slate()$output, function(x) {
      stopifnot(x$type %in% names(output.handlers))

      output.handlers[[ x$type ]]$create.ui(ns(x$id), x$title)
    })
    output.panel <- do.call(tabsetPanel, unname(output.tabs))

    # create and show modal
    showModal(
      modalDialog (
        title = slate.ini$title,
        fluidRow(
          column(6, input.ui),
          column(6, output.panel)
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("button_ok"), "OK")
        ),
        size = "l",
        fade = TRUE,
        easyClose = FALSE
      )
    )
  }

  list(
    id = id,
    slate = slate,
    input.list = input.list,
    show = show
  )
}




editr_slateUI <- function(id, blueprint) {
  ns <- NS(id)

  # create input pages defined in blueprint
  #input.tabs <- create_slate_inputs(ns, blueprint)
  #input.tabset <- do.call(slatesCompactTabsetPanel, unname(input.tabs))

  # create output pages defined in slate
  output.tabs <- lapply(blueprint$output, function(x) {
    stopifnot(x$type %in% names(output.handlers))

    output.handlers[[ x$type ]]$create.ui(ns(x$id), x$title)
  })
  output.tabs <- unname(output.tabs)
  output.tabs$id <- ns("output_tabs")
  output.tabs$type <- "hidden"
  output.panel <- do.call(tabsetPanel, output.tabs)

  button.data <- lapply(blueprint$output, function(x) {
    switch(x$type,
           plot = list(id = ns(paste0(x$id, "_button")), class = "slate-btn fa fa-chart-area"),
           source = list(id = ns(paste0(x$id, "_button")), class = "slate-btn fa fa-code")
    )
  })
  button.data[[1]]$class <- paste(button.data[[1]]$class, "in")
  output.buttons <- lapply(button.data, function(x) {
    tags$a(id = x$id, class = x$class)
  })
  output.buttons <- tags$div(
    class="flex-container justify-center",
    output.buttons
  )

  # output.buttons <- radioGroupButtons(
  #   inputId = "Id069",
  #   label = "",
  #   choices = c(`<i class='fa fa-bar-chart'></i>` = "bar", `<i class='fa fa-line-chart'></i>` = "line",
  #               `<i class='fa fa-pie-chart'></i>` = "pie"),
  #   justified = TRUE
  # )


  ui <- slate_container(ns, blueprint, output.panel, output.buttons)

  tagList(ui)
}


editr_slateServer <- function(input, output, session, blueprint, global.envir, open.settings = FALSE) {
  ns <- session$ns

  modal.edit <- slate_modal(ns, "edit_modal", input, output, session, blueprint, global.envir)

  observers <- reactiveValues()

  #input.values <- reactiveVal(lapply(blueprint$input.list, "[[", "value"))
  input.list <- reactiveVal(blueprint$input.list)

  selected.output <- reactiveVal(blueprint$output[[1]]$id)

  observers$edit <- observeEvent(input$menu_edit, {
    modal.edit$show(blueprint, callback = function(new.input.list, ...) {
      input.list(new.input.list)
    })
  })

  observers$remove <- observeEvent(input$slate_box$visible, {
    if (input$slate_box$visible == FALSE) {
      removeUI(selector = paste0('#', ns("slate_div")))

      # remove inputs
      # https://www.r-bloggers.com/2020/02/shiny-add-removing-modules-dynamically/
      # remove_shiny_inputs <- function(id, .input) {
      #   invisible(
      #     lapply(grep(id, names(.input), value = TRUE), function(i) {
      #       .subset2(.input, "impl")$.values$remove(i)
      #     })
      #   )
      # }

      # remove observers
      for (x in reactiveValuesToList(observers)) {
        x$destroy()
      }

      #slates[[ slate$id ]] <- NULL
    }
  })

  # call output observers
  observe({
    req(input$slate_box$visible)

    for (x in blueprint$output) {
      output.handlers[[ x$type ]]$observer(x$id, session, blueprint, input.list, global.envir())
    }
  })

  # create output panels
  for (x in blueprint$output) {
    handlers <- output.handlers[[ x$type ]]$create.outputs(x$id, blueprint, input.list, global.envir, input)
    for (id in names(handlers)) {
      output[[ id ]] <- handlers[[ id ]]
    }
  }

  # switch to the selected output
  observe({
    sel <- selected.output()

    for (x in blueprint$output) {
      shinyjs::removeClass(paste0(x$id, "_button"), "in")
    }
    shinyjs::addClass(paste0(sel, "_button"), "in")

    updateTabsetPanel(session = session,
                      inputId = "output_tabs",
                      selected = blueprint$output[[ sel ]]$title)
  })

  # handle output buttons
  for (x in blueprint$output) {
    (function(id) {
      shinyjs::onclick(paste0(id, "_button"), {
        selected.output(id)
      })
    })(x$id)
  }

  # create output panels
  # for (x in blueprint$output) {
  #   btn.id <- paste0(x$id, "_button")
  #   print(btn.id)
  #   observers[[ btn.id ]] <- observeEvent(input[[ btn.id ]], {
  #     print(btn.id)
  #     # isolate({
  #     #   for (out in blueprint$output) {
  #     #     removeClass(paste0(out$id, "_button"), "output-btn-selected")
  #     #   }
  #     #
  #     #   print(x$id)
  #     #   addClass(btn.id, "output-btn-selected")
  #     # })
  #   })
  # }

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

  # add tooltips as soon as ui is added
  tooltip.observer <- observe({
    req(input$slate_box$visible)

    for (x in blueprint$input.list) {
      shinyBS::addTooltip(session, ns(x$id),
                 title = paste0("<b>", x$type, "</b><br><br>", x$description),
                 placement = "right",
                 options = list(container = "body"))
    }

    # destroy this observer
    tooltip.observer$destroy()
  })

  if (open.settings == TRUE) {
    modal.edit$show(blueprint, callback = function(new.input.list, ...) {
      input.list(new.input.list)
    })
  }

  return(
    list(blueprint = blueprint,
         input.list = input.list)
  )
}
















editr_dataslateUI <- function(id, blueprint) {
  ns <- NS(id)

  # create input pages defined in blueprint
  #input.tabs <- create_slate_inputs(ns, blueprint)
  #input.tabset <- do.call(slatesCompactTabsetPanel, unname(input.tabs))

  # # create output pages defined in slate
  # output.tabs <- lapply(blueprint$output, function(x) {
  #   stopifnot(x$type %in% names(output.handlers))
  #
  #   output.handlers[[ x$type ]]$create.ui(ns(x$id), x$title)
  # })
  # output.tabs <- unname(output.tabs)
  # output.tabs$type <- "pills"
  # output.panel <- do.call(tabsetPanel, output.tabs)
  #
  # output.buttons <- lapply(blueprint$output, function(x) {
  #   #if (x$type == "plot") {
  #   button <- actionButton(ns(paste0(x$id, "_button")), label = "I", class="slate-btn-figure")
  #   #button <- radioGroupButtons(ns(x$id), label = "", choices = c("F"), selected = NULL)
  #   #button$attribs$class <- paste(button$attribs$class, "slate-btn-figure")
  #   #button <- radionput(ns(x$id), label = "F", )
  #   #} else if (x$type == "source") {
  #   #  button <- radioGroupButtons(ns(x$id), label = "", choices = c("S"))
  #   #  button$attribs$class <- paste(button$attribs$class, "slate-btn-figure")
  #   #}
  #
  #   return(button)
  # })
  # output.buttons <- unname(output.buttons)
  # output.buttons <- do.call(flowLayout, output.buttons)
  #
  #
  #
  # ui <- div(id = ns("slate_div"),
  #           shinydashboardPlus::box(id = ns("slate_box"),
  #                                   title = tagList(blueprint$title),
  #                                   icon = icon("chart-bar"),
  #                                   #status = "success",
  #                                   #solidHeader=TRUE,
  #                                   collapsible = TRUE,
  #                                   closable = TRUE,
  #                                   dropdownMenu = shinydashboardPlus::boxDropdown(
  #                                     shinydashboardPlus::boxDropdownItem("Configure", inputId = ns("menu_edit")),
  #                                     shinydashboardPlus::boxDropdownItem("Rename"),
  #                                     shinydashboardPlus::dropdownDivider(),
  #                                     shinydashboardPlus::boxDropdownItem("Export"),
  #                                     shinydashboardPlus::dropdownDivider(),
  #                                     shinydashboardPlus::boxDropdownItem("Delete")
  #                                   ),
  #                                   #input.tabset,
  #                                   output.panel
  #                                   #output.buttons
  #           )
  # )

  ui <- shinydashboard::infoBoxOutput(ns("info.box"))
  ui <- actionElement(ns("action"), ui)

  tagList(ui)
}


editr_dataslateServer <- function(input, output, session, blueprint, global.envir, open.settings = FALSE) {
  ns <- session$ns

  modal.edit <- slate_modal(ns, "edit_modal", input, output, session, blueprint, global.envir)

  slate <- reactiveVal(blueprint)
  dataset <- reactiveVal(NULL)

  observers <- reactiveValues()

  input.values <- reactive({
    vals <- sapply(slate()$input.list, "[[", "value")
    names(vals) <- sapply(slate()$input.list, "[[", "name")

    return(vals)
  })

  observers$action <- observeEvent(input$action, {
    modal.edit$show(slate(), callback = function(new_slate, new_dataset) {
      slate(new_slate)
      dataset(new_dataset)
    })
  })

  output$info.box <- shinydashboard::renderInfoBox({
    name <- input.values()[[ "varname" ]]

    if (is.null(dataset())) {
      desc <- "Click here to load dataset."
    } else if (class(dataset()) == "data.frame") {
        desc <- paste("data.frame with", nrow(dataset()), "observations of", ncol(dataset()), "variables.")
    } else {
      desc <- paste0("Object of class ", class(dataset()), ".")
    }

    shinydashboard::infoBox(
      title = if (is.null(dataset())) "" else name,
      subtitle = desc,
      icon = icon("file-csv")
    )

    # shinydashboard::infoBox(
    #   title = "test",
    #   subtitle = "bla",
    #   icon = icon("file-csv")
    # )
  })

  #
  # input.list <- modal.edit$input.list
  #
  #
  #
  #
  #
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
  #
  # # call output observers
  # observe({
  #   req(input$slate_box$visible)
  #
  #   for (x in slate()$output) {
  #     output.handlers[[ x$type ]]$observer(x$id, session, slate(), global.envir())
  #   }
  # })
  #
  # # create output panels
  # for (x in blueprint$output) {
  #   handlers <- output.handlers[[ x$type ]]$create.outputs(x$id, slate, global.envir, input)
  #   for (id in names(handlers)) {
  #     output[[ id ]] <- handlers[[ id ]]
  #   }
  # }
  #
  #
  # # create output panels
  # # for (x in blueprint$output) {
  # #   btn.id <- paste0(x$id, "_button")
  # #   print(btn.id)
  # #   observers[[ btn.id ]] <- observeEvent(input[[ btn.id ]], {
  # #     print(btn.id)
  # #     # isolate({
  # #     #   for (out in blueprint$output) {
  # #     #     removeClass(paste0(out$id, "_button"), "output-btn-selected")
  # #     #   }
  # #     #
  # #     #   print(x$id)
  # #     #   addClass(btn.id, "output-btn-selected")
  # #     # })
  # #   })
  # # }
  #
  # # initialize inputs
  # # for (page in blueprint$inputs) {
  # #   for (group in page$groups) {
  # #     if (group$type == "conditional-group") {
  # #       observers[[ group$id ]] <- observe({
  # #         test <- eval(group$condition, envir = list2env(input.values()))
  # #
  # #         shinyjs::toggle(group$id, condition = test)
  # #       })
  # #     }
  # #   }
  # # }
  #
  # # add tooltips as soon as ui is added
  # tooltip.observer <- observe({
  #   req(input$slate_box$visible)
  #
  #   for (x in slate()$input.list) {
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
  #   modal.edit$show(slate(), callback = function(new_slate) {
  #     slate(new_slate)
  #   })
  # }

  return(blueprint)
}















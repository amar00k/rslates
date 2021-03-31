


#' Create a Modal Dialog
#'
#' @param id id of the modal dialog.
#' @param session server session.
#' @param submit.fun a function without arguments that returns the a list
#'   of results of the modal dialog when the user presses OK.
#' @param ui.fun a function taking any number of arguments that returns
#'   the ui of the body of the modal.
#' @param observers an optional list of observers to be destroyed when the
#'   modal is destroyed.
#' @param focus.on.show unused
#'
#' @return a list with the following members. `id`: the id of the modal,
#'   `show`: a function with signature `function(callback, title = NULL, ...)`
#'   that brings up the modal dialog when called. The `callback` function is
#'   called by the modal dialog when the user presses OK. Each value returned
#'   by `submit.fun` is passed to this function as an argument. Extra `...`
#'   parameters are passed to the `ui.fun` function.
#'
#' @export
slatesModal <- function(id, session,
                        submit.fun,
                        ui.fun,
                        observers=list(),
                        focus.on.show = NULL) {
  ID <- function(x) paste0(id, "_", x)
  ns <- session$ns
  input <- session$input

  .callback <- NULL

  observeEvent(input[[ ID("btn_ok") ]], {
    removeModal(session)

    if (!is.null(.callback)) {
      results <- submit.fun()

      do.call(.callback, results)
    }
  })

  show <- function(callback, title = NULL, size = "m", ...) {
    ns <- session$ns

    .callback <<- callback

    md <- modalDialog(
      title = title,
      size = size,
      easyClose = TRUE,
      ui.fun(...),
      footer = tagList(
        modalButton("Cancel"),
        actionButton(ns(ID("btn_ok")), "OK")
      )
    )

    showModal(md, session)
  }

  list(
    id = id,
    show = show
  )
}




slatesSelectModal <- function(id, session) {
  ID <- function(x) paste0(id, "_", x)
  ns <- session$ns

  ui.fun <- function(label, choices) {
    selectInput(ns(ID("select_input")), label = label, choices = choices, selected = choices[1])
  }

  submit.fun <- function() {
    list(session$input[[ ID("select_input") ]])
  }

  slatesModal(id, session, ui.fun = ui.fun, submit.fun = submit.fun)
}


slatesTextModal <- function(id, session) {
  ID <- function(x) paste0(id, "_", x)
  ns <- session$ns

  ui.fun <- function(label, placeholder = NULL, value = "") {
    textInput(ns(ID("text_input")), label = label, value = value, placeholder = placeholder)
  }

  submit.fun <- function() {
    list(session$input[[ ID("text_input") ]])
  }

  slatesModal(id, session, ui.fun = ui.fun, submit.fun = submit.fun)
}


slatesFileInputModal <- function(id, session) {
  ID <- function(x) paste0(id, "_", x)
  ns <- session$ns

  ui.fun <- function(label, ...) {
    fileInput(ns(ID("file_input")), label = label, ...)
  }

  submit.fun <- function() {
    list(session$input[[ ID("file_input") ]])
  }

  slatesModal(id, session, ui.fun = ui.fun, submit.fun = submit.fun)
}





#' #' Construct a Multi-Page Modal Dialog
#' #'
#' #' @description Used to construct a multi-page modal dialog. Next and Back buttons are
#' #'   used to navigate the pages sequentially. Optional validation functions may be
#' #'   specified for each page. A callback function may be specified to obtain the
#' #'   user's inputs.
#' #'
#' #' @param id dialog id
#' #' @param session shiny server session
#' #' @param pages.ui a *list* of functions, each used to build a page. These functions
#' #'   may define any number of arguments used to build the page's UI. The return value
#' #'   of these functions should be a valid `shiny.tag` definition.
#' #' @param submit.fun a function that returns the results of the user interaction with
#' #'   the modal dialog. The return value should be a list with named elements.
#' #' @param validators a *list* of functions used to validate the state of each page. A
#' #'   user may not proceed to the next page of the modal dialog until the validator function
#' #'   for the current page returns `TRUE`.
#' #' @param back.button whether or not to show the back button on the modal's pages.
#' #' @param observers a list containing any number of observers. This is used solely to
#' #'   allow the modal dialog to destroy the observers when it is destroyed.
#' #' @param size default width of the modal dialog. One of "s", "m", "l", "xl". Note that
#' #' the `show()` function can override this setting if desired.
#' #'
#' #' @return A list containing the modal id and the `show()` function (see details below).
#' #'
#' #' @details After creating the modal dialog, the `show` function is used to display it. This
#' #'   function takes in a mandatory `callback` argument, and optional `title`. When the user
#' #'   exits the dialog by pressing the OK button, the provided `callback` function is called
#' #'   with each argument named as an element of the result from `submit.fun`. Additional
#' #'   arguments provided to the `show` method will be passed on to the UI definition
#' #'   functions provided in `pages.ui`. See example below.
#' #'
#' #' @export
#' #'
#' #' @examples
#' #' if (interactive()) {
#' #'   exampleMultiPageModal <- function(id, session) {
#' #'     ID <- function(x) paste0(id, "_", x)
#' #'     ns <- session$ns
#' #'     input <- session$input
#' #'
#' #'     pages.ui <- list(
#' #'       function(common.label, text.value = "") {
#' #'         tagList(
#' #'           helpText("Hello there! This is page 1! Please enter less than 5 characters."),
#' #'           textInput(ns(ID("text_input")), label = common.label, value = text.value)
#' #'         )
#' #'       },
#' #'       function(common.label, numeric.value) {
#' #'         tagList(
#' #'           helpText("Hi again! We're on page 2 now. Enter 42 below."),
#' #'           numericInput(ns(ID("numeric_input")), label = common.label, value = numeric.value)
#' #'         )
#' #'       }
#' #'     )
#' #'
#' #'     submit.fun <- function() {
#' #'       list(text.value = session$input[[ ID("text_input") ]],
#' #'            numeric.value = session$input[[ ID("numeric_input") ]])
#' #'     }
#' #'
#' #'     validators <- list(
#' #'       function() {
#' #'         return(nchar(input[[ ID("text_input") ]]) < 5)
#' #'       },
#' #'       function() {
#' #'         return(input[[ ID("numeric_input") ]] == 42)
#' #'       }
#' #'     )
#' #'
#' #'     slatesModalMultiPage(id, session,
#' #'                          pages.ui = pages.ui,
#' #'                          submit.fun = submit.fun,
#' #'                          validators = validators)
#' #'   }
#' #'
#' #'   app <- shinyApp(
#' #'     ui = fluidPage(
#' #'       shinyjs::useShinyjs(),
#' #'       actionButton("show", "Show Modal")
#' #'     ),
#' #'     server = function(input, output, session) {
#' #'       modal <- exampleMultiPageModal("modal", session)
#' #'
#' #'       observeEvent(input$show, {
#' #'         modal$show(
#' #'           common.label = "Some input", text.value = "Something", numeric.value = 41,
#' #'           title = "Example Multi-Page Modal",
#' #'           callback = function(text.value, numeric.value) {
#' #'             print(paste("Modal returned:", text.value, ",", numeric.value))
#' #'           }
#' #'         )
#' #'       })
#' #'     }
#' #'   )
#' #'
#' #'   runApp(app)
#' #' }
#' slatesModalMultiPage <- function(id, session,
#'                                  pages.ui,
#'                                  submit.fun,
#'                                  validators = NULL,
#'                                  back.button = TRUE,
#'                                  default.size = c("m", "s", "l", "xl"),
#'                                  observers = list()) {
#'   ID <- function(x) paste0(id, "_", x)
#'   ns <- session$ns
#'   input <- session$input
#'
#'   default.size <- match.arg(default.size)
#'
#'   current.page <- reactiveVal(1)
#'   num.pages <- length(pages.ui)
#'   .callback <- NULL
#'
#'   if (is.null(validators)) {
#'     validators <- lapply(1:num.pages, function(x) function() { TRUE })
#'   }
#'
#'   observe({
#'     # observe all inputs so we always update the buttons
#'     invisible(lapply(names(input), function(x) input[[ x ]]))
#'
#'     valid <- validators[[ current.page() ]]()
#'
#'     shinyjs::toggleElement(
#'       ID("btn_back"),
#'       condition = (back.button == TRUE && current.page() > 1)
#'     )
#'
#'     if (current.page() < num.pages) {
#'       shinyjs::toggleElement(ID("btn_next"), condition = TRUE)
#'       shinyjs::toggleState(ID("btn_next"), condition = valid)
#'       shinyjs::toggleElement(ID("btn_ok"), condition = FALSE)
#'     } else {
#'       shinyjs::toggleElement(ID("btn_next"), condition = FALSE)
#'       shinyjs::toggleElement(ID("btn_ok"), condition = TRUE)
#'       shinyjs::toggleState(ID("btn_ok"), condition = valid)
#'     }
#'   })
#'
#'   observeEvent(input[[ ID("btn_back") ]], {
#'     if (current.page() > 1) {
#'       current.page(current.page() - 1)
#'
#'       updateTabsetPanel(session, inputId = ID("tabs"), selected = as.character(current.page()))
#'     }
#'   })
#'
#'   observeEvent(input[[ ID("btn_next") ]], {
#'     if (current.page() < num.pages) {
#'       current.page(current.page() + 1)
#'
#'       updateTabsetPanel(session, inputId = ID("tabs"), selected = as.character(current.page()))
#'     }
#'   })
#'
#'   observeEvent(input[[ ID("btn_ok") ]], {
#'     removeModal(session)
#'
#'     if (!is.null(.callback)) {
#'       results <- submit.fun()
#'
#'       do.call(.callback, results)
#'     }
#'   })
#'
#'   show <- function(callback, title = NULL, size = default.size, ...) {
#'     ns <- session$ns
#'
#'     .callback <<- callback
#'     current.page(1)
#'
#'     tabs <- lapply(seq_along(pages.ui), function(i) {
#'       fun <- pages.ui[[i]]
#'       argnames <- intersect(names(formals(fun)), names(list(...)))
#'       args <- list(...)[ argnames ]
#'
#'       tabPanel(title = as.character(i), do.call(fun, args))
#'     })
#'     tabs$id <- ns(ID("tabs"))
#'     tabs$type <- "hidden"
#'
#'     tabset <- do.call(tabsetPanel, tabs) %>%
#'       div(style = "overflow-y: auto; overflow-x: hidden;")
#'
#'     if (size == "xl")
#'       modal.size = "m"
#'     else
#'       modal.size = size
#'
#'     md <- modalDialog(
#'       title = title,
#'       easyClose = FALSE,
#'       size = modal.size,
#'       tabset,
#'       footer = tagList(
#'         modalButton("Cancel"),
#'         actionButton(ns(ID("btn_back")), "Back"),
#'         actionButton(ns(ID("btn_next")), "Next"),
#'         actionButton(ns(ID("btn_ok")), "OK")
#'       )
#'     )
#'
#'     if (size == "xl")
#'       md <- tagAppendAttributes(md, class = "modal-xl")
#'
#'     showModal(md, session)
#'   }
#'
#'   list(
#'     id = id,
#'     show = show
#'   )
#' }
#'
#'
#'
#'
#'
#'
#'
#' #' Construct a Multi-Page Modal Dialog
#' #'
#' #' @description Used to construct a multi-page modal dialog. Next and Back buttons are
#' #'   used to navigate the pages sequentially. Optional validation functions may be
#' #'   specified for each page, enabling or disabling the Next and OK buttons appropriately.
#' #'   A callback function may be specified to obtain the user's inputs when the OK button is
#' #'   pressed on the last page.
#' #'
#' #' @param id dialog id
#' #' @param pages a function that takes a `session` argument and returns a named list of
#' #'   functions used to construct the UI for each page of the modal dialog.
#' #' @param submit a function that takes a `session` argument and return a named list of values
#' #'   to be passed on to the `callback` function.
#' #' @param validators a function that takes a `session` argument and returns a named list of
#' #'   function used to validate the current input values. If the function returns `FALSE`
#' #'   the Next or OK button on the page is disabled, otherwise the buttons are enabled.
#' #' @param back.button whether or not to show the back button on the modal's pages.
#' #' @param default.size the size of the modal dialog. One of "m", "s", "l" or "xl".
#' #' @return A list containing the modal id and the `show()` function (see details below).
#' #'
#' #' @details After creating the modal dialog, the `show` function is used to display it. This
#' #'   function takes in a mandatory `callback` argument, and optional `title`. When the user
#' #'   exits the dialog by pressing the OK button, the provided `callback` function is called
#' #'   with each argument named as an element of the result from `submit.fun`. Additional
#' #'   arguments provided to the `show` method will be passed on to the UI definition
#' #'   functions provided in `pages`. See example below.
#' #'
#' #' @export
#' #'
#' #' @examples
#' #' pages <- function(session) {
#' #'   ns <- session$ns
#' #'
#' #'   list(
#' #'     page1 = function(common.label, text.value = "") {
#' #'       tagList(
#' #'         helpText("Hello there! This is page 1! Please enter less than 5 characters."),
#' #'         textInput(ns("text_input"), label = common.label, value = text.value)
#' #'       )
#' #'     },
#' #'     page2 = function(common.label, numeric.value) {
#' #'       tagList(
#' #'         helpText("Hi again! We're on page 2 now. Enter an even number."),
#' #'         numericInput(ns("numeric_input"), label = common.label, value = numeric.value)
#' #'       )
#' #'     }
#' #'   )
#' #' }
#' #'
#' #' submit <- function(session) {
#' #'   input <- session$input
#' #'
#' #'   list(text.value = input$text_input,
#' #'        numeric.value = input$numeric_input)
#' #' }
#' #'
#' #' validators <- function(session) {
#' #'   input <- session$input
#' #'
#' #'   list(
#' #'     page1 = function() {
#' #'       return(nchar(input$text_input) < 5)
#' #'     },
#' #'     page2 = function() {
#' #'       return(input$numeric_input %% 2 == 0)
#' #'     }
#' #'   )
#' #' }
#' #'
#' #'
#' #' app <- shinyApp(
#' #'   ui = fluidPage(
#' #'     shinyjs::useShinyjs(),
#' #'     actionButton("show", "Show Modal")
#' #'   ),
#' #'   server = function(input, output, session) {
#' #'     modal <- slatesMultiPageModalServer(
#' #'       "example_modal",
#' #'       pages = pages,
#' #'       submit = submit,
#' #'       validators = validators
#' #'     )
#' #'
#' #'     observeEvent(input$show, {
#' #'       modal$show(
#' #'         common.label = "Some input", text.value = "Something", numeric.value = 41,
#' #'         title = "Example Multi-Page Modal",
#' #'         callback = function(text.value, numeric.value) {
#' #'           print(paste("Modal returned:", text.value, ",", numeric.value))
#' #'         }
#' #'       )
#' #'     })
#' #'   }
#' #' )
#' #'
#' #' runApp(app)
#' slatesMultiPageModalServer <- function(id, pages, submit,
#'                                        validators = NULL,
#'                                        observers = function(session) list(),
#'                                        back.button = TRUE,
#'                                        default.size = c("m", "s", "l", "xl")) {
#'   default.size <- match.arg(default.size)
#'
#'   moduleServer(id, function(input, output, session) {
#'     ns <- session$ns
#'
#'     pages <- pages(session)
#'     num.pages <- length(pages)
#'
#'     if (is.null(validators)) {
#'       validators <- map(1:num.pages, ~function() { TRUE }) %>%
#'         set_names(names(pages))
#'     } else {
#'       validators <- validators(session)
#'     }
#'
#'     observers <- observers(session)
#'
#'     current.page <- reactiveVal(1)
#'     .callback <- NULL
#'
#'     valid <- reactive({
#'       page.name <- names(pages)[[ current.page() ]]
#'       valid <- validators[[ page.name ]]()
#'     })
#'
#'
#'     observe({
#'       # observe all inputs so we always update the buttons
#'       invisible(lapply(names(input), function(x) input[[ x ]]))
#'
#'       shinyjs::toggleElement("btn_back",
#'         condition = (back.button == TRUE && current.page() > 1)
#'       )
#'
#'       if (current.page() < num.pages) {
#'         shinyjs::toggleElement("btn_next", condition = TRUE)
#'         shinyjs::toggleState("btn_next", condition = valid())
#'         shinyjs::toggleElement("btn_ok", condition = FALSE)
#'       } else {
#'         shinyjs::toggleElement("btn_next", condition = FALSE)
#'         shinyjs::toggleElement("btn_ok", condition = TRUE)
#'         shinyjs::toggleState("btn_ok", condition = valid())
#'       }
#'     })
#'
#'
#'     observeEvent(input$btn_back, {
#'       if (current.page() > 1) {
#'         current.page(current.page() - 1)
#'
#'         updateTabsetPanel(session, inputId = "tabs", selected = as.character(current.page()))
#'       }
#'     })
#'
#'
#'     observeEvent(input$btn_next, {
#'       req(valid())
#'
#'       if (current.page() < num.pages) {
#'         current.page(current.page() + 1)
#'
#'         updateTabsetPanel(session, inputId = "tabs", selected = as.character(current.page()))
#'       }
#'     })
#'
#'
#'     observeEvent(input$btn_ok, {
#'       req(valid())
#'
#'       removeModal(session)
#'
#'       if (!is.null(.callback)) {
#'         results <- submit(session)
#'
#'         do.call(.callback, results)
#'       }
#'     })
#'
#'
#'     show <- function(callback, title = NULL, size = default.size, ...) {
#'       ns <- session$ns
#'
#'       .callback <<- callback
#'       current.page(1)
#'
#'       tabs <- lapply(seq_along(pages), function(i) {
#'         fun <- pages[[i]]
#'         argnames <- intersect(names(formals(fun)), names(list(...)))
#'         args <- list(...)[ argnames ]
#'
#'         tabPanel(title = as.character(i), do.call(fun, args))
#'       })
#'       tabs$id <- ns("tabs")
#'       tabs$type <- "hidden"
#'
#'       tabset <- do.call(tabsetPanel, tabs) %>%
#'         div(style = "overflow-y: auto; overflow-x: hidden;")
#'
#'       if (size == "xl")
#'         modal.size = "m"
#'       else
#'         modal.size = size
#'
#'       md <- modalDialog(
#'         title = title,
#'         easyClose = FALSE,
#'         size = modal.size,
#'         tabset,
#'         footer = tagList(
#'           modalButton("Cancel"),
#'           actionButton(ns("btn_back"), "Back"),
#'           actionButton(ns("btn_next"), "Next"),
#'           actionButton(ns("btn_ok"), "OK")
#'         )
#'       )
#'
#'       if (size == "xl")
#'         md <- tagAppendAttributes(md, class = "modal-xl")
#'
#'       showModal(md, session)
#'     }
#'
#'
#'     destroy <- function() {
#'       for (x in observers) {
#'         if ("Observer" %in% class(x))
#'           x$destroy()
#'       }
#'     }
#'
#'
#'     list(
#'       id = id,
#'       show = show,
#'       destroy = destroy
#'     )
#'   })
#' }





# app <- shinyApp(
#   ui = fluidPage(
#     shinyjs::useShinyjs(),
#     actionButton("show", "Show Modal")
#   ),
#   server = function(input, output, session) {
#     modal <- slatesMultiPageModal("example_modal", function(input, output, session) {
#       ns <- session$ns
#
#       pages <- list(
#         page1 = function(common.label, text.value = "") {
#           tagList(
#             helpText("Hello there! This is page 1! Please enter less than 5 characters."),
#             textInput(ns("text_input"), label = common.label, value = text.value)
#           )
#         },
#         page2 = function(common.label, numeric.value) {
#           tagList(
#             helpText("Hi again! We're on page 2 now. Enter an even number to proceed."),
#             numericInput(ns("numeric_input"), label = common.label, value = numeric.value)
#           )
#         }
#       )
#
#       validators <- list(
#         page1 = function() {
#           return(nchar(input$text_input) < 5)
#         },
#         page2 = function() {
#           return(input$numeric_input %% 2 == 0)
#         }
#       )
#
#       submit <- function() {
#         input <- session$input
#
#         list(text.value = input$text_input,
#              numeric.value = input$numeric_input)
#       }
#
#       list(
#         pages = pages,
#         validators = validators,
#         submit = submit
#       )
#     })
#
#     observeEvent(input$show, {
#       modal$show(
#         common.label = "Some input", text.value = "Something", numeric.value = 41,
#         title = "Example Multi-Page Modal",
#         callback = function(text.value, numeric.value) {
#           print(paste("Modal returned:", text.value, ",", numeric.value))
#         }
#       )
#     })
#   }
# )
#
# runApp(app)
slatesMultiPageModal <- function(id, modal.fun,
                                 back.button = TRUE,
                                 default.size = c("m", "s", "l", "xl")) {
  default.size <- match.arg(default.size)

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    modal.data <- modal.fun(input, output, session)

    pages <- modal.data$pages
    validators <- modal.data$validators
    observers <- modal.data$observers

    num.pages <- length(pages)

    if (is.null(validators)) {
      validators <- map(1:num.pages, ~function() { TRUE }) %>%
        set_names(names(pages))
    }

    current.page <- reactiveVal(1)
    .callback <- NULL

    valid <- reactive({
      page.name <- names(pages)[[ current.page() ]]
      valid <- validators[[ page.name ]]()
    })


    observe({
      # observe all inputs so we always update the buttons
      invisible(lapply(names(input), function(x) input[[ x ]]))

      shinyjs::toggleElement(
        "btn_back",
        condition = (back.button == TRUE && current.page() > 1)
      )

      if (current.page() < num.pages) {
        shinyjs::toggleElement("btn_next", condition = TRUE)
        shinyjs::toggleState("btn_next", condition = valid())
        shinyjs::toggleElement("btn_ok", condition = FALSE)
      } else {
        shinyjs::toggleElement("btn_next", condition = FALSE)
        shinyjs::toggleElement("btn_ok", condition = TRUE)
        shinyjs::toggleState("btn_ok", condition = valid())
      }
    })


    observeEvent(input$btn_back, {
      if (current.page() > 1) {
        current.page(current.page() - 1)

        updateTabsetPanel(session, inputId = "tabs", selected = as.character(current.page()))
      }
    })


    observeEvent(input$btn_next, {
      req(valid())

      if (current.page() < num.pages) {
        current.page(current.page() + 1)

        updateTabsetPanel(session, inputId = "tabs", selected = as.character(current.page()))
      }
    })


    observeEvent(input$btn_ok, {
      req(valid())

      removeModal(session)

      if (!is.null(.callback)) {
        results <- submit(session)

        do.call(.callback, results)
      }
    })


    show <- function(callback, title = NULL, size = default.size, ...) {
      ns <- session$ns

      .callback <<- callback
      current.page(1)

      tabs <- lapply(seq_along(pages), function(i) {
        fun <- pages[[i]]
        argnames <- intersect(names(formals(fun)), names(list(...)))
        args <- list(...)[ argnames ]

        tabPanel(title = as.character(i), do.call(fun, args))
      })
      tabs$id <- ns("tabs")
      tabs$type <- "hidden"

      tabset <- do.call(tabsetPanel, tabs) %>%
        div(style = "overflow-y: auto; overflow-x: hidden;")

      if (size == "xl")
        modal.size = "m"
      else
        modal.size = size

      md <- modalDialog(
        title = title,
        easyClose = FALSE,
        size = modal.size,
        tabset,
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("btn_back"), "Back"),
          actionButton(ns("btn_next"), "Next"),
          actionButton(ns("btn_ok"), "OK")
        )
      )

      if (size == "xl")
        md <- tagAppendAttributes(md, class = "modal-xl")

      showModal(md, session)
    }


    destroy <- function() {
      for (x in observers) {
        if ("Observer" %in% class(x))
          x$destroy()
      }
    }


    list(
      id = id,
      show = show,
      destroy = destroy
    )
  })
}







#' Create the new project modal
#'
#' @description
#' Creates a modal to be displayed when creating a new project. Allows
#' input of the project's initital configuration, such as title and author.
#'
#' @param id the id for this modal.
#' @param session the session from the server function.
#'
#' @return
#' A list containing the `show` function used to display the modal after being
#' created.
#'
#' @export
create_new_project_modal <- function(id, session) {
  ID <- function(x) paste0(id, "_", x)

  ns <- session$ns
  input <- session$input

  .callback <- NULL

  observeEvent(input[[ ID("btn_ok") ]], {
    removeModal(session)

    if (!is.null(.callback)) {
      .callback(input[[ ID("title_input") ]], input[[ ID("author_input") ]])
    }
  })

  show <- function(callback, ...) {
    ns <- session$ns

    .callback <<- callback

    showModal(
      modalDialog(
        title = "New Project",
        textInput(ns(ID("title_input")),
                  label = "Title",
                  value = "",
                  placeholder = "Enter a title for the project"),
        textInput(ns(ID("author_input")),
                  label = "Author(s)",
                  value = "",
                  placeholder = "Enter the name(s) of the author(s)"),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns(ID("btn_ok")), "OK")
        )
      ),
      session
    )
  }

  list(
    id = id,
    show = show
  )
}





create_file_import_modal <- function(id, session) {
  ID <- function(x) paste0(id, "_", x)
  ns <- session$ns
  input <- session$input

  .callback <- NULL

  observeEvent(input[[ ID("button_ok") ]], {
    removeModal()

    if (!is.null(.callback)) {
      type <- input[[ ID("radio_input") ]]

      if (type == "local") {
        res <- list(type = "local",
                    filename = input[[ ID("file_input") ]]$name,
                    datapath = input[[ ID("file_input") ]]$datapath,
                    size = input[[ ID("file_input") ]]$size,
                    data = NULL)
      } else if (type == "url") {
        res <- list(type = "url",
                    filename = input[[ ID("url_input") ]],
                    datapath = NULL,
                    size = 0,
                    data = NULL)
      } else if (type == "builtin") {
        res <- list(type = "builtin",
                    filename = NULL,
                    datapath = NULL,
                    size = NULL,
                    data = NULL)
      }

      .callback(res)
    }
  })

  show <- function(callback) {
    .callback <<- callback

    showModal(
      modalDialog(
        title = "Import Dataset",
        shinyWidgets::radioGroupButtons(
          ns(ID("radio_input")),
          label = "Import from",
          choices = c("Local File"="local", "URL"="url", "Built-in Dataset"="builtin")),
        conditionalPanel(
          condition = paste0("input[ '", ns(ID("radio_input")), "'] === 'local'"),
          slatesFileInput(ns(ID("file_input")), label = "File")
        ),
        conditionalPanel(
          condition = paste0("input[ '", ns(ID("radio_input")), "'] === 'url'"),
          flowLayout(
            textInput(
              ns(ID("url_input")),
              label = "URL",
              placeholder = "http://my.site.org/file.txt"),
            actionButton(
              ns(ID("download_btn")),
              "Download",
              icon = icon("download"))
          )
        ),
        footer = tagList(
          modalButton("Cancel"),
          shinyjs::disabled(actionButton(ns(ID("button_ok")), "OK"))
        )
      )
    )
  }

  list(
    id = id,
    show = show
  )
}







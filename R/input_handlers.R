#
# Inputs
#


isValidExpression <- function(expr) {
  tryCatch({
    parse(text = expr)
    return(TRUE)
  },
  error = function(e) {
    return(FALSE)
  })
}



#' Define behaviour of an input type
#'
#' @param create.ui function used to create the input UI.
#' @param update.ui function used to update the input UI.
#' @param get.value function to get the current value of the input.
#'
#' @return a list of functions to handle the the input type.
#' @export
#'
#' @examples
input_handler <- function(create.ui = function(...) { tagList() },
                          update.ui = function(...) {},
                          get.value = function(x, session, ...) { session$input[[ x$id ]] },
                          create.observer = function(...) {}) {
  list(
    create.ui = create.ui,
    update.ui = update.ui,
    get.value = get.value,
    create.observer = create.observer
  )
}


input.handlers <- list(
  logical = input_handler(
    create.ui = function(id, x) {
      # checkboxInput(id, label = label, value = as.logical(value))
      slatesSelectInput(id, label = x$name,
                  choices = c(TRUE, FALSE), selected = x$value,
                  wizards = x$wizards)
    },
    update.ui = function(session, id, ...) {
      #updateCheckboxInput(session, inputId = id, ...)
      updateSelectInput(session, inputId = id, ...)
    }
  ),
  character = input_handler(
    create.ui = function(id, x) {
      slatesTextInput(id, label = x$name, value = x$value, wizards = x$wizards)
    },
    update.ui = function(session, id, ...) {
      updateTextInput(session, inputId = id, ...)
    }
  ),
  numeric = input_handler(
    create.ui = function(id, x) {
      slatesNumericInput(id, label = x$name, value = x$value, wizards = x$wizards)
    },
    update.ui = function(session, id, ...) {
      updateNumericInput(session, inputId = id, ...)
    }
  ),
  expression = input_handler(
    create.ui = function(id, x) {
      slatesExpressionInput(id, label = x$name, value = x$value, wizards = x$wizards)
    },
    update.ui = function(session, id, ...) {
      updateTextInput(session, inputId = id, ...)
    },
    create.observer = function(session, id) {
      my_id <- id

      observeEvent(session$input[[ my_id ]], {
        shinyjs::removeClass(my_id, "invalid-expression")

        if (!isValidExpression(session$input[[ my_id ]]))
          shinyjs::addClass(my_id, "invalid-expression")
      })
    },
    get.value = function(x, session, ...) {
      is.valid <- isValidExpression(session$input[[ x$id ]])
      late.eval <- if (is.null(x$late.eval)) FALSE else x$late.eval

      if (!is.valid)
        return(NULL)

      if (late.eval == FALSE) {
        eval(parse(text = session$input[[ x$id ]]), env = new.env())
      } else {
        parse(text = session$input[[ x$id ]])
      }
    }
  ),
  choices = input_handler(
    create.ui = function(id, x) {
      multiple <- if (is.null(x$multiple)) FALSE else x$multiple

      slatesSelectInput(id, label = x$name, selected = x$value,
                        choices = x$choices, multiple = multiple,
                        wizards = x$wizards)
    },
    update.ui = function(session, id, ...) {
      updateSelectInput(session, inputId = id, ...)
    }
  ),
  `free-choices` = input_handler(
    create.ui = function(id, x) {
      multiple <- if (is.null(x$multiple)) FALSE else x$multiple

      selectizeInput(
        id, label = x$name, choices = x$choices,
        selected = x$value, multiple = multiple,
        options = list(
          delimiter = '',
          create = "function(input) { return { value: input, text: input } }"
        )
      )
    },
    update.ui = function(session, id, ...) {
      # updateSelectInput(session, inputId = id, ...)
    }
  ),
  numeric2 = input_handler(
    create.ui = function(id, x) {
      slatesNumeric2Input(id, label = x$name, value = x$value, wizards = x$wizards)
    },
    update.ui = function(session, id, ...) {
      #updateNumericInput(session, inputId = id, ...)
    },
    get.value = function(x, session, ...) {
      sapply(1:2, function(i) {
        session$input[[ paste0(x$id, "_", i) ]]
      })
    }
  ),
  numeric4 = input_handler(
    create.ui = function(id, x) {
      slatesNumeric4Input(id, label = x$name, value = x$value, wizards = x$wizards)
    },
    update.ui = function(session, id, ...) {
      #updateNumericInput(session, inputId = id, ...)
    },
    get.value = function(x, session, ...) {
      sapply(1:4, function(i) {
        session$input[[ paste0(x$id, "_", i) ]]
      })
    }
  )
)


dataset.handlers <- list(
  file = input_handler(
    get.value = function(s, session, ...) {
      if (!is.null(s$data)) {
        gsub("\\\\", "/", s$data$datapath)
      } else {
        NULL
      }
    }
  ),
  standalone = input_handler(
    get.value = function(s, session, ...) { NULL }
  )
)




createInput <- function(input, ns = identity) {
  input.handlers[[ input$input.type ]]$create.ui(ns(input$id), input)
}


createInputGroup <- function(group, id = NULL, ns = identity) {
  ui.elements <- unname(lapply(group$inputs, function(x) {
    createInput(x, ns = ns)
  }))

  if (!is.null(id))
    ui.elements$id <- ns(id)

  layoutFun <- switch(group$layout,
                      "flow-2" = function(...) tags$div(class = "slates-flow-2", ...),
                      "flow-3" = function(...) tags$div(class = "slates-flow-3", ...),
                      "flow-4" = function(...) tags$div(class = "slates-flow-4", ...),
                      "vertical" = shiny::verticalLayout)

  tagList(
    do.call(layoutFun, ui.elements)
  )
}


createInputPage <- function(page, id = NULL, ns = identity, layout = "flow") {
  if (length(page$groups) == 0)
    return(tagList())

  ui.groups <- lapply(page$groups, function(x) {
    if (class(x) == "list" && x$type == "group")
      createInputGroup(x, ns = ns)
    else if (class(x) == "list" && x$type == "input")
      createInput(x, ns = ns)
  })

  if (!is.null(page$description)) {
    ui.groups <- append(list(helpText(page$description)), ui.groups)
  }

  do.call(verticalLayout, ui.groups)
}


createInputLayout <- function(pages,
                              ns = identity,
                              container = c("tabset", "collapse")) {
  container <- match.arg(container)

  # find main page if it exists
  page.names <- sapply(pages, "[[", "name")
  main.index <- grep("^Main$", page.names)
  if (length(main.index) == 1) {
    main.page <- pages[[ main.index ]]
    pages <- pages[ -main.index ]
  } else {
    main.page <- NULL
  }

  # build pages
  pages <- lapply(pages, function(x) {
    x$ui <- createInputPage(x, ns = ns)

    return(x)
  })

  # build the container
  if (container == "tabset") {
    # simple tabset
    tabs <- unname(lapply(pages, function(x) tabPanel(title = x$name, x$ui)))

    ui <- do.call(tabsetPanel, tabs)
  } else if (container == "collapse") {
    # bs4 accordion
    accordion.id <- seq.uid("accordion")

    tabs <- lapply(seq_along(pages), function(i) {
      x <- pages[[ i ]]

      tab.id <- seq.uid("accordion_tab")

      tags$div(
        tags$div(
          class = "card-header",
          tags$a(
            class = "card-link",
            `data-toggle` = "collapse",
            href = paste0("#", tab.id),
            x$name
          )
        ),
        tags$div(
          id = tab.id,
          class = if (i == 1) "collapse show" else "collapse",
          `data-parent` = paste0("#", accordion.id),
          x$ui
        )
      )
    })

    ui <- tags$div(
      id = accordion.id,
      tabs
    )
  }

  # build and pre-append main page
  if (!is.null(main.page)) {
    main.ui <- createInputPage(main.page, ns = ns)

    ui <- tagList(main.ui, ui)
  }

  return(ui)
}









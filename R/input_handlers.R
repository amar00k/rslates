#
# Inputs
#


isValidExpression <- function(expr) {
  tryCatch({
    if (expr == "")
      return(TRUE)
    else
      parse(text = expr)

    return(TRUE)
  },
  error = function(e) {
    return(FALSE)
  })
}


quote.input <- function(x) {
  paste0('"', x, '"')
}


#' Define behaviour of an input type
#'
#' @description This function prepares a structure that defines all aspects of an
#'   input type: UI creation, UI updating, observer function and string representation
#'   of the input value.
#'
#' @param default.value default value of this input type when no default value is provided
#' @param params.list list of parameters used by the input type. Each element of the list
#'   describes a parameter and has the following members: name (pretty name of the parameter),
#'   choices (vector of allowed values), default (default value when not specified).
#' @param create.ui function used to create the input UI.
#' @param update.ui function used to update the input UI.
#' @param get.value function that transforms the current value of the input
#'   to the appropriate output string value. If value is provided, then
#'   this value is used instead of the input element value.
#' @param get.inputs used for testing purporses. Returns a list of pairs (input_id, value).
#' @param get.source get the value of this input in source form.
#' @param observer actions to be taken as an observer
#'
#' @return a list of functions to handle the the input type.
#' @export
inputHandler <- function(default.value,
                         params.list = list(),
                         create.ui = function(...) { tagList() },
                         update.ui = function(x, session) {},
                         get.inputs = function(x, session, value) {
                           inputs <- list()
                           inputs[[ x$id ]] <- value
                           return(inputs)
                         },
                         get.value = function(x, session = NULL, value = NULL) {
                           if (is.null(value))
                             value <- session$input[[ x$id ]]

                           return (value)
                         },
                         get.source = function(x, session = NULL, value = NULL) {
                           as.character(getHandler(x)$get.value(x, session, value))
                         },
                         observer = function(x, session) {}) {
  list(
    default.value = default.value,
    params.list = params.list,
    create.ui = create.ui,
    update.ui = update.ui,
    get.inputs = get.inputs,
#    get.input.value = get.input.value,
    get.value = get.value,
    get.source = get.source,
    observer = observer
  )
}


input.handlers <- list(
  logical = inputHandler(
    default.value = FALSE,
    params.list = list(
      display.type = list(label = "Display Type", type = "choices",
                          choices = c("select", "checkbox", "switch"), default = "select")
    ),
    create.ui = function(id, x) {
      switch(
        x$display.type,
        "select" = slatesSelectInput(
          id, label = x$name,
          choices = c(TRUE, FALSE),
          selected = as.logical(x$value),
          wizards = x$wizards
        ),
        "checkbox" = checkboxInput(
          id, label = x$name,
          value = as.logical(x$value)
        ),
        "switch" = slatesSwitchInput(
          id, label = x$name,
          value = as.logical(x$value),
          on.off.labels = c("True", "False")
        )
      )
    },
    update.ui = function(x, session) {
      # TODO
      # updateSelectInput(session, inputId = id, ...)
    }
  ),
  character = inputHandler(
    default.value = "",
    params.list = list(
#      quote = list(label = "Quote", type = "logical", default = TRUE)
    ),
    create.ui = function(id, x) {
      slatesTextInput(id, label = x$name, value = x$value, wizards = x$wizards)
    },
    update.ui = function(x, session) {
      #updateTextInput(session, inputId = id, ...)
    },
    get.value = function(x, session = NULL, value = NULL) {
      if (is.null(value))
        value <- session$input[[ x$id ]]

      return(value)
    },
    get.source = function(x, session = NULL, value = NULL) {
      value <- input.handlers$character$get.value(x, session, value)

      paste0('"', value, '"')
    }
  ),
  numeric = inputHandler(
    default.value = 0,
    params.list = list(
    ),
    create.ui = function(id, x) {
      slatesNumericInput(id, label = x$name, value = x$value, wizards = x$wizards)
    },
    update.ui = function(session, id, ...) {
      updateNumericInput(session, inputId = id, ...)
    }
  ),
  expression = inputHandler(
    default.value = "",
    params.list = list(
      check.valid = list(label = "Check Valid Expression", type = "logical", default = TRUE)
    ),
    create.ui = function(id, x) {
      slatesExpressionInput(id, label = x$name, value = x$value, wizards = x$wizards)
    },
    update.ui = function(session, id, ...) {
      updateTextInput(session, inputId = id, ...)
    },
    observer = function(x, session) {
      shinyjs::removeClass(x$id, "invalid-expression")

      if (!isValidExpression(session$input[[ x$id ]]))
        shinyjs::addClass(x$id, "invalid-expression")
    },
    get.value = function(x, session = NULL, value = NULL) {
      if (is.null(value))
        value <- session$input[[ x$id ]]

      return(value)
    },
    get.source = function(x, session = NULL, value = NULL) {
      if (is.null(value))
        value <- session$input[[ x$id ]]

      return(value)
    }
  ),
  choices = inputHandler(
    default.value = list(),
    params.list = list(
      choices = list(label = "Choices", type = "list", default = ""),
      multiple = list(label = "Allow Multiple Values", type = "logical", default = FALSE),
      custom = list(label = "Allow Custom Value(s)", type = "logical", default = FALSE)
    ),
    create.ui = function(id, x) {
      multiple <- if (is.null(x$multiple)) FALSE else x$multiple
      custom <- if (is.null(x$custom)) FALSE else x$custom

      if (!custom) {
        slatesSelectInput(id, label = x$name, selected = x$value,
                          choices = x$choices, multiple = multiple,
                          wizards = x$wizards)
      } else {
        options <- list(
          delimiter = '',
          create = "function(input) { return { value: input, text: input } }"
        )

        selectizeInput(
          inputId = id, label = x$name, selected = x$value,
          choices = x$choices, multiple = multiple,
          options = options
        )
      }

      # if (custom == TRUE)
      #   options <- list(
      #     delimiter = '',
      #     create = "function(input) { return { value: input, text: input } }"
      #   )
      # else
      #   options <- NULL

      # selectizeInput(
      #   id, label = x$name, selected = x$value,
      #   choices = x$choices, multiple = multiple,
      #   options = options
      # )
    },
    update.ui = function(x, session) {
      # TODO
      #updateSelectInput(session, x$id, choices = x$choices, selected = x$value, multiple = multiple)
    },
    get.value = function(x, session = NULL, value = NULL) {
      if (is.null(value))
        value <- session$input[[ x$id ]]

      return(value)
    },
    get.source = function(x, session = NULL, value = NULL) {
      value <- input.handlers$choices$get.value(x, session, value)

      if (length(value) > 1)
        paste0("c(", paste0('"', value, '"', collapse = ", "), ")")
      else
        paste0('"', value, '"')
    }
  ),
  numeric2 = inputHandler(
    default.value = c(0, 0),
    params.list = list(
    ),
    create.ui = function(id, x) {
      slatesNumeric2Input(id, label = x$name, value = x$value, wizards = x$wizards)
    },
    update.ui = function(session, id, ...) {
      #updateNumericInput(session, inputId = id, ...)
    },
    get.value = function(x, session = NULL, value = NULL) {
      if (is.null(value)) {
        value <- sapply(1:2, function(i) {
          session$input[[ paste0(x$id, "_", i) ]]
        })
      }

      if (length(value) < 2)
        value <- c(value, rep(NA, 2 - length(value)))

      return(value)
    },
    get.source = function(x, session = NULL, value = NULL) {
      value <- input.handlers$numeric2$get.value(x, session, value)

      paste0("c(", paste(as.numeric(value), collapse = ", "), ")")
    },
    get.inputs = function(x, session, value) {
      inputs <- list()
      inputs[[ paste0(x$id, "_", 1) ]] <- value[1]
      inputs[[ paste0(x$id, "_", 2) ]] <- value[2]
      return(inputs)
    }
  ),
  numeric4 = inputHandler(
    default.value = c(0, 0, 0, 0),
    params.list = list(
    ),
    create.ui = function(id, x) {
      slatesNumeric4Input(id, label = x$name, value = x$value, wizards = x$wizards)
    },
    update.ui = function(session, id, ...) {
      #updateNumericInput(session, inputId = id, ...)
    },
    get.value = function(x, session = NULL, value = NULL) {
      if (is.null(value)) {
        value <- sapply(1:4, function(i) {
          session$input[[ paste0(x$id, "_", i) ]]
        })
      }

      if (length(value) < 4)
        value <- c(value, rep(NA, 4 - length(value)))

      return(value)
    },
    get.source = function(x, session = NULL, value = NULL) {
      value <- input.handlers$numeric4$get.value(x, session, value)

      print(value)

      paste0("c(", paste(as.numeric(value), collapse = ", "), ")")
    },
    get.inputs = function(x, session, value) {
      inputs <- list()
      inputs[[ paste0(x$id, "_", 1) ]] <- value[1]
      inputs[[ paste0(x$id, "_", 2) ]] <- value[2]
      inputs[[ paste0(x$id, "_", 3) ]] <- value[3]
      inputs[[ paste0(x$id, "_", 4) ]] <- value[4]
     return(inputs)
    }
  )
)


dataset.handlers <- list(
  file = inputHandler(
    default.value = "",
    get.value = function(s, session = NULL, value = NULL) {
      if (!is.null(s$data)) {
        gsub("\\\\", "/", s$data$datapath)
      } else {
        NULL
      }
    }
  ),
  standalone = inputHandler(
    default.value = "",
    get.value = function(s, session = NULL, value = NULL) { NULL }
  )
)


getLayoutFun <- function(layout) {
  switch(layout,
         "flow-2" = function(...) tags$div(class = "slates-flow-2", ...),
         "flow-3" = function(...) tags$div(class = "slates-flow-3", ...),
         "flow-4" = function(...) tags$div(class = "slates-flow-4", ...),
         "vertical" = shiny::verticalLayout)
}


createInputUI <- function(input, ns = identity) {
  getHandler(input)$create.ui(ns(input$id), input)
}


createGroupUI <- function(group, inputs, ns = identity) {
  my.inputs <- inputs %>%
    keep(map(., "parent") == group$name) %>%
    map(createInputUI, ns = ns) %>%
    tagList() %>%
    do.call(getLayoutFun(group$layout), .)

  description.ui <- tags$p(
    class = "slates-group-description",
    group$description
  )

  ui <- tagList(
    description.ui,
    my.inputs
  )

  return(ui)
}


createPageUI <- function(page, groups = list(), inputs = list(), ns = identity) {
  my.inputs <- inputs %>%
    keep(map(., "parent") == page$name) %>%
    map(createInputUI, ns = ns) %>%
    tagList() %>%
    do.call(getLayoutFun(page$layout), .)

  my.groups <- groups %>%
    keep(map(., "parent") == page$name) %>%
    map(createGroupUI, inputs, ns = ns) %>%
    tagList() %>%
    do.call(getLayoutFun("vertical"), .)

  description.ui <- tags$p(
    class = "slates-page-description",
    page$description
  )

  ui <- tagList(
    description.ui,
    my.inputs,
    my.groups
  )

  return(ui)
}



#' Create the UI For the Inputs Panel
#'
#' @param pages list of pages.
#' @param groups list of groups.
#' @param inputs list of inputs.
#' @param ns the namespace function to use for creating tag ids
#' @param inputs.style the style of the input panel. `tabset` generates a
#'   tabsetPanel with each page in a tabPanel element. `collapses` uses
#'   Bootstrap 4 collapses to compartmentalize pages. `flowing` places
#'   all pages vertically in the same container and uses h5 tags for the
#'   page titles.
#'
#' @return a tag.list structure.
#' @export
#'
#' @examples
createInputLayout <- function(pages, groups, inputs,
                              ns = identity,
                              inputs.style = c("tabset", "collapses", "flowing")) {
  inputs.style <- match.arg(inputs.style)

  # default.page <- pages[[ "default" ]]

  # build pages (except default page)
  pages %<>%
    #keep(map(., "name") != "default") %>%
    map(~list_modify(., ui = createPageUI(., groups, inputs, ns = ns)))

  # build the container
  if (inputs.style == "tabset") {
    # simple tabset
    tabs <- unname(lapply(pages, function(x) tabPanel(title = x$name, x$ui)))

    ui <- do.call(tabsetPanel, tabs)
  } else if (inputs.style == "collapses") {
    # bs4 accordion
    accordion.id <- seq.uid("accordion")

    tabs <- lapply(seq_along(pages), function(i) {
      x <- pages[[ i ]]

      tab.id <- seq.uid("accordion_tab")

      tags$div(
        tags$div(
          class = "card-header position-relative",
          tags$a(
            class = "card-link stretched-link",
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
  } else if (inputs.style == "flowing") {
    ui.list <- lapply(pages, function(x) {
      tags$div(
        tags$h5(
          class = "slates-page-title-flowing",
          x$name
        ),
        x$ui
      )
    })

    ui <- do.call(verticalLayout, ui.list)
  }

  # build and prepend default page
  my.inputs <- inputs %>%
    keep(map_lgl(., ~is.null(.$parent))) %>%
    map(createInputUI, ns = ns) %>%
    tagList() %>%
    do.call(getLayoutFun("flow-2"), .)

  ui <- tagList(my.inputs, ui)

  return(ui)
}









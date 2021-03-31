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


jsBoolean <- function(val) {
  if (val) "true" else "false"
}




#' Define behaviour of an input type
#'
#' @description This function prepares a structure that defines all aspects of an
#'   input type: UI creation, UI updating, observer function and string representation
#'   of the input value.
#'
#' @param default.value default value of this input type
#' @param params.list list of parameters used by the input type. Each element of the list
#'   describes a parameter and has the following members: `name` (pretty name of
#'   the parameter), type (one of `character`, `numeric`, `logical`, `choices`),
#'   `choices` (when type is `choices`, the vector of allowed values), `default`
#'   (default value).
#' @param createUI function used to create the input UI.
#' @param updateUI function used to update the input UI.
#' @param get.input function that returns the raw input(s) from the `session$input`
#' object as a *list*.
#' @param as.value function that transforms the value from `get.input()`
#'   to the appropriate format for the input type. If `value` is provided, then
#'   the function tries to coerce this value to the appropriate type.
#' @param as.source function that transforms the value from `as.value()` to source code.
#' @param observer actions to be taken as an observer of any reactive expression(s).
#'
#' @details The default functions assume a single input and return the value without
#'   transformations `get.input()` returns a list of one element, `as.value` unlists
#'   this element, and `as.source()` returns that value as a character value.
#'
#' @return the list of functions to handle the the input type.
#' @export
inputHandler <- function(
  default.value = "",
  params.list = list(),

  initInput = function(x) { x },

  createUI = function(x, ns = identity, value = NULL) {
    tags$div(id = session$ns(x$id))
  },

  updateUI = function(x, session) {},

  setState = function(x, session, enabled, return.js = FALSE) {
    if (class(x) == "character")
      id <- x
    else
      id <- x$id

    selector <- paste0('$("#', session$ns(id), '")')

    js <- paste0(selector, '.attr("disabled",', jsBoolean(!enabled), ')') %>% print

    if (return.js)
      return(js)
    else
      shinyjs::runjs(js)
  },

  setVisible = function(x, session, visible, tag.only = FALSE, return.js = FALSE) {
    if (class(x) == "character")
      id <- x
    else
      id <- x$id

    action <- if (visible) 'removeClass("d-none")' else 'addClass("d-none")'

    if (tag.only)
      selector <- paste0('$("#', session$ns(id), '")')
    else
      selector <- paste0('$("#', session$ns(id), '").parent()')

    js <- paste0(selector, ".", action, ";")

    if (return.js)
      return(js)
    else
      shinyjs::runjs(js)
  },

  # isNull = function(x, session) {
  #   #null.input <- session$input[[ paste0(x$id, "-null") ]]
  #   chooser.input <- session$input[[ paste0(x$id, "-chooser") ]]
  #
  #   return(!is.null(chooser.input) && chooser.input == "NULL")
  # },

  get.input = function(x, session) {
    return(
      session$input[[ x$id ]]
    )
  },

  set.input = function(x, session, value) {
    updateUI(x, session, value)
  },

  as.value = function(x, session = NULL, value = NULL) {
    if (is.null(value))
      value <- getHandler(x)$get.input(x, session)

    value <- as.character(value)

    if (length(value) > 1)
      stop("input must have length 1.")

    return(value)
  },

  as.string = function(x, session, value = NULL) {
    getHandler(x)$as.value(x, session, value) %>%
      toString
  },

  as.source = function(x, session = NULL, value = NULL) {
    getHandler(x)$as.value(x, session, value) %>%
      as.character
  },

  observer = function(x, session) {
  }) {

  initInputFun <- function(x) {
    # insert type-specific params
    pars <- getHandler(x)$params.list %>%
      map("default")

    w <- which(!(names(pars) %in% names(x)))
    x <- modifyList(x, pars[w])

    initInput(x)
  }

  as.value.fun <- function(x, session = NULL, value = NULL) {
    if (!is.null(value))
      return(as.value(x, session, value))

    chooser.input <- session$input[[ paste0(x$id, "-chooser") ]]
    if (!is.null(chooser.input) && chooser.input == "NULL")
      return(NULL)
    else
      return(as.value(x, session, value))
  }

  as.source.fun <- function(x, session = NULL, value = NULL) {
    if (!is.null(value))
      return(as.source(x, session, value))

    chooser.input <- session$input[[ paste0(x$id, "-chooser") ]]
    if (!is.null(chooser.input) && chooser.input == "NULL")
      return("NULL")
    else
      return(as.source(x, session, value))
  }

  observerFun <- function(x, session) {
    # handle null toggle
    # null.input <- session$input[[ paste0(x$id, "-null") ]]
    # getHandler(x)$setVisible(x, session, is.null(null.input), tag.only = TRUE)
    chooser.input <- session$input[[ paste0(x$id, "-chooser") ]]

    if (!is.null(chooser.input))
      getHandler(x)$setVisible(x, session, chooser.input != "NULL", tag.only = TRUE)

    # handle type-specific observers
    observer(x, session)
  }

  list(
    initInput = initInputFun,
    default.value = default.value,
    params.list = params.list,
    createUI = createUI,
    setState = setState,
    setVisible = setVisible,
    updateUI = updateUI,
    get.input = get.input,
    as.value = as.value.fun,
    as.source = as.source.fun,
    observer = observerFun
  )
}


input.handlers <- list(

  #
  # logical
  #
  logical = inputHandler(

    default.value = FALSE,

    params.list = list(
      display.type = list(label = "Display Type", type = "choices",
                          choices = c("select", "checkbox", "switch"), default = "select")
    ),

    createUI = function(x, ns = identity) {
      value <- if (!is.null(x$value)) x$value else x$default

      switch(
        x$display.type,
        "select" = slatesSelectInput(
          ns(x$id), label = x$name,
          choices = c(TRUE, FALSE),
          value = as.logical(value),
          allow.null = x$allow.null,
          wizards = x$wizards
        ),
        "checkbox" = checkboxInput(
          ns(x$id), label = x$name,
          value = as.logical(value)
        ),
        "switch" = slatesSwitchInput(
          ns(x$id), label = x$name,
          value = as.logical(value),
          on.off.labels = c("True", "False")
        )
      )
    },

    updateUI = function(x, session) {
      switch(
        x$display.type,
        "select" = updateSelectInput(session, inputId = x$id, value = as.character(x$value))
      )
    },

    setVisible = function(x, session, visible, tag.only = FALSE, return.js = FALSE) {
      if (class(x) == "character")
        id <- x
      else
        id <- x$id

      action <- if (visible) 'removeClass("d-none")' else 'addClass("d-none")'

      if (tag.only)
        selector <- paste0('$("#', session$ns(id), '").parent()')
      else
        selector <- paste0('$("#', session$ns(id), '").parent().parent()')

      js <- paste0(selector, ".", action, ";")

      if (return.js)
        return(js)
      else
        shinyjs::runjs(js)
    },

    as.value = function(x, session = NULL, value = NULL) {
      if (is.null(value))
        value <- input.handlers$logical$get.input(x, session)[[1]]

      value <- as.logical(value)

      if (length(value) > 1)
        stop("logical input must have length 1.")

      return(value)
    }
  ),

  #
  # character
  #
  character = inputHandler(

    default.value = "",

    params.list = list(
      quote = list(label = "Quote", type = "logical", default = TRUE)
    ),

    createUI = function(x, ns = identity) {
      value <- if (!is.null(x$value)) x$value else x$default

      slatesTextInput(ns(x$id), label = x$name,
                      value = value,
                      allow.null = x$allow.null,
                      wizards = x$wizards)
    },

    updateUI = function(x, session) {
      updateTextInput(session, inputId = x$id, value = as.character(x$value))
    },

    as.source = function(x = NULL, session = NULL, value = NULL) {
      if (is.null(value))
        value <- input.handlers$character$as.value(x, session, value)

      if (x$quote == TRUE)
        paste0('"', value, '"')
      else
        value
    }
  ),

  #
  # numeric
  #
  numeric = inputHandler(

    default.value = 0,

    params.list = list(
      min = list(label = "Min", type = "numeric", default = NULL),
      max = list(label = "Max", type = "numeric", default = NULL)
    ),

    createUI = function(x, ns = identity, value = NULL) {
      value <- if (!is.null(x$value)) x$value else x$default

      slatesNumericInput(ns(x$id), label = x$name,
                         value = value,
                         allow.null = x$allow.null,
                         wizards = x$wizards)
    },

    updateUI = function(x, session) {
      updateNumericInput(session, inputId = x$id, value = x$value)
    },

    as.value = function(x, session = NULL, value = NULL) {
      if (is.null(value))
        value <- input.handlers$numeric$get.input(x, session)[[1]]

      value <- as.numeric(value)

      if (length(value) > 1)
        stop("numeric input must have length 1.")

      as.numeric(value)
    }
  ),

  #
  # expression
  #
  expression = inputHandler(

    default.value = "",

    params.list = list(
      check.valid = list(label = "Check Valid Expression", type = "logical", default = TRUE)
    ),

    createUI = function(x, ns = identity) {
      value <- if (!is.null(x$value)) x$value else x$default

      slatesExpressionInput(ns(x$id), label = x$name,
                            value = value,
                            allow.null = x$allow.null,
                            wizards = x$wizards)
    },

    updateUI = function(x, session) {
      updateTextInput(session, inputId = x$id, value = as.character(x$value))
    },

    as.value = function(x = NULL, session = NULL, value = NULL) {
      if (is.null(value))
        value <- input.handlers$expression$get.input(x, session)[[1]]

      value <- as.character(value)

      # value <- tryCatch({
      #   str2expression(value)
      # },
      # error = function(e) {
      #   return(as.character(value))
      # })

      return(value)
    },

    as.source = function(x = NULL, session = NULL, value = NULL) {
      value <- input.handlers$expression$as.value(x, session, value)

      # if (class(value) == "expression")
      #   value <- as.character(value) %>% print
      # else # is an invalid expression
      #   value <- paste0("stop(\"Error: invalid expression found in input '", x$name, "'.\")")

      return(value)
    },

    observer = function(x, session) {
      shinyjs::removeClass(x$id, "invalid-expression")

      value <- input.handlers$expression$as.value(x, session)

      valid <- tryCatch({
        str2expression(value)

        return(TRUE)
      },
      error = function(e) {
        return(FALSE)
      })

      #if (!(class(value) == "expression"))
      if (!valid)
        shinyjs::addClass(x$id, "invalid-expression")
    }
  ),

  #
  # choices
  #
  choices = inputHandler(

    default.value = list(),
    params.list = list(
      quote = list(label = "Quote Values", type = "logical", default = TRUE),
      choices = list(label = "Choices", type = "list", default = ""),
      multiple = list(label = "Allow Multiple Values", type = "logical", default = FALSE),
      custom = list(label = "Allow Custom Value(s)", type = "logical", default = FALSE)
    ),

    createUI = function(x, ns = identity) {
      value <- if (!is.null(x$value)) x$value else x$default

      slatesSelectInput(ns(x$id), label = x$name, value = value,
                        choices = x$choices, multiple = x$multiple,
                        custom = x$custom,
                        allow.null = x$allow.null,
                        wizards = x$wizards)
    },

    setVisible = function(x, session, visible, tag.only = FALSE, return.js = FALSE) {
      if (class(x) == "character")
        id <- x
      else
        id <- x$id

      action <- if (visible) 'removeClass("d-none")' else 'addClass("d-none")'

      if (tag.only)
        selector <- paste0('$("#', session$ns(id), '").parent()')
      else
        selector <- paste0('$("#', session$ns(id), '").parent().parent()')

      js <- paste0(selector, ".", action, ";")

      if (return.js)
        return(js)
      else
        shinyjs::runjs(js)
    },

    updateUI = function(x, session) {
      if (!(x$custom == TRUE))
        updateSelectInput(session, x$id, selected = as.character(x$value))
      else
        updateSelectizeInput(session, x$id, selected = as.character(x$value))
    },

    as.value = function(x, session = NULL, value = NULL) {
      if (is.null(value))
        value <- input.handlers$expression$get.input(x, session)[[1]]

      value <- as.character(value)

      return(value)
    },

    as.source = function(x = NULL, session = NULL, value = NULL) {
      value <- input.handlers$choices$as.value(x, session, value)

      if (x$quote == TRUE)
        value %<>% paste0('"', ., '"')

      if (length(value) > 1)
        value %<>% paste(collapse = ", ") %>% paste0("c(", ., ")")

      return (value)
    }
  ),

  #
  # numeric2
  #
  numeric2 = inputHandler(

    default.value = c(0, 0),

    params.list = list(
    ),

    createUI = function(x, ns = identity) {
      value <- if (!is.null(x$value)) x$value else x$default

      slatesNumeric2Input(ns(x$id), label = x$name, value = value,
                          allow.null = x$allow.null,
                          wizards = x$wizards)
    },

    updateUI = function(x, session) {
      map(1:2, ~updateNumericInput(
        session,
        inputId = paste0(x$id, "_", .),
        value = as.character(x$value[ . ])))
    },

    get.input = function(x, session) {
      map(1:2, ~session$input[[ paste0(x$id, "_", .) ]])
    },

    as.value = function(x, session = NULL, value = NULL) {
      if (is.null(value))
        value <- input.handlers$numeric2$get.input(x, session)

      if (any(map_lgl(value, is.null)))
        return(NULL)

      value <- as.numeric(value)

      if (length(value) < 2)
        value <- c(value, rep(NA, 2 - length(value)))
      else if (length(value) > 2)
        stop("numeric2 input must have length 2.")

      return(value)
    },

    as.source = function(x = NULL, session = NULL, value = NULL) {
      if (is.null(value))
        value <- input.handlers$numeric2$as.value(x, session, value)

      value %>% paste(collapse = ", ") %>% paste0("c(", ., ")")
    }
  ),

  #
  # numeric4
  #
  numeric4 = inputHandler(


    default.value = c(0, 0, 0, 0),

    params.list = list(
    ),

    createUI = function(x, ns = identity) {
      value <- if (!is.null(x$value)) x$value else x$default

      slatesNumeric4Input(ns(x$id), label = x$name, value = value,
                          allow.null = x$allow.null,
                          wizards = x$wizards)
    },

    updateUI = function(x, session) {
      map(1:4, ~updateNumericInput(
        session,
        inputId = paste0(x$id, "_", .),
        value = as.character(x$value[ . ])))
    },

    get.input = function(x, session) {
      map(1:4, ~session$input[[ paste0(x$id, "_", .) ]])
    },

    as.value = function(x, session = NULL, value = NULL) {
      if (is.null(value))
        value <- input.handlers$numeric4$get.input(x, session)

      if (any(map_lgl(value, is.null)))
        return(NULL)

      value <- as.numeric(value)

      if (length(value) < 4)
        value <- c(value, rep(NA, 4 - length(value)))
      else if (length(value) > 4)
        stop("numeric4 input must have length 4.")

      return(value)
    },

    as.source = function(x = NULL, session = NULL, value = NULL) {
      if (is.null(value))
        value <- input.handlers$numeric4$as.value(x, session, value)

      value %>% paste(collapse = ", ") %>% paste0("c(", ., ")")
    }
  ),

  #
  # multi
  #
  multi = inputHandler(
    default.value = "character",

    params.list = list(
      inputs = list(label = "Parameters for Each Type", type = "list",
                    default = list("character" = list(), "expression" = list()))
    ),

    initInput = function(x) {
      # make multi-inputs
      x$inputs %<>%
        imap(~list_modify(.x,
                          input.type = .y,
                          name = paste0(x$name, "-", .y))) %>%
        map(~do.call(slateInput, .))

      if (class(x$default) != "character")
        stop("Default must be a single character string indicating the selected input type.")

      x$default <- list(selected = x$default,
                        values = map(x$inputs, "default"))

      return(x)
    },

    createUI = function(x, ns = identity) {
      value <- if (!is.null(x$value)) x$value else x$default

      makeSlatesMultiInput(
        ns(x$id), label = x$name,
        value = value$selected,
        inputs = x$inputs,
        allow.null = x$allow.null
      )
    },

    updateUI = function(x, session) {

    },

    get.input = function(x, session) {
      selected <- session$input[[ x$id ]]

      values <- x$inputs %>%
        map(~getHandler(.)$get.input(., session))

      # if (!selected %in% names(x$inputs) ||
      #     any(map_lgl(values, is.null))) {
      #   return(x$default)
      # }

      list(selected = selected, values = values)
    },

    as.value = function(x, session = NULL, value = NULL) {
      if (is.null(value))
        value <- input.handlers$multi$get.input(x, session)

      if (length(value) != 2 || class(value) != "list")
        stop("Value must be a list of length 2.")

      if (is.null(value$selected) || value$selected == "NULL")
        return(NULL)

      value$values %<>%
        imap(~input.handlers[[ .y ]]$as.value(x = x$inputs[[ .y ]], value = .x))

      return(value)
    },

    as.source = function(x = NULL, session = NULL, value = NULL) {
      if (is.null(value))
        value <- input.handlers$multi$as.value(x, session, value)

      if (is.null(value))
        return("NULL")

      source <- input.handlers[[ value$selected ]]$as.source(
        x = x$inputs[[ value$selected ]],
        value = value$values[[ value$selected ]]
      )

      return(source)
    },

    observer = function(x, session) {
      req(selected <- session$input[[ x$id ]])

      # selected input id
      sel.name <- paste0(x$name, "-", selected)

      js <- map_chr(x$inputs, ~{
        getHandler(.)$setVisible(., session, visible = (selected != "NULL" && .$name == sel.name),
                                  tag.only = TRUE, return.js = TRUE)
      }) %>% paste(collapse = "\n")

      shinyjs::runjs(js)
    }
  )

)



dataset.handlers <- list(
  file = inputHandler(
    default.value = "",
    as.value = function(s, session = NULL, value = NULL) {
      if (!is.null(s$data)) {
        gsub("\\\\", "/", s$data$datapath)
      } else {
        NULL
      }
    }
  ),
  standalone = inputHandler(
    default.value = "",
    as.value = function(s, session = NULL, value = NULL) { NULL }
  )
)


getLayoutFun <- function(layout) {
  switch(layout,
         "flow-2" = function(...) tags$div(class = "slates-flow slates-flow-2", ...),
         "flow-3" = function(...) tags$div(class = "slates-flow slates-flow-3", ...),
         "flow-4" = function(...) tags$div(class = "slates-flow slates-flow-4", ...),
         "vertical" = shiny::verticalLayout)
}


createInputUI <- function(input, ns = identity) {
  elem <- getHandler(input)$createUI(input, ns)

  if (!is.null(input$description) && input$description != "") {
    tooltip <-paste0("<b>", input$input.type, "</b>",
                     "<br>", input$description) %>%
      shinyBS::bsTooltip(ns(input$id), ., placement = "top")

    return(tagList(elem, tooltip))
  } else {
    return(elem)
  }
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
    map(createGroupUI, inputs = inputs, ns = ns) %>%
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

  # build pages (except default page)
  pages %<>%
    #keep(map(., "name") != "default") %>%
    map(~list_modify(., ui = createPageUI(., groups = groups, inputs = inputs, ns = ns)))

  # build the container
  if (length(pages) == 0) {
    ui <- tagList()
  } else
    if (inputs.style == "tabset") {
    # simple tabset
    ui <- pages %>%
      map(~tabPanel(title = .$title, .$ui)) %>%
      unname %>%
      do.call(tabsetPanel, .)

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
            x$title
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
    ui <- map(pages, ~{
      tags$div(
        tags$h5(
          class = "slates-page-title-flowing",
          .x$title
        ),
        .x$ui
      )
    }) %>%
      do.call(verticalLayout, .)
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









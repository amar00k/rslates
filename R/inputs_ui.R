


slatesInputLabel <- function(label, input.id, make.id = FALSE) {
  tag <- tags$label(
    class = "control-label",
    `for` = input.id,
    label
  )

  if (make.id == TRUE) {
    tag$id <- paste0(input.id, "-label")
  }

  return(tag)
}


slatesWizardButton <- function(id, wizards) {
  # TODO: this is dirty af, find where to pass the wizard list along
  wizards <- lapply(wizards, function(x) wizard.list[[ x ]])

  wizard.list <- lapply(seq_along(wizards), function(i) {
    wiz <- wizards[[ i ]]
    actionLink(paste0(id, "-", i), label = wiz$name)
  })

  button.params <- list(
    inputId = paste0(id, "-btn"),
    label = "",
    size = "xs",
    icon = icon("hat-wizard"),
    circle = FALSE,
    inline = TRUE
  )

  btn <- do.call(shinyWidgets::dropdownButton,
                 append(button.params, unname(wizard.list)))

  addTagAttribs(tag = btn, class = "wizard-btn")
}


makeSlatesInput <- function(tagFun, id, label, ..., wizards = NULL) {
  if (!is.null(wizards) && length(wizards) > 0) {
    wizard.button <-
      slatesWizardButton(paste0(id, "-wizard"), wizards) %>%
      tagAppendAttributes(style = "float: right;")
  } else {
    wizard.button <- tagList()
  }

  input <- div(
    class = "form-group shiny-input-container",
    slatesInputLabel(label, id),
    wizard.button,
    tagFun(id, ...)
  )

  return(input)
}

slatesTextInputTag <- function(id, value = "", ...) {
  textInput(id, label = "", value = value, ...)$children[[2]]
}

slatesExpressionInputTag <- function(id, value = "", ...) {
  textInput(id, label = "", value = value, ...)$children[[2]] %>%
    tagAppendAttributes(class = "expression-input")
}

slatesNumericInputTag <- function(id, value = "", ...) {
  numericInput(id, label = "", value = value, ...)$children[[2]] %>%
    tagAppendAttributes(class = "numeric-input")
}

slatesChoicesInputTag <- function(id, ...) {
  selectInput(id, label = "", ...)$children[[2]]
}

slatesNumeric2InputTag <- function(id, value = c(0, 0)) {
  tags$div(
    class = "numeric-input",
    style = "position: relative;",
    # this tag mimicks a numeric input
    tags$input(type = "text", class = "form-control", style = "position: absolute;"),
    # this tag masks the previous one, preventing clicking
    # TODO: input tag is still able to be selected by tabbing
    div(style = "position: absolute; width: 100%; height: 100%;"),
    tags$div(
      #class = "slates-flow-4",
      style = "display: flex; flex-wrap: nowrap; align-items: baseline; padding: 3px 6px;",
      tags$input(id = paste0(id, "_1"), type = "number",
                 class = "form-control numeric4-input", value = value[1]),
      tags$span(",", style = "z-index: 1;"),
      tags$input(id = paste0(id, "_2"), type = "number",
                 class = "form-control numeric4-input", value = value[2]),
    )
  )
}

slatesNumeric4InputTag <- function(id, value = c(0, 0, 0, 0)) {
  tags$div(
    class = "numeric-input",
    style = "position: relative;",
    # this tag mimicks a numeric input
    tags$input(type = "text", class = "form-control", style = "position: absolute;"),
    # this tag masks the previous one, preventing clicking
    # TODO: input tag is still able to be selected by tabbing
    div(style = "position: absolute; width: 100%; height: 100%;"),
    tags$div(
      #class = "slates-flow-4",
      style = "display: flex; flex-wrap: nowrap; align-items: baseline; padding: 3px 6px;",
      tags$input(id = paste0(id, "_1"), type = "number", class = "form-control numeric4-input", value = value[1]),
      tags$span(",", style = "z-index: 1;"),
      tags$input(id = paste0(id, "_2"), type = "number", class = "form-control numeric4-input", value = value[2]),
      tags$span(",", style = "z-index: 1;"),
      tags$input(id = paste0(id, "_3"), type = "number", class = "form-control numeric4-input", value = value[3]),
      tags$span(",", style = "z-index: 1;"),
      tags$input(id = paste0(id, "_4"), type = "number", class = "form-control numeric4-input", value = value[4])
    )
  )
}





#
# Shortcut functions
#

slatesTextInput <- function(id, label, value, ...) {
  makeSlatesInput(slatesTextInputTag, id, label, value, ...)
}

slatesExpressionInput <- function(id, label, value, ...) {
  makeSlatesInput(slatesExpressionInputTag, id, label, value, ...)
}

slatesNumericInput <- function(id, label, value, ...) {
  makeSlatesInput(slatesNumericInputTag, id, label, value, ...)
}

slatesSelectInput <- function(id, label, choices, ...) {
  makeSlatesInput(slatesChoicesInputTag, id, label, choices = choices, ...)
}

slatesNumeric2Input <- function(id, label, value, ...) {
  makeSlatesInput(slatesNumeric2InputTag, id, label, value, ...)
}

slatesNumeric4Input <- function(id, label, value, ...) {
  makeSlatesInput(slatesNumeric4InputTag, id, label, value, ...)
}










tagRemoveClass <- function(tag, class) {
  tag$attribs$class %<>%
    strsplit(split = " ") %>%
    pluck(1) %>%
    keep(~. != class) %>%
    paste(collapse = " ")

  return(tag)
}








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







slatesTextInputTag <- function(id, value = "", visible = TRUE, size = "m", ...) {
  textInput(id, label = "", value = value)$children[[2]] %>%
    tagAppendAttributes(class = paste0("form-control-", size)) %>%
    tagAppendAttributes(class = if (visible) "" else "d-none")
}

slatesExpressionInputTag <- function(id, value = "", visible = TRUE, size = "m", ...) {
  textInput(id, label = "", value = value)$children[[2]] %>%
    tagAppendAttributes(class = "expression-input") %>%
    tagAppendAttributes(class = paste0("form-control-", size)) %>%
    tagAppendAttributes(class = if (visible) "" else "d-none")
}

slatesNumericInputTag <- function(id, value = "", visible = TRUE, size = "m", ...) {
  numericInput(id, label = "", value = value)$children[[2]] %>%
    tagAppendAttributes(class = "numeric-input") %>%
    tagAppendAttributes(class = paste0("form-control-", size)) %>%
    tagAppendAttributes(class = if (visible) "" else "d-none")
}

slatesChoicesInputTag <- function(id, choices, value = NULL, size = "m",
                                  multiple = FALSE, custom = FALSE, visible = TRUE, ...) {
  if (custom == TRUE) {
    options <- list(
      delimiter = '',
      create = "function(input) { return { value: input, text: input } }"
    )
  } else {
    options <- NULL
  }

  tag <- selectizeInput(
    id, label = "",
    choices = choices,
    selected = value,
    multiple = multiple,
    options = options)$children[[2]] %>%
    tagAppendAttributes(class = if (visible) "" else "d-none")

  tag$children[[1]] %<>% tagAppendAttributes(class = paste0("form-control-", size))

  return(tag)
}

slatesNumeric2InputTag <- function(id, value = c(0, 0), visible = TRUE, size = "m", ...) {
  tags$div(
    id = id,
    style = "position: relative;",
    # this tag mimicks a numeric input
    tags$input(type = "text",
               class = paste0("form-control numeric-input form-control-", size),
               style = "position: absolute;"),
    # this tag masks the previous one, preventing clicking
    # TODO: input tag is still able to be selected by tabbing
    tags$div(style = "position: absolute; width: 100%; height: 100%;"),
    tags$div(
      #class = "slates-flow-4",
      style = "display: flex; flex-wrap: nowrap; align-items: baseline; padding: 3px 6px;",
      tags$input(id = paste0(id, "_1"), type = "number",
                 class = paste0("form-control numeric2-input form-control-", size), value = value[1]),
      tags$span(",", style = "z-index: 1;"),
      tags$input(id = paste0(id, "_2"), type = "number",
                 class = paste0("form-control numeric2-input form-control-", size), value = value[2]),
    )
  ) %>%
    tagAppendAttributes(class = if (visible) "" else "d-none")
}

slatesNumeric4InputTag <- function(id, value = c(0, 0, 0, 0), visible = TRUE, size = "m", ...) {
  tags$div(
    id = id,
    style = "position: relative;",
    # this tag mimicks a numeric input
    tags$input(type = "text",
               class = paste0("form-control numeric-input form-control-", size),
               style = "position: absolute;"),
    # this tag masks the previous one, preventing clicking
    # TODO: input tag is still able to be selected by tabbing
    div(style = "position: absolute; width: 100%; height: 100%;"),
    tags$div(
      #class = "slates-flow-4",
      style = "display: flex; flex-wrap: nowrap; align-items: baseline; padding: 3px 6px;",
      tags$input(id = paste0(id, "_1"), type = "number",
                 class = paste0("form-control numeric4-input form-control-", size), value = value[1]),
      tags$span(",", style = "z-index: 1;"),
      tags$input(id = paste0(id, "_2"), type = "number",
                 class = paste0("form-control numeric4-input form-control-", size), value = value[2]),
      tags$span(",", style = "z-index: 1;"),
      tags$input(id = paste0(id, "_3"), type = "number",
                 class = paste0("form-control numeric4-input form-control-", size), value = value[3]),
      tags$span(",", style = "z-index: 1;"),
      tags$input(id = paste0(id, "_4"), type = "number",
                 class = paste0("form-control numeric4-input form-control-", size), value = value[4]),
    )
  ) %>%
    tagAppendAttributes(class = if (visible) "" else "d-none")
}



shortFromType <- list(
  "character" = "CHR",
  "expression" = "EXP",
  "logical" = "LGL",
  "choices" = "SEL",
  "numeric" = "NUM",
  "numeric2" = "NUM2",
  "numeric4" = "NUM4"
)

tagFromType <- list(
  "character" = slatesTextInputTag,
  "expression" = slatesExpressionInputTag,
  "choices" = slatesChoicesInputTag,
  "numeric" = slatesNumericInputTag,
  "numeric2" = slatesNumeric2InputTag,
  "numeric4" = slatesNumeric4InputTag
)


slatesMultiInputRadio <- function(id, types, allow.null = FALSE) {
  button.labels <- set_names(types, shortFromType[ types ])

  if (allow.null)
    button.labels <- c(button.labels, "NULL"="NULL")

  shinyWidgets::radioGroupButtons(
    inputId = id,
    choices = button.labels,
    status = "primary",
    size = "sm"
  ) %>%
    tagAppendAttributes(class = "slates-multi-button", style = "float: right;")
}


makeSlatesInput <- function(type, id, label, ..., wizards = NULL, allow.null = FALSE) {
  if (!is.null(wizards) && length(wizards) > 0) {
    wizard.button <-
      slatesWizardButton(paste0(id, "-wizard"), wizards) %>%
      tagAppendAttributes(style = "float: right;")
  } else {
    wizard.button <- tagList()
  }

  if (allow.null == TRUE) {
    # null.button <- shinyWidgets::checkboxGroupButtons(
    #   inputId = paste0(id, "-null"),
    #   choices = list("NULL"="NULL"),
    #   selected = NULL,
    #   size = "sm",
    #   status = "primary"
    # ) %>%
    #   tagAppendAttributes(style = "display: inline-block;") %>%
    #   tagRemoveClass("form-group")

    multi.radio <- slatesMultiInputRadio(paste0(id, "-chooser"), type, allow.null = TRUE)
  } else {
    multi.radio <- tagList()
  }

  tagFun <- tagFromType[[ type ]]

  input <- div(
    class = "form-group shiny-input-container",
    slatesInputLabel(label, id),
    wizard.button,
    # multi switcher
    multi.radio,
    tagFun(id, ...)
  )

  return(input)
}



makeSlatesMultiInput <- function(id, label,
                             inputs,
                             allow.null = FALSE,
                             wizards = NULL) {
  if (length(inputs) < 1)
    stop("Need at least 1 type.")

  input.tags <- map(inputs, ~{
    fun <- tagFromType[[ .$input.type ]]
    tag.id <- paste0(id, "-", .$input.type)

    .$id <- tag.id
    .$value <- .$default

    # if first
    if (.$input.type == inputs[[1]]$input.type)
      .$visible <- TRUE
    else
      .$visible <- FALSE

    do.call(fun, .)
  })

  multi.radio <- slatesMultiInputRadio(id, map_chr(inputs, "input.type"), allow.null)

  div(
    class = "form-group shiny-input-container",
    slatesInputLabel(label, id),
    multi.radio,
    #wizard.button,
    input.tags
  )

}


#
# Shortcut functions
#

slatesTextInput <- function(id, label, value, ...) {
  makeSlatesInput("character", id, label, value, ...)
}

slatesExpressionInput <- function(id, label, value, ...) {
  makeSlatesInput("expression", id, label, value, ...)
}

slatesNumericInput <- function(id, label, value, ...) {
  makeSlatesInput("numeric", id, label, value, ...)
}

slatesSelectInput <- function(id, label, choices, value, ...) {
  makeSlatesInput("choices", id, label, choices = choices, value = value, ...)
}

slatesNumeric2Input <- function(id, label, value, ...) {
  makeSlatesInput("numeric2", id, label, value, ...)
}

slatesNumeric4Input <- function(id, label, value, ...) {
  makeSlatesInput("numeric4", id, label, value, ...)
}






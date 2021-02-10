


#
# Utilities
#

addTagAttribs <- function(tag, class="", style="") {
  tag$attribs$class <- paste(tag$attribs$class, class)
  tag$attribs$style <- paste(tag$attribs$style, style)
  return(tag)
}

#
# Inputs with wizard
#

createWizardButton <- function(id, wizards) {
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


slatesExpressionInput <- function(id, ..., wizards = NULL) {
  input <- textInput(id, ...)
  input <- addTagAttribs(input, class = "expression-input")

  if (is.null(wizards) || length(wizards) == 0) {
    return(input)
  }

  wizard.button <- createWizardButton(paste0(id, "-wizard"), wizards)
  wizard.button <- addTagAttribs(tag = wizard.button, style = "float: right;")

  input$children <- list(input$children[[1]],
                         wizard.button,
                         input$children[[2]])

  return(input)
}


slatesTextInput <- function(id, ..., wizards = NULL) {
  input <- textInput(id, ...)

  if (is.null(wizards) || length(wizards) == 0) {
    return(input)
  }

  wizard.button <- createWizardButton(paste0(id, "-wizard"), wizards)
  wizard.button <- addTagAttribs(tag = wizard.button, style = "float: right;")

  input$children <- list(input$children[[1]],
                         wizard.button,
                         input$children[[2]])

  return(input)
}


slatesSelectInput <- function(id, ..., wizards = NULL) {
  input <- selectInput(id, ...)

  if (is.null(wizards) || length(wizards) == 0) {
    return(input)
  }

  wizard.button <- createWizardButton(paste0(id, "-wizard"), wizards)
  wizard.button <- addTagAttribs(tag = wizard.button, style = "float: right;")

  input$children <- list(input$children[[1]],
                         wizard.button,
                         input$children[[2]])

  return(input)
}


slatesNumericInput <- function(id, ..., wizards = NULL) {
  input <- numericInput(id, ...)

  if (is.null(wizards) || length(wizards) == 0) {
    return(input)
  }

  wizard.button <- createWizardButton(paste0(id, "-wizard"), wizards)
  wizard.button <- addTagAttribs(tag = wizard.button, style = "float: right;")

  input$children <- list(input$children[[1]],
                         wizard.button,
                         input$children[[2]])

  return(input)
}





#' File Upload Control
#'
#' @description Fixes an issue where the page jumps to top when clicking the Browse... button.
#'
#' @param id
#' @param label
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
fileInput <- function(id, label, ...) {
  tag <- shiny::fileInput(id, label = label, ...)
  tag$children[[2]]$children[[1]]$children[[1]]$children[[2]]$attribs$style <- "display: none"
  return(tag)
}







#'
#'
#'
#'
#'
#' slatesCompactWellPanel <- function(...) {
#'   panel <- wellPanel(...)
#'   panel$attribs$class <- paste(panel$attribs$class, "slates-compact")
#'
#'   return(panel)
#' }
#'
#'
#' slatesCompactTabsetPanel <- function(...) {
#'   panel <- tabsetPanel(...)
#'   panel$attribs$class <- paste(panel$attribs$class, "slates-compact")
#'
#'   return(panel)
#' }
#'
#'
#' #' Make a UI element react to clicks.
#' #'
#' #' @description Encloses an arbitrary UI element in a div with the *action-button* class.
#' #'
#' #' @param clickId Id of the click handler.
#' #' @param el The \code{shiny.tag} element to modify.
#' #'
#' #' @return The modified \code{shiny.tag} element.
#' #' @export
#' #'
#' #' @examples
#' actionElement <- function(clickId, el) {
#'   a(id = clickId, class = "action-button", style = "color: inherit;", href="#", el)
#' }
#'
#'
#'
#'
#' bs4Accordion <- function(id, ...) {
#'   tags$div(
#'     id = id,
#'     class = "collapse show"
#'   )
#' }
#'
#'
#'
#'
#'
#' #' Create a bsCollapsePanel with toggle icon.
#' #'
#' #' @param title
#' #' @param ...
#' #' @param value
#' #' @param style
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' slatesCollapsePanel <- function (title, ..., value = title, style = NULL) {
#'   content <- list(...)
#'   id <- make_uid("cpanel")
#'
#'   if (is.null(value)) {
#'     value = title
#'   }
#'
#'   if (is.null(style)) {
#'     style = "default"
#'   }
#'
#'   bsTag <- shiny::tags$div(
#'     class = paste0("panel panel-", style),
#'     value = value,
#'     shiny::tags$div(
#'       class = "panel-heading",
#'       role = "tab",
#'       id = paste0("heading_", id),
#'       shiny::tags$h4(
#'         class = "panel-title",
#'         shiny::tags$a(
#'           `data-toggle` = "collapse",
#'           href = paste0("#", id),
#'           title,
#'           shiny::tags$li(
#'             class="more-less glyphicon glyphicon-menu-down"
#'           )
#'         )
#'       )
#'     ),
#'     shiny::tags$div(
#'       id = id,
#'       class = "panel-collapse collapse",
#'       role = "tabpanel",
#'       `aria-expanded` = "true",
#'       shiny::tags$div(
#'         class = "panel-body",
#'         content
#'       )
#'     )
#'   )
#'
#'   # TODO: Add dependencies...
#'   #htmltools::attachDependencies(bsTag, shinyBSDep)
#' }







#
# Utilities
#

addTagAttribs <- function(tag, class="", style="") {
  tag$attribs$class <- paste(tag$attribs$class, class)
  tag$attribs$style <- paste(tag$attribs$style, style)
  return(tag)
}

# removeTagClass <- function(tag, class) {
#   tag.classes <- trimws(strsplit(tag$attribs$class, split = " ")[[1]])
#   tag$attribs$class <- paste(tag.classes[ tag.classes != class ], collapse = " ")
#
#   return(tag)
# }


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
  input <- numericInput(id, ...) %>%
    addTagAttribs(class = "numeric-input")

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


slatesNumeric2Input <- function(id, label, value = c(0,0), ..., wizards = NULL) {
  input <- tags$div(
    class = "form-group shiny-input-container",
    tags$label(
      class = "control-label",
      id = paste0(id, "-label"),
      `for` = id,
      label
    ),
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
  )

  return(input)
}


slatesNumeric4Input <- function(id, label, value = c(0,0,0,0), ..., wizards = NULL) {
  input <- tags$div(
    class = "form-group shiny-input-container",
    tags$label(
      class = "control-label",
      id = paste0(id, "-label"),
      `for` = id,
      label
    ),
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
  )

  return(input)
}


slatesSwitchInput <- function(id, label, value = FALSE, on.off.labels = c("True", "False"), wizards = NULL) {
  input <- tags$div(
    class = "form-group shiny-input-container",
    tags$label(
      class = "control-label",
      id = paste0(id, "-label"),
      `for` = id,
      label
    ),
    shinyWidgets::switchInput(
      id, label = "",
      value = value,
      onLabel = on.off.labels[1], offLabel = on.off.labels[2],
      width = "auto"
    )
  )
}



#' Shiny File Upload Control
#'
#' @description Fixes an issue where the page jumps to top when clicking the "Browse..." button.
#'
#' @export
slatesFileInput <- function(id, label,
                      multiple = FALSE,
                      accept = NULL,
                      width = NULL,
                      buttonLabel = "Browse...",
                      placeholder = "No file selected",
                      class = "") {
  tag <- shiny::fileInput(id, label = label,
                          multiple = multiple,
                          accept = accept,
                          width = width,
                          buttonLabel = buttonLabel,
                          placeholder = placeholder)

  tag <- addTagAttribs(tag, class = class)
  tag$children[[2]]$children[[1]]$children[[1]]$children[[2]]$attribs$style <- "display: none"

  return(tag)
}







slatesNavbarPage <- function(title, tabs,
                             header = tagList(),
                             footer = tagList(),
                             theme = getOption("rslates.default.theme"),
                             ace.theme = getOption("rslates.default.ace.theme"),
                             session.info = TRUE,
                             ns = identity) {

  settings <- shinyWidgets::dropdownButton(
    inputId = paste(ns("app_settings")),
    label = "",
    icon = icon("cog"),
    circle = FALSE,
    inline = TRUE,
    right = TRUE,
    selectInput("select_theme",
                label = "Theme",
                choices = getOption("rslates.themes"),
                selected = theme),
    selectInput("select_ace_theme",
                label = "Ace Editor Theme",
                choices = shinyAce::getAceThemes(),
                selected = ace.theme)
  )

  tabset <- do.call(tabsetPanel, append(list(id = ns("header_tabset")), tabs))

  links <- tabset$children[[1]]
  classes <- trimws(strsplit(links$attribs$class, split = " ")[[1]])
  links$attribs$class <- paste(
    classes[ !classes %in% c("shiny-tab-input", "shiny-bound-input") ],
    collapse = " "
  )
  links$attribs$class <- gsub("nav-tabs", "navbar-nav", links$attribs$class)

  navpanel <- tags$nav(
    id = "title-navbar",
    class = "navbar navbar-dark navbar-static-top bg-title mb-0",
    role = "navigation",
    tags$div(
      class = "container-fluid d-flex align-items-baseline mt-2",
      tags$div(
        class = "navbar-header",
        tags$a(
          class = "navbar-brand d-flex align-items-baseline",
          href = "#",
          tags$img(src = "slates-logo-white-24dpi.png",
                   style="padding-right: .5em"),
          title
        ),
      ),
      # div(class = "text-muted", "|"),
      # if (length(tabs) > 1) links else tagList(),
      links,
      div(
        class = "align-self-center",
        settings
      )
    )
  )

  if (session.info == TRUE) {
    footer <- tags$div(
      footer,
      class = "container",
      tags$h3("Session Info"),
      tags$div(HTML(paste(captureSessionInfo(320), collapse="<br>")))
    )
  }

  bootstrapPage(
    shinyjs::useShinyjs(),
    shiny::bootstrapLib(),
    shiny::tags$link(rel = "stylesheet", type = "text/css", href = "slates.css"),
    thematic::thematic_shiny(),
    title = title,
    theme = loadTheme(theme),
    navpanel,
    header,
    tabset$children[[2]],
    tags$br(),
    footer,
    tags$br(),
    tags$div(
      class = "bg-title p-2",
      tags$div(class = "container",
      tags$span("Copyright (c) 2021 Daniel Neves"),
      tags$span(class = "float-right", paste("rlates", packageVersion("rslates")))
      )
    )
  )

}


slatesNavbarPageHandler <- function(session) {



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





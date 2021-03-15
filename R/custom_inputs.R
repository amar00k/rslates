


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


#
# Multi input
#
MULTI.INPUT.TYPES <- data.frame(
  type = c("character", "expression", "logical", "choices", "numeric", "numeric2", "numeric4"),
  short = c("CHR", "EXP", "LOG", "SEL", "NUM", "N2", "N4")
  #input.type = c("text", "text", "text", "text", "")
)


slatesMultiInput <- function(id, label, value = "", types = c("character"), wizards = NULL) {

  labels <- MULTI.INPUT.TYPES$short[ match(types, MULTI.INPUT.TYPES$type) ]

  buttons <- shinyWidgets::radioGroupButtons(
    inputId = paste0(id, "-radio"),
    choices = labels,
    status = "primary",
    size = "sm"
  ) %>%
    tagAppendAttributes(class = "slates-multi-button", style = "float: right;")

  input <- tags$div(
    class = "form-group shiny-input-container",
    tags$div(
      tags$label(
        class = "control-label",
        id = paste0(id, "-label"),
        `for` = id,
        label
      ),
      buttons
    ),
    tags$input(
      id = id,
      type = "text",
      class = "form-control",
      value = value[1]
    )
  )
}





# slatesMultiInput <- function(id, label, value = "", types = c("character"), wizards = NULL) {
#   labels <- multi.labels$short[ match(types, multi.labels$type) ]
#
#   buttons <- shinyWidgets::radioGroupButtons(
#     inputId = paste0(id, "-type"),
#     choices = labels,
#     status = "primary",
#     size = "sm"
#   ) %>%
#     tagAppendAttributes(class = "slates-multi-button", style = "float: right;")
#
#   tagList(
#     shiny::singleton(
#       shiny::tags$head(
#         shiny::tags$script(src = "js/multi-input-binding.js")
#       )
#     ),
#     tags$div(
#       class = "form-group shiny-input-container",
#       tags$label(
#         class = "control-label", `for` = id, label
#       ),
#       buttons,
#       tags$input(
#         id = id,
#         type = "text",
#         class = "form-control",
#         value = value[1]
#       )
#     )
#   )
# }





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
    icon = icon("cogs"),
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
          tags$img(src = "slates-logo-white-small.png",
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
    #htmltools::findDependencies(shinyBS::bsButton("test", "test")),
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
      class = "bg-title px-2 py-3",
      tags$div(class = "container",
      tags$span("Copyright (c) 2021 Daniel Neves"),
      tags$span(class = "float-right", paste("rslates", packageVersion("rslates")))
      )
    ),
    # includes fixes to work under Bootstrap 4
    tags$script(src = "rslates_shinyBS.js")
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





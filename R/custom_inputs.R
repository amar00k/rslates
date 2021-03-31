


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







#' Shiny File Upload Control
#'
#' @description Fixes an issue where the page jumps to top when clicking the "Browse..." button.
#'
#' @export
slatesFileInput <- function(id, label, ..., class = "") {
  tag <- shiny::fileInput(id, label = label, ...)

  tag <- tagAppendAttributes(tag, class = class)
  tag$children[[2]]$children[[1]]$children[[1]]$children[[2]]$attribs$style <- "display: none"

  return(tag)
}







slatesNavbarPage <- function(title, tabs,
                             header = tagList(),
                             footer = tagList(),
                             theme = getOption("rslates.themes")$default,
                             ace.theme = getOption("rslates.themes-ace")$default,
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
                choices = getOption("rslates.themes.list"),
                selected = theme),
    selectInput("select_ace_theme",
                label = "Ace Editor Theme",
                choices = getOption("rslates.themes.ace.list"),
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






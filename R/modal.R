


modal <- function(id, session, results.fun, body.ui, title = NULL, observers=list()) {
  ID <- function(x) paste0(id, "_", x)
  ns <- session$ns
  input <- session$input

  .callback <- NULL

  observeEvent(input[[ ID("btn_ok") ]], {
    removeModal(session)

    if (!is.null(.callback)) {
      results <- results.fun()

      do.call(.callback, results)
    }
  })

  show <- function(callback, ...) {
    ns <- session$ns

    .callback <<- callback

    showModal(
      modalDialog(
        title = title,
        easyClose = TRUE,
        body.ui,
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns(ID("btn_ok")), "OK")
        ),
        ...
      ),
      session
    )

    # TODO: set focus on show
  }

  list(
    id = id,
    show = show
  )
}


new_input_modal <- function(id, session) {
  ID <- function(x) paste0(id, "_", x)
  ns <- session$ns
  input <- session$input

  body.ui <- tagList(
    textInput(ns(ID("name_input")), label = "Input Name", value = ""),
    selectInput(ns(ID("type_input")), label = "Input Type",
                selectize = TRUE,
                choices = c("Select input type"="", "numeric", "logical", "character", "expression", "choices"),
                selected = "")

  )

  results.fun <- function(session) {
    list(name = input[[ ID("name_input") ]],
         type = input[[ ID("type_input") ]])
  }

  accept.observer <- observe({
    name <- input[[ ID("name_input") ]]
    type <- input[[ ID("type_input") ]]

    shinyjs::disable(ID("btn_ok"))

    req(name, type)

    if (name != "" && type != "")
      shinyjs::enable(ID("btn_ok"))
  })

  modal(id, session,
        results.fun = results.fun,
        body.ui = body.ui,
        observers = list(accept.observer)
        )
}



#' Create a modal with a single text input
#'
#' @param id
#' @param session
#'
#' @return
#' @export
#'
#' @examples
create_text_input_modal <- function(id, session) {
  ID <- function(x) paste0(id, "_", x)

  ns <- session$ns
  input <- session$input

  .callback <- NULL

  observeEvent(input[[ ID("btn_ok") ]], {
    removeModal(session)

    if (!is.null(.callback)) {
      .callback(input[[ ID("text_input") ]])
    }
  })

  observeEvent(input[[ ID("text_input") ]], {
    txt <- input[[ ID("text_input") ]]

    if (is.null(txt) || nchar(txt) == 0)
      shinyjs::disable(ID("btn_ok"))
    else
      shinyjs::enable(ID("btn_ok"))
  })

  show <- function(query, callback, value = "", placeholder = "", title = NULL, ...) {
    ns <- session$ns

    .callback <<- callback

    showModal(
      modalDialog(
        title = title,
        easyClose = TRUE,
        textInput(ns(ID("text_input")), label = query, value = value, placeholder = placeholder),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns(ID("btn_ok")), "OK")
        ),
        ...
      ),
      session
    )

    # TODO: set focus on show
  }

  list(
    id = id,
    show = show
  )
}



create_select_input_modal <- function(id, session) {
  ID <- function(x) paste0(id, "_", x)

  ns <- session$ns
  input <- session$input

  .callback <- NULL

  observeEvent(input[[ ID("btn_ok") ]], {
    removeModal()

    if (!is.null(.callback)) {
      .callback(input[[ ID("select_input") ]])
    }
  })

  show <- function(query, choices, callback, selected = NULL) {
    ns <- session$ns

    .callback <<- callback

    showModal(
      modalDialog(
        selectInput(ns(ID("select_input")),
                    label = query,
                    choices = choices,
                    selected = selected),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns(ID("btn_ok")), "OK")
        )
      )
    )
  }

  list(
    id = id,
    show = show
  )
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
#'
#' @examples
#' if (interactive()) {
#'   shinyApp(
#'     ui = fluidPage(
#'       actionButton("show_modal", "Show Modal"),
#'       verbatimTextOutput("result")
#'     ),
#'     server = function(input, output, session) {
#'       # create the modal object
#'       modal <- create_new_project_modal("new_project", session)
#'
#'       # create a reactiveVal to store the result
#'       project <- reactiveVal(list())
#'
#'       # show modal when button is pressed
#'       observeEvent(input$show_modal, {
#'         modal$show(callback = function(title, authors) {
#'           project(list(title=title, authors=authors))
#'         })
#'       })
#'
#'       # show the contents of project
#'       output$result <- renderPrint(project())
#'     }
#'   )
#' }
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





#' Create a file import modal
#'
#' @description Creates a modal.
#'
#' @param id
#' @param input
#' @param output
#' @param session
#'
#' @return
#' @export
#'
#' @examples
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
          fileInput(ns(ID("file_input")), label = "File")
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







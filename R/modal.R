


#' Create a Modal Dialog
#'
#' @param id id of the modal dialog.
#' @param session server session.
#' @param submit.fun a function without arguments that returns the a list
#'   of results of the modal dialog when the user presses OK.
#' @param ui.fun a function taking any number of arguments that returns
#'   the ui of the body of the modal.
#' @param observers an optional list of observers to be destroyed when the
#'   modal is destroyed.
#'
#' @return a list with the following members. `id`: the id of the modal,
#'   `show`: a function with signature `function(callback, title = NULL, ...)`
#'   that brings up the modal dialog when called. The `callback` function is
#'   called by the modal dialog when the user presses OK. Each value returned
#'   by `submit.fun` is passed to this function as an argument. Extra `...`
#'   parameters are passed to the `ui.fun` function.
#'
#' @export
#'
#' @examples
slatesModal <- function(id, session,
                  submit.fun,
                  ui.fun,
                  observers=list(),
                  focus.on.show = NULL) {
  ID <- function(x) paste0(id, "_", x)
  ns <- session$ns
  input <- session$input

  .callback <- NULL

  observeEvent(input[[ ID("btn_ok") ]], {
    removeModal(session)

    if (!is.null(.callback)) {
      results <- submit.fun()

      do.call(.callback, results)
    }
  })

  show <- function(callback, title = NULL, ...) {
    ns <- session$ns

    .callback <<- callback

    md <- modalDialog(
      title = title,
      easyClose = TRUE,
      ui.fun(...),
      footer = tagList(
        modalButton("Cancel"),
        actionButton(ns(ID("btn_ok")), "OK")
      ),
      ...
    )
    # md$children[[1]]$children[[1]]$attribs$id <- "modal-content"
    # md$children[[1]]$children[[1]]$children[[2]]$attribs$id <- "modal-body"

    showModal(md, session)

    # i <- 0
    # if (!is.null(focus.on.show)) {
    #   o <- observe({
    #     #input[[ focus.on.show ]], {
    #     req(!is.null(input[[ focus.on.show ]]))
    #
    #     pprint("focus", focus.on.show, input[[ focus.on.show ]])
    #
    #     # shinyjs::js$focus("modal-content")
    #     # shinyjs::js$focus("modal-body")
    #     shinyjs::js$focus(focus.on.show)
    #
    #     i <<- i + 1
    #     print(i)
    #     if (i == 2) {
    #       o$destroy()
    #     } else {
    #       invalidateLater(200)
    #     }
    #
    #
    #     #o$destroy()
    #   })
    # }

    # TODO: set focus on show
  }

  list(
    id = id,
    show = show
  )
}


slatesSelectModal <- function(id, session) {
  ID <- function(x) paste0(id, "_", x)
  ns <- session$ns

  ui.fun <- function(label, choices) {
    selectInput(ns(ID("select_input")), label = label, choices = choices, selected = choices[1])
  }

  submit.fun <- function() {
    list(session$input[[ ID("select_input") ]])
  }

  slatesModal(id, session, ui.fun = ui.fun, submit.fun = submit.fun)
}


slatesTextModal <- function(id, session) {
  ID <- function(x) paste0(id, "_", x)
  ns <- session$ns

  ui.fun <- function(label, placeholder = NULL, value = "") {
    textInput(ns(ID("text_input")), label = label, value = value, placeholder = placeholder)
  }

  submit.fun <- function() {
    list(session$input[[ ID("text_input") ]])
  }

  slatesModal(id, session, ui.fun = ui.fun, submit.fun = submit.fun)
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







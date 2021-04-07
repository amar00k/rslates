


#
# Utilities
#


# make a reactable theme from the current bslib theme
autoReactableTheme <- function(bslib.theme = getCurrentTheme(), options = list()) {
  vars <- bslib::bs_get_variables(
    bslib.theme,
    varnames = c("fg", "bg", "primary", "secondary",
                 "success", "info", "warning", "danger")
  )

  options$color <- vars[[ "fg" ]]
  options$backgroundColor <- vars[[ "bg" ]]

  do.call(reactable::reactableTheme, options)
}



copyEnvironment <- function(envir) {
  list2env(as.list(envir))
}


#
# Outputs
#

outputHandler <- function(createUI = function(...) { tagList() },
                          createRenderer = function(...) {},
                          observer = function(...) {}) {
  list(
    createUI = createUI,
    createRenderer = createRenderer,
    observer = observer
  )
}


output.handlers <- list(
  plot = outputHandler(
    createUI = function(x, session) {
      plotOutput(session$ns(x$id))
    },
    createRenderer = function(x, session, sources, inputs, envir) {
      renderPlot({
        text <- sources()$outputs[[ x$name ]]$source

        eval(str2expression(text), envir = copyEnvironment(envir()))
      })
    }
  ),
  table = outputHandler(
    createUI = function(x, session) {
      tags$div(
        class = "slates-output slates-output-table",
        tableOutput(session$ns(x$id))
      )
    },
    createRenderer = function(x, session, sources, inputs, envir) {
      name <- x$name

      renderTable({
        text <- sources()$outputs[[ x$name ]]$source

        eval(str2expression(text), envir = copyEnvironment(envir()))
      }, spacing = "s")
    }
  ),
  reactable = outputHandler(
    createUI = function(x, session) {
      reactable::reactableOutput(session$ns(x$id))
    },
    createRenderer = function(x, session, sources, inputs, envir) {
      name <- x$name

      reactable::renderReactable({
        text <- sources()$outputs[[ x$name ]]$source

        reactable::reactable(
          eval(str2expression(text), envir = copyEnvironment(envir()))
          #theme = theme
        )
      })
    }
  ),
  print = outputHandler(
    createUI = function(x, session) {
      tags$div(
        # TODO: make the height adapt to the slate height
        style="overflow: auto; max-height: 400px;",
        verbatimTextOutput(session$ns(x$id))
      )
    },
    createRenderer = function(x, session, sources, inputs, envir) {
      name <- x$name

      renderPrint({
        text <- sources()$outputs[[ name ]]$source

        eval(str2expression(text), envir = copyEnvironment(envir()))
      })
    }
  ),
  markdown = outputHandler(
    createUI = function(x, session) {
      uiOutput(session$ns(x$id))
    },
    createRenderer = function(x, session, sources, inputs, envir) {
      name <- x$name

      renderUI({
        text <- sources()$outputs[[ name ]]$source
        # text <- eval(str2expression(source), envir = new.env(parent = envir()))

        knitr::knit(text = text, envir = copyEnvironment(envir()), quiet = TRUE) %>%
          markdown::markdownToHTML(text = ., fragment.only = TRUE) %>%
          HTML
      })
    }
  )

  # source = outputHandler(
  #   create.ui = function(id, title, options) {
  #     shinyAce::aceEditor(id,
  #                         mode = "r",
  #                         height = "300px",
  #                         readOnly = TRUE,
  #                         showLineNumbers = TRUE,
  #                         highlightActiveLine = FALSE)
  #   },
  #   observer = function(id, session, sources, inputs, envir, global.options) {
  #     text <-
  #       map(sources(), ~paste0("#-- ", .$name, "\n", .$source)) %>%
  #       paste(collapse = "\n\n")
  #
  #     shinyAce::updateAceEditor(
  #       session, editorId = id, value = text, theme = global.options$ace.theme
  #     )
  #   }
  # )

)




create_slate_outputs <- function(ns, slate) {

}





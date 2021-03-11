


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



#
# Outputs
#

outputHandler <- function(create.ui = function(...) { tagList() },
                          create.output = function(...) {},
                          observer = function(...) {}) {
  list(
    create.ui = create.ui,
    create.output = create.output,
    observer = observer
  )
}


output.handlers <- list(
  plot = outputHandler(
    create.ui = function(id, title) {
      plotOutput(id)
    },
    create.output = function(x, session, sources, inputs, envir) {
      session$output[[ x$id ]] <- renderPlot({
        text <- sources()[[ x$name ]]$source

        eval(str2expression(text), envir = new.env(parent = envir()))
      })
    }
  ),
  table = outputHandler(
    create.ui = function(id, title) {
      tags$div(
        style="overflow: auto; max-height: 400px;",
        tableOutput(id)
      )
    },
    create.output = function(x, session, sources, inputs, envir) {
      session$output[[ x$id ]] <- renderTable({
        text <- sources()[[ x$name ]]$source

        eval(str2expression(text), envir = new.env(parent = envir()))
      })
    }
  ),
  reactable = outputHandler(
    create.ui = function(id, title) {
      reactable::reactableOutput(id)
    },
    create.output = function(x, session, sources, inputs, envir) {
      session$output[[ x$id ]] <- reactable::renderReactable({
        text <- sources()[[ x$name ]]$source

        reactable::reactable(
          eval(str2expression(text), envir = new.env(parent = envir()))
          #theme = theme
        )
      })
    }
  ),
  print = outputHandler(
    create.ui = function(id, title) {
      tags$div(
        # TODO: make the height adapt to the slate height
        style="overflow: auto; max-height: 400px;",
        verbatimTextOutput(id)
      )
    },
    create.output = function(x, session, sources, inputs, envir) {
      name <- x$name

      session$output[[ x$id ]] <- renderPrint({
        text <- sources()[[ name ]]$source

        eval(str2expression(text), envir = new.env(parent = envir()))
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





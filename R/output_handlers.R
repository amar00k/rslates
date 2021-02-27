


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
    create.output = function(x, session, blueprint, input.list, envir) {
      session$output[[ x$id ]] <- renderPlot({
        req(envir())

        src <- buildSource(x$source, input.list())
        eval(parse(text = src), envir = new.env(parent = envir()))
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
    create.output = function(x, session, blueprint, input.list, envir) {
      session$output[[ x$id ]] <- renderTable({
        req(envir())

        src <- buildSource(x$source, input.list())
        eval(parse(text = src), envir = new.env(parent = envir()))
      })
    }
  ),
  reactable = outputHandler(
    create.ui = function(id, title) {
      reactable::reactableOutput(id)
    },
    create.output = function(x, session, blueprint, input.list, envir) {
      session$output[[ x$id ]] <- reactable::renderReactable({
        req(envir())

        src <- buildSource(x$source, input.list())

        reactable::reactable(
          eval(parse(text = src), envir = new.env(parent = envir()))
          #theme = theme
        )
      })
    }
  ),
  print = outputHandler(
    create.ui = function(id, title) {
      tags$div(
        style="overflow: auto; max-height: 400px;",
        verbatimTextOutput(id)
      )
    },
    create.output = function(x, session, blueprint, input.list, envir) {
      session$output[[ x$id ]] <- renderPrint({
        req(envir())

        src <- buildSource(x$source, input.list())
        eval(parse(text = src), envir = new.env(parent = envir()))
      })
    }
  ),
  source = outputHandler(
    create.ui = function(id, title) {
      shinyAce::aceEditor(id,
                          mode = "r",
                          height = "300px",
                          readOnly = TRUE,
                          showLineNumbers = TRUE,
                          highlightActiveLine = FALSE)
    },
    observer = function(id, session, blueprint, input.list, envir, global.options) {
      if (length(blueprint$output) == 0) {
        return("")
      }

      sources <- list()

      to.display <- as.logical(sapply(blueprint$datasets, function(x) !is.null(x$source)))
      sources$datasets.src <- lapply(blueprint$datasets[ to.display ], function(x) {
        paste0("#-- ", x$name, "\n", assignValue(buildSource(x$source, input.list()), x$name))
      }) %>% paste(collapse = "\n\n")

      to.display <- sapply(blueprint$output, function(x) !is.null(x$source) && x$name != "Source")
      sources$outputs.src <- paste(
        lapply(blueprint$output[ to.display ], function(x) {
          paste0("#-- ", x$name, "\n", buildSource(x$source, input.list()))
        }), collapse="\n\n")

      sources <- sources[ sources != "" ]
      src <- paste(sources, collapse="\n\n")

      shinyAce::updateAceEditor(session, editorId = id, value = src, theme = global.options$ace.theme)
    }
  )
  # debug = outputHandler(
  #   create.ui = function(id, title) {
  #     tabPanel(title,
  #              tagList(
  #                textInput(paste0(id, "_input"), label = "Source"),
  #                verbatimTextOutput(id)))
  #   },
  #   create.output = function(id, blueprint, input.list, envir, input) {
  #     outputs <- list()
  #     outputs[[ id ]] <- renderPrint({
  #       # req(slate()$outputs[[ id ]]$source)
  #
  #       source_assignments(input.list())
  #
  #       src <- input[[ paste0(id, "_input") ]]
  #
  #       eval(parse(text = src), envir = new.env(parent = envir()))
  #     })
  #
  #     return(outputs)
  #   }
  # )
)



buildSource <- function(lines, inputs) {
  lines %>%
    map(~srcBuild(srcParse(.x), inputs)) %>%
    paste(collapse = "\n")

  # tryCatch({
  #   result <- character(0)
  #   for (x in src.list) {
  #     result <- c(result, srcBuild(srcParse(x), inputs))
  #   }
  #
  #   paste(result, collapse = "\n")
  # },
  # error = function(e) {
  #   pprint("THERE WAS BAD MOJO WHILE PREPROCESSING:", x)
  #   print(inputs)
  #   print(e)
  # })
  #
  # return(result)
}

create_slate_outputs <- function(ns, slate) {

}





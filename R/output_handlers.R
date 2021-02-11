
#
# Outputs
#

output_handler <- function(create.ui = function(...) { tagList() },
                           create.output = function(...) {},
                           observer = function(...) {}) {
  list(
    create.ui = create.ui,
    create.output = create.output,
    observer = observer
  )
}


output.handlers <- list(
  plot = output_handler(
    create.ui = function(id, title) {
      plotOutput(id)
    },
    create.output = function(id, session, blueprint, input.list, envir) {
      session$output[[ id ]] <- renderPlot({
        req(blueprint$outputs[[ id ]]$source)
        req(envir())

        src <- buildSource(blueprint$outputs[[ id ]]$source, input.list())

        eval(parse(text = src), envir = new.env(parent = envir()))
      })
    }
  ),
  table = output_handler(
    create.ui = function(id, title) {
      tags$div(
        style="overflow: auto; max-height: 400px;",
        tableOutput(id)
      )
    },
    create.output = function(id, session, blueprint, input.list, envir) {
      session$output[[ id ]] <- renderTable({
        req(blueprint$outputs[[ id ]]$source)
        req(envir())

        src <- buildSource(blueprint$outputs[[ id ]]$source, input.list())
        eval(parse(text = src), envir = new.env(parent = envir()))
      })
    }
  ),
  reactable = output_handler(
    create.ui = function(id, title) {
      reactable::reactableOutput(id)
    },
    create.output = function(id, session, blueprint, input.list, envir) {
      session$output[[ id ]] <- reactable::renderReactable({
        req(blueprint$outputs[[ id ]]$source)
        req(envir())

        src <- buildSource(blueprint$outputs[[ id ]]$source, input.list())

        # info <- getCurrentOutputInfo()
        # print(info)
        # theme <- reactable::reactableTheme(
        #   color = info$fg(),
        #   backgroundColor = info$bg()
        # )

        reactable::reactable(
          eval(parse(text = src), envir = new.env(parent = envir()))
          #theme = theme
        )
      })
    }
  ),
  print = output_handler(
    create.ui = function(id, title) {
      tags$div(
        style="overflow: auto; max-height: 400px;",
        verbatimTextOutput(id)
      )
    },
    create.output = function(id, session, blueprint, input.list, envir) {
      session$output[[ id ]] <- renderPrint({
        req(blueprint$outputs[[ id ]]$source)
        req(envir())

        src <- buildSource(blueprint$outputs[[ id ]]$source, input.list())
        eval(parse(text = src), envir = new.env(parent = envir()))
      })
    }
  ),
  source = output_handler(
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
  # debug = output_handler(
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

buildSource <- function(src.list, inputs) {
  paste(lapply(src.list, function(x) {
    srcBuild(srcParse(x), inputs)
  }), collapse = "\n")
}

create_slate_outputs <- function(ns, slate) {

}





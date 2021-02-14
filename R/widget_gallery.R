



widgetGalleryUI <- function(id, input.list) {
  ns <- NS(id)

  inputs.ui <- tagList(
    createInputGroup(inputGroup(name = "inputs", layout = "flow-3", inputs = input.list), ns = ns)
  )

  outputs.ui <- tagList()

  tabsetPanel(
    tabPanel(title = "Inputs", tags$div(class = "container p-3", inputs.ui)),
    tabPanel(title = "Outputs", tags$div(class = "container p-3", outputs.ui))
  )
}


widgetGalleryServer <- function(id, input.list, global.options = NULL) {
  moduleServer(id, function(input, output, session) {
    # initialize input observers
    for (x in input.list) {
      input.handlers[[ x$input.type ]]$create.observer(session, x$id)
    }

    observeEvent(input$input_expr, {
      print("hi there")
    })
  }
  )
}



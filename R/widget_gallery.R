


widgetGalleryUI <- function(id, input.list) {
  ns <- NS(id)

  inputs.ui <- tagList(
    createInputGroup(inputGroup(name = "inputs", layout = "flow-3", inputs = input.list), ns = ns),
    #hr(),
    #reactable::reactableOutput(ns("input_table"))
    tableOutput(ns("input_table"))
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


    inputs <- reactive({
      lapply(input.list, function(x) {
        x$value <- input.handlers[[ x$input.type ]]$get.value(x, session)
        return(x)
      }) %>% set_names(sapply(., "[[", "name"))
    })


    # reactable.theme <- reactive({
    #   vars <- bslib::bs_get_variables(
    #     global.options$bslib.theme,
    #     varnames = c("fg", "bg", "primary", "secondary",
    #                  "success", "info", "warning", "danger")
    #   )
    #
    #   reactable::reactableTheme(
    #     color = vars[[ "fg" ]],
    #     backgroundColor = vars[[ "bg" ]]
    #   )
    # })

    objectSummary <- function(obj, max.length = 400) {
      s <- toString(obj)

      if (nchar(s) > (max.length))
        s <- paste0(substring(s, 1, max.length / 2), "...",
                    substring(s, nchar(s) - max.length / 2, nchar(s)))

      return(s)
    }


    inputs.table <- reactive({
      inputs <- inputs()

      data.frame(
        Name = sapply(inputs, "[[", "name"),
        Type = sapply(inputs, "[[", "input.type"),
        Value = sapply(inputs, function(x) objectSummary(x$value)),
        Source = sapply(inputs, function(x) input.handlers[[ x$input.type ]]$get.source(x, session))
      )
    })

    # output$input_table <- reactable::renderReactable({
    #   inputs <- inputs()
    #
    #   reactable::reactable(
    #     data.frame(
    #       Name = sapply(inputs, "[[", "name"),
    #       Id = sapply(inputs, "[[", "id"),
    #       Type = sapply(inputs, "[[", "input.type"),
    #       Value = sapply(inputs, function(x) objectSummary(x$value))
    #     ),
    #     rownames = FALSE,
    #     theme = reactable.theme(),
    #     borderless = TRUE,
    #     sortable = FALSE
    #   )
    # })



    output$input_table <- renderTable({
      inputs.table()
    })


  })
}



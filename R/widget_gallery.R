


widgetGalleryInputsUI <- function(id, inputs, group) {
  ns <- NS(id)

  div(
    class = "container",
    createGroupUI(group, inputs, ns),
    #reactable::reactableOutput(ns("input_table"))
    div(
      class = "card align-items-center mt-3",
      div(
        class = "card-body",
        tableOutput(ns("input_table"))
      )
    )
  )
}



widgetGalleryServer <- function(id, inputs, global.options = NULL) {
  moduleServer(id, function(input, output, session) {

    ready <- uiReady(session)

    # input observers
    observe({
      for (x in inputs) {
        getHandler(x)$observer(x, session)
      }
    })


    valueString <- function(val, max.length = 400) {
      val <- toString(val)
      if (nchar(val) > (max.length))
        val <- paste0(
          substring(val, 1, max.length / 2),
          "...",
          substring(val, nchar(val) - max.length / 2, nchar(val))
        )

      return(val)
    }


    inputs.table <- reactive({
      req(ready())

      values <- map(inputs, ~getHandler(.x)$as.value(.x, session))

      data.frame(
        Name = map_chr(inputs, "name"),
        Type = map_chr(inputs, "type"),
        Value = map_chr(values, valueString),
        Class = map_chr(values, class),
        Source = map_chr(inputs, ~getHandler(.x)$as.source(.x, session)),
        check.names = FALSE
      )
    })

    output$input_table <- renderTable({
      inputs.table()
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


    # output$input_table <- reactable::renderReactable({
    #   inputs <- inputs()
    #
    #   reactable::reactable(
    #     inputs.table(),
    #     rownames = FALSE,
    #     theme = reactable.theme(),
    #     borderless = TRUE,
    #     sortable = FALSE
    #   )
    # })


  })
}



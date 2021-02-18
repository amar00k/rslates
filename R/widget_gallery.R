


widgetGalleryInputsUI <- function(id, input.list) {
  ns <- NS(id)

  div(
    class = "container",
    createInputGroup(inputGroup(name = "inputs", layout = "flow-3", children = input.list), ns = ns),
    #reactable::reactableOutput(ns("input_table"))
    div(
      class = "card align-items-center",
      div(
        class = "card-body",
        tableOutput(ns("input_table"))
      )
    )
  )
}


widgetGalleryServer <- function(id, input.list, global.options = NULL) {
  moduleServer(id, function(input, output, session) {


    # initialize input observers
    observe({
      for (x in input.list) {
        getHandler(x)$observer(x, session)
      }
    })


    inputValueString <- function(x, max.length = 400) {
      val <- getHandler(x)$get.value(x, session)

      if (x$input.type == "expression" && val != "" && isValidExpression(val)) {
        val <- eval(parse(text = val), envir = new.env())
      }

      val <- toString(val)
      if (nchar(val) > (max.length))
        val <- paste0(substring(val, 1, max.length / 2), "...",
                    substring(val, nchar(val) - max.length / 2, nchar(val)))

      return(val)
    }


    inputs.table <- reactive({
      inputs <- input.list

      data.frame(
        Name = sapply(inputs, "[[", "name"),
        Type = sapply(inputs, "[[", "input.type"),
        Source = sapply(inputs, function(x) getHandler(x)$get.source(x, session)),
        Value = sapply(inputs, function(x) inputValueString(x))
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



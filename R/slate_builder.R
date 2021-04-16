









createTypeSpecificUI <- function(ns, item) {
  parseChoices <- function(choices) {
    if (!is.null(names(choices))) {
      paste(names(item$choices), item$choices, sep = "=")
    } else {
      choices
    }
  }

  params <- getHandler(item)$params.list

  if (length(params) > 0) {
    tags <- lapply(names(params), function(name) {
      par <- params[[ name ]]
      id <- paste0("input_", name)

      switch(
        par$type,
        "list" = selectizeInput(
          ns(id), label = par$label,
          choices = parseChoices(item[[ name ]]),
          selected = parseChoices(item[[ name ]]),
          multiple = TRUE,
          options = list(
            delimiter = '',
            create = "function(input) { return { value: input, text: input } }"
          )
        ),
        "choices" = selectInput(
          ns(id), label = par$label,
          choices = par$choices, selected = item[[ name ]]
        ),
        "logical" = checkboxInput(
          ns(id), label = par$label, value = item[[ name ]]
        )
      )
    })

    div(
      class = "slates-flow-3",
      tags
    )
  } else {
    tagList()
  }
}


createItemPropertiesUI <- function(id, item) {
  ns <- NS(id)

  if (class(item) == "slatePage") {
    ui <- tagList(
      tags$h5("Page Properties", paste0("(", item$name, ")")),
      tags$hr(),
      textAreaInput(ns("page_description"), label = "Description", item$description)
    )
  } else if (class(item) == "slateGroup") {
    ui <- tagList(
      tags$h5("Group Properties"),
      tags$hr(),
      selectInput(ns("group_layout"), label = "Layout",
                  choices = c("flow-2",
                              "flow-3",
                              "flow-4",
                              "vertical"),
                  selected = item$layout),
      textInput(ns("group_condition"), label = "Condition", item$condition)
    )
  } else if (class(item) == "slateInput") {
    ui <- tagList(
      tags$h5("Input Properties", paste0("(", item$name, ")")),
      tags$hr(),
      tags$div(
        class = "slates-flow-2",
        selectInput(ns("input_input.type"), label = "Type",
                    selectize = TRUE,
                    choices = names(input.handlers),
                    selected = item$type),
        uiOutput(ns("input_default_ui"))
      ),
      createTypeSpecificUI(ns, item),
      selectizeInput(
        ns("input_wizards"), label = "Wizards",
        choices = names(wizard.list), multiple = TRUE,
        selected = item$wizards
      ),
      textAreaInput(ns("input_description"), label = "Description", value = item$description)
    )
  }

  return (ui)
}


inputItemServer <- function(id, item, global.options = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    item <- reactiveVal(item)

    need.redraw <- reactiveVal("")  # change when we need to redraw this whole page
    redraw.default.ui <- reactiveVal("") # change to update default input (on entering page)


    createUI <- function() {
      dlog(item())

      createItemPropertiesUI(id, isolate(item()))
    }


    # output$input_default_ui <- renderUI({
    #   req(isolate(item <- item()))
    #
    #   pprint("slate_builder.R: redraw default for",
    #          item$id, "( default =", paste(item$default, collapse = ", "), ")")
    #
    #   # listen to these events
    #   redraw.default.ui()
    #   input$input_input.type
    #
    #   item$name <- "Default Value"
    #   item$value <- item$default
    #   item$wizards <- NULL
    #   getHandler(item)$create.ui(session$ns(item$id), item)
    # })
    #
    #
    # observe({
    #   req(item <- item())
    #
    #   # check for a change in input.type. If changed:
    #   # 1. set need.redraw to a new value to redraw the UI
    #   # 2. re-initialize the input to fill in missing type-specific variables
    #   # 3. restore the item's ancestry
    #   if (item$type == "input" &&
    #       !is.null(input$input_input.type) &&
    #       item$input.type != input$input_input.type) {
    #     need.redraw(runif(1))
    #
    #     ancestry <- item$ancestry
    #     item <- slateInput(name = item$name, input.type = input$input_input.type,
    #                        long.name = item$long.name, description = item$description,
    #                        wizards = item$wizards)
    #     print(item)
    #     item$ancestry <- ancestry
    #   }
    #
    #   # check for changes in general item properties
    #   variables <- c("description", "layout", "condition", "description", "wizards")
    #   for (var in variables) {
    #     input.id <- paste0(item$type, "_", var)
    #
    #     if (!is.null(input[[ input.id ]]))
    #       item[[ var ]] <- input[[ input.id ]]
    #   }
    #
    #   # check for changes in input type-specific parameters
    #   if (item$type == "input") {
    #     params <- getHandler(item)$params.list
    #
    #     for (var in names(params)) {
    #       par <- params[[ var ]]
    #       input.id <- paste0("input_", var)
    #
    #       if (!is.null(input[[ input.id ]])) {
    #         item[[ var ]] <- switch(
    #           par$type,
    #           "list" = (function(x) {
    #             if (all(grepl("=", x))) {
    #               strsplit(x, split="=") %>%
    #                 { setNames(lapply(., "[[", 2), sapply(., "[[", 1)) }
    #             } else {
    #               as.list(x) %>% unname
    #             }
    #           })(input[[ input.id ]]),
    #           "choices" = input[[ input.id ]],
    #           "logical" = as.logical(input[[ input.id ]])
    #         )
    #       }
    #     }
    #   }
    #
    #   # if any type-specific parameters changed, redraw default input
    #   if (item$type == "input") {
    #     params <- getHandler(item)$params.list
    #     if (length(params) > 0 &&
    #         !all(sapply(names(params), function(var) identical(item[[ var ]], item()$var)))) {
    #       print("here")
    #       redraw.default.ui(runif(1))
    #     }
    #   }
    #
    #   if (!identical(item, item())) {
    #     pprint("slate_builder.R: update item (", item$name, ")")
    #
    #     item(item)
    #   }
    # })
    #
    #
    # # get the value of the default input, taking the input type into consideration
    # default.value <- reactive({
    #   req(
    #     item <- isolate(item()),
    #     input$input_input.type
    #   )
    #
    #   value <- getHandler(item)$get.value(item, session)
    #
    #   if (length(value) > 0) {
    #     req(all(!sapply(value, is.null)))
    #   } else {
    #     req(!is.null(value))
    #   }
    #
    #   return(value)
    # })
    #
    #
    # # check for changes in default value
    # observe({
    #   req(
    #     item <- isolate(item())
    #   )
    #
    #   value <- default.value()
    #
    #   if (!identical(item$default, value)) {
    #     pprint("Setting new default for", item$name, ":", paste(value, collapse=", "))
    #
    #     item$value <- item$default <- value
    #
    #     item(item)
    #   }
    # })

    list(
      item = item,
      createUI = createUI,
      need.redraw = need.redraw,
      redraw.default.ui = redraw.default.ui
    )
  })
}






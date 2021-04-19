


inputsToTree <- function(pages, groups, inputs, selected = "") {
  inputs.tree <- flatInputsToTree(pages, groups, inputs)

  node_type <- function(x) {
    switch(class(x),
           "slateInput" = "input",
           "slateGroup" = "group",
           "slatePage" = "page")
  }

  jstree <- function(tree) {
    id <- tree$id

    nodes <- map(
      tree$inputs,
      ~structure("", sttype = "input", stclass = "input",
                 stinfo = .$id, stselected = .$id == selected)
    )

    if (length(tree$groups) > 0) {
      nodes <- map2(
        tree$groups, map(tree$groups, jstree),
        ~structure(.y,
                   sttype = "group", stclass = "group",
                   stinfo = .x$id, stselected = .x$id == selected)
      ) %>% append(nodes, .)
    }

    if (length(tree$pages) > 0) {
      nodes <- map2(
        tree$pages, map(tree$pages, jstree),
        ~structure(.y,
                   sttype = "pages", stclass = "pages",
                   stinfo = .x$id, stselected = .x$id == selected)
      ) %>% append(nodes, .)
    }

    return(nodes)
  }

  tree <- jstree(inputs.tree)

  if (selected == "")
    attr(tree[[1]], "stselected") <- TRUE

  return(tree)
}




#
# Output item tab server
#


outputItemServer <- function(id, item, global.options) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    item <- reactiveVal(item)


    createUI <- function() {
      req(item <- isolate(item()))

      tagList(
        tags$h5(item$name),
        tags$hr(),
        selectInput(
          ns("type"), label = "Type",
          choices = names(output.handlers) %>% set_names(map(output.handlers, "name")),
          selected = item$type
        ),
        tags$p("Source Template"),
        shinyAce::aceEditor(
          ns("source"),
          mode = "r",
          autoComplete = "live",
          height = "200px",
          placeholder = "Enter output source code template...",
          value = item$source
        )
      )
    }


    observe({
      req(
        input$type,
        input$source,
        item <- isolate(item())
      )

      new.item <- item %>%
        list_modify(type = input$type, source = input$source)

      dlog(item$type, new.item$type)

      if (!identical(item, new.item))
        item(new.item)
    })


    list(
      item = item,
      createUI = createUI
    )
  })
}


#
# Editor UI
#

blueprintEditor2UI  <- function(id, blueprint) {
  ns <- NS(id)

  metadata.ui <- tags$div(
    tags$div(
      class = "slates-flow slates-flow-2",
      textInput(ns("blueprint_name"), "Name", value = blueprint$name),
      textInput(ns("blueprint_author"), "Author(s)", value = blueprint$author),
      selectInput(ns("blueprint_category"), "Category",
                  choices = "", selected = ""),
      selectizeInput(ns("blueprint_tags"), label = "Tags", multiple = TRUE,
                     choices = getOption("rslates.blueprint.tags"),
                     selected = blueprint$tags,
                     options = list(
                       delimiter = '',
                       create = "function(input) { return { value: input, text: input } }"
                     )
      )
    )
  )


  inputs.ui <- tags$div(
    class = "row h-100",
    column(
      width = 4,
      tags$div(
        class = "well mb-2",
        style = "height: calc(100% - 4em);",
        shinyTree::shinyTree(ns("inputs_tree"),
                             stripes = FALSE,
                             multiple = FALSE,
                             animation = FALSE,
                             contextmenu = FALSE,
                             dragAndDrop = TRUE,
                             wholerow = TRUE,
                             theme = "proton",
                             types =
          "{
            '#': { 'max_depth' : 3, 'valid_children' : ['page', 'group', 'input'] },
            'page' : { 'icon' : 'fa fa-folder', 'valid_children' : ['group', 'input'] },
            'group' : { 'icon' : 'fa fa-object-group', 'valid_children' : ['input'] },
            'input' : { 'icon' : 'fa fa-pen', 'valid_children' : [''] }
          }"
        )
      ),
      tags$div(
        class = "",
        style = "height: 4em;",
        shinyWidgets::dropdownButton(
          inputId = ns("inputs_add"),
          label = "Add",
          circle = FALSE,
          inline = TRUE,
          actionLink(ns("inputs_add_input"), label = "New Input"),
          actionLink(ns("inputs_add_group"), label = "New Group"),
          actionLink(ns("inputs_add_page"), label = "New Page")
        ),
        actionButton(ns("inputs_rename"), class = "ml-2", label = "Rename"),
        actionButton(ns("inputs_remove"), class = "ml-auto", label = "Delete")
      )
    ),
    tags$div(
      class = "col-8 h-100",
      style = "overflow-y: auto;",
      shinyjs::hidden(textInput(ns("input_id"), label = "")),
      uiOutput(ns("input_item_ui"))
    )
  )


  outputs.ui <- fluidRow(
    column(
      width = 4,
      tags$div(
        class = "well mb-2",
        style = "height: calc(100% - 4em);",
        shinyTree::shinyTree(ns("outputs_tree"),
                             stripes = FALSE,
                             multiple = TRUE,
                             animation = FALSE,
                             contextmenu = FALSE,
                             dragAndDrop = TRUE,
                             wholerow = TRUE,
                             theme = "proton",
                             types =
                               "{
            '#': { 'max_depth' : 1, 'valid_children' : ['output'] },
            'output' : { 'icon' : 'fa fa-poll', 'valid_children' : [] }
          }"
        )
      ),
      tags$div(
        class = "",
        style = "height: 4em;",
        actionButton(ns("add_output"), label = "Add"),
        actionButton(ns("rename_output"), class = "ml-2", label = "Rename"),
        actionButton(ns("remove_output"), class = "ml-auto", label = "Delete")
      )
    ),
    tags$div(
      class = "col-8 h-100",
      style = "overflow-y: auto;",
      uiOutput(ns("output_item_ui"))
    )
  )


  code.ui <- tags$div(
    shinyAce::aceEditor(
      ns("blueprint_source"),
      height = "350px",
      mode = "r",
      value = blueprint$source
    ),
    uiOutput(ns("blueprint_alerts"))
  )

  export.ui <- tags$div(
    class = "d-flex flex-column",
    radioButtons(
      ns("export_format"),
      inline = TRUE,
      label = "Format",
      choices = c("YAML", "JSON")
    ),
    verbatimTextOutput(
      ns("export_output")
    )
  )

  editor.tab <- function(x) {
    tags$div(
      class = "pt-3",
      style = "height: 400px",
      x
    )
  }

  editor.tab <- tabPanel(
    title = "Editor",
    class = "container-fluid pt-3",
    #navlistPanel(
    #  widths = c(3, 9),
    #  fluid = FALSE,
    tabsetPanel(
      type = "pills",
      tabPanel(title = "Metadata", editor.tab(metadata.ui)),
      tabPanel(title = "Inputs", editor.tab(inputs.ui)),
      tabPanel(title = "Outputs", editor.tab(outputs.ui)),
      tabPanel(title = "Code", editor.tab(code.ui)),
      tabPanel(title = "Export", editor.tab(export.ui))
    )
  )

  debug.tab <- tabPanel(
    title = "Inspector",
    class = "container-fluid pt-3",
    tags$div(
      class = "slates-flow slates-flow-3",
      selectInput(ns("debug_type"), label = "Debug",
                  choices = c("Blueprint", "State")),
      selectInput(ns("debug_element"),
                  label = "Element", choices = "")
    ),
    verbatimTextOutput(ns("blueprint_debug")) %>%
      tagAppendAttributes(style = "overflow: auto; height: 250px;")
  )

  preview.tab <- tabPanel(
    title = "Preview",
    class = "container-fluid pt-3",
    flowLayout(
      selectInput(
        ns("preview_inputs_style"),
        label = "Input Panel Style",
        choices = list("tabset", "collapses", "flowing"),
        selected = "tabset"
      ),
      textInput(ns("slate_height"), "Slate Height", value = "520px"),
      checkboxGroupInput(
        ns("slate_options"), "Card",
        choices = c("Use Card" = "use.card",
                    "Show Header" = "card.header"),
        selected = c("use.card", "card.header"))
    ),
    shinyBS::bsTooltip(ns("preview_inputs_style"), title = "Style of the inputs panel.",
                       placement = "top"),
    shinyBS::bsTooltip(ns("slate_height"), title = "Height of the slate in any valid css unit.",
                       placement = "top"),
    slateUI(ns("slate"))
  )

  tabsetPanel(
    editor.tab,
    debug.tab,
    preview.tab
  )
}



blueprintEditor2Server <- function(id, blueprint,  global.options = NULL) {
  stopifnot(class(blueprint) == "slateBlueprint")

  blueprint.ini <- blueprint

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # create the blueprint container
    blueprint.data <- reactiveValues(
      name = blueprint.ini$name,
      author = blueprint.ini$author,
      category = blueprint.ini$category,
      tags = blueprint.ini$tags,
      source = blueprint.ini$source,
      pages = blueprint.ini$pages,
      groups = blueprint.ini$groups,
      inputs = blueprint.ini$inputs,
      outputs = blueprint.ini$outputs,
      imports = blueprint.ini$imports,
      exports = blueprint.ini$exports
    )

    slate.options <- do.call(
      reactiveValues,
      slateOptions(view.inputs = TRUE, open.editor = FALSE)
    )

    # Create the slate server
    slate <- slateServer(
      "slate",
      blueprint = blueprint,
      slate.options = slate.options,
      global.options = global.options
    )

    errors <- reactiveVal(list())

    text.input.modal <- textInputModal(ns("text_input_modal"))

    observe({
      slate.options$inputs.style = input$preview_inputs_style
      slate.options$height = input$slate_height
      slate.options$use.card = "use.card" %in% input$slate_options
      slate.options$card.header = "card.header" %in% input$slate_options
    })


    blueprint <- reactive({
      do.call(slateBlueprint, reactiveValuesToList(blueprint.data))
    })


    observe(label = "blueprint.changed", {
      dlog()

      slate$updateBlueprint(blueprint())
    })


    observe(label = "blueprint.metadata", {
      req(
        !is.null(input$blueprint_name),
        !all(identical(blueprint.data$name, input$blueprint_name),
             identical(blueprint.data$author, input$blueprint_author),
             identical(blueprint.data$category, input$blueprint_category),
             identical(blueprint.data$tags, input$blueprint_tags))
      )

      dlog()

      blueprint.data$name <- input$blueprint_name
      blueprint.data$author <- input$blueprint_author
      blueprint.data$category <- input$blueprint_category

      if (is.null(input$blueprint_tags))
        blueprint.data$tags <- list()
      else
        blueprint.data$tags <- input$blueprint_tags
    })


    # change the ace theme on all ace editors
    observeEvent(global.options$ace.theme, {
      #shinyAce::updateAceEditor(session, "blueprint_source", theme = global.options$ace.theme)
    })


    observe({
      shinyjs::toggle("debug_blueprint", condition = (input$debug_type == "Blueprint"))
      shinyjs::toggle("debug_state", condition = (input$debug_type == "State"))
    })


    observe({
      req(input$debug_type)

      selected <- input$debug_blueprint_element

      if (input$debug_type == "Blueprint")
        choices <-  names(blueprint())
      else
        choices <- names(slate)

      updateSelectInput(session, "debug_element",
                        choices = choices, selected = selected)
    })

    #
    # Debug tab
    #
    output$blueprint_debug <- renderText({
      str.str <- function(x) capture.output(str(x)) %>% paste(collapse = "\n")

      if (input$debug_type == "Blueprint") {
        obj <- blueprint()[[ input$debug_element ]]
      } else {
        obj <- slate[[ input$debug_element ]]
      }

      if ("reactivevalues" %in% class(obj))
        obj <- reactiveValuesToList(obj)
      else if ("reactive" %in% class(obj))
        obj <- obj()

      if (class(obj) == "environment")
        obj <- as.list(obj)

      str.str(obj)
    })


    #
    # Export tab
    #

    output$export_output <- renderText({
      if (input$export_format == "YAML")
        blueprintToYAML(blueprint())
      else
        blueprintToJSON(blueprint())
    })


    #
    # Inputs Tab
    #
    input.servers <- reactiveValues()
    input.tree <- reactive({
      req(
        pages <- blueprint.data$pages,
        groups <- blueprint.data$groups,
        inputs <- blueprint.data$inputs
      )

      inputsToTree(pages, groups, inputs)
    })

    selected.inputs <- reactive({
      dlog()

      if (is.null(input$inputs_tree))
        return(character(0))

      selected <- shinyTree::get_selected(input$inputs_tree) %>%
        map_chr(~attr(., "stinfo"))
    })

    # render inputs tree
    output$inputs_tree <- shinyTree::renderTree({
      req(tree <- input.tree())

      dlog("renderTree")

      tree
    })

    # create servers for inputs properties
    observe({
      req(
        inputs <- blueprint.data$inputs,
        pages <- blueprint.data$pages,
        groups <- blueprint.data$groups
      )

      all.inputs <- c(inputs, pages, groups)

      for (x in all.inputs) {
        if (is.null(input.servers[[ x$id ]])) {
          input.servers[[ x$id ]] <-
            inputItemServer(ns(paste0("builder_", x$id)), x, global.options)
        }
      }
    })

    # render the inputs properties UI
    output$input_item_ui <- renderUI({
      req(
        servers <- reactiveValuesToList(input.servers)
      )

      dlog()

      ids <- isolate(selected.inputs())

      tabs <- imap(servers, ~{
        tabPanelBody(
          value = .y,
          .x$ui.output
        )
      }) %>% unname

      do.call(tabsetPanel,
              append(list(id = ns("input_item_tabs"), type = "hidden", selected = ids[1]), tabs))
    })


    observeEvent(selected.inputs(), {
      req(
        ids <- selected.inputs(),
        length(ids) == 1
      )

      updateTabsetPanel(session, "input_item_tabs", selected = ids[1])
    })

    # observe selected item, and enable or disable buttons elements
    observe({
      req(selected <- selected.inputs())

      if (is.null(selected)) {
        shinyjs::hide("layout_add_input")
        shinyjs::hide("layout_add_group")
        return()
      }

      if (length(selected) > 1) {
        shinyjs::disable("layout_add")
        shinyjs::disable("layout_rename")
        return()
      }

      item.class <-
        c(blueprint.data$inputs, blueprint.data$pages, blueprint.data$groups) %>%
        keep(map(., "id") == selected) %>%
        class %>% print

      shinyjs::disable("layout_add")
      shinyjs::disable("layout_rename")
      shinyjs::show("layout_add_input")
      shinyjs::show("layout_add_group")
    })




    #
    # Outputs Tab
    #
    output.servers <- reactiveValues()
    output.names <- reactive({
      names(blueprint.data$outputs)
    })

    selected.outputs <- reactive({
      if (is.null(input$outputs_tree))
        return(NULL)

      selected <- shinyTree::get_selected(input$outputs_tree) %>%
        map_chr(~attr(., "stinfo"))

      if (length(selected) == 0)
        return(NULL)
      else
        return(selected)
    })

    # render the outputs selector
    output$outputs_tree <- shinyTree::renderTree({
      req(names <- output.names())

      selected <- isolate(selected.outputs()) %||% names[1]

      map(names, ~structure(
        "", sttype = "output", stinfo = ., stselected = . %in% selected
      )) %>% set_names(names)
    })

    # create servers for output properties
    observe({
      req(outputs <- blueprint.data$outputs)

      for (x in outputs) {
        if (is.null(output.servers[[ x$name ]])) {
          output.servers[[ x$name ]] <-
            outputItemServer(ns(paste0("builder_", x$name)), x, global.options)
        }
      }
    })

    # observe changes in output properties and update blueprint.data$outputs
    observe(label = "change.output", {
      req(
        outputs <- isolate(blueprint.data$outputs),
        new.outputs <- reactiveValuesToList(output.servers) %>%
          map(~.$item())
      )

      for (x in outputs) {
        if (!identical(x, new.outputs[[ x$name ]])) {
          dlog("changing", x$name)
          blueprint.data$outputs[[ x$name ]] <- new.outputs[[ x$name ]]
        }
      }
    })

    # render the output properties UI
    output$output_item_ui <- renderUI({
      req(
        names <- selected.outputs(),
        length(names) == 1,
        server <- output.servers[[ names[1] ]]
      )

      isolate(server$createUI())
    })

    observeEvent(input$add_output, {
      text.input.modal$show(
        title = "New Output",
        label = "Output Name",
        placeholder = "Enter output name",
        callback = function(value) {
          blueprint.data$outputs[[ value ]] <- slateOutput(name = value, type = "plot")
          updateSelectInput(session, "select_output",
                            choices = names(blueprint.data$outputs),
                            selected = value)
        })
    })

    observeEvent(input$rename_output, {
      text.input.modal$show(
        title = "Rename Output",
        label = "Output Name",
        placeholder = "Enter output name",
        value = blueprint.data$outputs[[ input$select_output ]]$name,
        callback = function(value) {
          blueprint.data$outputs[[ input$select_output ]]$name <- value
          names(blueprint.data$outputs) <- map(blueprint.data$outputs, "name")
          updateSelectInput(session, "select_output",
                            choices = names(blueprint.data$outputs),
                            selected = value)
        })
    })

    observeEvent(input$remove_output, {
      selected <- which(names(blueprint.data$outputs) == input$select_output) - 1
      if (length(blueprint.data$outputs) > 1 && selected == 0)
        selected <- 1

      blueprint.data$outputs[[ input$select_output ]] <- NULL

      selected <- names(blueprint.data$outputs)[ selected ]
      updateSelectInput(session, "select_output",
                        choices = names(blueprint.data$outputs),
                        selected = selected)
    })


    list(
      blueprint = blueprint
    )

  })
}





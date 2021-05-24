


#' Get a tree representation of the inputs structure
#'
#' @param pages
#' @param groups
#' @param inputs
#'
#' @return a tree of pages, groups and input names
#' @export
#'
#' @examples
inputsTree <- function(pages, groups, inputs) {
  make.node <- function(parent) {
    children <- c(inputs, groups) %>%
      keep(map_lgl(., ~.$parent == parent))

    if (length(children) == 0)
      return(character(0))

    map(children, ~make.node(.$name))
  }

  c(make.node(".root"),
    pages %>% map(~make.node(.$name)))
}


inputsToShinyTree <- function(pages, groups, inputs,
                              opened = "",
                              selected = "") {
  tree <- inputsTree(pages, groups, inputs)
  all.inputs <- c(inputs, groups, pages)

  make_node <- function(node, name) {
    if (length(node) == 0)
      node <- ""
    else
      node <- imap(node, make_node)

    item <- all.inputs[[ name ]]
    item.type <- class(item)

    structure(node, sttype = item.type, stinfo = item$name,
              stopened = item$name %in% opened,
              stselected = item$name %in% selected)
  }

  imap(tree, make_node)
}


shinyTreeToDf <- function(stree, old.df = NULL) {
  get_level <- function(node, parent = ".root") {
    name <- attr(node, "stinfo")

    if (!is.null(old.df))
      type <- old.df[ name, ]$type
    else
      type <- attr(node, "sttype")

    info <- list()
    info[[ name ]] <- list(
      name = name, type = type, parent = parent,
      opened = attr(node, "stopened"),
      selected = attr(node, "stselected")
    ) %>%
      modify_if(is.null, ~FALSE)

    if (length(attr(node, "names")) > 0) {
      children <-
        map(node, get_level, parent = attr(node, "stinfo")) %>%
        unlist(recursive = FALSE)

      info <- append(info, children)
    }

    return(info)
  }

  map(stree, get_level) %>%
    unlist(recursive = FALSE) %>%
    set_names(map(., "name")) %>%
    do.call(rbind.data.frame, .)
}


dfToShinyTree <- function(df) {
  make_node <- function(name) {
    x <- df[ name, ]
    children <- df[ which(df$parent == x$name), ]$name

    children %<>% map(make_node) %>% set_names(children)

    structure(children, sttype = x$type, stinfo = x$name,
              stopened = x$opened, stselected = x$selected)
  }

  names <- df[ which(df$parent == ".root"), ]$name
  map(names, make_node) %>%
    set_names(names)
}



#
# Modals
#


newInputModal <- function(id) {
  slatesModal_(id, function(input, output, session) {
    ns <- session$ns

    list(
      ui = function() {
        tagList(
          textInput(ns("name"), label = "Name", value = ""),
          selectInput(ns("type"), label = "Type",
                      choices = names(input.handlers))
        )
      },
      validator = function() {
        input$name != ""
      },
      submit = function() {
        list(name = input$name, type = input$type)
      }
    )
  })
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

blueprintEditorUI  <- function(id, blueprint) {
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
            'default': { 'max_depth' : 3, 'valid_children' : ['slatePage', 'slateGroup', 'slateInput'] },
            'slatePage' : { 'icon' : 'fa fa-folder', 'valid_children' : ['slateGroup', 'slateInput'] },
            'slateGroup' : { 'icon' : 'fa fa-object-group', 'valid_children' : ['slateInput'] },
            'slateInput' : { 'icon' : 'fa fa-pen', 'valid_children' : [''] }
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

  editor.ui <-
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


  # debug.tab <- tabPanel(
  #   title = "Inspector",
  #   class = "container-fluid pt-3",
  #   tags$div(
  #     class = "slates-flow slates-flow-3",
  #     selectInput(ns("debug_type"), label = "Debug",
  #                 choices = c("Blueprint", "State")),
  #     selectInput(ns("debug_element"),
  #                 label = "Element", choices = "")
  #   ),
  #   verbatimTextOutput(ns("blueprint_debug")) %>%
  #     tagAppendAttributes(style = "overflow: auto; height: 250px;")
  # )

  # preview.tab <- tabPanel(
  #   title = "Preview",
  #   class = "container-fluid pt-3",
  #   flowLayout(
  #     selectInput(
  #       ns("preview_inputs_style"),
  #       label = "Input Panel Style",
  #       choices = list("tabset", "collapses", "flowing"),
  #       selected = "tabset"
  #     ),
  #     textInput(ns("slate_height"), "Slate Height", value = "520px"),
  #     checkboxGroupInput(
  #       ns("slate_options"), "Card",
  #       choices = c("Use Card" = "use.card",
  #                   "Show Header" = "card.header"),
  #       selected = c("use.card", "card.header"))
  #   ),
  #   shinyBS::bsTooltip(ns("preview_inputs_style"), title = "Style of the inputs panel.",
  #                      placement = "top"),
  #   shinyBS::bsTooltip(ns("slate_height"), title = "Height of the slate in any valid css unit.",
  #                      placement = "top"),
  #   slateUI(ns("slate"))
  # )

  # tabsetPanel(
  #   editor.tab,
  #   debug.tab
  #   #preview.tab
  # )
}



blueprintEditorServer <- function(id, blueprint, slate.server = NULL, global.options = NULL) {
  stopifnot(class(blueprint) == "slateBlueprint")

  blueprint.ini <- blueprint

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # create the blueprint container
    blueprint.data <- reactiveValues()

    global.options$group.name.generator <- sequenceGenerator("group")

    # Create the slate server
    if (is.null(slate.server)) {
      slate <- slateServer(
        "slate",
        blueprint = blueprint,
        #slate.options = slate.options,
        global.options = global.options
      )
    } else {
      slate <- slate.server
    }

    errors <- reactiveVal(list())

    text.input.modal <- textInputModal(ns("text_input_modal"))

    # observe({
    #   slate.options$inputs.style = input$preview_inputs_style
    #   slate.options$height = input$slate_height
    #   slate.options$use.card = "use.card" %in% input$slate_options
    #   slate.options$card.header = "card.header" %in% input$slate_options
    # })

    blueprint <- reactive({
      data <- reactiveValuesToList(blueprint.data)

      df <- tree.data()
      inputs <- reactiveValuesToList(input.data)
      input.servers <- reactiveValuesToList(input.servers)

      req(all(rownames(df) %in% names(input.servers)))

      # data$pages <-
      #   inputs[ filter(df, type == "slatePage")$name ] %>%
      #   map(~list_modify(.,
      #                    parent = df[ .$name, ]$parent,
      #                    title = .$name))
      # data$groups <-
      #   inputs[ filter(df, type == "slateGroup")$name ] %>%
      #   map(~list_modify(., parent = df[ .$name, ]$parent))
      #
      # data$inputs <-
      #   inputs[ filter(df, type == "slateInput")$name ] %>%
      #   map(~list_modify(., parent = df[ .$name, ]$parent))
      data$pages <-
        filter(df, type == "slatePage")$name %>%
        map(~input.servers[[ . ]]$item()) %>%
        map(~list_modify(.,
                         parent = df[ .$name, ]$parent,
                         title = .$name))

      data$groups <-
        filter(df, type == "slateGroup")$name %>%
        map(~input.servers[[ . ]]$item()) %>%
        map(~list_modify(., parent = df[ .$name, ]$parent))

      data$inputs <-
        filter(df, type == "slateInput")$name %>%
        map(~input.servers[[ . ]]$item()) %>%
        map(~list_modify(., parent = df[ .$name, ]$parent))

      do.call(slateBlueprint, data)
    })


    observe(label = "blueprint.changed", {
      dlog()

      slate$updateBlueprint(blueprint())
    })


    getInputByName <- function(name) {
      c(blueprint.data$inputs, blueprint.data$pages, blueprint.data$groups) %>%
        keep(map(., "name") == name) %>%
        pluck(1)
    }


    #
    # Metadata Tab
    #

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

    input.data <- reactiveValues()
    tree.data <- reactiveVal()
    input.servers <- reactiveValues()


    observeEvent(input$inputs_tree, {
      dlog()

      tree.data(
        shinyTreeToDf(input$inputs_tree, tree.data())
      )
    })


    selected.inputs <- reactive({
      filter(tree.data(), selected == TRUE) %>% rownames
    })


    # render inputs tree
    output$inputs_tree <- shinyTree::renderTree({
      dlog("render tree")

      isolate(
        if (!is.null(input$inputs_tree))
          old.names <- shinyTreeToDf(input$inputs_tree, tree.data())$name
        else
          old.names <- ""
      )

      req(
        !identical(tree.data()$name, old.names),
        cancelOutput = TRUE
      )

      dfToShinyTree(tree.data())
    })


    # create servers for inputs properties
    observe({
      req(
        tree.data(),
        all(rownames(tree.data()) %in% names(input.data))
      )

      for (x in rownames(tree.data())) {
        if (is.null(input.servers[[ x ]])) {
          name <- x

          server <- inputItemServer(
            paste0("builder_", name), input.data[[ name ]], global.options
          )

          input.servers[[ name ]] <- server
        }
      }
    })


    observe({
      req(
        servers <- reactiveValuesToList(input.servers),
        selected <- selected.inputs()
      )

      items <- map(servers, ~.$item())
      for (x in items) {
        if (!identical(input.data[[ x$name ]], x)) {
          dlog(x$name)
          input.data[[ x$name ]] <- x
        }
      }
    })


    # render the inputs properties UI
    output$input_item_ui <- renderUI({
      req(
        servers <- reactiveValuesToList(input.servers)
      )

      dlog()

      selected <- isolate(selected.inputs())

      tabs <- imap(servers, function(x, name) {
        tabPanelBody(
          value = name,
          x$ui.output
        )
      }) %>% unname

      do.call(
        tabsetPanel,
        append(
          list(
            id = ns("input_item_tabs"),
            type = "hidden",
            selected = selected[1]
          ),
          tabs
        )
      )
    })


    observeEvent(selected.inputs(), {
      req(
        length(selected.inputs()) == 1
      )

      updateTabsetPanel(session, "input_item_tabs", selected = selected.inputs())
    })



    # observe selected item, and enable or disable buttons elements
    observe({
      req(selected <- selected.inputs())

      if (is.null(selected)) {
        shinyjs::hide("inputs_add_input")
        shinyjs::hide("inputs_add_group")
        return()
      }

      if (length(selected) > 1) {
        shinyjs::disable("inputs_add")
        shinyjs::disable("inputs_rename")
        return()
      }

      # item.class <-
      #   c(blueprint.data$inputs, blueprint.data$pages, blueprint.data$groups) %>%
      #   keep(map(., "id") == selected) %>%
      #   class

      shinyjs::enable("inputs_add")
      shinyjs::enable("inputs_rename")
      shinyjs::show("inputs_add_input")
      shinyjs::show("inputs_add_group")
    })


    addInputElement <- function(x) {
      tree.data(
        add_row(tree.data(),
                name = x$name, type = class(x), parent = x$parent,
                opened = FALSE, selected = TRUE) %>%
          set_rownames(.$name)
      )

      input.data[[ x$name ]] <- x
    }


    # handle the add page button
    observeEvent(input$inputs_add_page, {
      text.input.modal$show(
        title = "New Page",
        label = "Page Name",
        placeholder = "Enter page name/title",
        callback = function(value) {
          page <- slatePage(name = value)
          addInputElement(page)
        })
    })


    # handle the add group button
    observeEvent(input$inputs_add_group, {
      selected <- selected.inputs()

      req(length(selected) < 2)

      if (length(selected) == 0) {
        parent <- ".root"
      } else {
        item <- input.data[[ selected ]]

        if (class(item) != "slatePage")
          parent <- item$parent
        else
          parent <- item$name
      }

      group <- slateGroup(name = global.options$group.name.generator(),
                          parent = parent)
      addInputElement(group)

      shinyjs::click("inputs_add") # closes the dropdown menu
    })


    new.input.modal <- newInputModal("new_input")

    # handle the add input button
    observeEvent(input$inputs_add_input, {
      selected <- selected.inputs()

      req(length(selected) < 2)

      new.input.modal$show(
        title = "New Input",
        callback = function(name, type) {
          if (length(selected) == 0) {
            parent <- ".root"
          } else {
            item <- input.data[[ selected ]]

            if (class(item) == "slateInput")
              parent <- item$parent
            else
              parent <- item$name
          }

          new.input <- slateInput(name = name, type = type, parent = parent)
          addInputElement(new.input)
        })
    })


    # handle rename item button
    observeEvent(input$inputs_rename, {
      req(
        sel <- selected.inputs(),
        length(sel) == 1,
        item <- input.data[[ sel ]]
      )

      text.input.modal$show(
        title = "Rename Item",
        label = "Item Name",
        value = item$name,
        callback = function(value) {
          # rename item
          input.data[[ value ]] <- input.data[[ sel ]]
          input.data[[ value ]]$name <- value

          # update tree.data
          df <- tree.data() %>%
            mutate(name = ifelse(name == sel, value, name),
                   parent = ifelse(parent == sel, value, parent)) %>%
            set_rownames(.$name)

          tree.data(df)
        })
    })


    # handle remove item button
    observeEvent(input$inputs_remove, {
      req(
        sel <- selected.inputs(),
        length(sel) == 1,
        item <- input.data[[ sel ]]
      )

      tree.data(
        tree.data() %>% filter(name != sel)
      )
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
          # updateSelectInput(session, "select_output",
          #                   choices = names(blueprint.data$outputs),
          #                   selected = value)
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
          # updateSelectInput(session, "select_output",
          #                   choices = names(blueprint.data$outputs),
          #                   selected = value)
        })
    })

    observeEvent(input$remove_output, {
      selected <- which(names(blueprint.data$outputs) == input$select_output) - 1
      if (length(blueprint.data$outputs) > 1 && selected == 0)
        selected <- 1

      blueprint.data$outputs[[ input$select_output ]] <- NULL

      # selected <- names(blueprint.data$outputs)[ selected ]
      # updateSelectInput(session, "select_output",
      #                   choices = names(blueprint.data$outputs),
      #                   selected = selected)
    })



    # set the active blueprint and initialize everything
    setBlueprint <- function(bprint) {
      dlog()

      blueprint.data$name <- bprint$name
      blueprint.data$author <- bprint$author
      blueprint.data$category <- bprint$category
      blueprint.data$tags <- bprint$tags
      blueprint.data$source <- bprint$source
      blueprint.data$outputs <- bprint$outputs
      blueprint.data$imports <- bprint$imports
      blueprint.data$exports <- bprint$exports

      # regenerate group names
      gnames <-
        map_chr(seq_along(bprint$groups), ~global.options$group.name.generator()) %>%
        set_names(names(bprint$groups))

      bprint$groups %<>%
        map(~list_modify(., name = gnames[[ .$name ]])) %>%
        set_names(map(., "name"))

      bprint$inputs %<>%
        modify_if(~.$parent %in% names(gnames),
                  ~list_modify(., parent = gnames[[ .$parent ]]))

      tree.data(
        inputsToShinyTree(
          bprint$pages, bprint$groups, bprint$inputs
        ) %>%
          shinyTreeToDf
      )

      for (x in c(bprint$pages, bprint$groups, bprint$inputs))
        input.data[[ x$name ]] <- x
    }

    isolate(setBlueprint(blueprint.ini))

    list(
      blueprint = blueprint,
      slate.server = slate
    )
  })
}





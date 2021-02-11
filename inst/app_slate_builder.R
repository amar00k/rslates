


#
# Conversion utilities
#

# parseChoices <- function(choices.expression) {
#   if (is.null(choices.expression))
#     return(NULL)
#
#   tryCatch({
#     eval(parse(text = choices.expression))
#   },
#   error = function(e) {
#     return(NULL)
#   })
# }


# flat layout to st tree structure
layoutToTree <- function(layout, selected = "") {
  tree <- lapply(layout$pages, function(p) {
    p$groups <- lapply(p$groups, function(g) {
      g$name <- as.character(tags$i(seq.uid("group")))
      g
    })

    groups <- lapply(p$groups, function(g) {
      inputs <- lapply(g$inputs, function(i) {
        structure("", sttype = i$type, stclass = "input",
                  stinfo = i)
      }) %>% set_names(sapply(g$inputs, "[[", "name"))
      structure(inputs, sttype = "group", stclass = "group", stopened = TRUE,
                stinfo = g[ !(names(g) == "inputs") ])
    }) %>% set_names(sapply(p$groups, "[[", "name"))
    structure(groups, sttype = "page", stclass = "page", stopened = TRUE,
              stinfo = p[ !(names(p) == "groups") ])
  }) %>% set_names(sapply(layout$pages, "[[", "name"))

  attr(tree[[1]], "stselected") <- TRUE

  return(tree)
}


#' Convert jstree to blueprint layout
#'
#' @description
#' Must be carefull with JS transformations. Character vectors are converted
#' to lists... :(
#' In the future it might be a good idea to store node data separately from the tree.
#'
#' @param tree
#'
#' @return
#' @export
#'
#' @examples
treeToLayout <- function(tree) {
  pages <- lapply(tree, function(p) {
    page <- attr(p, "stinfo")

    if (length(names(p)) == 0)
      p$groups <- list()
    else
      page$groups <- lapply(p, function(g) {
        group <- attr(g, "stinfo")
        group$name <- ""

        if (length(names(g)) == 0)
          group$inputs <- list()
        else
          group$inputs <- lapply(g, function(i) {
            item <- attr(i, "stinfo")
            item$choices <- as.character(item$choices)
            item$wizards <- as.character(item$wizards)
            item
          }) %>% set_names(sapply(., "[[", "name"))

        return(group)
      }) %>% set_names(paste0("group_", 1:length(.)))

    return(page)
  }) %>% set_names(sapply(., "[[", "name"))

  return(list(pages = pages))
}


fixTree <- function(tree) {
  tree <- lapply(tree, function(x) {
    ax <- attributes(x)

    if (length(names(x)) > 0)
      x <- fixTree(x)

    attributes(x) <- ax
    attr(x, "sttype") <- attr(x, "stclass")

    return(x)
  }) %>% set_names(names(tree))

  return(tree)
}


addItemToTree <- function(tree, item, path = character(0), after=NULL, name=NULL) {
  node <- structure("", sttype = item$type, stclass = item$type,
                    stselected = FALSE, stopened = TRUE,
                    stinfo = item)

  if (is.null(name)) {
    name <- item$name
  }

  if (is.null(after)) {
    after <- if (length(path) > 0) length(names(tree[[ path ]])) else length(names(tree))
  }

  if (length(path) > 0) {
    atts <- attributes(tree[[ path ]])

    if (length(names(tree[[ path ]])) > 0)
      tree[[ path ]] <- append(tree[[ path ]], list(node), after)
    else
      tree[[ path ]] <- list(node)

    attributes(tree[[ path ]]) <- atts
    names(tree[[ path ]])[ after + 1 ] <- name
  } else {
    tree <- append(tree, list(node))
    names(tree)[ after + 1 ] <- name
  }

  return(tree)
}


updateTreeItem <- function(tree, item, path) {
  attr(tree[[ path ]], "stinfo") <- item

  ancestry <- path[ 0:(length(path) - 1) ]

  if (length(ancestry) > 0)
    names(tree[[ ancestry ]]) <- sapply(tree[[ ancestry ]], function(x) attr(x, "stinfo")$name)
  else
    names(tree) <- sapply(tree, function(x) attr(x, "stinfo")$name)

  return(tree)
}


#
# App
#
slateBuilderApp <- function(blueprint.ini = NULL, input.container = "tabset") {
  default.theme <- "solar"
  default.ace.theme <- "twilight"

  if (is.null(blueprint.ini)) {
    blueprint.ini <- slateBlueprint(
      title = "",
      input.layout = inputLayout(
        pages = list(inputPage(
          name = "Main",
          inputGroup()))))
  }

  builderUI <- function(id = NULL) {
    if (is.null(id))
      ns <- function(x) x
    else
      ns <- NS(id)


    imports.ui <- fluidRow(
      column(
        width = 4,
        selectInput(ns("select_import"),
                    label = "Imports",
                    selectize = FALSE,
                    choices = names(blueprint.ini$imports),
                    width = "100%",
                    size = 10),
        tags$div(
          class = "d-flex justify-content-start",
          actionButton(ns("add_import"), label = "Add"),
          actionButton(ns("rename_import"), class = "ml-2", label = "Rename"),
          actionButton(ns("remove_import"), class = "ml-auto", label = "Delete")
        )
      ),
      column(
        width = 8,
        #shinyjs::hidden(textInput(ns("output_id"), label = "")),
        #tags$h4(paste0(active$name)),
        tags$hr(),
        selectInput(
          ns("import_type"), label = "Type",
          choices = list("file", "url", "built-in"), selected = "file"
        ),
        textInput(ns("import_description"), label = "Description"),
        textInput(ns("import_filetype"), label = "Accepted File Types")
      )
    )

    layout.ui <- fluidRow(
      column(
        width = 4,
        wellPanel(
          style="overflow: auto;",
          shinyTree::shinyTree("layout_tree",
                               stripes = FALSE,
                               multiple = FALSE,
                               animation = FALSE,
                               contextmenu = FALSE,
                               dragAndDrop = TRUE,
                               wholerow = TRUE,
                               theme = "proton",
                               types =
                                 "{
                '#': { 'max_depth' : 3, 'valid_children' : ['page'] },
                'page' : { 'icon' : 'fa fa-folder', 'valid_children' : ['group'] },
                'group' : { 'icon' : 'fa fa-object-group', 'valid_children' : ['input'] },
                'input' : { 'icon' : 'fa fa-pen', 'valid_children' : [''] }
              }"
          )
        ),
        tags$div(
          class = "py-2 d-flex justify-content-start",
          shinyWidgets::dropdownButton(
            inputId = ns("layout_add"),
            label = "Add",
            circle = FALSE,
            inline = TRUE,
            actionLink(ns("layout_add_input"), label = "New Input"),
            actionLink(ns("layout_add_group"), label = "New Group"),
            actionLink(ns("layout_add_page"), label = "New Page")
          ),
          actionButton(ns("layout_rename"), class = "ml-2", label = "Rename"),
          actionButton(ns("layout_remove"), class = "ml-auto", label = "Delete")
        )
      ),
      column(
        width = 8,
        shinyjs::hidden(textInput(ns("input_id"), label = "")),
        tabsetPanel(
          id = ns("layout_tabs"),
          type = "hidden",
          tabPanel(
            title = "page",
            #tags$h4(paste0(item$name, " (Page)")),
            tags$hr(),
            textInput(ns("page_name"), label = "Name"),
            textAreaInput(ns("page_description"), label = "Description")
          ),
          tabPanel(
            title = "group",
            tags$h4("Group"),
            tags$hr(),
            selectInput(ns("group_layout"), label = "Layout",
                        choices = c("flow-2",
                                    "flow-3",
                                    "flow-4",
                                    "vertical")),
            textInput(ns("group_condition"), label = "Condition")
          ),
          tabPanel(
            title = "input",
            #tags$h4(paste0(item$name, " (Input)")),
            tags$hr(),
            tags$div(
              class = "slates-flow-3",
              selectInput(ns("input_type"), label = "Type",
                          selectize = TRUE,
                          choices = c("Select input type"="", "numeric", "logical", "character", "expression", "choices")),
              selectizeInput(
                ns("input_choices"), label = "Choices", choices = character(0),
                multiple = TRUE,
                options = list(
                  delimiter = '',
                  create = "function(input) { return { value: input, text: input } }"
                )
              ),
              textInput(ns("input_default"), label = "Default Value"),
              selectInput(ns("input_default_logical"), label = "Default Value",
                          choices = c(TRUE, FALSE)),
            ),
            selectizeInput(
              ns("input_wizards"), label = "Wizards",
              choices = names(wizard.list), multiple = TRUE
            ),
            textAreaInput(ns("input_description"), label = "Description")
          )
        )
      )
    )

    outputs.ui <- fluidRow(
      column(
        width = 4,
        selectInput(ns("select_output"),
                    label = "Outputs",
                    selectize = FALSE,
                    choices = names(blueprint.ini$outputs),
                    width = "100%",
                    size = 10),
        tags$div(
          class = "d-flex justify-content-start",
          actionButton(ns("add_output"), label = "Add"),
          actionButton(ns("rename_output"), class = "ml-2", label = "Rename"),
          actionButton(ns("remove_output"), class = "ml-auto", label = "Delete")
        )
      ),
      column(
        width = 8,
        shinyjs::hidden(textInput(ns("output_id"), label = "")),
        #tags$h4(paste0(active$name)),
        tags$hr(),
        selectInput(
          ns("output_type"), label = "Type",
          choices = list("Plot"="plot",
                         "Table (Default)"="table",
                         "Table (DataTables)"="data.table",
                         "Table (React Table)"="reactable",
                         "Text (Markdown)"="html",
                         "Print"="print")
        ),
        tags$p("Source Template"),
        shinyAce::aceEditor(
          ns("output_source"),
          mode = "r",
          autoComplete = "live",
          height = "200px",
          placeholder = "Enter output source code template..."
        )
      )
    )

    datasets.ui <- fluidRow(
      column(
        width = 4,
        selectInput(ns("select_dataset"),
                    label = "Datasets",
                    selectize = FALSE,
                    choices = names(blueprint.ini$datasets),
                    width = "100%",
                    size = 10),
        tags$div(
          class = "d-flex justify-content-start",
          actionButton(ns("add_dataset"), label = "Add"),
          actionButton(ns("rename_dataset"), class = "ml-2", label = "Rename"),
          actionButton(ns("remove_dataset"), class = "ml-auto", label = "Delete")
        )
      ),
      column(
        width = 8,
        shinyjs::hidden(textInput(ns("dataset_id"), label = "")),
        #tags$h4(paste0(active$name)),
        tags$hr(),
        flowLayout(
          checkboxInput(ns("dataset_export"), label = "Export Dataset"),
          textInput(ns("dataset_export_name"), label = "Export Name")
        ),
        tags$p("Source Template"),
        shinyAce::aceEditor(
          ns("dataset_source"),
          mode = "r",
          autoComplete = "live",
          height = "200px",
          placeholder = "Enter dataset source code template..."
        )
      )
    )

    export.ui <- fluidRow(
      tags$style(type='text/css', '#json_output { white-space: pre-wrap; max-height: 300px; }'),
      column(
        width = 6,
        h3("JSON"),
        verbatimTextOutput(ns("json_output")),
        radioButtons(ns("json_radio"), label = NULL,
                     choices = c("Compact", "Indented"),
                     inline = TRUE)
      )
    )

    ui <- fluidPage(
      shinyjs::useShinyjs(),
      shiny::bootstrapLib(bslib::bs_theme(bootswatch = default.theme, version = "4")),
      shiny::tags$link(rel = "stylesheet", type = "text/css", href = "slates.css"),
      thematic::thematic_shiny(),
      title = "Slate Builder",
      titlePanel("Slate Builder"),
      sidebarLayout(
        sidebarPanel = sidebarPanel(
          width = 2,
          tagList(
            selectInput(ns("select_theme"),
                        label = "Theme",
                        choices = bslib::bootswatch_themes(),
                        selected = default.theme),
            selectInput(ns("select_ace_theme"),
                        label = "Ace Editor Theme",
                        choices = shinyAce::getAceThemes(),
                        selected = default.ace.theme)
          )
        ),
        mainPanel = mainPanel(
          width = 10,
          #slateUI("slate_preview", blueprint, input.container),
          fileInput(ns("load_blueprint"), label = "Load Blueprint"),
          tags$div(
            class = "card",
            tags$div(
              class = "card-body",
              tags$div(
                class = "d-flex justify-content-between align-items-center",
                textInput(ns("blueprint_title"), label = "Blueprint Name", value = blueprint.ini$title),
                downloadButton(ns("save_blueprint"), label = "Save Blueprint")
              ),
              tabsetPanel(
                tabPanel(title = "Imports", tags$div(class = "container p-3", imports.ui)),
                tabPanel(title = "Inputs", tags$div(class = "container p-3", layout.ui)),
                tabPanel(title = "Datasets", tags$div(class = "container p-3", datasets.ui)),
                tabPanel(title = "Outputs", tags$div(class = "container p-3", outputs.ui)),
                tabPanel(title = "Export", tags$div(class = "container p-3",export.ui))
              )
            )
          ),
          tags$h3("Preview"),
          checkboxInput(ns("show_slate_preview"), label = "Show Preview", value = FALSE),
          tags$div(
            id = ns("slate_preview"),
            uiOutput(ns("slate_imports")),
            uiOutput(ns("slate_ui")),
          ),
          tags$h3("Session Info"),
          HTML(paste(capture.session.info(320), collapse="<br>"))
        )
      )
    )
  }

  builderServer <- function(input, output, session) {
    global.options <- reactiveValues(ace.theme = default.ace.theme)

    modal.text <- create_text_input_modal("modal_text", session)
    modal.new.input <- new_input_modal("modal_new_input", session)

    #
    # Themeing
    #

    observeEvent(input$select_ace_theme, {
      global.options$ace.theme <- input$select_ace_theme
    })

    observeEvent(input$select_theme, {
      print("select_theme")

      theme <- getCurrentTheme()

      if (!is.null(theme) && bslib::theme_bootswatch(theme) != input$select_theme) {
        theme <- bslib::bs_theme_update(theme, bootswatch = input$select_theme, version = "4")
        session$setCurrentTheme(theme)
      }
    })

    #
    # Blueprint data
    #

    blueprint.inputs <- reactiveVal()
    blueprint.outputs <- reactiveVal()
    blueprint.datasets <- reactiveVal()
    blueprint.imports <- reactiveVal()

    blueprint <- reactive({
      req(input$layout_tree)

      slateBlueprint(title = input$blueprint_title,
                     input.layout = treeToLayout(input$layout_tree),
                     outputs = blueprint.outputs(),
                     datasets = blueprint.datasets(),
                     imports = blueprint.imports())
    })


    #
    # Load/ Save
    #

    loadBlueprint <- function(bp) {
      print("loading")

      blueprint.imports(bp$imports)
      blueprint.inputs(bp$input.layout)
      blueprint.datasets(bp$datasets)
      blueprint.outputs(bp$outputs)

      if (length(bp$outputs) == 0)
        updateSelectInput(session, "select_output", choices = list())
      else
        updateSelectInput(session, "select_output",
                          choices = names(bp$outputs),
                          selected = names(bp$outputs)[1])

      if (length(bp$datasets) == 0)
        updateSelectInput(session, "select_dataset", choices = list())
      else
        updateSelectInput(session, "select_dataset",
                          choices = names(bp$datasets),
                          selected = names(bp$datasets)[1])

      if (length(bp$imports) == 0)
        updateSelectInput(session, "select_import", choices = list())
      else
        updateSelectInput(session, "select_import",
                          choices = names(bp$imports),
                          selected = names(bp$imports)[1])
    }

    # download button
    output$save_blueprint <- downloadHandler(
      filename = function() {
        paste0(gsub(" ", "_", input$blueprint_title), '.json')
      },
      content = function(con) {
        bprint <- simplifyBlueprint(restoreBlueprint(blueprint()))
        data <- jsonlite::toJSON(bprint, pretty = FALSE)

        writeLines(data, con)
      }
    )


    # upload blueprint
    observeEvent(input$load_blueprint, {
      data <- restoreBlueprint(blueprintFromJSON(input$load_blueprint$datapath))

      loadBlueprint(data)
    })


    #
    # Live slate
    #

    observe({
      shinyjs::toggle("slate_preview", condition = input$show_slate_preview)
    })

    slate.data <- reactiveValues()

    output$slate_imports <- renderUI({
      req(bprint <- blueprint())
      req(slate.data$module)

      print("slate_imports")

      inputs <- list()
      for (x in bprint$imports) {
        input.id <- paste0("slate_import_", x$name)

        if (x$description != "")
          label <- paste0(x$name, ": ", x$description)
        else
          label <- x$name

        if (x$type == "file") {
          inputs[[ x$name ]] <- fileInput(session$ns(input.id), label = label)
        } else if (x$type == "built-in") {
          datasets <- as.data.frame(data()$results)
          inputs[[ x$name ]] <- shinyWidgets::pickerInput(
            inputId = session$ns(input.id),
            label = label,
            choices = datasets$Item,
            choicesOpt = list(
              subtext = datasets$Title
            )
          )
        }

        name <- x$name
        observeEvent(input[[ input.id ]], {
          data <- input[[ input.id ]]

          slate.data$module$import.data[[ name ]]$data <- data
        })
      }

      do.call(flowLayout, unname(inputs))
    })

    output$slate_ui <- renderUI({
      req(blueprint())

      print("slate_ui")

      id <- paste0("slate_", seq.uid("preview"))

      isolate({
        if (!is.null(slate.data$module))
          slate.data$module$destroy()

        mod <- callModule(
          slateServer, id,
          blueprint = blueprint(),
          slate.options = list(
            envir = reactiveVal(new.env()),
            open.settings = TRUE
          ),
          global.options = global.options
        )
      })

      slate.data$module <- mod

      slateUI(id,
              blueprint(),
              input.container = input.container)
    })


    #
    # Import tab
    #

    observeEvent(input$select_import, {
      req(active <- blueprint.imports()[[ input$select_import ]])

      updateSelectInput(session, "import_type", selected = active$type)
      updateTextInput(session, "import_description", value = active$description)
      updateTextInput(session, "import_filetype", value = active$filetype)
    })

    updateImportVariable <- function(var.name, input.name) {
      req(sel <- input$select_import)
      #req(sel == input$output_id)
      req(imports <- blueprint.imports())
      req(imports[[ sel ]][[ var.name ]] != input[[ input.name ]])

      pprint("Update import", sel, ":", var.name, "=", input[[ input.name ]])

      imports[[ sel ]][[ var.name ]] <- input[[ input.name ]]

      blueprint.imports(imports)
    }

    observeEvent(input$import_type, updateImportVariable("type", "import_type"))
    observeEvent(input$import_filetype, updateImportVariable("filetype", "import_filetype"))
    observeEvent(input$import_description, updateImportVariable("description", "import_description"))

    observeEvent(input$add_import, {
      modal.text$show(
        query = "Import Name:",
        placeholder = "",
        callback = function(name) {
          imports <- blueprint.imports()
          imports[[ name ]] <- slateImport(name = name, type="file")
          names(imports) <- sapply(imports, "[[", "name")
          blueprint.imports(imports)
          updateSelectInput(session, "select_import", choices = names(imports),
                            selected = name)
        })
    })

    observeEvent(input$rename_import, {
      imports <- blueprint.imports()

      modal.text$show(
        title = "Rename Import",
        query = "Import Name",
        value = imports[[ input$select_import ]]$name,
        placeholder = "",
        callback = function(name) {
          imports[[ input$select_import ]]$name <- name
          names(imports) <- sapply(imports, "[[", "name")
          blueprint.imports(imports)
          updateSelectInput(session, "select_import", choices = names(imports),
                            selected = name)
        })
    })

    observeEvent(input$remove_import, {
      imports <- blueprint.imports()

      selected <- which(names(imports) == input$select_import) - 1
      if (length(imports) > 1 && selected == 0)
        selected <- 1

      imports[[ input$select_import ]] <- NULL
      blueprint.imports(imports)

      selected <- names(imports)[ selected ]
      updateSelectInput(session, "select_import", choices = names(imports),
                        selected = selected)
    })

    #
    # Input tab behaviour
    #

    # id of active in reactiveVal, to be set manually
    active.item.id <- reactiveVal(list(id = ""))
    active.item <- reactiveVal(NULL)


    # Inputs tree output
    output$layout_tree <- shinyTree::renderTree({
      # we only call this once; tree is updated with updateTree.
      print("redraw")
      #isolate(layoutToTree(blueprint.ini$input.layout))
      tree <- layoutToTree(blueprint.inputs())
    })

    # when tree changes
    observeEvent(input$layout_tree, {
      print("layout_tree: update active item")

      classid <- isolate(shinyTree::get_selected(input$layout_tree, format = "classid")[[1]])
      sel <- isolate(shinyTree::get_selected(input$layout_tree))

      isolate(
        current.id <- isolate(active.item.id()$id)
      )

      if (attr(classid, "id") != current.id) {
        active.item.id(list(
          time = Sys.time(),
          id = attr(classid, "id")
        ))
      }

      if (length(sel) < 1)
        return(NULL)

      sel <- sel[[1]]

      pprint("active_item:", attr(classid, "id"), as.character(sel))

      active.item(list(
        name = as.character(sel),
        id = attr(classid, "id"),
        type = attr(classid, "class"),
        ancestry = attr(sel, "ancestry"),
        item = attr(sel, "stinfo")
      ))
    })


    # observe selected item, and enable or disable buttons elements
    observe({
      print("active item: update buttons")
      req(item <- active.item()$item)

      if (is.null(item)) {
        shinyjs::hide("layout_add_input")
        shinyjs::hide("layout_add_group")
      } else {
        shinyjs::show("layout_add_group")
        shinyjs::toggle("layout_add_input", condition = (item$type %in% c("group", "input")))
        shinyjs::toggleState("layout_rename", condition = (item$type != "group"))
        shinyjs::toggleState("layout_remove", condition = !(item$type == "page" && item$name == "Main"))
      }
    })


    # handle the add page button
    observeEvent(input$layout_add_page, {
      modal.text$show(
        title = "New Input Page",
        query = "Page Title",
        placeholder = "",
        callback = function(title) {
          active <- active.item()
          tree <- input$layout_tree

          new.item <- inputPage(name = title)

          tree <- addItemToTree(tree, new.item)

          shinyTree::updateTree(session, "layout_tree", fixTree(tree))
        })
    })


    # handle the add group button
    observeEvent(input$layout_add_group, {
      active <- active.item()
      item <- active$item
      tree <- input$layout_tree

      if (item$type != "page")
        path <- active$ancestry[1]
      else
        path <- active$name

      new.item <- inputGroup()

      name <- as.character(tags$i(seq.uid("group")))
      tree <- addItemToTree(tree, new.item, path = path, name = name)

      shinyTree::updateTree(session, "layout_tree", fixTree(tree))
      shinyjs::click("layout_add") # closes the dropdown menu
    })


    # handle the add input button
    observeEvent(input$layout_add_input, {
      modal.new.input$show(
        callback = function(name, type) {
          active <- active.item()
          item <- active$item
          tree <- input$layout_tree

          if (item$type == "input")
            path <- active$ancestry
          else
            path <- c(active$ancestry, active$name)

          new.item <- slateInput(name = name, input.type = type, default = "")

          tree <- addItemToTree(tree, new.item, path = path)

          shinyTree::updateTree(session, "layout_tree", fixTree(tree))
        })
    })

    # handle rename item button
    observeEvent(input$layout_rename, {
      req(active <- active.item())

      modal.text$show(
        title = "Rename Input",
        query = "Input Name",
        value = active$item$name,
        placeholder = "",
        callback = function(name) {
          active <- active.item()
          tree <- input$layout_tree
          path <- c(active$ancestry, active$name)

          new.item <- active$item
          new.item$name <- name

          tree <- updateTreeItem(tree, new.item, path)
          shinyTree::updateTree(session, "layout_tree", fixTree(tree))
        })
    })


    # handle remove item button
    observeEvent(input$layout_remove, {
      req(item <- active.item())

      tree <- input$layout_tree
      branch <- if (length(item$ancestry) > 0) tree[[ item$ancestry ]] else tree
      ids <- sapply(branch, "attr", "id")
      branch[[ which(ids == item$id) ]] <- NULL

      if (length(item$ancestry) > 0)
        tree[[ item$ancestry ]] <- branch
      else
        tree <- branch

      shinyTree::updateTree(session, "layout_tree", fixTree(tree))
    })


    observeEvent(active.item.id(), {
      req(active.id <- active.item.id()$id)
      active <- active.item()
      item <- active$item

      updateTabsetPanel(session, "layout_tabs", selected = item$type)

      updateTextInput(session, "input_id", value = active$id)

      if (item$type == "page") {
        updateTextInput(session, "page_name", value = item$name)
        updateTextAreaInput(session, "page_description", value = item$description)
      } else if (item$type == "group") {
        updateSelectInput(session, "group_layout", selected = item$layout)
        updateTextInput(session, "group_condition", value = item$condition)
      } else if (item$type == "input") {
        updateSelectInput(session, "input_type", selected = item$input.type)
        updateSelectInput(session, "input_choices", choices = item$choices, selected = item$choices)
        updateTextInput(session, "input_default", value = item$default)
        updateSelectInput(session, "input_default_logical", selected = item$default)
        updateSelectizeInput(session, "input_wizards", selected = item$wizards)
        updateTextAreaInput(session, "input_description", value = item$description)
      }
    })


    observe({
      input$input_id

      shinyjs::toggle("input_choices", condition = (input$input_type == "choices"))
      shinyjs::toggle("input_default", condition = (input$input_type != "logical"))
      shinyjs::toggle("input_default_logical", condition = input$input_type == "logical")
    })


    updateInputVariable <- function(var.name, input.name, null.value = "") {
      req(active <- active.item())
      req(!is.null(active) && input$input_id == active$id)
      req(!identical(active$item[[ var.name ]], input[[ input.name ]]))
      req(tree <- input$layout_tree)

      new.val <- if (is.null(input[[ input.name ]])) null.value else input[[ input.name ]]

      pprint("Update input", active$item$name, ":", var.name, "=", new.val)

      active$item[[ var.name ]] <- new.val

      path <- c(active$ancestry, active$item$name)
      tree <- fixTree(updateTreeItem(tree, active$item, path))
      shinyTree::updateTree(session, "layout_tree", tree)
    }

    observeEvent(input$page_description, updateInputVariable("description", "page_description"))
    observeEvent(input$group_layout, updateInputVariable("layout", "group_layout"))
    observeEvent(input$group_condition, updateInputVariable("condition", "group_condition"))
    observeEvent(input$input_type, updateInputVariable("input.type", "input_type"))
    observeEvent(input$input_description, updateInputVariable("description", "input_description"))
    observeEvent(input$input_choices, updateInputVariable("choices", "input_choices"))
    observeEvent(input$input_default_logical, {
      if (input$input_type == "logical")
        updateInputVariable("default", "input_default_logical")
    })
    observeEvent(input$input_default, {
      if (input$input_type != "logical")
        updateInputVariable("default", "input_default")
    })
    observeEvent(input$input_wizards, updateInputVariable("wizards", "input_wizards", list()))


    #
    # Output tab
    #

    observeEvent(input$select_output, {
      req(active <- blueprint.outputs()[[ input$select_output ]])

      shinyAce::updateAceEditor(session, "output_source",
                                value = active$source)
      updateSelectInput(session, "output_type",
                        selected = active$type)
      updateTextInput(session, "output_id", value = input$select_output)
    })

    updateOutputVariable <- function(var.name, input.name) {
      req(sel <- input$select_output)
      req(sel == input$output_id)
      req(outputs <- blueprint.outputs())
      req(outputs[[ sel ]][[ var.name ]] != input[[ input.name ]])

      pprint("Update output", sel, ":", var.name, "=", input[[ input.name ]])

      outputs[[ sel ]][[ var.name ]] <- input[[ input.name ]]

      blueprint.outputs(outputs)
    }

    observeEvent(input$output_type, updateOutputVariable("type", "output_type"))
    observeEvent(input$output_source, updateOutputVariable("source", "output_source"))

    observeEvent(input$add_output, {
      modal.text$show(
        query = "Output Name:",
        placeholder = "",
        callback = function(name) {
          outputs <- blueprint.outputs()
          outputs[[ name ]] <- slateOutput(name = name, type="plot")
          names(outputs) <- sapply(outputs, "[[", "name")
          blueprint.outputs(outputs)
          updateSelectInput(session, "select_output", choices = names(outputs),
                            selected = name)
        })
    })

    observeEvent(input$rename_output, {
      outputs <- blueprint.outputs()

      modal.text$show(
        title = "Rename Output",
        query = "Output Name",
        value = outputs[[ input$select_output ]]$name,
        placeholder = "",
        callback = function(name) {
          outputs[[ input$select_output ]]$name <- name
          names(outputs) <- sapply(outputs, "[[", "name")
          blueprint.outputs(outputs)
          updateSelectInput(session, "select_output", choices = names(outputs),
                            selected = name)
        })
    })

    observeEvent(input$remove_output, {
      outputs <- blueprint.outputs()

      selected <- which(names(outputs) == input$select_output) - 1
      if (length(outputs) > 1 && selected == 0)
        selected <- 1

      outputs[[ input$select_output ]] <- NULL
      blueprint.outputs(outputs)

      selected <- names(outputs)[ selected ]
      updateSelectInput(session, "select_output", choices = names(outputs),
                        selected = selected)
    })


    #
    # Datasets tab
    #

    observeEvent(input$select_dataset, {
      req(active <- blueprint.datasets()[[ input$select_dataset ]])

      updateTextInput(session, "dataset_id", value = input$select_dataset)

      shinyAce::updateAceEditor(session, "dataset_source",
                                value = active$source)

      updateCheckboxInput(session, "dataset_export", value = active$export)
      updateTextInput(session, "dataset_export_name", value = active$export.name)
    })

    observe({
      shinyjs::toggle("dataset_export_name", condition = input$dataset_export)
    })

    updateDatasetVariable <- function(var.name, input.name) {
      req(sel <- input$select_dataset)
      req(sel == input$dataset_id)
      req(datasets <- blueprint.datasets())
      req(datasets[[ sel ]][[ var.name ]] != input[[ input.name ]])

      pprint("Update dataset", sel, ":", var.name, "=", input[[ input.name ]])

      datasets[[ sel ]][[ var.name ]] <- input[[ input.name ]]

      blueprint.datasets(datasets)
    }

    observeEvent(input$dataset_source, updateDatasetVariable("source", "dataset_source"))
    observeEvent(input$dataset_export, updateDatasetVariable("export", "dataset_export"))
    observeEvent(input$dataset_export_name, updateDatasetVariable("export.name", "dataset_export_name"))

    observeEvent(input$add_dataset, {
      modal.text$show(
        query = "Dataset Name:",
        placeholder = "",
        callback = function(name) {
          datasets <- blueprint.datasets()
          datasets[[ name ]] <- slateDataset(name = name, type="standalone")
          names(datasets) <- sapply(datasets, "[[", "name")
          blueprint.datasets(datasets)
          updateSelectInput(session, "select_dataset", choices = names(datasets),
                            selected = name)
        })
    })

    observeEvent(input$rename_dataset, {
      datasets <- blueprint.datasets()

      modal.text$show(
        title = "Rename Dataset",
        query = "Dataset Name",
        value = datasets[[ input$select_dataset ]]$name,
        placeholder = "",
        callback = function(name) {
          datasets[[ input$select_dataset ]]$name <- name
          names(datasets) <- sapply(datasets, "[[", "name")
          blueprint.datasets(datasets)
          updateSelectInput(session, "select_dataset", choices = names(datasets),
                            selected = name)
        })
    })

    observeEvent(input$remove_dataset, {
      datasets <- blueprint.datasets()

      selected <- which(names(datasets) == input$select_dataset) - 1
      if (length(datasets) > 1 && selected == 0)
        selected <- 1

      datasets[[ input$select_dataset ]] <- NULL
      blueprint.datasets(datasets)

      selected <- names(datasets)[ selected ]
      updateSelectInput(session, "select_dataset", choices = names(datasets),
                        selected = selected)
    })




    #
    # Export tab behaviour
    #

    output$json_output <- renderText({
      bprint <- simplifyBlueprint(restoreBlueprint(blueprint()))

      blueprintToJSON(bprint, pretty = input$json_radio == "Indented")
    })


    # initialize
    loadBlueprint(blueprint.ini)
  }

  shiny::shinyApp(builderUI(), builderServer)
}

blueprint <- getOption("rslates.builder.blueprint")
input.container <- getOption("rslates.builder.input.container")

slateBuilderApp(blueprint, input.container)


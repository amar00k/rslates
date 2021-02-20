


#
# Conversion utilities
#



# flat layout to st tree structure
layoutToTree <- function(layout, selected = "") {
  tree <- lapply(layout$pages, function(p) {
    groups <- lapply(p$children, function(g) {
      inputs <- lapply(g$children, function(i) {
        structure("", sttype = i$type, stclass = "input",
                  stinfo = i$id, stselected = i$id == selected)
      }) %>% set_names(sapply(g$children, "[[", "name"))
      structure(inputs, sttype = "group", stclass = "group", stopened = TRUE,
                stinfo = g$id, stselected = g$id == selected)
    }) %>% set_names(sapply(p$children, "[[", "name"))
    structure(groups, sttype = "page", stclass = "page", stopened = TRUE,
              stinfo = p$id, stselected = p$id == selected)
  }) %>% set_names(sapply(layout$pages, "[[", "name"))

  if (selected == "")
    attr(tree[[1]], "stselected") <- TRUE

  return(tree)
}


treeToLayout <- function(tree, flat.layout) {
  layout <- inputLayout()

  layout$pages <- lapply(tree, function(p) {
    page <- flat.layout[[ attr(p, "stinfo") ]]

    if (length(names(p)) == 0)
      page$children <- list()
    else
      page$children <- lapply(p, function(g) {
        group <- flat.layout[[ attr(g, "stinfo") ]]

        if (length(names(g)) == 0)
          group$children <- list()
        else
          group$children <- lapply(g, function(i) {
            flat.layout[[ attr(i, "stinfo") ]]
          }) %>% set_names(sapply(., "[[", "name"))

        return(group)
      }) %>% set_names(sapply(., "[[", "name"))

    return(page)
  }) %>% set_names(sapply(., "[[", "name"))

  return(layout)
}


printTree <- function(tree) {
  invisible(lapply(tree, function(p) {
    print(attr(p, "stinfo"))
    if (length(names(p)) > 0)
      lapply(p, function(g) {
        print(paste0("  ", attr(g, "stinfo")))
        if (length(names(g)) > 0)
          lapply(g, function(i) {
            print(paste0("    ", attr(i, "stinfo")))
          })
      })
  }))
}


# modals
newInputModal <- function(id, session) {
  ID <- function(x) paste0(id, "_", x)
  ns <- session$ns
  input <- session$input

  ui.fun <- function(...) {
    tagList(
      textInput(ns(ID("name_input")), label = "Input Name", value = ""),
      selectInput(ns(ID("type_input")), label = "Input Type",
                  selectize = TRUE,
                  choices = c("Select input type"="",
                              names(input.handlers)),
                  selected = "")

    )
  }

  submit.fun <- function() {
    list(name = input[[ ID("name_input") ]],
         type = input[[ ID("type_input") ]])
  }

  accept.observer <- observe({
    name <- input[[ ID("name_input") ]]
    type <- input[[ ID("type_input") ]]

    shinyjs::disable(ID("btn_ok"))

    req(name, type)

    if (name != "" && type != "")
      shinyjs::enable(ID("btn_ok"))
  })

  slatesModal(
    id, session,
    submit.fun = submit.fun,
    ui.fun = ui.fun,
    observers = list(accept.observer),
    focus = ID("name_input")
  )
}


#
# App
#
slateBuilderApp <- function(blueprint.ini = NULL) {
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
      ns <- identity
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
        style = "height: 400px; overflow-y: auto;",
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
        style = "height: 400px; overflow-y: auto;",
        width = 8,
        shinyjs::hidden(textInput(ns("input_id"), label = "")),
        uiOutput(ns("layout_item_ui"))
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
        style = "height: 400px; overflow-y: auto;",
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
        style = "height: 400px; overflow-y: auto;",
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
      shiny::bootstrapLib(),
      shiny::tags$link(rel = "stylesheet", type = "text/css", href = "slates.css"),
      thematic::thematic_shiny(),
      theme = loadTheme(getOption("rslates.default.theme")),
      title = "Slate Builder",
      titlePanel("Slate Builder"),
      sidebarLayout(
        sidebarPanel = sidebarPanel(
          width = 2,
          tagList(
            selectInput(ns("select_theme"),
                        label = "Theme",
                        choices = getOption("rslates.themes"),
                        selected = getOption("rslates.default.theme")),
            selectInput(ns("select_ace_theme"),
                        label = "Ace Editor Theme",
                        choices = shinyAce::getAceThemes(),
                        selected = default.ace.theme)
          )
        ),
        mainPanel = mainPanel(
          width = 10,
          fileInput(ns("load_blueprint"), label = "Load Blueprint"),
          tags$div(
            class = "card",
            #style = "height: 620px;",
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
          HTML(paste(captureSessionInfo(320), collapse="<br>"))
        )
      )
    )
  }

  builderServer <- function(input, output, session) {
    global.options <- reactiveValues(ace.theme = default.ace.theme)
    global.options$group.name.generator <- sequenceGenerator("group")

    modal.text <- slatesTextModal("modal_text", session)
    modal.new.input <- newInputModal("modal_new_input", session)

    blueprint.ini <- reactiveVal(blueprint.ini)

    #
    # Themeing
    #

    observeEvent(input$select_ace_theme, {
      global.options$ace.theme <- input$select_ace_theme
    })

    observeEvent(input$select_theme, {
      print("select_theme")

      theme <- loadTheme(input$select_theme)
      session$setCurrentTheme(theme)
    })

    #
    # Blueprint data
    #
    blueprint.inputs <- reactiveVal()
    blueprint.outputs <- reactiveVal()
    blueprint.datasets <- reactiveVal()
    blueprint.imports <- reactiveVal()

    flat.layout <- reactiveVal()

    flat.input.layout <- reactive({
      flat <- flattenInputLayout(blueprint.inputs())
      names(flat) <- paste0(sapply(flat, "[[", "type"), "_", sapply(flat, "[[", "name"))
      return(flat)
    })

    blueprint <- reactive({
      req(
        blueprint.inputs(),
        blueprint.outputs(),
        blueprint.datasets(),
        blueprint.imports()
      )

      print("create blueprint structure for export")

      input.layout <- traverseInputLayout(blueprint.inputs(), callback = function(x, a) {
        x$ancestry <- NULL
        return(x)
      })

      blueprint <- slateBlueprint(title = input$blueprint_title,
                                  input.layout = input.layout,
                                  outputs = blueprint.outputs(),
                                  datasets = blueprint.datasets(),
                                  imports = blueprint.imports())

      restoreBlueprint(blueprint)
    })


    #
    # Load/ Save
    #

    # load blueprint in blueprint.ini
    observeEvent(blueprint.ini(), {
      req(blueprint <- blueprint.ini())

      print("loading")

      # setup a sequence generator and rename groups
      # to ensure all groups have unique names
      global.options$group.name.generator <- sequenceGenerator("group")

      input.layout <- traverseInputLayout(blueprint$input.layout, function(x, ancestry) {
        x$ancestry <- ancestry

        if (x$type == "group")
          x$name <- global.options$group.name.generator()

        return(x)
      })

      blueprint.inputs(input.layout)
      blueprint.imports(blueprint$imports)
      blueprint.datasets(blueprint$datasets)
      blueprint.outputs(blueprint$outputs)

      if (length(blueprint$outputs) == 0)
        updateSelectInput(session, "select_output", choices = list())
      else
        updateSelectInput(session, "select_output",
                          choices = names(blueprint$outputs),
                          selected = names(blueprint$outputs)[1])

      if (length(blueprint$datasets) == 0)
        updateSelectInput(session, "select_dataset", choices = list())
      else
        updateSelectInput(session, "select_dataset",
                          choices = names(blueprint$datasets),
                          selected = names(blueprint$datasets)[1])

      if (length(blueprint$imports) == 0)
        updateSelectInput(session, "select_import", choices = list())
      else
        updateSelectInput(session, "select_import",
                          choices = names(blueprint$imports),
                          selected = names(blueprint$imports)[1])
    })


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
      blueprint.ini(restoreBlueprint(blueprintFromJSON(input$load_blueprint$datapath)))
    })


    #
    # Live slate
    #

    observe({
      shinyjs::toggle("slate_preview", condition = input$show_slate_preview)
    })

    slate.data <- reactiveValues()

    # output$slate_imports <- renderUI({
    #   req(bprint <- blueprint())
    #   req(slate.data$module)
    #
    #   print("slate_imports")
    #
    #   inputs <- list()
    #   for (x in bprint$imports) {
    #     input.id <- paste0("slate_import_", x$name)
    #
    #     if (x$description != "")
    #       label <- paste0(x$name, ": ", x$description)
    #     else
    #       label <- x$name
    #
    #     if (x$type == "file") {
    #       inputs[[ x$name ]] <- fileInput(session$ns(input.id), label = label)
    #     } else if (x$type == "built-in") {
    #       datasets <- as.data.frame(data()$results)
    #       inputs[[ x$name ]] <- shinyWidgets::pickerInput(
    #         inputId = session$ns(input.id),
    #         label = label,
    #         choices = datasets$Item,
    #         choicesOpt = list(
    #           subtext = datasets$Title
    #         )
    #       )
    #     }
    #
    #     name <- x$name
    #     observeEvent(input[[ input.id ]], {
    #       data <- input[[ input.id ]]
    #
    #       slate.data$module$import.data[[ name ]]$data <- data
    #     })
    #   }
    #
    #   do.call(flowLayout, unname(inputs))
    # })

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
              input.container = options()$rslates.input.container)
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
        label = "Import Name:",
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
    # Input tab
    #

    # id of active in reactiveVal, to be set manually
    active.item.id <- reactiveVal("")
    active.item <- reactiveVal(NULL)

    # Inputs tree output
    output$layout_tree <- shinyTree::renderTree({
      req(blueprint.inputs())

      print("redraw tree")

      isolate(selected <- active.item.id())

      tree <- layoutToTree(blueprint.inputs(), selected = selected)
    })
    outputOptions(output, "layout_tree", suspendWhenHidden = FALSE)


    # when tree changes
    observeEvent(input$layout_tree, {
      req(sel <- shinyTree::get_selected(input$layout_tree)[[1]])
      req(classid <- shinyTree::get_selected(input$layout_tree, format = "classid")[[1]])
      isolate(current.id <- active.item.id())

      # update active item
      if (attr(sel, "stinfo") != current.id) {
        pprint("set active item id:", attr(sel, "stinfo"))

        active.item.id(attr(sel, "stinfo"))
      }

      item <- flat.input.layout()[[ active.item.id() ]]

      if (!identical(item, isolate(active.item()))) {
        pprint("reset active item:", item$id, "(", item$type, ")")

        active.item(item)
      }


      # handle drag drag-and-drop reordering
      layout <- blueprint.inputs()
      flat <- flat.input.layout()
      tree <- input$layout_tree

      # compute layout from tree
      layout <- treeToLayout(tree, flat)

      if (!identical(layout, blueprint.inputs())) {
        blueprint.inputs(layout)
      }
    })


    # list of builderItemServer
    layout.item.servers <- reactiveValues()
    active.layout.server <- reactiveVal(NULL)

    # observe active.item.id and switch to the corresponding
    # properties page. Create the page if necessary and add to
    # layout.item.servers.
    observeEvent(active.item.id(), {
      req(
        active.id <- active.item.id(),
        item <- isolate(active.item()),
      )

      active.server <- isolate(active.layout.server())
      if (!is.null(active.server))
        active.server$redraw.default.ui(runif(1))

      server <- layout.item.servers[[ active.id ]]

      if (is.null(server)) {
        server <- builderItemServer(session$ns(paste0("builder_", item$id)),
                                    item, global.options)
      }

      layout.item.servers[[ active.id ]] <- server
      active.layout.server(server)
    })

    # render the active item's properties page.
    # also listen to changes in input.type and update the ui.
    output$layout_item_ui <- renderUI({
      req(
        active.server <- active.layout.server()
      )

      active.server$need.redraw()          # listen to changes
      active.server$redraw.default.ui(runif(1)) # send signal

      active.server$createUI()
    })


    # observe changes in layout.item.servers items and
    # update blueprint.inputs reactiveVal if any items have changes
    observe({
      flat <- isolate(flat.input.layout())
      layout <- isolate(blueprint.inputs())

      itemsIdentical <- function(item1, item2) {
        item1$children <- NULL
        item2$children <- NULL

        identical(item1, item2)
      }

      changed <- c()
      for (name in names(layout.item.servers)) {
        item <- layout.item.servers[[ name ]]$item()

        # this comparison assumes item has the ancestry property
        old.item <- flat[[ item$id ]]
        if (!itemsIdentical(item, old.item)) {
          if (item$type == "page")
            item$children <- old.item$children
          else if (item$type == "group")
            item$children <- old.item$children

          layout <- updateInputLayoutItem(layout, item, item$ancestry)

          changed <- c(changed, item$id)
        }
      }

      if (length(changed) > 0) {
        pprint("Items changed:", paste(changed, collapse=", "))

        blueprint.inputs(layout)
      }
    })


    # observe selected item, and enable or disable buttons elements
    observe({
      req(item <- active.item())

      if (is.null(item)) {
        shinyjs::hide("layout_add_input")
        shinyjs::hide("layout_add_group")
      } else {
        shinyjs::show("layout_add_group")
        shinyjs::toggle("layout_add_input", condition = (item$type %in% c("group", "input")))
        shinyjs::toggleState(
          "layout_rename",
          condition = (item$type != "group") && !(item$type == "page" && item$name == "Main"))
        shinyjs::toggleState("layout_remove", condition = !(item$type == "page" && item$name == "Main"))
      }
    })


    # handle the add page button
    observeEvent(input$layout_add_page, {
      modal.text$show(
        title = "New Input Page",
        label = "Page Title",
        placeholder = "",
        callback = function(title) {
          new.item <- inputPage(name = title)

          blueprint.inputs(updateInputLayoutItem(blueprint.inputs(), new.item))
        })
    })


    # handle the add group button
    observeEvent(input$layout_add_group, {
      req(item <- active.item())

      if (item$type != "page")
        path <- item$ancestry[1]
      else
        path <- item$name

      new.item <- inputGroup(name = global.options$group.name.generator())
      new.item$ancestry <- path[1]

      blueprint.inputs(updateInputLayoutItem(blueprint.inputs(), new.item, path))

      shinyjs::click("layout_add") # closes the dropdown menu
    })


    # handle the add input button
    observeEvent(input$layout_add_input, {
      req(item <- active.item())

      modal.new.input$show(
        callback = function(name, type) {
          if (item$type == "input")
            path <- item$ancestry
          else
            path <- c(item$ancestry, item$name)

          new.item <- slateInput(name = name, input.type = type)

          blueprint.inputs(updateInputLayoutItem(blueprint.inputs(), new.item, path))
        })
    })


    # handle rename item button
    observeEvent(input$layout_rename, {
      req(item <- active.item())

      modal.text$show(
        title = "Rename Input",
        label = "Input Name",
        value = active$item$name,
        placeholder = "",
        callback = function(name) {
          new.item <- item
          new.item$name <- name

          blueprint.inputs(
            updateInputLayoutItem(blueprint.inputs(), new.item, item$ancestry, item$name)
          )
        })
    })


    # handle remove item button
    observeEvent(input$layout_remove, {
      req(item <- active.item())

      blueprint.inputs(
        updateInputLayoutItem(blueprint.inputs(), NULL, item$ancestry, item$name)
      )
    })


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


    # # initialize
    # loadBlueprint(blueprint.ini)
  }

  if (options()$rslates.run.themer == TRUE)
    bslib::run_with_themer(shiny::shinyApp(builderUI(), builderServer))
  else
    shiny::shinyApp(builderUI(), builderServer)
}

blueprint <- getOption("rslates.builder.blueprint")
#input.container <- getOption("rslates.builder.input.container")

slateBuilderApp(blueprint)





#
# Conversion utilities
#



# flat layout to st tree structure
layoutToTree <- function(layout, selected = "") {
  tree <- lapply(layout$pages, function(p) {
    groups <- lapply(p$groups, function(g) {
      inputs <- lapply(g$inputs, function(i) {
        structure("", sttype = i$type, stclass = "input",
                  stinfo = i$id, stselected = i$id == selected)
      }) %>% set_names(sapply(g$inputs, "[[", "name"))
      structure(inputs, sttype = "group", stclass = "group", stopened = TRUE,
                stinfo = g$id, stselected = g$id == selected)
    }) %>% set_names(sapply(p$groups, "[[", "name"))
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
      page$groups <- list()
    else
      page$groups <- lapply(p, function(g) {
        group <- flat.layout[[ attr(g, "stinfo") ]]

        if (length(names(g)) == 0)
          group$inputs <- list()
        else
          group$inputs <- lapply(g, function(i) {
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
                          choices = names(input.handlers)),
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
      shinyjs::extendShinyjs(
        functions = "focus",
        text = "
          shinyjs.focus = function(e_id) {
            document.getElementById(e_id).focus();
          }"
      ),
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
          fileInput(ns("load_blueprint"), label = "Load Blueprint"),
          tags$div(
            class = "card",
            style = "height: 620px;",
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

    flat.input.layout <- reactive({
      flat <- flattenInputLayout(blueprint.inputs())
      names(flat) <- paste0(sapply(flat, "[[", "type"), "_", sapply(flat, "[[", "name"))
      return(flat)
    })

    blueprint <- reactive({
      req(input$layout_tree)

      req(blueprint.inputs(),
          blueprint.outputs(),
          blueprint.datasets(),
          blueprint.imports())

      slateBlueprint(title = input$blueprint_title,
                     input.layout = blueprint.inputs(),
                     outputs = blueprint.outputs(),
                     datasets = blueprint.datasets(),
                     imports = blueprint.imports())
    })


    #
    # Load/ Save
    #

    # load blueprint in blueprint.ini
    observeEvent(blueprint.ini(), {
      req(bprint <- blueprint.ini())

      print("loading")

      # setup a sequence generator and rename groups
      # to ensure all groups have unique names
      global.options$group.name.generator <- sequenceGenerator("group")

      input.layout <- traverseInputLayout(bprint$input.layout, function(x, ...) {
        if (x$type == "group")
          x$name <- global.options$group.name.generator()

        return(x)
      })

      blueprint.inputs(input.layout)
      blueprint.imports(bprint$imports)
      blueprint.datasets(bprint$datasets)
      blueprint.outputs(bprint$outputs)

      if (length(bprint$outputs) == 0)
        updateSelectInput(session, "select_output", choices = list())
      else
        updateSelectInput(session, "select_output",
                          choices = names(bprint$outputs),
                          selected = names(bprint$outputs)[1])

      if (length(bprint$datasets) == 0)
        updateSelectInput(session, "select_dataset", choices = list())
      else
        updateSelectInput(session, "select_dataset",
                          choices = names(bprint$datasets),
                          selected = names(bprint$datasets)[1])

      if (length(bprint$imports) == 0)
        updateSelectInput(session, "select_import", choices = list())
      else
        updateSelectInput(session, "select_import",
                          choices = names(bprint$imports),
                          selected = names(bprint$imports)[1])
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
    # Input tab behaviour
    #

    # id of active in reactiveVal, to be set manually
    active.item.id <- reactiveVal(list(id = ""))
    active.item <- reactiveVal(NULL)


    # Inputs tree output
    output$layout_tree <- shinyTree::renderTree({
      req(blueprint.inputs())

      print("redraw tree")

      isolate(active <- active.item())

      if(!is.null(active))
        selected <- active$item$id
      else
        selected <- ""

      tree <- layoutToTree(blueprint.inputs(), selected = selected)
    })
    outputOptions(output, "layout_tree", suspendWhenHidden = FALSE)


    # when tree changes
    observeEvent(input$layout_tree, {
      req(sel <- shinyTree::get_selected(input$layout_tree)[[1]])
      req(classid <- shinyTree::get_selected(input$layout_tree, format = "classid")[[1]])
      isolate(current.id <- active.item.id()$id)

      # update active item
      if (sel != current.id) {
        active.item.id(list(
          time = Sys.time(),
          id = as.character(sel)
        ))

        item <- flat.input.layout()[[ paste0(attr(classid, "stclass"), "_", sel) ]]

        active.item(list(
          ancestry = attr(sel, "ancestry"),
          item = item
        ))

        pprint("update active item:", active.item()$item$name)
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


    # observe selected item, and enable or disable buttons elements
    observe({
      print("update buttons")
      req(item <- active.item()$item)

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
      req(active <- active.item())

      if (active$item$type != "page")
        path <- active$ancestry[1]
      else
        path <- active$item$name

      new.item <- inputGroup(name = global.options$group.name.generator())

      blueprint.inputs(updateInputLayoutItem(blueprint.inputs(), new.item, path))

      shinyjs::click("layout_add") # closes the dropdown menu
    })


    # handle the add input button
    observeEvent(input$layout_add_input, {
      req(active <- active.item())

      modal.new.input$show(
        callback = function(name, type) {
          if (active$item$type == "input")
            path <- active$ancestry
          else
            path <- c(active$ancestry, active$item$name)

          new.item <- slateInput(name = name, input.type = type, default = "")

          blueprint.inputs(updateInputLayoutItem(blueprint.inputs(), new.item, path))
        })
    })


    # handle rename item button
    observeEvent(input$layout_rename, {
      req(active <- active.item())

      modal.text$show(
        title = "Rename Input",
        label = "Input Name",
        value = active$item$name,
        placeholder = "",
        callback = function(name) {
          path <- active$ancestry

          new.item <- active$item
          new.item$name <- name

          blueprint.inputs(
            updateInputLayoutItem(blueprint.inputs(), new.item, path, active$item$name)
          )
        })
    })


    # handle remove item button
    observeEvent(input$layout_remove, {
      req(active <- active.item())

      path <- active$ancestry

      blueprint.inputs(
        updateInputLayoutItem(blueprint.inputs(), NULL, path, active$item$name)
      )
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

        if (!is.null(names(item$choices)))
          choices.strings <- paste(names(item$choices), item$choices, sep = "=")
        else
          choices.strings <- item$choices

        updateSelectInput(session, "input_choices",
                          choices = choices.strings,
                          selected = choices.strings)

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


    choices.to.list <- function(choices) {
      if (all(grepl("=", choices))) {
        strsplit(choices, split="=") %>%
          { setNames(lapply(., "[[", 2), sapply(., "[[", 1)) }
      } else {
        as.list(choices) %>% unname
      }
    }


    updateInputVariable <- function(var.name, input.name,
                                    null.value = "",
                                    transform.fun = identity) {
      req(active <- active.item())
      req(!identical(active$item[[ var.name ]], transform.fun(input[[ input.name ]])))

      new.val <- if (is.null(input[[ input.name ]])) null.value else input[[ input.name ]]

      pprint("Update input", active$item$name, ":", var.name, "=", paste(new.val, collapse=", "))

      active$item[[ var.name ]] <- transform.fun(new.val)

      path <- c(active$ancestry, active$item$name)
      blueprint.inputs(updateInputLayoutItem(blueprint.inputs(), active$item, path))
    }

    observeEvent(input$page_description, updateInputVariable("description", "page_description"))
    observeEvent(input$group_layout, updateInputVariable("layout", "group_layout"))
    observeEvent(input$group_condition, updateInputVariable("condition", "group_condition"))
    observeEvent(input$input_type, updateInputVariable("input.type", "input_type"))
    observeEvent(input$input_description, updateInputVariable("description", "input_description"))
    observeEvent(input$input_choices, updateInputVariable("choices", "input_choices",
                                                          transform.fun = choices.to.list))
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




# https://glin.github.io/reactable/articles/examples.html#basic-usage
#






#' Launch R Slates Application
#'
#' @param ...
#'
#' @return the shiny app.
#' @export
slatesApp <- function(...) {

    # local.projects.ui <- wellPanel(
    #     id="local-projects",
    #     shiny::tags$b("Local projects:"),
    #     uiOutput("ui_local_projects")
    # )

    # google.drive.ui <- wellPanel(
    #     id="google-drive",
    #     actionLink("gdrive_connect", label = "Connect to Google Drive", icon=icon("google-drive")),
    #     uiOutput("ui_gdrive_projects")
    # )

    # dropbox.ui <- wellPanel(
    #     id="dropbox",
    #     actionLink("dropbox_connect", label = "Connect to Dropbox", icon=icon("dropbox")),
    #     uiOutput("ui_dropbox_projects")
    # )
    home.page.ui <- tagList(
        fluidRow(
            column(6,
                   h2("Welcome"),
                   helpText("Welcome to Slates."),
                   h2("News"),
                   helpText("No news.")
            ),
            column(6,
                   uiOutput("ui_local_projects"),
                   tags$br(),
                   actionButton("btn_new_project", label = "New Project")
                   #open.projects.ui,
                   #tags$hr(),
                   #google.drive.ui,
                   #dropbox.ui
            )
        )
    )

    slatesUI <- tagList(
        shinyjs::useShinyjs(),
        shiny::bootstrapLib(bslib::bs_theme(bootswatch = "solar", version = "4")),
        shinyStore::initStore("store", "rslates"),
        fluidPage(
            #shinythemes::themeSelector(),
            #theme = shinythemes::shinytheme("darkly"),
            title = "Slates",
            tags$div(
                id="title",
                #tags$img(src = "logo.png"),
                shiny::tags$h2("Slates")
            ),
            tags$br(),
            uiOutput("main")
        ),
        shiny::tags$link(rel = "stylesheet", type = "text/css", href = "slates.css")
    )


    # Define server logic
    slatesServer <- function(input, output, session) {

        # Initialize the session data object
        session.data <- reactiveValues()

        session.data$slate.blueprints <- options()$slate.blueprints
        isolate(print(session.data$slate.blueprints))
        session.data$active.projects <- list()

        # retrieve list of projects stored locally in browser cache
        session.data$storedProjects <- reactive({
            store <- input$store

            lapply(store, function(x) {
                if (!(class(x) == "list") | (x$type != "project"))
                    return(NULL)

                x$date.created <- as.POSIXct(x$date.created)
                x$date.modified <- as.POSIXct(x$date.modified)
                x$date.saved <- as.POSIXct(x$date.saved)

                return(x)
            })
        })

        # keep a list of observers
        observers <- reactiveValues()

        # used to tell the application to switch to a specific project tab rendering the main menu
        switch.to.project <- reactiveVal(NULL)

        # create modals used in main page
        modal.create.project <- create_new_project_modal("create_project_modal", session)

        # Main menu
        output$main <- renderUI({
            nav.list <- list(
                id = "main_menu",
                widths = c(2, 10),
                well = FALSE,
                tabPanel(
                    title = "Home",
                    value = "home",
                    icon = icon("home"),
                    home.page.ui
                )
            )

            if (length(session.data$active.projects) > 0) {
                project.panels <- lapply(session.data$active.projects, function(x) {
                    tabPanel(
                        title = x$title,
                        value = x$uid,
                        icon = icon("file-alt"),
                        x$ui
                    )
                })

                nav.list <- append(nav.list, c("Active projects", unname(project.panels)))
            }

            # if switch.to.project is set (not NULL), jump to that tab
            isolate({
                if (!is.null(switch.to.project())) {
                    nav.list$selected <- switch.to.project()
                    switch.to.project(NULL)
                }
            })

            do.call(navlistPanel, nav.list)
        })

        # open a project
        openProject <- function(project) {
            if (!(project$uid %in% names(session.data$active.projects))) {
                # create the project ui
                project$ui <- slates_editUI(project$editor.id,
                                            project)

                project <- callModule(module = slates_editServer,
                                      id = project$editor.id,
                                      project,
                                      session.data)

                # add to active project list
                session.data$active.projects[[ project$uid ]] <- project

                #project$ui <- NULL
                #print(str(project))
            }

            # tell the main menu to switch to this project's tab
            switch.to.project(project$uid)
        }

        # Button to create new project
        observeEvent(input$btn_new_project, {
            # show the create project modal
            modal.create.project$show(callback = function(title, authors) {
                # create new project
                uid <- b64.uid(32)
                now <- Sys.time()

                project <- list(
                    type = "project",
                    uid = uid,
                    title = title,
                    authors = authors,
                    date.created = now,
                    date.modified = now,
                    date.saved = NULL,
                    datasets = list(),
                    slates = list(),
                    editor.id = paste0(uid, "_editor")
                )

                openProject(project)
            }, title = "New Project")
        })

        # show list of projects in local cache
        output$ui_local_projects <- renderUI({
            # get the total size of stored objects
            store <- isolate(input$store)
            size <- utils:::format.object_size(nchar(jsonlite::toJSON(store)), units = "auto")

            # get stored projects
            stored <- session.data$storedProjects()
            stored <- stored[ rev(order(sapply(stored, "[[", "date.modified"))) ]

            # get list of active projects uids
            active.uids <- names(session.data$active.projects)

            # make list items to display
            items <- verticalLayout(lapply(stored, function(x) {
                label <- x$title

                if (x$uid %in% active.uids)
                    label <- paste0(label, " (active)")

                # time difference since last save
                dt <- difftime(Sys.time(), x$date.saved, units = "auto")

                # create an observer to open project, or switch to the project tab if already open
                link.id <- paste0(x$uid, "_open")
                if (!(link.id %in% names(observers))) {
                    observers[[ link.id ]] <- observeEvent(input[[ link.id ]], {
                        openProject(x)
                    })
                }

                tags$li(actionLink(link.id, label = label),
                        "|",
                        paste("Saved", format(dt, digits = 1), "ago."))
            }))

            # update this panel every 30 seconds
            invalidateLater(30000)

            tagList(tags$h2("Local Projects"),
                    tags$p("Total storage used: ", size),
                    items)
        })


        # save modified project(s) to local storage
        observe({
            active <- session.data$active.projects

            for (x in active) {
                print(x$state())

                dt <- as.numeric(difftime(x$date.modified, x$date.saved, units = "secs"))

                # in case project was never saved before
                if (length(dt) == 0)
                    dt <- 100 # arbitrary positive integer

                if (dt > 0) {
                    now <- Sys.time()
                    isolate(session.data$active.projects[[ x$uid ]]$date.saved <- now)

                    x$date.saved <- now
                    x$ui <- NULL

                    json <- jsonlite::toJSON(x)

                    print(json)

                    shinyStore::updateStore(session, name = x$uid, value = json)
                }
            }
        })




        # observe({
        #     session.data$active.projects
        #
        #
        # })

        # local.project <- isolate(input$store$project)
        #
        # # create editor for initial project
        # project.uid <- ruid.64(32)
        # session.data <- callModule(slates_editServer, "slates_edit", session.data,
        #                            local.project$project.name, local.project$slates)


        # output$ui_open_projects <- renderUI({
        #     pname <- local.project$project.name
        #
        #     blist <- lapply(1:3, function(x) {
        #        actionLink(session$ns(paste0("open_project_", x)), label = pname)
        #     })
        #
        #     df <- data.frame(Project = sapply(blist, as.character))
        #
        #     HTML(knitr::kable(df, escape = FALSE))
        # })
        #
        # output$ui_local_projects <- renderUI({
        #     tagList(
        #         shiny::tags$i("No projects found.")
        #     )
        # })


        # output$ui_dropbox_projects <- renderUI({
        #     tagList(
        #         shiny::tags$i("Please sign in to your Dropbox account to enable loading and saving projects.")
        #     )
        # })




        #
        # Google Drive stuff
        #

        # options(gargle_oauth_cache = TRUE)
        # modal.gg.save <- create_gg_save_modal(NULL, "gg_save_modal", input, output, session)
        #
        # # modal to save to google drive
        # gdrive_user <- reactiveVal(
        #     if (googledrive::drive_has_token()) googledrive::drive_user() else NULL
        # )
        #
        # observeEvent(input$button_save_gg, {
        #     modal.gg.save$show(callback = function() {
        #         print("ok")
        #     })
        # })
        #
        # observeEvent(input$gdrive_connect, {
        #     if (!googledrive::drive_has_token() | is.null(gdrive_user())) {
        #         googledrive::drive_auth()
        #     } else {
        #         googledrive::drive_deauth()
        #     }
        #
        #     gdrive_user(googledrive::drive_user())
        # })
        #
        # observe({
        #     if (is.null(gdrive_user())) {
        #         txt <- "Connect to Google Drive"
        #     } else {
        #         txt <- "Logout from Google Drive"
        #     }
        #
        #     updateActionLink(session, "gdrive_connect", label = txt)
        # })

        # output$ui_gdrive_projects <- renderUI({
        #     if (is.null(gdrive_user())) {
        #         shiny::tags$i("Please sign in to your Google Drive account to enable loading and saving projects.")
        #     } else {
        #         user <- gdrive_user()
        #
        #         tagList(
        #             shiny::tags$p(
        #                 style="color: green;",
        #                 paste0("Signed in as ", user$displayName, " (", user$emailAddress, ").")),
        #             shiny::tags$i("No projects found.")
        #         )
        #     }
        # })

    }

    # Run the application
    shiny::shinyApp(ui = slatesUI, server = slatesServer)
}


#' Load JSON blueprints from file or folder
#'
#' @param path
#'
#' @return
#' @export
loadBlueprints <- function(path) {
    filenames <- dir(path, pattern = ".json$", full.names = TRUE)
    blueprints <- lapply(filenames, blueprintFromJSON)
    setNames(blueprints, sapply(blueprints, "[[", "title"))
}

options(rslates.default.ace.theme = "dawn")

runSlatesApp <- function() {
    options(rslates.blueprints = loadBlueprints(system.file("blueprints", package="rslates")))
    runApp(system.file("app_rslates.R", package = "rslates"))
}


runSlatePreviewApp <- function(blueprint,
                               theme = "Natural (soft light)") {
    options(rslates.preview.blueprint = blueprint)
    options(rslates.default.theme = theme)
    options(rslates.themes = sort(c(names(rslate.themes), bslib::bootswatch_themes())))

    runApp(system.file("app_slate_preview.R", package = "rslates"))
}


runSlateBuilderApp <- function(blueprint = NULL,
                               theme = "Natural (soft light)",
                               run.themer = FALSE) {
    options(rslates.builder.blueprint = blueprint)
    options(rslates.default.theme = theme)
    options(rslates.themes = sort(c(names(rslate.themes), bslib::bootswatch_themes())))
    options(rslates.run.themer = run.themer)

    runApp(system.file("app_slate_builder.R", package = "rslates"))
}


runProjectEditorApp <- function(project = NULL, input.container = "collapse",
                                theme = "Natural (soft light)" , run.themer = FALSE) {
    options(rslates.blueprints = loadBlueprints(system.file("blueprints", package="rslates")))
    options(rslates.data.blueprints = loadBlueprints(system.file("blueprints/data_blueprints", package="rslates")))
    options(rslates.input.container = input.container)
    options(rslates.editor.project = project)
    options(rslates.default.theme = theme)
    options(rslates.themes = sort(c(names(rslate.themes), bslib::bootswatch_themes())))
    options(rslates.run.themer = run.themer)

    runApp(system.file("app_project_editor.R", package = "rslates"))
}

runSlatesWidgetGalleryApp <- function(theme = "Natural (soft light)", run.themer = FALSE) {
    options(rslates.default.theme = theme)
    options(rslates.themes = sort(c(names(rslate.themes), bslib::bootswatch_themes())))
    options(rslates.run.themer = run.themer)

    runApp(system.file("app_slates_widget_gallery.R", package = "rslates"))
}


# -------------

runSlateViewerApp <- function(blueprint = slateBlueprint("untitled"),
                              theme = "Natural (soft light)") {
    options(rslates.viewer.blueprint = blueprint)
    options(rslates.default.theme = theme)
    options(rslates.themes = sort(c(names(rslate.themes), bslib::bootswatch_themes())))

    runApp(system.file("app_slate_viewer.R", package = "rslates"))
}


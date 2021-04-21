


INLINE.MAX.CHAR <- 8
ARG.TYPES <- c("numeric", "logical", "character", "expression", "choices", "file")


#' Create a slate blueprint object
#'
#' @param name name (or title) for the blueprint
#' @param author author of this blueprint
#' @param category
#' @param tags
#' @param source
#' @param preprocess
#' @param pages
#' @param groups
#' @param inputs
#' @param outputs
#' @param imports
#' @param exports
#' @param ...
#'
#' @return
#' @export
slateBlueprint <- function(name = "Untitled",
                           author = "",
                           category = "",
                           tags = list(),
                           source = NULL,
                           preprocess = TRUE,
                           pages = list(),
                           groups = list(),
                           inputs = list(),
                           outputs = list(),
                           imports = list(),
                           exports = list(),
                           ...) {
  blueprint <- list(
    name = name,
    author = author,
    category = category,
    tags = tags,
    pages = pages,
    groups = groups,
    inputs = inputs,
    blocks = list(),
    toplevel = character(0),
    outputs = outputs,
    imports = imports,
    exports = exports,
    source = source,
    substitutions = list()
  )

  class(blueprint) <- "slateBlueprint"

  blueprint$md5 <- digest::digest(blueprint, algo = "md5")

  if (preprocess == TRUE) {
    # get all blueprint sources
    text <- map(blueprint$outputs, "source") %>%
      unlist(recursive = FALSE) %>%
      map("text") %>%
      paste(collapse = "\n")

    #text <-

    # Handle substitutions
    blueprint$substitutions <-
      gregexpr("\\$\\{.*?\\}", text) %>%
      regmatches(text, .) %>%
      unlist %>%
      map(preprocessSubstitutionBlock)

  }

  if (preprocess == TRUE && !is.null(source)) {
    preprocessed <- preprocessSource(source)

    blueprint$pages <- preprocessed$pages
    blueprint$groups <- preprocessed$groups
    blueprint$inputs <- preprocessed$inputs
    blueprint$substitutions <- preprocessed$blocks
    blueprint$toplevel <- preprocessed$toplevel
    blueprint$outputs <- preprocessed$outputs
    blueprint$imports <- preprocessed$imports
    blueprint$exports <- preprocessed$exports
  }

  # make sure we dont have parent="auto" anymore
  clear_auto <- . %>%
    modify_if(~.$parent == "auto", ~list_modify(., parent = ".root"))

  blueprint$inputs %<>% clear_auto
  blueprint$groups %<>% clear_auto

  return(blueprint)
}


#' Define a Slate Input
#'
#' @param name the name of the input.
#' @param type the type of the input. use `names(input.handlers)` for a list of valid
#'   input types.
#' @param default the default value for the input.
#' @param parent the parent of this input. Can be either the name of a group or page, or NULL
#'   to have the input be a child of root, or "auto" (the default) to infer the parent based
#'   on the order of page, group and input definitions. See [`inferSlateLayout()`] for details.
#' @param long.name the name to display in the user interface, or NULL to use the input's name
#' @param description the description of the input. Usually this is shown via a tooltip.
#' @param condition an expression that determines the visibility of this input, or NULL to
#'   always show the input.
#' @param wizards a list of wizards associated with this input. Wizards will ususally be
#'   displayed in a dropdown menu above the input. See [`slateWizard()`] for details.
#' @param input.type *legacy: use `type` instead*
#' @param ... additional input parameters to be stored in the input structure. These are usually
#'   type-specific parameters, such as `choices` if the input is of type `select`.
#' @param null if TRUE, a toggle is displayed to allow the user to select NULL as the input value.
#'
#' @return A list of input parameters suitable to be passed along to other `rslates` functions.
#' @export
slateInput <- function(name,
                       type = "character",
                       default = NULL,
                       allow.null = FALSE,
                       parent = "auto", # "auto", "none", id
                       long.name = NULL,
                       description = "",
                       condition = NULL,
                       wizards = list(),
                       ...) {
  if (is.null(default))
    default <- input.handlers[[ type ]]$default.value

  input <- list(
    name = name,
    type = type,
    default = default,
    allow.null = allow.null,
    parent = parent,
    long.name = long.name,
    description = description,
    condition = condition,
    wizards = wizards
  )

  class(input) <- "slateInput"

  # add additional input parameters specified
  input <- modifyList(input, list(...))

  # check if valid input type
  if (!(input$type %in% names(input.handlers)))
    stop("\"", input$type, "\" is not a valid input type.")

  # set default value
  if (is.null(input$default))
    input$default <- getHandler(input)$default.value

  # initialize type specific variables
  input <- getHandler(input)$initInput(input)

  # coerce default value
  input$default <- getHandler(input)$as.value(x = input, value = input$default)

  # make sure the id is set
  input$id <- paste0("input_", name)
  input$value <- input$default

  return (input)
}


#' Define a Page For Slate Inputs
#'
#' @param name
#' @param title
#' @param layout
#' @param description
#'
#' @return
#' @export
#'
#' @examples
slatePage <- function(name,
                      title = name,
                      layout = "flow-2",
                      description = "") {
  page <- list(
    name = name,
    title = title,
    description = description,
    layout = layout
  )

  page$id <- paste0("page_", name)

  class(page) <- "slatePage"

  return(page)
}


#' Define a Group For Slate Inputs
#'
#' @param name
#' @param ...
#' @param parent
#' @param description
#' @param layout
#' @param condition
#'
#' @return
#' @export
#'
#' @examples
slateGroup <- function(name, ...,
                       parent = "auto",
                       description = "",
                       layout = "flow-2",
                       condition = NULL) {
  group <- list(
    name = name,
    parent = parent,
    description = description,
    layout = layout,
    condition = condition,
    id = paste0("group_", name)
  )

  group <- c(group, list(...))
  group$id <- paste0("group_", name)

  class(group) <- "slateGroup"

  return(group)
}


#' Infer the Parent of Layout Elements By Their Order
#'
#' @details This function takes in a list of pages, groups and inputs and
#' replaces the `parent` of elements with `parent = "auto"` with the name of the
#' previous group or page.
#'
#' @param layout an ordered list of layout elements (pages, groups and inputs).
#'
#' @return The same structure passed in `layout` with the appropriate modifications
#'   to `parent` values
#' @export
inferSlateLayout <- function(layout) {
  last.page <- NULL
  last.group <- NULL

  for (i in seq_along(layout)) {
    x <- layout[[ i ]]

    if (!is.null(x$parent) && x$parent == "auto") {
      if (class(x) == "slateGroup" || (class(x) == "slateInput" && is.null(last.group))) {
        layout[[ i ]]$parent <- last.page$name
      } else if (class(x) == "slateInput") {
        layout[[ i ]]$parent <- last.group$name
      }
    }

    if (class(x) == "slatePage") {
      last.page <- x
      last.group <- NULL
    } else if (class(x) == "slateGroup") {
      last.group <- x
    }
  }

  return(layout)
}


#' Create a slate output object
#'
#' @param name
#' @param type
#' @param source
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
slateOutput <- function(name, type, source = "", ...) {
  output.data <- list(
    name = name,
    type = type,
    source = source
  )

  # add additional output parameters specified
  output.data <- c(output.data, list(...))

  output.data$id <- paste0("output_", name)

  class(output.data) <- "slateOutput"

  return (output.data)
}



import.handlers <- list(
  file = list(
    name = "file",
    make.source = function(x, data) {
      data <- list(
        name = data$name,
        path = data$datapath,
        size = data$size,
        md5 = digest::digest(data$datapath, file = TRUE, algo = "md5")
      )

      paste(x$name, "<-", toString(list(data)))
    }
  ),
  RData = list(
    name = "RData"
  )
)


#' Create a slate import object
#'
#' @param name
#' @param type
#' @param description
#'
#' @return
#' @export
#'
#' @examples
slateImport <- function(name, type, description = "") {
  stopifnot(type %in% names(import.handlers))

  list(
    name = name,
    type = type,
    description = description
  ) %>% structure(class = "slateImport")
}


#' Create a slate export object
#'
#' @param var.name
#' @param out.name
#'
#' @return
#' @export
#'
#' @examples
slateExport <- function(var.name, out.name = var.name) {
  list(
    var.name = var.name,
    out.name = out.name
  )
}


getHandler <- function(x) {
  if (class(x) == "slateInput") {
    input.handlers[[ x$type ]]
  } else if (class(x) == "slateOutput") {
    output.handlers[[ x$type ]]
  }
}


assignInputValues <- function(x, values) {
  x %>%
    modify_if(~.$name %in% names(values),
              ~list_modify(., value = values[[ .$name ]]))
}


#
# Save / Load
#

page.defaults <- list(
  name = "untitled",
  description = ""
)

group.defaults <- list(
  name = "Group",
  layout = "flow-2",
  condition = ""
)

input.defaults <- list(
  default = "",
  allow.null = FALSE,
  long.name = NULL,
  description = "",
  parent = "auto",
  wizards = list()
)

clearDefaults <- function(x, defaults) {
  is.default <- map_lgl(names(x), ~identical(x[[ . ]], defaults[[ . ]]))
  x[ !is.default ]
}

#' Simplify a blueprint by removing default values
#'
#' @param blueprint the blueprint to simplify.
#'
#' @return The simplified blueprint. This blueprint is ideal for export in
#'   JSON format or other format.
#' @export
#'
#' @seealso [restoreBlueprint()] to restore a simplified blueprint to its initial state.
simplifyBlueprint <- function(blueprint) {
  blueprint$pages %<>% map(~clearDefaults(., page.defaults)) %>%
    map(~list_modify(., id = NULL, value = NULL))

  blueprint$groups %<>% map(~clearDefaults(., group.defaults)) %>%
    map(~list_modify(., id = NULL, value = NULL))

  blueprint$inputs %<>% map(function(x) {
    input.defaults <- c(input.defaults, map(getHandler(x)$params.list, "default"))
    clearDefaults(x, input.defaults)
  }) %>%
    map(~list_modify(., id = NULL, value = NULL))

  return(blueprint)
}


#' Restore a blueprint by filling-in default values
#'
#' @param blueprint the blueprint to be restored.
#'
#' @return the restored blueprint containing all fields, including those with default values.
#' @export
#'
#' @seealso [simplifyBlueprint()] to remove default values from a blueprint.
restoreBlueprint <- function(blueprint) {
  blueprint$pages %<>% map(~do.call(slatePage, .))
  blueprint$groups %<>% map(~do.call(slateGroup, .))
  blueprint$inputs %<>% map(~do.call(slateInput, .))

  return(blueprint)
}


# inputs tree conversions

flatInputsToTree <- function(pages, groups, inputs) {
  groups <-
    map(groups, function(g) {
      list_modify(g, inputs = inputs %>% keep(map_lgl(., ~.$parent == g$name)))
    })

  pages <- map(pages, function(p) {
    list_modify(
      p,
      groups = groups %>% keep(map_lgl(., ~.$parent == p$name)),
      inputs = inputs %>% keep(map_lgl(., ~.$parent == p$name))
    )
  })

  list(
    pages = pages,
    groups = keep(groups, map_lgl(groups, ~.$parent == ".root")),
    inputs = keep(inputs, map_lgl(inputs, ~.$parent == ".root"))
  )
}


flattenInputsTree <- function(tree) {

}


# Load/Save Blueprint

blueprintToJSON <- function(blueprint, pretty = FALSE) {
  data <- list(
    name = blueprint$name,
    author = blueprint$author,
    category = blueprint$category,
    tags = blueprint$tags,
    source = blueprint$source
  )

  jsonlite::toJSON(data, pretty = pretty)
}


blueprintFromJSON <- function(filename=NULL, text=NULL, preprocess = TRUE) {
  if (!is.null(filename) && !is.null(text))
    stop("Only one of filename or text must be supplied.")

  if (!is.null(filename))
    data <- jsonlite::fromJSON(filename, simplifyVector = TRUE)
  else
    data <- jsonlite::fromJSON(txt = text, simplifyVector = TRUE)

  blueprint <- slateBlueprint(
    name = data$name,
    author = data$author,
    category = data$category,
    tags = data$tags,
    source = data$source,
    preprocess = preprocess
  )

  return(blueprint)
}


blueprintToYAML <- function(blueprint) {
  blueprint <- simplifyBlueprint(blueprint)

  clean_node <- function(x) {
    x$name <- NULL
    x$parent <- NULL
    x$inputs <- if (!is.null(x$inputs)) map(x$inputs, clean_node)
    x$groups <- if (!is.null(x$groups)) map(x$groups, clean_node)
    x$pages <- if (!is.null(x$pages)) map(x$pages, clean_node)

    return(x)
  }

  tree <- flatInputsToTree(blueprint$pages, blueprint$groups, blueprint$inputs)

  data <- list(
    name = blueprint$name,
    author = blueprint$author,
    category = blueprint$category,
    tags = blueprint$tags,
    outputs = blueprint$outputs %>% map(~list_modify(., name = NULL, id = NULL)),
    inputs = map(tree$inputs, clean_node),
    groups = map(tree$groups, clean_node),
    pages = map(tree$pages, clean_node)
  )

  yaml::as.yaml(data)
}

blueprintFromYAML <- function(filename=NULL, text=NULL, preprocess = TRUE) {
  if (!is.null(filename) && !is.null(text))
    stop("Only one of filename or text must be supplied.")

  if (!is.null(filename))
    data <- yaml::read_yaml(filename)
  else
    data <- yaml::yaml.load(string = text)

  pages <- data$pages
  groups <- data$groups
  inputs <- data$inputs

  pages <- imap(pages, function(page, name) {
    groups <<- append(groups, page$groups %>% map(~list_modify(., parent = name)))
    inputs <<- append(inputs, page$inputs %>% map(~list_modify(., parent = name)))
    page$name <- name
    page$groups <- NULL
    page$inputs <- NULL
    do.call(slatePage, page)
  })

  groups <- imap(groups, function(group, name) {
    inputs <<- append(inputs, group$inputs %>% map(~list_modify(., parent = name)))
    group$name <- name
    group$inputs <- NULL
    do.call(slateGroup, group)
  })

  inputs <- imap(inputs, function(input, name) {
    input$name <- name
    do.call(slateInput, input)
  })

  outputs <- imap(data$outputs, ~do.call(slateOutput, append(.x, list(name = .y))))

  blueprint <- slateBlueprint(
    name = data$name,
    author = data$author,
    category = data$category,
    tags = data$tags,
    outputs = outputs,
    inputs = inputs,
    groups = groups,
    pages = pages
    #source = data$source,
    #preprocess = preprocess
  )

  return(blueprint)
}


blueprintToTxt <- function(blueprint) {
  data <- list(
    name = blueprint$name,
    author = blueprint$author,
    category = blueprint$category,
    tags = paste(blueprint$tags, collapse = ", ")
  )

  imap(data, ~paste0(.y, ": ", .x)) %>%
    paste(collapse = "\n") %>%
    paste0("\n---\n", blueprint$source)
}


blueprintFromTxt <- function(filename=NULL, text=NULL, preprocess = TRUE) {
  if (!is.null(filename) && !is.null(text))
    stop("Only one of filename or text must be supplied.")

  if (!is.null(filename))
    lines <- readLines(filename)
  else
    lines <- strsplit(text, split = "\n")[[1]]

  sep <- grep("^---$", lines)[1]

  data <- lines[ 1:(sep-1) ] %>%
    map(~{
      strsplit(., split = ":")[[1]] %>%
        trimws
    }) %>%
    transpose
  data <- data[[2]] %>% set_names(data[[1]])

  data$tags <- strsplit(data$tags, split = ",")[[1]] %>% trimws

  data$source <-
    lines[ sep + 1:length(lines) ] %>%
    paste(collapse = "\n")

  blueprint <- slateBlueprint(
    name = data$name,
    author = data$author,
    category = data$category,
    tags = data$tags,
    source = data$source,
    preprocess = preprocess
  )

  return(blueprint)
}



loadBlueprint <- function(filename,
                          format = c("auto", "yaml", "txt", "json"),
                          preprocess = TRUE) {
  dlog(filename)

  format <- match.arg(format)

  if (format == "auto") {
    format <- gsub("^.*\\.(.*?)$", "\\1", filename)

    if (!(format %in% c("yaml", "yml", "txt", "json")))
      stop("Unknown format: ", format)
  }

  if (format == "txt") {
    blueprint <- blueprintFromTxt(filename = filename, preprocess = preprocess)
  } else if (format == "json") {
    blueprint <- blueprintFromJSON(filename = filename, preprocess = preprocess)
  } else if (format %in% c("yml", "yaml")) {
    blueprint <- blueprintFromYAML(filename = filename, preprocess = preprocess)
  }

  return(blueprint)
}







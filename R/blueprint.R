


INLINE.MAX.CHAR <- 8
ARG.TYPES <- c("numeric", "logical", "character", "expression", "choices", "file")


slateBlueprint <- function(name = "Untitled",
                           author = "",
                           category = "",
                           tags = list(),
                           source = "",
                           preprocess = TRUE) {
  md5 <- list(name, author, category, tags, source) %>%
    digest::digest(algo = "md5")

  if (preprocess == TRUE) {
    preprocessed <- preprocessSource(source)

    list(
      name = name,
      author = author,
      category = category,
      tags = tags,
      pages = preprocessed$pages,
      groups = preprocessed$groups,
      inputs = preprocessed$inputs,
      blocks = preprocessed$blocks,
      toplevel = preprocessed$toplevel,
      outputs = preprocessed$outputs,
      imports = preprocessed$imports,
      exports = preprocessed$exports,
      datasets = list(),
      source = source,
      md5 = md5
    )
  } else {
    list(
      name = name,
      author = author,
      category = category,
      tags = tags,
      pages = list(),
      groups = list(),
      inputs = list(),
      blocks = list(),
      toplevel = character(0),
      outputs = list(),
      imports = list(),
      exports = list(),
      datasets = list(),
      source = source,
      md5 = md5
    )
  }
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
                       parent = "auto",
                       long.name = "",
                       description = "",
                       condition = NULL,
                       wizards = list(),
                       input.type = type,  # legacy
                       ...) {
  if (is.null(default))
    default <- input.handlers[[ input.type ]]$default.value

  input <- list(
    name = name,
    input.type = input.type,
    default = default,
    allow.null = allow.null,
    parent = parent,
    long.name = long.name,
    description = description,
    condition = condition,
    wizards = wizards,
    type = "input"
  )

  # add additional input parameters specified
  input <- modifyList(input, list(...))

  # check if valid input type
  if (!(input$input.type %in% names(input.handlers)))
    stop("\"", input$input.type, "\" is not a valid input type.")

  # set default value
  if (is.null(input$default))
    input$default <- getHandler(input)$default.value

  # initialize type specific variables
  input <- getHandler(input)$initInput(input)

  # coerce default value
  input$default <- getHandler(input)$as.value(x = input, value = input$default)

  # make sure the id is set
  input$id <- paste0("input_", name)


  #input$value <- input$default

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
    layout = layout,
    type = "page"
  )

  page$id <- paste0("page_", name)

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
    id = paste0("group_", name),
    type = "group")

  group <- c(group, list(...))
  group$id <- paste0("group_", name)

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
      if (x$type == "group" || (x$type == "input" && is.null(last.group))) {
        layout[[ i ]]$parent <- last.page$name
      } else if (x$type == "input") {
        layout[[ i ]]$parent <- last.group$name
      }
    }

    if (x$type == "page") {
      last.page <- x
      last.group <- NULL
    } else if (x$type == "group") {
      last.group <- x
    }
  }

  return(layout)
}


slateOutput <- function(name, type, source = "", ...) {
  output.data <- list(
    name = name,
    type = type,
    source = source
  )

  # add additional output parameters specified
  output.data <- c(output.data, list(...))

  output.data$id <- paste0("output_", name)

  return (output.data)
}


slateDataset <- function(name, type, source = "", export = FALSE, export.name = "") {
  list(
    name = name,
    source = source,
    export = export,
    export.name = export.name
  )
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


slateImport <- function(name, type, description = "") {
  stopifnot(type %in% names(import.handlers))

  list(
    name = name,
    type = type,
    description = description
  )
}


slateExport <- function(var.name, out.name = var.name) {
  list(
    var.name = var.name,
    out.name = out.name
  )
}


getHandler <- function(x) {
  if (x$type == "input") {
    input.handlers[[ x$input.type ]]
  } else if (x$type %in% names(output.handlers)) {
    output.handlers[[ x$type ]]
  }
}


assignInputValues <- function(inputs, values) {
  inputs %>%
    modify_if(~.$name %in% names(values),
              ~list_modify(., value = values[[ .$name ]]))
}


#
# Save / Load
#

page.defaults <- list(
  type = "page",
  name = "untitled",
  description = ""
)

group.defaults <- list(
  type = "group",
  name = "Group",
  layout = "flow",
  condition = ""
)

input.defaults <- list(
  type = "input",
  default = "",
  long.name = "",
  description = "",
  wizards = list()
)

clearDefaults <- function(x, defaults) {
  is.default <- sapply(names(x), function(name) identical(x[[ name ]], defaults[[ name ]]))
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
  blueprint$pages %<>% map(~clearDefaults(., page.defaults))
  blueprint$groups %<>% map(~clearDefaults(., group.defaults))

  blueprint$inputs %<>% map(function(x) {
    input.defaults <- c(input.defaults, map(getHandler(x)$params.list, "default"))
    clearDefaults(x, input.defaults)
  })

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
  data <- list(
    name = blueprint$name,
    author = blueprint$author,
    category = blueprint$category,
    tags = blueprint$tags
  )

  strsplit(blueprint$source, split = "\n")[[1]] %>%
    map(~paste0("  ", .)) %>%
    paste(collapse = "\n") %>%
    paste0(yaml::as.yaml(data), "\n", "source: |-2\n", .)
}


blueprintFromYAML <- function(filename=NULL, text=NULL, preprocess = TRUE) {
  if (!is.null(filename) && !is.null(text))
    stop("Only one of filename or text must be supplied.")

  if (!is.null(filename))
    data <- yaml::read_yaml(filename)
  else
    data <- yaml::yaml.load(string = text)

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







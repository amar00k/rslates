


INLINE.MAX.CHAR <- 8
ARG.TYPES <- c("numeric", "logical", "character", "expression", "choices", "file")


slateBlueprint <- function(name = "Untitled",
                           author = "",
                           category = "",
                           tags = list(),
                           source = "",
                           preprocess = TRUE) {
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
      outputs = preprocessed$outputs,
      imports = preprocessed$imports,
      datasets = list(),
      source = source
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
      outputs = list(),
      datasets = list(),
      imports = list(),
      source = source
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


slatePage <- function(name, ...,
                      description = "",
                      layout = "flow-2") {
  page <- list(
    name = name,
    description = description,
    layout = layout,
    type = "page"
  )

  page <- c(page, list(...))
  page$id <- paste0("page_", name)

  return(page)
}


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


slateOutput <- function(type, name, source = "", ...) {
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




#
#
# loadBlueprint <- function(filename,
#                           format = c("auto", "txt", "json")) {
#   format <- match.arg(format)
#
#   if (format == "auto") {
#     format <- gsub("^.*\\.(.*?)$", "\\1", filename)
#
#     if (!(format %in% c("txt", "json")))
#       stop("File extension must be txt or json.")
#   }
#
#   if (format == "txt") {
#     source <- readLines(filename) %>% paste(collapse = "\n")
#
#     parsed <- preprocessSource(source)
#
#     inputs <- parsed$inputs
#     outputs <- parsed$outputs
#
#     blueprint <- slateBlueprint(
#       title = "Untitled",
#       pages = parsed$pages,
#       groups = parsed$groups,
#       inputs = parsed$inputs,
#       outputs = parsed$outputs
#     )
#
#     blueprint$source <- source
#   } else if (format == "json") {
#     blueprint <- blueprintFromJSON(filename)
#   }
#
#   return(blueprint)
# }
#




#
# Blueprint utilities
#

# printInputItem <- function(x) {
#   #x$children <- NULL
#   print(paste(names(x), x, sep = " = ", collapse=", "))
# }


# printInputLayout <- function(layout) {
#   indent <- list("", "  ", "    ")
#
#   invisible(traverseInputLayout(layout, function(x, ancestry) {
#     print(paste0(indent[[ length(ancestry) + 1 ]], x$type, ": ", x$name))
#     x
#   }))
# }
#
#
# flattenInputLayout <- function(layout, clear.children = FALSE) {
#   lapply(layout$pages, function(p) {
#     lapply(p$children, function(g) {
#       lapply(g$children, function(i) {
#         i$ancestry <- c(p$name, g$name)
#         i
#       })
#     }) %>%
#       unlist(recursive = FALSE) %>%
#       append(lapply(p$children, function(g) {
#         g$ancestry <- p$name
#         g
#       }))
#   }) %>%
#     unlist(recursive = FALSE) %>%
#     append(layout$pages) %>%
#     set_names(sapply(., "[[", "id"))
# }
#
#
#
# traverseInputLayout <- function(layout, callback = function(x, ancestry) x, flatten = FALSE) {
#   layout$pages <- lapply(layout$pages, function(p) {
#     p <- callback(p, NULL)
#     p$children <- lapply(p$children, function(g) {
#       g <- callback(g, p$name)
#       g$children <- lapply(g$children, function(i) {
#         callback(i, c(p$name, g$name))
#       }) %>% set_names(sapply(., "[[", "name"))
#       g
#     }) %>% set_names(sapply(., "[[", "name"))
#     p
#   }) %>% set_names(sapply(., "[[", "name"))
#
#   if (flatten == TRUE)
#     layout <- flattenInputLayout(layout)
#
#   return(layout)
# }
#
#
# updateInputLayoutItem <- function(layout, item, ancestry = c(), name = NULL) {
#   if (is.null(name))
#     path <- c(ancestry, item$name)
#   else
#     path <- c(ancestry, name)
#
#   if (length(path) == 1) {
#     layout$pages[[ path ]] <- item
#     names(layout$pages) <- sapply(layout$pages, "[[", "name")
#   } else if (length(path) == 2) {
#     layout$pages[[ path[1] ]]$children[[ path[2] ]] <- item
#     names(layout$pages[[ path[1] ]]$children) <-
#       sapply(layout$pages[[ path[1] ]]$children, "[[", "name")
#   } else {
#     layout$pages[[ path[1] ]]$children[[ path[2] ]]$children[[ path[3] ]] <- item
#     names(layout$pages[[ path[1] ]]$children[[ path[2] ]]$children) <-
#       sapply(layout$pages[[ path[1] ]]$children[[ path[2] ]]$children, "[[", "name")
#   }
#
#   return(layout)
# }


# getInputs <- function(blueprint) {
#   Filter(function(x) x$type == "input",
#          flattenInputLayout(blueprint$input.layout)) %>%
#     set_names(sapply(., "[[", "name"))
# }



# slate_dataset <- function(id, source) {
#   list(
#     id = id,
#     source = source
#   )
# }





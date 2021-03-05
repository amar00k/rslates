


INLINE.MAX.CHAR <- 8
ARG.TYPES <- c("numeric", "logical", "character", "expression", "choices", "file")


slateBlueprint <- function(title,
                           pages = list(),
                           groups = list(),
                           inputs = list(),
                           outputs = list(),
                           datasets = list(),
                           imports = list()) {
  pages %<>% set_names(map(., "name"))
  groups %<>% set_names(map(., "name"))
  inputs %<>% set_names(map(., "name"))
  outputs %<>% set_names(map(., "name"))

  # if (!("default" %in% names(pages)))
  #   pages %<>% prepend(list(default = slatePage("default")))

  list(
    title = title,
    pages = pages,
    groups = groups,
    inputs = inputs,
    outputs = outputs,
    datasets = datasets,
    imports = imports
  )
}


slateInput <- function(name, input.type,
                       parent = NULL,
                       default = NULL,
                       long.name = "",
                       description = "",
                       condition = NULL,
                       wizards = list(),
                       ...) {
  if (is.null(default))
    default <- input.handlers[[ input.type ]]$default.value

  input <- list(
    name = name,
    input.type = input.type,
    parent = parent,
    default = default,
    long.name = long.name,
    description = description,
    condition = condition,
    wizards = wizards,
    type = "input"
  )

  # add additional input parameters specified
  input <- modifyList(input, list(...))

  # include type-specific default values that were not specified
  pars <- getHandler(input)$params.list %>%
    map("default")

  input <- modifyList(input, pars)

  # make sure the id is set
  input$id <- paste0("input_", name)

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
                       parent = NULL,
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


slateImport <- function(name, type, value = "", description = "") {
  list(
    name = name,
    type = "file",
    description = description,
    value = "",
    data = NULL
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
  values <- as.character(values)

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
  jsonlite::toJSON(blueprint, pretty = pretty)
}

blueprintFromJSON <- function(filename=NULL, text=NULL) {
  if (!is.null(filename) && !is.null(text))
    stop("Only one of filename or text must be supplied.")

  if (!is.null(filename))
    blueprint <- jsonlite::fromJSON(filename, simplifyVector = TRUE)
  else
    blueprint <- jsonlite::fromJSON(txt = text, simplifyVector = TRUE)

  blueprint <- restoreBlueprint(blueprint)

  return(blueprint)
}






loadBlueprint <- function(filename, format = c("auto", "txt", "json")) {
  format <- match.arg(format)

  if (format == "auto") {
    format <- gsub("^.*\\.(.*?)$", "\\1", filename)

    if (!(format %in% c("txt", "json")))
      stop("File extension must be txt or json.")
  }

  if (format == "txt") {
    source <- readLines(filename) %>% paste(collapse = "\n")

    parsed <- preprocessSource(source)

    inputs <- parsed$inputs %>%
      map(~do.call(slateInput, .))

    outputs <- parsed$sections %>%
      map(~do.call(slateOutput, .))

    blueprint <- slateBlueprint(
      "untitled",
      inputs = inputs,
      outputs = outputs
    )

    blueprint$source <- source
  } else if (format == "json") {
    blueprint <- blueprintFromJSON(filename)
  }

  return(blueprint)
}





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





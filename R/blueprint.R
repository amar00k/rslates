


INLINE.MAX.CHAR <- 8
ARG.TYPES <- c("numeric", "logical", "character", "expression", "choices", "file")


slateBlueprint <- function(title,
                           input.layout = list(),
                           outputs = list(),
                           datasets = list(),
                           imports = list()) {

  # outputs[[ length(outputs) + 1 ]] <- slateOutput("source", title="Source", type="source")

  # names(inputs) <- sapply(inputs, "[[", "name")

  input.layout$pages <- lapply(input.layout$pages, function(p) {
    p$groups <- lapply(p$groups, function(g) {
      g$inputs <- lapply(g$inputs, function(i) {
        i$value <- i$default
        i
      }) %>% set_names(sapply(., "[[", "name"))
      g
    }) %>% set_names(sapply(., "[[", "id"))
    p
  }) %>% set_names(sapply(., "[[", "name"))

  names(outputs) <- sapply(outputs, "[[", "name")

  list(
    title = title,
    input.layout = input.layout,
    outputs = outputs,
    datasets = datasets,
    imports = imports
  )
}


slateInput <- function(name, input.type,
                       default = NULL,
                       value = default,
                       long.name = "",
                       description = "",
                       wizards = list(),
                       ...) {
  if (is.null(default))
    default <- input.handlers[[ input.type ]]$default.value

  input <- list(
    name = name,
    type = "input",
    long.name = long.name,
    description = description,
    input.type = input.type,
    default = default,
    value = value,
    wizards = wizards
  )

  # add additional input parameters specified
  input <- c(input, list(...))

  # include type-specific default values that were not specified
  param.defaults <- lapply(input.handlers[[ input$input.type ]]$params.list, "[[", "default")

  if (length(param.defaults) > 0) {
    param.names <- names(param.defaults)
    w <- sapply(param.names, function(x) is.null(input[[ x ]]))
    input <- c(input, param.defaults[ w ])
  }

  # make sure the id is set
  input$id <- paste0("input_", name)

  return (input)
}


inputLayout <- function(pages = list(), main.page = NULL) {
  if (!is.null(main.page))
    pages <- append(list(main.page), pages)

  names(pages) <- sapply(pages, "[[", "name")

  return(list(pages = pages))
}


inputPage <- function(name, ..., description="", groups = NULL) {
  if (is.null(groups))
    groups <- list(...)

  if (length(groups) > 0)
    names(groups) <- sapply(groups, "[[", "name")

  return(list(name = name,
              id = paste0("page_", name),
              description = description,
              type = "page",
              groups = groups))
}


inputGroup <- function(name, ..., layout = "flow-2", condition = "", inputs = NULL) {
  if (is.null(inputs))
    inputs <- list(...)

  names(inputs) <- sapply(inputs, "[[", "name")

  return(list(name = name,
              id = paste0("group_", name),
              condition = condition,
              type = "group",
              layout = layout,
              inputs = inputs))
}


slateOutput <- function(name, type, source="") {
  output.data <- list(
    name = name,
    type = type,
    source = source
  )

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

#' Define a slate import
#'
#' @param name
#' @param type file or dataframe
#' @param value path of file or name of dataframe
#'
#' @return
#' @export
#'
#' @examples
slateImport <- function(name, type, value = "", description = "") {
  list(
    name = name,
    type = "file",
    description = description,
    value = "",
    data = NULL
  )
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
#' @return The simplified blueprint. This blueprint is ideal for export in JSON format or other format.
#' @export
#'
#' @seealso [restoreBlueprint()] to restore a simplified blueprint to its initial state.
#'
#' @examples
simplifyBlueprint <- function(blueprint) {
  blueprint$input.layout$pages <- lapply(blueprint$input.layout$pages, function(p) {
    p <- clearDefaults(p, page.defaults)
    p$groups <- lapply(p$groups, function(g) {
      g <- clearDefaults(g, group.defaults)
      g$inputs <- lapply(g$inputs, function(i) {
        input.defaults <-
          c(input.defaults,
            sapply(input.handlers[[ i$input.type ]]$params.list, "[[", "default"))

        clearDefaults(i, input.defaults)
      })
      g
    })
    p
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
#'
#' @examples
restoreBlueprint <- function(blueprint) {
  blueprint$input.layout$pages <- lapply(blueprint$input.layout$pages, function(p) {
    p$groups <- lapply(p$groups, function(g) {
      g$inputs <- lapply(g$inputs, function(i) {
        do.call(slateInput, i)
      })
      do.call(inputGroup, g)
    })
    do.call(inputPage, p)
  })

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



#
# Blueprint utilities
#

printInputLayout <- function(layout) {
  indent <- list("", "  ", "    ")

  invisible(traverseInputLayout(layout, function(x, ancestry) {
    print(paste0(indent[[ length(ancestry) + 1 ]], x$type, ": ", x$name))
    x
  }))
}


flattenInputLayout <- function(layout) {
  lapply(layout$pages, function(p) {
    lapply(p$groups, function(g) {
      lapply(g$inputs, function(i) {
        i$ancestry <- c(p$name, g$name)
        i
      })
    }) %>%
      unlist(recursive = FALSE) %>%
      append(lapply(p$groups, function(g) {
        g$ancestry <- p$name
        g
      }))
  }) %>%
    unlist(recursive = FALSE) %>%
    append(layout$pages) %>%
    set_names(sapply(., "[[", "id"))
}



traverseInputLayout <- function(layout, callback = function(x, ancestry) x, flatten = FALSE) {
  layout$pages <- lapply(layout$pages, function(p) {
    p <- callback(p, NULL)
    p$groups <- lapply(p$groups, function(g) {
      g <- callback(g, p$name)
      g$inputs <- lapply(g$inputs, function(i) {
        callback(i, c(p$name, g$name))
      }) %>% set_names(sapply(., "[[", "name"))
      g
    }) %>% set_names(sapply(., "[[", "name"))
    p
  }) %>% set_names(sapply(., "[[", "name"))

  if (flatten == TRUE)
    layout <- flattenInputLayout(layout)

  return(layout)
}


updateInputLayoutItem <- function(layout, item, ancestry = c(), name = NULL) {
  if (is.null(name))
    path <- c(ancestry, item$name)
  else
    path <- c(ancestry, name)

  if (length(path) == 1) {
    layout$pages[[ path ]] <- item
    names(layout$pages) <- sapply(layout$pages, "[[", "name")
  } else if (length(path) == 2) {
    layout$pages[[ path[1] ]]$groups[[ path[2] ]] <- item
    names(layout$pages[[ path[1] ]]$groups) <-
      sapply(layout$pages[[ path[1] ]]$groups, "[[", "name")
  } else {
    layout$pages[[ path[1] ]]$groups[[ path[2] ]]$inputs[[ path[3] ]] <- item
    names(layout$pages[[ path[1] ]]$groups[[ path[2] ]]$inputs) <-
      sapply(layout$pages[[ path[1] ]]$groups[[ path[2] ]]$inputs, "[[", "name")
  }

  return(layout)
}


getInputs <- function(blueprint) {
  lapply(blueprint$input.layout$pages, function(p) {
    lapply(p$groups, "[[", "inputs") %>% unlist(recursive = FALSE)
  }) %>% unlist(recursive = FALSE) %>% set_names(sapply(., "[[", "name"))
}



# slate_dataset <- function(id, source) {
#   list(
#     id = id,
#     source = source
#   )
# }





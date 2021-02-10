


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
                       default = "",
                       value = default,
                       long.name = "",
                       description = "",
                       choices = list(),
                       wizards = list(),
                       ...) {
  default = if (is.null(default)) "" else default

  input <- list(
    id = paste0("arg_", name),
    name = name,
    type = "input",
    long.name = long.name,
    description = description,
    input.type = input.type,
    default = default,
    value = value,
    choices = choices,
    wizards = wizards
  )

  # args <- list(...)
  # for (n in names(args))
  #   input[[ n ]] <- args[[ n ]]

  return (input)
}


inputLayout <- function(pages = list(), main.page = NULL) {
  if (!is.null(main.page))
    pages <- append(list(main.page), pages)

  names(pages) <- sapply(pages, "[[", "name")

  return(list(pages = pages))
}


inputPage <- function(..., name="untitled", description="", groups = NULL) {
  if (is.null(groups))
    groups <- list(...)

  if (length(groups) > 0)
    names(groups) <- sapply(groups, "[[", "name")

  return(list(name = name,
              description = description,
              type = "page",
              groups = groups))
}


inputGroup <- function(..., name = "Group", layout = "flow", condition = "", inputs = NULL) {
  if (is.null(inputs))
    inputs <- list(...)

  names(inputs) <- sapply(inputs, "[[", "name")

  return(list(name = name,
              id = seq.uid("group"),
              condition = condition,
              type = "group",
              layout = layout,
              inputs = inputs))
}


slateOutput <- function(name, type, source="") { #source=list(generate=function(...) return(""))) {
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
    type = type,
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
    value = ""
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
  choices = list(),
  wizards = list()
)

clearDefaults <- function(x, defaults) {
  is.default <- sapply(names(x), function(name) identical(x[[ name ]], defaults[[ name ]]))
  x[ !is.default ]
}

simplifyBlueprint <- function(blueprint) {
  blueprint$input.layout$pages <- lapply(blueprint$input.layout$pages, function(p) {
    p <- clearDefaults(p, page.defaults)
    p$groups <- lapply(p$groups, function(g) {
      g <- clearDefaults(g, group.defaults)
      g$inputs <- lapply(g$inputs, clearDefaults, input.defaults)
      g
    })
    p
  })

  return(blueprint)
}

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





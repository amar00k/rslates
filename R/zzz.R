#' rslates
#'
#' Easy to use interactive R chunks and application.
#'
#' @author Daniel Neves, \email{danielneves2005@gmail.com}
#'
#' @import shiny
#' @import magrittr
#' @import dplyr
#' @import purrr
#'
#' @docType package
#' @name rslates-package
NULL


.onLoad <- function(libname, pkgname) {
  # op <- options()
  # op.rslates <- list(
  #   #slate.blueprints = create_slate_blueprints()
  #   #slate.data.blueprints = create_data_blueprints()
  # )
  # toset <- !(names(op.rslates) %in% names(op))
  # if(any(toset)) options(op.rslates[toset])
  #
  # invisible()

  options(rslates.default.theme = "Natural (soft light)")
  options(rslates.default.ace.theme = "dawn")



}


#
# Utilities
#



#' Observe Shiny Inputs for 'Readyness'
#'
#' @param session the server session
#'
#' @return a reactive that evaluates to TRUE when all inputs have been initialized
#' @export
uiReady <- function(session) {
  reactive({
    return(
      length(names(session$input)) > 0 &&
      map_lgl(names(session$input), ~!is.null(session$input[[ . ]]))
    )
  })
}




#' Paste and print
#'
#' @param ... arguments for the paste call
#'
#' @return
pprint <- function(...) {
  print(paste(...))
}


dlog <- function(..., format = NULL, use.time = TRUE, use.from = TRUE, level = 3) {
  if (!is.null(format)) {
    format <- strsplit(format, split = "")[[1]]
    use.time <- "t" %in% format
    use.from <- "f" %in% format
  }

  time <- if (use.time) paste0(format(Sys.time(), usetz = FALSE), " ") else ""
  from <- if (use.from) paste0("[", toString(sys.call(sys.parent())), "] ")

  txt <- map_chr(list(...), toString) %>%
    paste(collapse = "; ")

  cat(time, from, txt, "\n")
}


b64.uid <- function(size = 64) {
  paste(sample(c(LETTERS, letters, 0:9), size = size, replace=TRUE), collapse="")
}

seq.uid <- function(prefix = "_") {
  counts <- attr(seq.uid, "counts")
  if (is.null(counts))
    counts <- list()

  if (!hasName(counts, prefix))
    counts[[ prefix ]] <- 1
  else
    counts[[ prefix ]] <- counts[[ prefix ]] + 1

  attr(seq.uid, "counts") <<- counts

  return(paste(prefix, counts[[ prefix ]], sep="_"))
}



sequenceGenerator <- function(prefix = "seq") {
  count <- 0

  function() {
    count <<- count + 1
    paste0(prefix, "_", count)
  }
}


captureSessionInfo <- function(width = 80) {
  # set console width
  opt.width <- options()$width
  options(width = width)

  res <- capture.output(sessionInfo())

  # restore console width
  options(width = opt.width)

  return(res)
}






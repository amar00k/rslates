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
}


#
# Utilities
#



#' Observe shiny inputs for readyness
#'
#' @param session the server session
#'
#' @return a reactiveVal that if TRUE indicates the UI has been initialized.
#' @export
uiReady <- function(session) {
  ready <- reactiveVal(FALSE)
  input <- session$input

  observer <- observe({
    if (length(names(input)) == 0)
      ready <- FALSE
    else
      ready <- all(sapply(names(input), function(name) !is.null(input[[ name ]])))

    if (ready)
      observer$destroy()

    ready(ready)
  })

  return(ready)
}


#' Paste and print
#'
#' @param ... arguments for the paste call
#'
#' @return
pprint <- function(...) {
  print(paste(...))
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






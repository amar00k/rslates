#' rslates
#'
#' Easy to use interactive R chunks and application.
#'
#' @author Daniel Neves, \email{danielneves2005@gmail.com}
#'
#' @import shiny
#' @import magrittr
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


#' Paste and print
#'
#' @param ... arguments for the paste call
#'
#' @return
#'
#' @examples
pprint <- function(...) {
  print(paste(...))
}


#' Generate a random string
#'
#' @description Generates a random string of specified length using valid
#' base 64 characters.
#'
#' @param size length of the string to generate.
#'
#' @return a random string.
#' @export
#'
#' @examples
b64.uid <- function(size = 64) {
  paste(sample(c(LETTERS, letters, 0:9), size = size, replace=TRUE), collapse="")
}

#' Generate a sequencial id
#'
#' @description This function generates a unique sequencial identifier with a given prefix
#' each time it is called in the current session.
#'
#' @param prefix the prefix string to use.
#'
#' @return a unique string id.
#' @export
#'
#' @examples
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












# Variable substitution:
#
# Syntax: ${ [global_options::] [options:][named_argument=]variable_name [...] }
#
# Options:
#   # v - value
#   # n - name
#   x - reset all options (useful to override global options)
#   q/Q - quote value with single quote (') or double quotes (")
#   n/N - suppress if NULL or empty / don't suppress NULL or empty values
#   d/D - suppress if default value / don't suppress defaults
#   l[num]
#
# Examples:
#   plot(${x})

SRC.VALID.OPTIONS <- strsplit("xqQnNdD", split="")[[1]]

srcResolveOptions <- function(var.opts, global.opts) {
  if (any(c("n", "N") %in% var.opts))
    global.opts <- global.opts[ !(global.opts %in% c("n", "N")) ]

  if (any(c("d", "D") %in% var.opts))
    global.opts <- global.opts[ !(global.opts %in% c("d", "D")) ]

  if (any(c("q", "Q") %in% var.opts))
    global.opts <- global.opts[ !(global.opts %in% c("q", "Q")) ]

  if ("x" %in% var.opts) {
    opts <- var.opts[ var.opts != "x" ]
  } else {
    opts <- unique(c(var.opts, global.opts))
  }

  stopifnot(!all(c("n", "N") %in% opts))
  stopifnot(!all(c("d", "D") %in% opts))
  stopifnot(!all(c("q", "Q") %in% opts))

  return(opts)
}


srcParseOptions <- function(str) {
  opts <- strsplit(str, split = "")[[1]]

  stopifnot(all(opts %in% SRC.VALID.OPTIONS))

  return(opts)
}


srcParseBlock <- function(str) {
  # remove enclosure
  str <- sub("^\\$\\{(.*?)\\}", "\\1", str)

  # check for global options (::)
  if (grepl("::", str)) {
    global.opts <- srcParseOptions(trimws(sub("::.*$", "", str)))
    str <- trimws(sub("^.*::", "", str))
  } else {
    global.opts <- character(0)
  }

  # split variable definitions
  variables <- trimws(strsplit(str, split = ",")[[1]])

  # for each variable definition
  lapply(variables, function(x) {
    if (grepl(":", x)) {
      opts <- srcParseOptions(trimws(sub(":.*$", "", x)))
      opts <- srcResolveOptions(opts, global.opts)
    } else {
      opts <- global.opts
    }

    # remove options
    x <- trimws(sub("^.*:", "", x))

    if (grepl("=", x)) {
      assign.name <- trimws(sub("=.*$", "", x))
      input.name <- trimws(sub("^.*=", "", x))
    } else {
      assign.name <- NULL
      input.name <- x
    }

    list(
      opts = opts,
      assign.name = assign.name,
      input.name = input.name
    )
  })
}


srcParse <- function(str) {
  m <- gregexpr("\\$\\{.*?\\}", str)[[1]]
  ml <- attr(m, "match.length")

  starts <- sort(c(m, 1, m+ml))
  stops <- sort(c(m+ml-1, m-1, nchar(str)))

  stxt <- unname(mapply(substr, str, start = starts, stop = stops))

  # which parts to process
  w <- grep("\\$\\{.*?\\}", stxt)

  blocks <- lapply(stxt[w], function(x) {
    srcParseBlock(x)
  })

  res <- as.list(stxt)
  res[ w ] <- blocks

  return(res)
}


srcBuild <- function(src, inputs) {
  if (is.null(names(inputs)))
    names(inputs) <- sapply(inputs, "[[", "name")

  # iterate through source code elements
  paste(lapply(src, function(x) {
    # this is a chunk of code without variables
    if (class(x) == "character")
      return(x)

    # iterate through variables
    vars <- lapply(x, function(v) {
      input <- inputs[[ v$input.name ]]

      val.null <- (is.null(input$value) || input$value == "")
      val.default <- (input$value == getHandler(input)$get.value(input, NULL, value = input$default))

      # n: suppress null
      if (val.null && ("n" %in% v$opts))
        return(NULL)

      # d: suppress default
      if (val.default && ("d" %in% v$opts))
        return(NULL)

      if (val.null)
        value.text <- "NULL"
      else
        value.text <- getHandler(input)$get.source(input, value = input$value) # input$source

      # q: single quote
      if (!val.null && ("q" %in% v$opts))
        value.text <- paste0("'", value.text, "'")

      # Q: double quotes
      if (!val.null && ("Q" %in% v$opts))
        value.text <- paste0('"', value.text, '"')

      # result with or without assignment
      if (!is.null(v$assign.name))
        paste0(v$assign.name, "=", value.text)
      else
        value.text
    })

    vars <- vars[ !sapply(vars, is.null) ]

    paste(vars, collapse = ", ")
  }), collapse = "")
}


assignValue <- function(src, varname) {
  src <- strsplit(src, split = "\n")[[1]]
  src[ length(src) ] <- paste(varname, "<-", src[ length(src) ])
  paste(src, collapse="\n")
}













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

  if (!all(opts %in% SRC.VALID.OPTIONS)) {
    stop(opts[ which(!(opts %in% SRC.VALID.OPTIONS)) ], " is not a valid option.")
  }

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

      if (!is.null(input)) {
        val.null <- (is.null(input$value) || input$value == "")
        val.default <- (input$value == getHandler(input)$get.value(input, NULL, value = input$default))
      } else {
        val.null <- TRUE
        val.default <- FALSE
      }

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


#
# Blueprint preprocessor
#


example.source <- "
### This is a preprocessor comment. It will be suppressed in source code and
### will not be processed by the preprocessor.

### Below is a section declaration. The section contents ends when another
### section begins or when \"#-- end section\" is read, or when the file ends.
#-- print  ### A comment can appear inline. All further contents of line is ignored.

# This is a normal R comment. It will be shown in source code.
df <- iris

#-- plot plot 1, width = 120px

plot(${dn :: x:x(expression), y=y, type=type, main=main(character, \"Hello\"), xlab=xlab, ylab=ylab})

#-- table my table

head(iris, ${num_rows(numeric, 6, page = \"Preview Settings\")})

"




preprocessInputOptions <- function(text) {
  stopifnot("Text must be a single string." = (length(text) == 1))

  if (text == "")
    return(list(psource = ""))

  opts <- strsplit(text, split = ",")[[1]] %>% trimws

  # first unnamed value is input.type
  if (!grepl("=", opts[[1]]))
    opts[[1]] <- paste0("input.type = '", opts[[1]], "'")

  # if second unnamed value, it's the default value
  if (length(opts) > 1 && !grepl("=", opts[[2]]))
    opts[[2]] <- paste("default = ", opts[[2]], "")

  # make a list
  options <- new.env()
  opts <- paste(opts, collapse = "\n")
  eval(parse(text = opts), envir = options)
  options <- as.list(options)

  return(options)
}


#' Extract Input Definitions From Blueprint Source
#'
#' @param text a single character string representing the source code
#'   to preprocess.
#'
#' @return A list where each element represents an input. Each element of
#'   an input represents a option that can be passed on to slateInput() in
#'   order to construct the input.
#' @export
#'
#' @examples
preprocessInputs <- function(text) {
  # Note to self: I'm having way too much fun with purrr...
  stopifnot("Text must be a single string." = (length(text) == 1))

  # split into elements of the form ${<element>}
  x <-
    regmatches(text, gregexpr("\\$\\{.*?\\}", text))[[1]] %>%
    { gsub("^\\$\\{.*?::", "", .) } %>%    # remove "${ xxx::"
    { gsub("^\\$\\{|\\}$", "", .) } %>%    # remove  "${" and "}"
    trimws

  # get all input names
  input.names <-
    gsub("\\(.*?\\)", "", x) %>%
    strsplit(split = ",") %>%
    unlist %>%
    { gsub(":.*$|^.*=", "", .) } %>%
    unique

  # get options for inputs that have them
  input.options <-
    map(x, ~regmatches(.x, gregexpr("[^ :=]+\\(.*?\\)", .x))) %>%
    unlist %>%
    { set_names(., gsub("\\(.*$", "", .)) } %>%
    { gsub("^.*\\(|\\)$", "", .) } %>%
    map(~preprocessInputOptions(.x))

  # put it all together
  inputs <-
    map(input.names, ~c(list(name = .x), input.options[[ .x ]])) %>%
    set_names(map(., "name")) %>%
    modify_if(~is.null(.$input.type), ~c(.x, input.type = "character"))


  return(inputs)
}



preprocessSection <- function(lines) {
  section <- list()

  # preprocess the header
  if (grepl("^#--", lines[1])) {
    # has header
    if (length(lines) == 1) {
      return(NULL)
    }

    x <-
      gsub("^#--", "", lines[1]) %>%
      { strsplit(., split = ",")[[1]] } %>%
      trimws

    section$type <- sub("^(.*?) .*$", "\\1", x[[1]])

    if (grepl(" ", x[[1]]) == TRUE)
      section$name <- sub("^.*? (.*)$", "\\1", x[[1]])
    else
      section$name <- "Untitled"

    section$params <- if (length(x) > 1) x[[2]] else ""
    section$source <- paste(lines[2:length(lines)], collapse="\n")
  } else {
    # no header: generic text output
    section$type <- "print"
    section$name <- "Generic"
    section$params <- ""
    section$source <- paste(lines, collapse = "\n")
  }

  return(section)
}


preprocessSections <- function(text) {
  stopifnot("Text must be a single string." = (length(text) == 1))

  # sections
  lines <- strsplit(text, split = "\n")[[1]]

  # check for section markers
  idx <- grep("^#\\-\\-", lines)
  if (length(idx) > 0) {
    idx <- c(idx, length(lines)+1)

    sections <-
      map2(idx[1:(length(idx)-1)],
           idx[ 2:length(idx) ] - 1,
           ~lines[ .x:.y ]) %>%
      map(preprocessSection)
  } else {
    sections <- list()
  }

  # check for unmarked sections
  if (length(idx) == 0) {
    sections <- list(preprocessSection(lines))
  } else if (idx[1] > 1) {
    sections <-
      lines[1:(idx[1] - 1)] %>%
      preprocessSection %>%
      { c(list(.), sections) }
  }

  sections %<>%
    discard(is.null) %>%
    set_names(map(., "name"))

  return(sections)
}




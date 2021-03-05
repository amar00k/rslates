


# utility function to split a list of expressions separated by comma
# while ignoring commas inside parenthesis
split.parameters <- function(text) {
  stopifnot("Text must be a single string." = (length(text) == 1))

  # split characters
  charmap <- strsplit(text, split="") %>%
    unlist

  # ( = 1, ) = -1, else = 0
  parmap <- charmap %>%
    map_dbl(~switch(., "(" = 1, ")" = -1, 0))

  # check
  if (sum(parmap) != 0)
    stop("Mismatched parenthesis in: '", text, "'")

  charmap %>%
    # inside parenthesis, replace "," by "#"
    modify_at(which(charmap == "," & cumsum(parmap) > 0), ~"#") %>%
    # restore the string
    paste(collapse="") %>%
    # split by "," and trim (this splits the string by variable)
    strsplit(split=",") %>%
    unlist %>%
    trimws %>%
    # restore the "," separating variable options
    gsub("#", ",", .)
}


#' Clear comments from text
#'
#' @param text a string
#' @param n number of # symbols (i.e. n = 2 clears text appearing after ## or ###
#'   but not #)
#'
#' @return the clean text
#'
#' @examples
clear.comments <- function(text, n = 1) {
  stopifnot("Text must be a single string." = (length(text) == 1))

  map(text, strsplit, split = "\n") %>%
    unlist %>%
    sub(paste0(s, "{", n, "}.*$"), "", .)
}


#
# Input substitutions in preprocessed source
#

substituteVariable <- function(input, opts, assign = "") {
  if (is.null(input))
    stop("Need input, got NULL.")

  value <- input$value

  if (is.null(value))
    value <- input$default

  is.val.null <- (value == "" || is.na(value))
  is.val.default <- (value == input$default)

  # n: suppress null
  if (is.val.null && ("n" %in% opts))
    return(character(0))

  # d: suppress default
  if (is.val.default && ("d" %in% opts))
    return(character(0))

  if (is.val.null)
    value.text <- "NULL"
  else
    value.text <- getHandler(input)$as.source(input, value = value)

  # q: single quote
  if (!is.val.null && ("q" %in% opts))
    value.text <- paste0("'", value.text, "'")

  # Q: double quotes
  if (!is.val.null && ("Q" %in% opts))
    value.text <- paste0('"', value.text, '"')

  # result with or without assignment
  if (assign != "")
    paste0(assign, "=", value.text)
  else
    value.text

  return(value.text)
}


substituteVariableBlock <- function(block, inputs) {
  map(block, ~substituteVariable(inputs[[ .$varname ]], .$output.options, .$assign)) %>%
    paste(collapse = ", ")
}



substituteVariables <- function(section, blocks, inputs) {
  # find blocks that occur in section
  blocks <- blocks[
    map(names(blocks), ~grepl(., section, fixed = TRUE)) %>%
      unlist
  ]

  # compute the substitutions
  subs <- map(blocks, substituteVariableBlock, inputs)

  # apply substitutions to section text
  map2(names(subs), subs, sub(.x, .y, section, fixed = TRUE))
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

SRC.VALID.OPTIONS <- strsplit("xqQnNdDl", split="")[[1]]

resolveVariableOutputOptions <- function(var.opts, global.opts) {
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

  stopifnot('Options "n" and "N" cannot be used together.' = !all(c("n", "N") %in% opts))
  stopifnot('Options "d" and "D" cannot be used together.' = !all(c("d", "D") %in% opts))
  stopifnot('Options "q" and "Q" cannot be used together.' = !all(c("q", "Q") %in% opts))

  return(opts)
}


preprocessVariableOutputOptions <- function(text) {
  stopifnot("Text must be a single string." = (length(text) == 1))

  opts <- strsplit(text, split = "")[[1]]

  if (!all(opts %in% SRC.VALID.OPTIONS)) {
    stop("'", opts[ which(!(opts %in% SRC.VALID.OPTIONS)) ], "' is not a valid output option.")
  }

  return(opts)
}


# text: string of the form
# [type][, default][, name1 = value1][...]
preprocessVariableInputOptions <- function(text) {
  stopifnot("Text must be a single string." = (length(text) == 1))

  opts <- split.parameters(text)

  # first unnamed value is input.type
  if (!grepl("=", opts[[1]]))
    opts[[1]] <- paste0("input.type = \"", opts[[1]], "\"")

  # if second unnamed value, it's the default value
  if (length(opts) > 1 && !grepl("=", opts[[2]]))
    opts[[2]] <- paste0("default = ", opts[[2]])

  if (!all(grepl("=", opts))) {
    stop("Error parsing variable definition: ", text,
         ". Missing \"=\" sign in '",
         opts[ which(!grepl("=", opts))[1] ], "'.")
  }

  options <- tryCatch({
    # evaluate the options
    options <- new.env()
    opts <- paste(opts, collapse = "\n")
    eval(parse(text = opts), envir = options)

    # character is default input type
    if (is.null(options$input.type))
      options$input.type <- "character"
    else if (!(options$input.type %in% names(input.handlers)))
      stop(options$input.type, " is not a valid input type.")

    # coerce the default value to the appropriate type
    # (this may raise an error)
    if (!is.null(options$default)) {
      options$default <- input.handlers[[ options$input.type ]]$as.value(
        value = options$default
      )
    }

    # coerce to list type and sort alphabetically
    options <- as.list(options)
    options <- options[ order(names(options)) ]

    options
  },
  error = function(e) {
    stop(paste0("Error parsing variable definition: ", text))
  })

  return(options)
}


# text: a single input variable
# [output_options:][assing.name = ]varname[(input options)]
preprocessInputVariable <- function(text) {
  stopifnot("Text must be a single string." = (length(text) == 1))

  # extract output options
  if (grepl("^[^\\()]*?:", text)) {
    text <- strsplit(text, ":")[[1]] %>% trimws
    output.options <- preprocessVariableOutputOptions(text[1])
    text <- paste(text[ 2:length(text) ], collapse = ":")
  } else {
    output.options <- ""
  }

  # extract assign name
  if (grepl("^[^\\(]*?=", text)) {
    text <- strsplit(text, "=")[[1]] %>% trimws
    assign <- text[1]
    text <- paste(text[ 2:length(text) ], collapse = "=")
  } else
    assign <- ""

  # extract varname and input options
  if (grepl("\\(.*\\)", text)) {
    varname <- sub("^(.*?)\\(.*$", "\\1", text) %>% trimws
    input.options <-
      trimws(text) %>%
      sub("^.*?\\(", "", .) %>%
      sub("\\)$", "", .) %>%
      trimws %>%
      preprocessVariableInputOptions
  } else {
    varname <- text
    input.options <- list(input.type = "character")
  }

  var <- list(
    varname = varname,
    output.options = output.options,
    assign = assign,
    input.options = input.options
  )

  return(var)
}


# text: a block definition of the form:
# [global.opts::] [opts1:]var1[(options1)], [opts2:]var2[(options2)], ...
preprocessInputVariableBlock <- function(text) {
  stopifnot("Text must be a single string." = (length(text) == 1))

  # global block options
  if (grepl("::", text)) {
    global.opts <-
      text %>%
      sub("::.*$", "", .) %>%
      preprocessVariableOutputOptions

    text <- sub("^.*?::", "", text) %>% trimws
  } else {
    global.opts <- character(0)
  }

  # preprocess the variables
  split.parameters(text) %>%
    map(preprocessInputVariable) %>%
    set_names(map_chr(., "varname"))
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
  stopifnot("Text must be a single string." = (length(text) == 1))

  # split into elements of the form ${<element>}
  block.matches <-
    regmatches(text, gregexpr("\\$\\{.*?\\}", text))[[1]]

  blocks <-
    block.matches %>%
    gsub("^\\$\\{.*?::", "", .) %>%    # remove "${ xxx::"
    gsub("^\\$\\{|\\}$", "", .) %>%    # remove  "${" and "}"
    trimws %>%
    map(preprocessInputVariableBlock) %>%
    set_names(block.matches)

  inputs <-
    unlist(blocks, recursive = FALSE) %>%
    map(~c(name = .$varname, .$input.options)) %>%
    set_names(map(., "name"))

  return(list(blocks = blocks, inputs = inputs))
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

    if (!(section$type %in% names(output.handlers)))
      stop("Invalid output type: ", section$type)

    if (grepl(" ", x[[1]]) == TRUE) {
      section$name <- sub("^.*? (.*)$", "\\1", x[[1]]) %>%
        clear.comments %>%
        trimws
    }

    if (is.null(section$name) || section$name == "")
      section$name <- section$type

    section$params <- if (length(x) > 1) x[[2]] else ""
    section$source <- paste(lines[2:length(lines)], collapse="\n")
  } else {
    # no header: generic text output
    section$type <- "print"
    section$name <- "Preamble"
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
  idx <- grep("^#--", lines)
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


preprocessSource <- function(text) {
  input.data <- preprocessInputs(text)
  section.data <- preprocessSections(text)

  return(list(
    inputs = input.data$inputs,
    blocks = input.data$blocks,
    sections = section.data
  ))
}






# utility function to split a list of expressions separated by comma
# while ignoring commas inside parenthesis
split.parameters <- function(text) {
  # split characters
  charmap <- strsplit(text, split="") %>%
    unlist

  # ( = 1, ) = -1, else = 0
  parmap <- charmap %>%
    map_dbl(~switch(., "(" = 1, ")" = -1, 0))

  # check
  if (sum(parmap) != 0)
    stop("Mismatched parenthesis:", text)

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


parseVariableOutputOptions <- function(text) {
  opts <- strsplit(text, split = "")[[1]]

  if (!all(opts %in% SRC.VALID.OPTIONS)) {
    stop('"', opts[ which(!(opts %in% SRC.VALID.OPTIONS)) ], "' is not a valid output option.")
  }

  return(opts)
}


# text: string of the form
# [type][, default][, name1 = value1][...]
parseVariableInputOptions <- function(text) {
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
         ". Missing \"=\" sign in ",
         opts[ which(!grepl("=", opts))[1] ], ".")
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
parseInputVariable <- function(text) {
  stopifnot("Text must be a single string." = (length(text) == 1))

  # extract output options
  if (grepl("^[^\\()]*?:", text)) {
    text <- strsplit(text, ":")[[1]] %>% trimws
    output.options <- parseVariableOutputOptions(text[1])
    text <- paste(text[ 2:length(text) ], collapse = ":")
  } else {
    output.options <- ""
  }

  # extract assign name
  if (grepl("^[^\\(]*?=", text)) {
    text <- strsplit(text, "=")[[1]] %>% trimws
    assign.name <- text[1]
    text <- paste(text[ 2:length(text) ], collapse = "=")
  } else
    assign.name <- ""

  # extract varname and input options
  if (grepl("\\(.*\\)", text)) {
    varname <- sub("^(.*?)\\(.*$", "\\1", text) %>% trimws
    input.options <-
      trimws(text) %>%
      sub("^.*?\\(", "", .) %>%
      sub("\\)$", "", .) %>%
      trimws %>%
      parseVariableInputOptions
  } else {
    varname <- text
    input.options <- ""
  }

  var <- list(
    varname = varname,
    output.options = output.options,
    assign.name = assign.name,
    input.options = input.options
  )

  return(var)
}


# text: a block definition of the form:
# [global.opts::] [opts1:]var1[(options1)], [opts2:]var2[(options2)], ...
parseInputVariableBlock <- function(text) {
  text <- paste(text, collapse = ", ")

  # global block options
  if (grepl("::", text)) {
    global.opts <-
      text %>%
      sub("::.*$", "", .) %>%
      parseVariableOutputOptions

    text <- sub("^.*?::", "", text) %>% trimws
  } else {
    global.opts <- character(0)
  }

  # parse the variables
  split.parameters(text) %>%
    map(parseInputVariable) %>%
    set_names(map_chr(., "varname"))
}




substituteValue <- function(opts, varname, inputs, assign = NULL) {
  input <- inputs[[ varname ]]

  if (is.null(input))
    stop("Need input, got NULL.")

  value <- input$value

  # if (is.null(input))
  #    stop("Input must not be NULL.")

  # if (!is.null(input)) {
  #   is.val.null <- is.null(input$value)
  #   is.val.default <- input$value == input$default
  # } else {
  #   is.val.null <- TRUE
  #   is.val.default <- FALSE
  # }

  is.val.null <- is.null(value)
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
  if (!is.null(assign))
    paste0(assign, "=", value.text)
  else
    value.text

  return(value.text)
}


# Tests:
# "${x(expression), 1:10)"
# "${d:x_x(numeric, 20, min = 20, max = 100), q:res(numeric), nd:val=value}"
#
substituteVariable <- function(text, inputs) {
  text <-
    text %>%
    gsub("^\\$\\{|\\}$", "", .) %>%   # remove enclosing ${...}
    gsub("\\(.*?\\)", "", .)          # remove every pair of (...)

  # global block options
  if (grepl("::", text)) {
    global.opts <-
      text %>%
      sub("::.*$", "", .) %>%
      parseOptions
  } else {
    global.opts <- character(0)
  }

  values <-
    text %>%
    sub("^.*?::", "", .) %>%
    strsplit(split = ",") %>%
    unlist %>%
    trimws %>%
    map(~{
      if (grepl(":", .x)) {
        opts <- parseOptions(sub(":.*$", "", .x)) %>%
          resolveOptions(global.options)
        .x <- sub("^.*?:", "", .x)
      } else {
        opts <- character(0)
      }

      if (grepl("=", .x)) {
        assign <- sub("=.*$", "", .x)
        .x <- sub("^.*=", "", .x)
      } else {
        assign <- NULL
      }

      c(varname = .x, opts = opts, assign = assign)
    }) %>%
    map(~substituteValue(.["opts"], .["varname"], inputs, .["namedvar"])) %>%
    paste(collapse = ", ")
}


substituteVariables <- function(text, inputs) {
  matches <- regmatches(text, gregexpr("\\$\\{.*?\\}", text))[[1]]

  for (m in matches)
    text <- sub(m, substituteVariable(m, inputs), text, fixed = TRUE)

  return(text)
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
  blocks <-
    regmatches(text, gregexpr("\\$\\{.*?\\}", text))[[1]] %>%
    { gsub("^\\$\\{.*?::", "", .) } %>%    # remove "${ xxx::"
    { gsub("^\\$\\{|\\}$", "", .) } %>%    # remove  "${" and "}"
    trimws %>%
    map(parseInputVariableBlock)

  inputs <-
    unlist(blocks, recursive = FALSE) %>%
    map(~c(name = .$varname, .$input.options))

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




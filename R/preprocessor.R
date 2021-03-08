




removeComments <- function(text, n = 1) {
  stopifnot("Text must be a single string." = (length(text) == 1))

  gsub(paste0("(?<!#)#{", n, "}(?!#).*?(\n|$)"), "", text, perl = TRUE)
}


disableComments <- function(text, n = 1) {
  stopifnot("Text must be a single string." = (length(text) == 1))

  gsub(paste0("(?<!#)#{", n, "}(?!#)"), "", text, perl = TRUE)
}


# make regular expression for preprocessor directives
makePreprocessorDirectiveRE <- function(tokens) {
  tokens.re <-
    paste(tokens, collapse = "|") %>%
    paste0("(", ., ")")

  re.1 <- paste0("\\$@ *", tokens.re, " *[^ \n]+ *\\((.|\n)*?\\) *(?=\n)")
  re.2 <- paste0("\\$@ *", tokens.re, " *[^ \n]+")
  re.3 <- paste0("\\$@ *", tokens.re)

  paste(re.1, re.2, re.3, sep = "|")
}


cleanPreprocessorDirectives <- function(text) {
  stopifnot("Text must be a single string." = (length(text) == 1))

  tokens <- c("title", "tags", "preamble",
              "page", "group", "input",
              "output", "end-output",
              "dataset", "end-dataset",
              "if", "end-if")

  re <- makePreprocessorDirectiveRE(tokens)

  gsub(re, "", text, perl = TRUE)
}


#
# Input substitutions in preprocessed source
#

substituteVariable <- function(input, opts, assign = "") {
  if (is.null(input))
    stop("need input, got NULL.")

  value <- input$value

  if (is.null(value))
    value <- input$default

  is.val.null <- identical(value, "")
  is.val.default <- identical(value, input$default)

  # n: suppress null
  if (is.val.null && ("n" %in% opts))
    return(NULL)

  # d: suppress default
  if (is.val.default && ("d" %in% opts))
    return(NULL)

  #if (is.val.null)
  #  value.text <- "NULL"
  #else
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

substituteVariables <- function(text, blocks, inputs) {
  stopifnot("Text must be a single string." = (length(text) == 1))

  # find blocks that occur in text
  blocks %<>%
    keep(
      map_lgl(map(blocks, "source"), ~grepl(., text, fixed = TRUE))
    )

  if (length(blocks) == 0)
    return (text)

  # compute the substitutions
  substitutions <-
    map_chr(blocks, ~{
      variables <- .$variables

      map(variables, ~substituteVariable(inputs[[ .$name ]], .$options, .$assign)) %>%
        discard(is.null) %>%
        paste(collapse = ", ")
    })

  substitutions <-
    list(from = map_chr(blocks, "source"), to = substitutions) %>%
    transpose

  # apply substitutions to text
  for (x in substitutions) {
    text <- sub(x$from, x$to, text, fixed = TRUE)
  }

  return(text)
}


#
# Blueprint preprocessor
#


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

SUBSTITUTE.OPTIONS <- strsplit("xqQnNdDl", split="")[[1]]


#' Resolves Conflicts Between Variable and Block Substitution Options
#'
#' @param var.opts character vector of variable options
#' @param block.opts character vector of block options
#'
#' @return The resolved variable options.
#'
#' @details throws an error if the options combination cannot be resolved.
resolveSubstitutionOptions <- function(var.opts, block.opts) {
  if (any(c("n", "N") %in% var.opts))
    block.opts <- block.opts[ !(block.opts %in% c("n", "N")) ]

  if (any(c("d", "D") %in% var.opts))
    block.opts <- block.opts[ !(block.opts %in% c("d", "D")) ]

  if (any(c("q", "Q") %in% var.opts))
    block.opts <- block.opts[ !(block.opts %in% c("q", "Q")) ]

  if ("x" %in% var.opts) {
    opts <- var.opts[ var.opts != "x" ]
  } else {
    opts <- unique(c(var.opts, block.opts))
  }

  stopifnot('Options "n" and "N" cannot be used together.' = !all(c("n", "N") %in% opts))
  stopifnot('Options "d" and "D" cannot be used together.' = !all(c("d", "D") %in% opts))
  stopifnot('Options "q" and "Q" cannot be used together.' = !all(c("q", "Q") %in% opts))

  return(opts)
}


#' Preprocess Substitution Options for a Variable or Block
#'
#' @param text the text of the options
#'
#' @return A character vector of individual options
#'
#' @details throws an error if any of the options is invalid.
preprocessSubstitutionOptions <- function(text) {
  stopifnot("Text must be a single string." = (length(text) == 1))

  opts <- strsplit(text, split = "")[[1]]

  if (!all(opts %in% SUBSTITUTE.OPTIONS)) {
    stop("'", opts[ which(!(opts %in% SUBSTITUTE.OPTIONS)) ], "' is not a valid output option.")
  }

  return(opts)
}


# ${ [global.opts::] [opts1:]var1[(options1)], [opts2:]var2[(options2)], ... }
preprocessSubstitutionBlock <- function(text) {
  stopifnot("Text must be a single string." = (length(text) == 1))

  body <- gsub("^\\$\\{|\\}$", "", text) %>%    # remove  "${" and "}"
    trimws

  # global block options
  if (grepl("::", body)) {
    block.opts <-
      sub("::.*$", "", body) %>%
      trimws %>%
      preprocessSubstitutionOptions

    body <-
      sub("^.*?::", "", body) %>%
      trimws
  } else {
    block.opts <- character(0)
  }

  # Split into variables
  # we temporarily "mask" commas inside parenthesis to avoid
  # splitting the string inside an inline variable definition
  charmap <- strsplit(body, split="") %>% unlist
  parmap <- map_dbl(charmap, ~switch(., "(" = 1, ")" = -1, 0))

  # stop early if the expression is invalid
  if (sum(parmap) != 0)
    stop("Mismatched parenthesis in: '", body, "'")

  variables <-
    # inside parenthesis, replace "," by "#"
    modify_at(charmap, which(charmap == "," & cumsum(parmap) > 0), ~"#") %>%
    # restore the string
    paste(collapse="") %>%
    # split by "," and trim (this splits the string by variable)
    strsplit(split=",") %>%
    unlist %>%
    trimws %>%
    # restore the "," separating variable options
    gsub("#", ",", .)

  # preprocess substitution options
  opts <- ifelse(grepl("^[^\\(]*:", variables),
                 sub("^([^\\(]*):.*$", "\\1", variables), "") %>%
    map(preprocessSubstitutionOptions) %>%
    map(resolveSubstitutionOptions, block.opts = block.opts)

  variables <- sub("^[^\\(]*:", "", variables)

  # preprocess assignments
  assign <- ifelse(grepl("^[^\\(]*=", variables),
                   sub("^([^\\(]*)=.*$", "\\1", variables), "")

  variables <- sub("^[^\\(]*=", "", variables)

  # get the variable names
  varnames <- sub("\\(.*\\).*$", "", variables)

  # preprocess inline definitions
  inputs <- map(variables, preprocessLayoutDefinition, type = "input")

  variables <- list(name = varnames, options = opts, assign = assign, input = inputs) %>%
    transpose %>%
    set_names(map(., "name"))

  # put it all together
  list(
    source = text,
    variables = variables
  )
}


# x(type, default, ...)
preprocessLayoutDefinition <- function(text, type) {
  stopifnot("Text must be a single string." = (length(text) == 1))

  # extract name of input
  name <- strsplit(text, "\\(")[[1]][1]

  call.fun <- switch(type,
                     "input" = "slateInput",
                     "group" = "slateGroup",
                     "page" = "slatePage")

  # insert the name as the first argument inside the parenthesis
  # and add call to call.fun function
  if (!grepl("\\(.*\\)", text)) {
    call.text <- paste0(call.fun, "(\"", name, "\")")
  } else {
    call.text <- sub("^.*?(?=\\()", "", text, perl = TRUE) %>%
      sub("^\\(", paste0(call.fun, "(\"", name, "\", "), .)
  }

  # handle input type using a
  # trick to allow unquoted input types
  if (type == "input") {
    types.list <- as.list(names(input.handlers)) %>% set_names(.)
    env <- list2env(types.list)
  } else {
    env <- parent.frame()
  }

  input <- tryCatch({
    # create the expression (may throw error)
    expr <- str2expression(call.text)

    # evaluate the slateInput(...) call
    eval(expr, envir = env)
  },
  error = function(e) {
    stop(paste0("Error parsing variable definition: ", text))
  })
}


preprocessOutput <- function(lines) {
  header <- sub("^.*\\$@ *output *", "", lines[1])
  name <- sub(" *\\(.*", "", header)

  call.text <- sub("^.*?(?=\\()", "", header, perl = TRUE) %>%
    sub("^\\(", paste0("slateOutput(\"", name, "\", "), .)

  types.list <- as.list(names(output.handlers)) %>% set_names(.)
  env <- list2env(types.list)

  output <- tryCatch({
    expr <- str2expression(call.text)
    eval(expr, envir = env)
  },
  error = function(e) {
    stop(paste0("Error parsing output definition: ", header))
  })

  body <- lines[2:length(lines)]

  output$source <-
    paste(body, collapse="\n") %>%
    cleanPreprocessorDirectives

  return(output)
}



preprocessSource <- function(text) {
  stopifnot("Text must be a single string." = (length(text) == 1))

  # initialize the return structure
  blueprint.data <- list(
    title = "Untitled",
    preamble = FALSE,
    pages = list(),
    groups = list(),
    inputs = list(),
    outputs = list(),
    datasets = list(),
    blocks = list()
  )

  # clear preprocessor-level comments
  clean.text <- text %>%
    removeComments(n = 3) %>%
    disableComments(n = 2)

  # get matches
  matches <- makePreprocessorDirectiveRE(c("title", "preamble")) %>%
    gregexpr(clean.text, perl = TRUE) %>%
    regmatches(clean.text, .) %>%
    unlist() %>%
    sub("^\\$@ *", "", .)

  # extract the type of definition
  matches.type <- strsplit(matches, " ") %>%
    map_chr(~.[[1]])

  # set the title
  if ("title" %in% matches.type) {
    blueprint.data$title <-
      matches[[ match("title", matches.type) ]] %>%
      sub("^title *", "", .)
  }

  # set the pramble
  if ("preamble" %in% matches.type) {
    blueprint.data$preamble <-
      matches[[ match("preamble", matches.type) ]] %>%
      sub("^preamble *", "", .) %>%
      as.logical
  }

  # parse pages
  blueprint.data$pages <- makePreprocessorDirectiveRE("page") %>%
    gregexpr(clean.text, perl = TRUE) %>%
    regmatches(clean.text, .) %>%
    unlist() %>%
    sub("^\\$@page *", "", .) %>%
    map(preprocessLayoutDefinition, "page") %>%
    set_names(map(., "name"))

  # parse groups
  blueprint.data$groups <- makePreprocessorDirectiveRE("group") %>%
    gregexpr(clean.text, perl = TRUE) %>%
    regmatches(clean.text, .) %>%
    unlist() %>%
    sub("^\\$@group *", "", .) %>%
    map(preprocessLayoutDefinition, "group") %>%
    set_names(map(., "name"))

  # parse inputs
  blueprint.data$inputs <- makePreprocessorDirectiveRE("input") %>%
    gregexpr(clean.text, perl = TRUE) %>%
    regmatches(clean.text, .) %>%
    unlist() %>%
    sub("^\\$@input *", "", .) %>%
    map(preprocessLayoutDefinition, "input") %>%
    set_names(map(., "name"))


  # Handle outputs
  lines <- strsplit(text, split = "\n")[[1]]

  start.idx <- makePreprocessorDirectiveRE("output") %>%
    grep(lines, perl = TRUE)
  end.idx <- makePreprocessorDirectiveRE("end-output") %>%
    grep(lines, perl = TRUE)

  idx <- sort(c(start.idx, end.idx, length(lines)+1))

  # last line of each output
  end.idx <- map_dbl(start.idx, ~{
    min(idx[ which(idx > .)]) - 1
  })

  blueprint.data$outputs <-
    map2(start.idx, end.idx, ~lines[ .x:.y ]) %>%
    map(preprocessOutput) %>%
    set_names(map(., "name"))


  # Handle substitution blocks
  block.matches <-
    gregexpr("\\$\\{.*?\\}", text) %>%
    regmatches(text, .) %>%
    unlist

  blueprint.data$blocks <- block.matches %>%
    map(preprocessSubstitutionBlock)

  # resolve conficts with inputs
  block.inputs <- map(blueprint.data$blocks, "variables") %>%
    unlist(recursive = FALSE) %>%
    map("input") %>%
    keep(~!is.null(.)) %>%
    discard(~.$name %in% names(blueprint.data$inputs)) %>%
    set_names(map(., "name"))

  if (length(block.inputs) > 0)
    blueprint.data$inputs %<>% append(block.inputs)

  return(blueprint.data)
}


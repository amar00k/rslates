

quoteString <- function(text, single = FALSE) {
  if (single)
    paste0("'", text, "'")
  else
    paste0('"', text, '"')
}


removeComments <- function(text, n = 1) {
  stopifnot("Text must be a single string." = (length(text) == 1))

  gsub(paste0("(?<!#)#{", n, "}(?!#).*?(\n|$)"), "", text, perl = TRUE)
}


disableComments <- function(text, n = 1) {
  stopifnot("Text must be a single string." = (length(text) == 1))

  gsub(paste0("(?<!#)#{", n, "}(?!#)"), "", text, perl = TRUE)
}


# make regular expression for preprocessor directives
makePreprocessorDirectiveRE <- function(tokens, level = 3) {
  tokens.re <-
    paste(tokens, collapse = "|") %>%
    paste0("(", ., ")")

  re <- c(#paste0("\\$@ *", tokens.re, " *[^ \n]+ *\\((.|\n)*?\\) *(?=\n)"),
          paste0("\\$@ *", tokens.re, " *[^ ,\n]+ *(.|\n)*?[^(, ?)(\\( ?)]\n"),
          paste0("\\$@ *", tokens.re, " *[^\n]+"),
          paste0("\\$@ *", tokens.re))
  re <- re[ 1:level ]

  paste(re, collapse = "|")
}


cleanPreprocessorDirectives <- function(text) {
  stopifnot("Text must be a single string." = (length(text) == 1))

  tokens <- c("page", "group", "input",
              "output", "end-output",
              "import", "export",
              "if", "end-if")

  re <- makePreprocessorDirectiveRE(tokens)

  gsub(re, "", text, perl = TRUE)
}


cleanEmptyLines <- function(text) {
  strsplit(text, split = "\n") %>%
    pluck(1) %>%
    trimws %>%
    keep(map_lgl(., ~. != "")) %>%
    paste(collapse = "\n")
}


syntaxText <- function(type) {
  switch(
    type,
    "export" = "$@export <var.name>, <out.name>",
    ""
  )
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
    value.text <- paste0(assign, "=", value.text)
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
  inputs <- map(variables, preprocessInlineInputDirective)
    #map(~list_modify(., parent = NULL))

  variables <- list(name = varnames, options = opts, assign = assign, input = inputs) %>%
    transpose %>%
    set_names(map(., "name"))

  # put it all together
  list(
    source = text,
    variables = variables
  )
}


#' Preprocess an Inline Input Directive
#'
#' @details Preprocess a directive of the form `name(parameter1, parameter2, ...)`. We do this
#'   by transforming the text into the form `name, parameter1, parameter2, ...` and passing
#'   it on to `preprocessDirective()`.
#'
#' @param text The directive text.
#'
#' @return An input obtained from a call to `slateInput`.
#'
#' @examples
preprocessInlineInputDirective <- function(text) {
  stopifnot("Text must be a single string." = (length(text) == 1))

  text <- trimws(text)

  if (grepl("^.*\\(.*\\)$", text)) {
    parameters <- sub("^.*?\\((.*)\\)$", ", \\1", text)
    preprocessDirective(paste(name, parameters), type = "input")$object
  } else {
    preprocessDirective(text, type = "input")$object
  }
}


#' Parse a Preprocessor Directive
#'
#' @description Parse a preprocessor directive of the form `$@<directive> name, ...`.
#'
#' @param text Text of the full directive including all parameters.
#' @param type Type of directive if the `$@<directive>` has been scrubbed.
#'
#' @return The resulting blueprint element structure obtained from a call to `slateInput`,
#'  `slateGroup`, `slatePage`, `slateImport` or `slateExport`.
#'
#' @examples
preprocessDirective <- function(text, type = NULL) {
  stopifnot("Text must be a single string." = (length(text) == 1))

  text <- trimws(text)

  if (is.null(type))
    type <- sub("^\\$@(page|group|input|import|export).*", "\\1", text)

  def.text <- sub("^\\$@(page|group|input|import|export) *", "", text)

  # make sure the first argument is quoted
  def.text <- strsplit(def.text, ",")[[1]] %>%
    modify_at(1, ~{
      if(!grepl("^\".*\"$|^'.*'$", .))
        quoteString(.)
      else
        .
    }) %>%
    paste(collapse = ",")

  call.fun <- switch(type,
                     "input" = "slateInput",
                     "group" = "slateGroup",
                     "page" = "slatePage",
                     "import" = "slateImport",
                     "export" = "slateExport")

  def.text <- paste0("rslates::", call.fun, "(", def.text, ")")

  # trick to allow unquoted input types
  env <- c(names(input.handlers), names(import.handlers)) %>%
    as.list %>%
    set_names(.) %>%
    list2env

  object <- tryCatch({
    # create the expression (may throw error)
    expr <- str2expression(def.text)

    # evaluate the slateInput(...) call
    eval(expr, envir = env)
  },
  error = function(e) {
    stop("Error preprocessing directive: ", text, ". ", e)
  })

  return(list(
    type = type,
    object = object
  ))
}


# syntax:
# $@output <type>, <name>[, option1,
preprocessOutput <- function(lines) {
  header <- sub("^.*\\$@ *output *", "", lines[1])

  params <- strsplit(header, split = ",")[[1]]

  if (length(params) < 2)
    stop("Need name and type.")

  name <- params[[1]] %>% trimws
  type <- params[[2]] %>% trimws

  if (!(type %in% names(output.handlers)))
    stop("Unknown output type: '", type, "'")

  if (length(params) > 2)
    params <- paste(params[3:length(params)], collapse = ",") %>% paste(", ", .)
  else
    params <- ""

  call.text <- paste0("slateOutput('", name, "', '", type, "'", params, ")")

  output <- tryCatch({
    expr <- str2expression(call.text)
    eval(expr) #, envir = env)
  },
  error = function(e) {
    stop(paste0("Error parsing output definition: ", header))
  })

  if (length(lines) > 1)
    body <- lines[2:length(lines)]
  else
    body <- ""

  output$source <-
    paste(body, collapse = "\n") %>%
    cleanPreprocessorDirectives

  return(output)
}


splitIntoDefinitionBlocks <- function(text) {
  stopifnot("Text must be a single string." = (length(text) == 1))

  lines <- strsplit(text, split = "\n")[[1]]

  if (length(lines) == 0)
    return(list())

  # split the lines into blocks, each associated with
  # a specific definition, or belonging to the top-level
  start.tokens <- c("\\$@output") # , "\\$@import")
  end.tokens <- c("\\$@end-output") #, "\\$@end-import")

  end.idx <- paste(end.tokens, collapse = "|") %>%
    grep(lines) %>%
    append(length(lines) + 1)

  start.idx <- paste(start.tokens, collapse = "|") %>%
    grep(lines) %>%
    prepend(1) %>%
    unique

  if (max(end.idx) < length(lines) + 1)
    start.idx <- append(start.idx, max(end.idx) + 1)

  idx <- sort(c(start.idx, end.idx, length(lines)+1))

  # last line of each output
  end.idx <- map_dbl(start.idx, ~{
    min(idx[ which(idx > .)]) - 1
  })

  map2(start.idx, end.idx, ~lines[ .x:.y ]) %>%
    map(~{
      header <- .[1]
      if (grepl("\\$@output", header))
        type <- "output"
      else
        type <- "top-level"

      list(
        type = type,
        body = .
      )
    })
}


preprocessSource <- function(text) {
  stopifnot("Text must be a single string." = (length(text) == 1))

  # initialize the return structure
  blueprint.data <- list(
    pages = list(),
    groups = list(),
    inputs = list(),
    outputs = list(),
    blocks = list(),
    imports = list(),
    exports = list(),
    datasets = list()
  )

  # sanitize text by removing \r
  text <- gsub("\r", "", text)

  # clear preprocessor-level comments
  clean.text <- text %>%
    removeComments(n = 3) %>%
    disableComments(n = 2)

  # process layout elements (pages, groups, inputs)
  directives <-
    makePreprocessorDirectiveRE("input|page|group|import|export") %>%
    gregexpr(clean.text, perl = TRUE) %>%
    regmatches(clean.text, .) %>%
    unlist() %>%
    map(preprocessDirective)

  layout <-
    keep(directives, ~.$type %in% c("page", "group", "input")) %>%
    map("object") %>%
    inferSlateLayout %>%
    set_names(map(., "name"))

  blueprint.data$pages <- keep(layout, ~.$type == "page")
  blueprint.data$groups <- keep(layout, ~.$type == "group")
  blueprint.data$inputs <- keep(layout, ~.$type == "input")

  blueprint.data$imports <- keep(directives, ~.$type == "import") %>%
    map("object") %>%
    set_names(map(., "name"))
  blueprint.data$exports <- keep(directives, ~.$type == "export") %>%
    map("object") %>%
    set_names(map(., "name"))

  # Handle substitutions
  blueprint.data$blocks <-
    gregexpr("\\$\\{.*?\\}", text) %>%
    regmatches(text, .) %>%
    unlist %>%
    map(preprocessSubstitutionBlock)

  # resolve conficts with inputs
  sub.inputs <- map(blueprint.data$blocks, "variables") %>%
    unlist(recursive = FALSE) %>%
    map("input") %>%
    keep(~!is.null(.)) %>%
    discard(~.$name %in% names(blueprint.data$inputs)) %>%
    set_names(map(., "name"))

  if (length(sub.inputs) > 0)
    blueprint.data$inputs %<>% append(sub.inputs)

  # prepare blocks
  source.blocks <- splitIntoDefinitionBlocks(text)

  # Handle top-level code
  blueprint.data$toplevel <-
    source.blocks %>%
    keep(map(., "type") == "top-level") %>%
    map(~paste(.$body, collapse = "\n")) %>%
    map(cleanPreprocessorDirectives) %>%
    map(cleanEmptyLines) %>%
    paste(collapse = "\n\n")

  # Handle outputs
  blueprint.data$outputs <-
    source.blocks %>%
    keep(map(., "type") == "output") %>%
    map("body") %>%
    map(preprocessOutput) %>%
    set_names(map(., "name"))

  return(blueprint.data)
}




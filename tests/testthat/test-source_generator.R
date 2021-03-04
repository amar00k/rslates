

# source <- readLines(system.file("test/test_blueprint.txt", package = "rslates")) %>%
#                       paste(collapse="\n")
# inputs <- preprocessInputs(source)
# sections <- preprocessSections(source)




test_that("parseInputVariableOptions works", {
  tests <- list(
    'character' = list(input.type = "character"),
    'numeric, 20' = list(default = 20, input.type = "numeric"),
    'numeric, default = 20' = list(default = 20, input.type = "numeric"),
    'expression, "1:10"' = list(default = "1:10", input.type = "expression"),
    'numeric' = list(input.type = "numeric"),
    'character, quote = FALSE' = list(input.type = "character", quote = FALSE),
  )

  for (name in names(tests)) {
    expect_equal(parseInputVariableOptions(name), tests[[name]])
  }
})


test_that("parseInputVariable works", {
  tests <- list(
    'qdl:num=num_lines(numeric, 10)' =
      list(varname = "num_lines", output.options = "qdl", assign.name = "num"))

  for (name in names(tests)) {
    got <- parseInputVariable(name)
    expect <- tests[[name]]

    for (var in names(expect))
      expect_true(got[[ var ]] == expect[[ var ]])
  }

})

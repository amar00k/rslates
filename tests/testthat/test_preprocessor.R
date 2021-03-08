

# source <- readLines(system.file("test/test_blueprint.txt", package = "rslates")) %>%
#                       paste(collapse="\n")
# inputs <- preprocessInputs(source)
# sections <- preprocessSections(source)



test_that("removeComments and disableComments works", {

  str <- "# line 1\n## line 2\n### line 3 ## more\n## line 4"

  expect_equal(removeComments(str, 1), "## line 2\n### line 3 ## more\n## line 4")
  expect_equal(removeComments(str, 2), "# line 1\n### line 3 ")
  expect_equal(removeComments(str, 3), "# line 1\n## line 2\n## line 4")

  expect_equal(disableComments(str, 1), " line 1\n## line 2\n### line 3 ## more\n## line 4")
  expect_equal(disableComments(str, 2), "# line 1\n line 2\n### line 3  more\n line 4")
  expect_equal(disableComments(str, 3), "# line 1\n## line 2\n line 3 ## more\n## line 4")

})


# test_that("parseInputVariableOptions works", {
#   tests <- list(
#     'character' = list(input.type = "character"),
#     'numeric, 20' = list(default = 20, input.type = "numeric"),
#     'numeric, default = 20' = list(default = 20, input.type = "numeric"),
#     'expression, "1:10"' = list(default = "1:10", input.type = "expression"),
#     'numeric' = list(input.type = "numeric"),
#     'character, quote = FALSE' = list(input.type = "character", quote = FALSE)
#   )
#
#   for (name in names(tests)) {
#     expect_equal(parseInputVariableOptions(name), tests[[name]])
#   }
# })
#

# test_that("parseInputVariable works", {
#   tests <- list(
#     'qdl:num=num_lines(numeric, 10)' =
#       list(varname = "num_lines", output.options = "qdl", assign.name = "num"))
#
#   for (name in names(tests)) {
#     got <- parseInputVariable(name)
#     expect <- tests[[name]]
#
#     for (var in names(expect))
#       expect_true(got[[ var ]] == expect[[ var ]])
#   }
#
# })


#blueprint <- blueprintFromJSON(system.file("blueprints/Scatterplot.json", package="rslates"))

pages <- list(slatePage("x", description = "Something."),
              slatePage("y", layout = "flow-4", extra = 1))

# test_that("simplifyBlueprint and restoreBlueprint work", {
#   #expect_identical(blueprint, restoreBlueprint(simplifyBlueprint(blueprint)))
# }

# test_that("flattenInputLayout works", {
#   layout <- blueprint$input.layout
#
#   expect_equal(
#     unique(sapply(flattenInputLayout(layout), "[[", "type")),
#     c("input", "group", "page")
#   )
#
#   expect_true(
#     all(sapply(flattenInputLayout(layout), "[[", "type") %in% c("page", "group", "input"))
#   )
# })
#
#
# test_that("traverseInputLayout works", {
#   layout <- blueprint$input.layout
#
#   expect_equal(layout, traverseInputLayout(layout))
#
#   test.layout <- traverseInputLayout(layout, function(x, a) { x$name <- "test"; x }, flatten = TRUE)
#   expect_true(all(sapply(test.layout, "[[", "name") == "test"))
# })
#
#
# test_that("updateInputLayoutItem works", {
#   layout <- blueprint$input.layout
#
#   new.layout <- updateInputLayoutItem(layout, inputPage("page"))
#   expect_true(length(new.layout$pages) == 4)
#   expect_true(names(new.layout$pages)[4] == "page")
#
#   new.layout <- updateInputLayoutItem(layout, inputGroup("group"), c("Graphical parameters"))
#   expect_true(length(new.layout$pages$`Graphical parameters`$children) == 2)
#
#   new.input <- slateInput("x", "character", "")
#   new.layout <- updateInputLayoutItem(layout, new.input, c("Graphical parameters"))
#   # TODO
# })
#
#
# test_that("simplifyBlueprint and restoreBlueprint work", {
#   expect_identical(blueprint, restoreBlueprint(simplifyBlueprint(blueprint)))
#   expect_lt(object.size(simplifyBlueprint(blueprint)), object.size(blueprint))
# })
#




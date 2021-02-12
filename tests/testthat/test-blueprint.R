

blueprint <- blueprintFromJSON(system.file("blueprints/Scatterplot.json", package="rslates"))


test_that("flattenInputLayout works", {
  layout <- blueprint$input.layout

  expect_equal(
    unique(sapply(flattenInputLayout(layout), "[[", "type")),
    c("input", "group", "page")
  )

  expect_true(
    all(sapply(flattenInputLayout(layout), "[[", "type") %in% c("page", "group", "input"))
  )
})


test_that("traverseInputLayout works", {
  layout <- blueprint$input.layout

  expect_equal(layout, traverseInputLayout(layout))

  test.layout <- traverseInputLayout(layout, function(x) { x$name <- "test"; x }, flatten = TRUE)
  expect_true(all(sapply(test.layout, "[[", "name") == "test"))
})



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

  test.layout <- traverseInputLayout(layout, function(x, ...) { x$name <- "test"; x }, flatten = TRUE)
  expect_true(all(sapply(test.layout, "[[", "name") == "test"))
})


test_that("updateInputLayoutItem works", {
  layout <- blueprint$input.layout

  new.input <- slateInput("x", "character", "")
  new.group <- inputGroup("group")
  new.page <- inputPage("page")

  new.layout <- updateInputLayoutItem(layout, new.page)
  expect_true(length(new.layout$pages) == 4)
  expect_true(names(new.layout$pages)[4] == "page")

  new.layout <- updateInputLayoutItem(layout, new.group, c("Graphical parameters"))
  expect_true(length(new.layout$pages$`Graphical parameters`$groups) == 2)

  new.layout <- updateInputLayoutItem(layout, new.input, c("Graphical parameters"))
})




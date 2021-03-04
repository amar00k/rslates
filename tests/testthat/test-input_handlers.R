
pages <- list(
  slatePage("page_1", description = "Something."),
  slatePage("page_2", layout = "flow-4", extra = 1)
) %>% set_names(map(., "name"))

groups <- list(
  slateGroup("group_1", parent = "page_1", layout = "flow-3"),
  slateGroup("group_2", parent = "page_1", layout = "flow-2"),
  slateGroup("group_3", parent = "page_2")
) %>% set_names(map(., "name"))

inputs <- list(
  slateInput("character", "character", default = "Some text", parent = "page_1"),
  slateInput("expression", "expression", default = "rep(1:10, 2)", parent = "page_1"),
  slateInput("numeric", "numeric", default = 42, parent = "group_1"),
  slateInput("numeric2", "numeric2", default = c(0,1000), parent = "group_1"),
  slateInput("numeric4", "numeric4", default = c(0,1,2,3), parent = "group_2"),
  slateInput("logical", "logical", default = TRUE, parent = "group_2"),
  slateInput("choices_single", "choices", choices = c(LETTERS), default = "S", parent = "group_3"),
  slateInput("choices_multiple", "choices", choices = c(LETTERS),
             default = c("S", "L", "A", "T", "E", "S"), parent = "group_3"),
  slateInput("choices_free", "choices",
             default = c("A", "B"), choices = c("A", "B", "C", "D"),
             multiple = TRUE, parent = "group_3")
) %>% set_names(map(., "name"))

test_that("inputs return the right values", {
  x <- inputs[[ "character" ]]
  expect_equal(getHandler(x)$get.source(x, value = "Some text"), '"Some text"')
  expect_equal(getHandler(x)$get.source(x, value = ""), '""')

  x <- inputs[[ "expression" ]]
  expect_equal(getHandler(x)$get.source(x, value = "rep(1:10, 2)"), "rep(1:10, 2)")
  expect_equal(getHandler(x)$get.source(x, value = ""), "")

  x <- inputs[[ "numeric" ]]
  expect_equal(getHandler(x)$get.source(x, value = 42), "42")
  expect_equal(getHandler(x)$get.source(x, value = "42"), "42")
  expect_equal(getHandler(x)$get.source(x, value = ""), "")

  x <- inputs[[ "numeric2" ]]
  expect_equal(getHandler(x)$get.source(x, value = c(3, 7)), "c(3, 7)")
  expect_equal(getHandler(x)$get.source(x, value = c("3", "7")), "c(3, 7)")
  expect_equal(getHandler(x)$get.source(x, value = 3), "c(3, NA)")
  expect_equal(getHandler(x)$get.source(x, value = NA), "c(NA, NA)")
  expect_equal(getHandler(x)$get.source(x, value = ""), "c(NA, NA)")

  x <- inputs[[ "numeric4" ]]
  expect_equal(getHandler(x)$get.source(x, value = c(1, 2, 3, 4)), "c(1, 2, 3, 4)")
  expect_equal(getHandler(x)$get.source(x, value = c("1", 2, "3", 4)), "c(1, 2, 3, 4)")
  expect_equal(getHandler(x)$get.source(x, value = c(1, 2)), "c(1, 2, NA, NA)")
  expect_equal(getHandler(x)$get.source(x, value = NA), "c(NA, NA, NA, NA)") # Batman
  expect_equal(getHandler(x)$get.source(x, value = ""), "c(NA, NA, NA, NA)")

})


test_that("UI layout functions work", {


})







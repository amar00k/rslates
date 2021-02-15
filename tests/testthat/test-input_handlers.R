

input.list <- list(
  slateInput("character", "character", default = "Some text"),
  slateInput("expression", "expression", default = "rep(1:10, 2)"),
  slateInput("numeric", "numeric", default = 42),
  slateInput("numeric2", "numeric2", default = c(0,1000)),
  slateInput("numeric4", "numeric4", default = c(0,1,2,3)),
  slateInput("logical", "logical", default = TRUE),
  slateInput("choices_single", "choices", choices = c(LETTERS), default = "S"),
  slateInput("choices_multiple", "choices", choices = c(LETTERS),
             default = c("S", "L", "A", "T", "E", "S")),
  slateInput("choices_free", "free-choices",
             default = c("A", "B"), choices = c("A", "B", "C", "D"), multiple = TRUE)
) %>% set_names(sapply(., "[[", "name"))

test_that("inputs return the right values", {
  x <- input.list[[ "character" ]]
  expect_equal(input.handlers[[ x$input.type ]]$get.value(x, value = "Some text"), '"Some text"')
  expect_equal(input.handlers[[ x$input.type ]]$get.value(x, value = ""), '""')

  x <- input.list[[ "expression" ]]
  expect_equal(input.handlers[[ x$input.type ]]$get.value(x, value = "rep(1:10, 2)"), "rep(1:10, 2)")
  expect_equal(input.handlers[[ x$input.type ]]$get.value(x, value = ""), "")

  x <- input.list[[ "numeric" ]]
  expect_equal(input.handlers[[ x$input.type ]]$get.value(x, value = 42), "42")
  expect_equal(input.handlers[[ x$input.type ]]$get.value(x, value = "42"), "42")
  expect_equal(input.handlers[[ x$input.type ]]$get.value(x, value = ""), "")

  x <- input.list[[ "numeric2" ]]
  expect_equal(input.handlers[[ x$input.type ]]$get.value(x, value = c(3, 7)), "c(3, 7)")
  expect_equal(input.handlers[[ x$input.type ]]$get.value(x, value = c("3", "7")), "c(3, 7)")
  expect_equal(input.handlers[[ x$input.type ]]$get.value(x, value = 3), "c(3, NA)")
  expect_equal(input.handlers[[ x$input.type ]]$get.value(x, value = NA), "c(NA, NA)")
  expect_equal(input.handlers[[ x$input.type ]]$get.value(x, value = ""), "c(NA, NA)")

  x <- input.list[[ "numeric4" ]]
  expect_equal(input.handlers[[ x$input.type ]]$get.value(x, value = c(1, 2, 3, 4)), "c(1, 2, 3, 4)")
  expect_equal(input.handlers[[ x$input.type ]]$get.value(x, value = c("1", 2, "3", 4)), "c(1, 2, 3, 4)")
  expect_equal(input.handlers[[ x$input.type ]]$get.value(x, value = c(1, 2)), "c(1, 2, NA, NA)")
  expect_equal(input.handlers[[ x$input.type ]]$get.value(x, value = NA), "c(NA, NA, NA, NA)") # Batman
  expect_equal(input.handlers[[ x$input.type ]]$get.value(x, value = ""), "c(NA, NA, NA, NA)")

})

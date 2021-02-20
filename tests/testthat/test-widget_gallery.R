

input.list <- list(
  slateInput("character_", "character", default = "Some text"),
  slateInput("expression_", "expression", default = "rep(1:10, 2)"),
  slateInput("numeric_", "numeric", default = "42"),
  slateInput("numeric2", "numeric2", default = c(0,1000)),
  slateInput("numeric4", "numeric4", default = c(0,1,2,3)),
  slateInput("logical_", "logical", default = TRUE),
  slateInput("choices_single", "choices", choices = c(LETTERS), default = "S"),
  slateInput("choices_multiple", "choices", choices = c(LETTERS),
             default = c("S", "L", "A", "T", "E", "S")),
  slateInput("choices_free", "free-choices",
             default = c("A", "B"), choices = c("A", "B", "C", "D"), multiple = TRUE)
) %>% set_names(sapply(., "[[", "name"))


test_that("widgetGalleryServer widgets return the appropriate values", {
  # testServer(
  #   widgetGalleryServer,
  #   args = list(input.list = input.list, global.options = NULL), {
  #     do.call(session$setInputs, lapply(input.list, "[[", "default"))
  #     #session$setInputs(charf = "Some text")
  #     #print(names(input.list))
  #     #print(names(inputs()))
  #     #expect_equal(inputs()$character, "Some text")
  #     #expect_equal(inputs()$expression, c(1:10, 1:10))
  #   }
  # )
})

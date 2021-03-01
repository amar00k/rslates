

inputs <- list(
  slateInput("x", "character", "hello", "HELLO"),
  slateInput("y", "numeric", 42)
)

test_that("srcBuild works", {
  expect_equal(srcBuild(srcParse("paste(${x}, ${y})"), inputs), 'paste("HELLO", 42)')
})


test_that("extractInputs works", {

  text <- "range <- ${from}:${to}"
  expect_equal(ppExtractInputs(text)$input.names, c("from", "to"))

  text <- "plot(${dn :: x:x, y=y, type=type, main=main(character, Hello), xlab=xlab, ylab=ylab})"
  expect_equal(ppExtractInputs(text)$input.names, c("x", "y", "type", "main", "xlab", "ylab"))

})

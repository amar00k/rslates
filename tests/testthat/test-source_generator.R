

inputs <- list(
  slateInput("x", "character", "hello", "HELLO"),
  slateInput("y", "numeric", 42)
)

test_that("srcBuild works", {
  expect_equal(srcBuild(srcParse("paste(${Q:x}, ${y})"), inputs), 'paste("HELLO", 42)')
})

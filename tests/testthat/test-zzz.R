

test_that("sequenceGenerator works", {
  expect_equal(sequenceGenerator()(), "seq_1")

  test <- sequenceGenerator("test")

  expect_equal(test(), "test_1")
  expect_equal(test(), "test_2")
  expect_equal(test(), "test_3")
})


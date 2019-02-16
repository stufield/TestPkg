context("test-unit")

test_that("test failure control", {
  #expect_true(FALSE)
  expect_true(TRUE)
})

test_that("sort() ignores case", {
  x <- c("ABC", "ZAP", "null")
  expect_equal(sort(x), c("ABC", "null", "ZAP"))
})

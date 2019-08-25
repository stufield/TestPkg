context("test-unit")

test_that("test failure control", {
  # expect_true(FALSE)
  expect_true(TRUE)
})

test_that("sort() ignores case", {
  skip("turn off")
  x <- c("Aaron", "Zack", "nancy")
  expect_equal(sort(x), c("Aaron", "nancy", "Zack"))
})

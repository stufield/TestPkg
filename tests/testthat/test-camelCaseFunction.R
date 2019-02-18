context("test-unit")

test_that("test failure control", {
  #expect_true(FALSE)
  expect_true(TRUE)
})

print(Sys.getlocale("LC_COLLATE"))
Sys.setlocale(locale = "en_US.UTF-8")
print(Sys.getlocale("LC_COLLATE"))

test_that("sort() ignores case", {
  x <- c("Aaron", "Zack", "nancy")
  expect_equal(sort(x), c("Aaron", "nancy", "Zack"))
})

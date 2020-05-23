
x <- c("Aaron", "Zack", "nancy", "clark")

test_that("sort() ignores case via locale = 'en_US.UTF-8'", {
  on_check <- !identical(Sys.getenv("_R_CHECK_PACKAGE_NAME_"), "")
  skip_if(on_check, "On devtools::check()")
  withr::local_collate("en_US.UTF-8")
  expect_equal(Sys.getlocale("LC_COLLATE"), "en_US.UTF-8")     # sanity check
  expect_equal(sort(x), c("Aaron", "clark", "nancy", "Zack"))
})

test_that("sort() does not ignore case via locale = 'C'", {
  withr::local_collate("C")
  expect_equal(Sys.getlocale("LC_COLLATE"), "C")               # sanity check
  expect_equal(sort(x), c("Aaron", "Zack", "clark", "nancy"))
})

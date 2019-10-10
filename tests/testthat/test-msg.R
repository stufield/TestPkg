context("test locale and case-sensitive sorting")

x <- c("Aaron", "Zack", "nancy", "clark")

test_that("sort() ignores case via locale = 'en_US.UTF-8'", {
  withr::with_locale(
    c(LC_COLLATE = "en_US.UTF-8"), {
      expect_equal(Sys.getlocale("LC_COLLATE"), "en_US.UTF-8") # sanity check
      expect_equal(sort(x), c("Aaron", "clark", "nancy", "Zack"))
    }
  )
})

test_that("sort() does not ignore case via locale = 'C'", {
  withr::with_locale(
    c(LC_COLLATE = "C"), {
      expect_equal(Sys.getlocale("LC_COLLATE"), "C")    # sanity check
      expect_equal(sort(x), c("Aaron", "Zack", "clark", "nancy"))
    }
  )
})

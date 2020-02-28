

skip_on_check <- function() {
  on_check <- !identical(Sys.getenv("_R_CHECK_PACKAGE_NAME_"), "")
  skip_if(on_check, "On devtools::check()")
}

test_that("TestPkg has no spelling errors", {
  skip_on_check()
  skip_on_covr()
  spelling_errors <- devtools::spell_check()$word
  expect_length(spelling_errors, 0)
})

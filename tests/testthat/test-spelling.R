context("Spell checking code")
if ( utils::packageVersion("testthat") < "2.2.0" ) {
  skip_on_covr <- function()  {
    if ( !identical(Sys.getenv("R_COVR"), "true") ) {
      return(invisible(TRUE))
    }
    skip("On covr")
  }
}
test_that("TestPkg has no spelling errors", {
  skip_on_cran()
  skip_on_covr()
  spelling_errors <- devtools::spell_check()$word
  expect_length(spelling_errors, 0)
})

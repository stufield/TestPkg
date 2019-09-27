context("Static Code Style Compliance Linter")
if ( utils::packageVersion("testthat") < "2.2.0" ) {
  skip_on_covr <- function()  {
    if ( !identical(Sys.getenv("R_COVR"), "true") ) {
      return(invisible(TRUE))
    }
    skip("On covr")
  }
}
test_that("TestPkg is in style compliance", {
  # lintr::expect_lint_free()
  skip_on_cran()     # don't run in check()
  skip_on_covr()     # don't run if in 'covr'
  skip_if_not_installed("lintr")
  lints <- lintr::lint_package()
  expect_length(lints, 0)
  if ( interactive() ) {
    files <- names(lints) %>%
      unique() %>%
      basename() %>%
      paste0(collapse = ", ")
    message("Please lint the following files: ", files)
  }
})

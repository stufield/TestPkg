if ( requireNamespace("lintr", quietly = TRUE) ) {
  context("Static Code Style Compliance Linter")
  test_that("The somaverse is in style compliance", {
    print(fs::dir_ls())
    print(Sys.getenv("NOT_CRAN"))
    Sys.setenv(NOT_CRAN="true")
    lints       <- lintr::lint_package()
    has_lints   <- length(lints) > 0
    print(has_lints)
    lint_output <- NULL
    if ( has_lints ) {
     lint_output <- paste(collapse = "\n", capture.output(print(lints)))
    }
    expect(!has_lints, paste("Not lint free", lint_output, sep = "\n"))
  })
}

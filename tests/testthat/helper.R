
skip_on_check <- function() {
  on_check <- !identical(Sys.getenv("_R_CHECK_PACKAGE_NAME_"), "")
  skip_if(on_check, "On devtools::check()")
}

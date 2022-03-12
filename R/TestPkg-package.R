#' @keywords internal
#' @import withr
"_PACKAGE"

## TestPkg namespace: start
#' @useDynLib TestPkg, .registration = TRUE
#' @importFrom Rcpp sourceCpp
## TestPkg namespace: end
NULL

# for use with usethis::use_release_issue
release_bullets <- function() {
  c(
    "Submit PR to `master` branch",
    "Merge PR via 'Rebase and merge'",
    "Add next release version tag",
    "Add release to public repo :partying_face:",
    ":tada:"
  )
}

checklist <- function() {
  c("Prepare for release:",
    "",
    paste("- [ ]", release_bullets())
  )
}

create_release_issue <- function(ver = NULL) {
  stopifnot(!is.null(ver))
  projfile <- dir(pattern = "[.]Rproj$")
  project  <- gsub("\\.Rproj$", "", projfile)
  issue <- gh::gh(
    endpoint = "POST /repos/{owner}/{repo}/issues",
    owner    = "stufield",
    repo     = "TestPkg",
    .api_url = "https://api.github.com",
    title    = sprintf("Release %s %s", project, ver),
    body     = paste0(checklist(), "\n", collapse = "")
  )
  if ( interactive() ) {
    Sys.sleep(1)
    utils::browseURL(issue$html_url)
  }
}

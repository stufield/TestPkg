
#' Release a New `somaverse` Package Version
#'
#' Go through a release checklist to perform steps necessary for a `somaverse`
#' package release.
#'
#' @param ver Character. See `usethis::use_version()`.
#' @param tag Character. The name of the version tag for the release, e.g. "v3.2.1"
#' @param check Logical. Should `devtools::check()` be run internally?
#' @return The values of `usethis::sitrep()` and `git2r::status()` are returned.
#' @examples
#' \dontrun{
#' soma_release()
#' }
#' @export init_release
init_release <- function(ver = NULL, check = FALSE) {

  # Run devtools::check() ----
  if ( check ) {
    devtools::check()
  }

  # Update README.md ----
  ui_todo("Rendering {ui_value('README.Rmd')}")
  rmarkdown::render("README.Rmd", quiet = TRUE)
  git2r::add(get_repo(), path = "README.md")

  # Bump version ----
  usethis::use_version(ver)    # say no when asked to commit

  # Update NEWS.md ----
  update_news()

  ui_done("Opening NEWS.md for editing and pre-release curation")
  usethis::edit_file("NEWS.md")
  return()
}

#' @describeIn init_release Complete the package release.
#' @export complete_release
complete_release <- function(tag = NULL) {

  # Commit
  git2r::commit(get_repo(), message = "Increment version number", all = TRUE)

  # Tag commit ----
  update_tag(tag)

  # Update pkgdown ----
  if ( length(devtools::spell_check()$word) == 0 ) {
    update_pkgdown()
  } else {
    ui_todo("Spelling errors detected")
    ui_todo("Please fix them and run `update_pkgdown()`")
  }

  # Bump to dev version ----
  usethis:::use_description_field("Version", usethis:::choose_version("dev"),
                                  overwrite = TRUE)
  git2r::add(get_repo(), "DESCRIPTION")
  git2r::commit(get_repo(), message = "Bump to dev version")

  # Git situation report
  git2r::status()
}

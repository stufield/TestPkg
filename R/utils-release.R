
#' @importFrom usethis proj_get
#' @importFrom git2r repository
#' @noRd
get_repo <- function() {
  proj <- usethis::proj_get()
  git2r::repository(proj)
}

#' @importFrom git2r tags
#' @noRd
get_recent_tag <- function() {
  git2r::tags(get_repo())[[1L]]
}

#' @importFrom git2r commits lookup_commit
#' @noRd
get_top_commits <- function(since = get_recent_tag()) {
  commit <- git2r::commits(get_repo(), time = FALSE, n = 1L)[[1L]]
  if ( !is.null(since) ) {
    since <- git2r::lookup_commit(since)
  }
  get_first_parent(commit, since)
}

#' @importFrom git2r parents
#' @importFrom purrr some
#' @noRd
get_first_parent <- function(commit, since) {
  commits <- list(commit)
  if ( !is.null(since) && commit$sha == since$sha ) {
    return(commits)
  }
  repeat {
    all_parents <- git2r::parents(commit)
    if ( length(all_parents) == 0 ) return(commits)
    # Compatibility with git-flow, where tags were set on the production branch
    # which was then merged to master
    if ( !is.null(since) ) {
      if ( purrr::some(all_parents, ~.$sha == since$sha) ) return(commits)
    }
    first_parent <- all_parents[[1L]]
    commits <- c(commits, list(first_parent))
    commit <- first_parent
  }
}

#' @importFrom purrr map_chr keep flatten_chr discard
#' @importFrom usethis ui_todo ui_done ui_value
#' @importFrom stringr str_trim
#' @noRd
add_news <- function(commits) {
  usethis::ui_todo("Scraping {ui_value(length(commits))} commit messages")
  messages <- gsub("\r\n", "\n", purrr::map_chr(commits, "message"))
  messages_before_triple_dash <- purrr::map_chr(strsplit(messages, "\n---", fixed = TRUE), 1)
  message_lines <- strsplit(messages_before_triple_dash, "\n", fixed = TRUE)
  message_bullets <- purrr::map(message_lines, purrr::keep, ~ grepl("^[*-]", .)) %>%
    purrr::discard(~ length(.) == 0)
  has_issue <- function(.x) grepl("SOMA.*-[0-9]{1,5}", .x[length(.x)])
  message_items <- purrr::map_if(message_bullets, has_issue, ~ {
    idx <- length(.x)
    issue <- gsub("^-", "", .x[idx]) %>% stringr::str_trim()
    .x[idx - 1] %<>% paste0(., " (", issue, ")")
    .x[-idx]
    }) %>%
    purrr::flatten_chr()

  if (length(message_items) == 0) {
    if (length(commits) <= 1) {
      message_items <- "- Same as previous version."
    } else {
      message_items <- "- Internal changes only."
    }
  }
  usethis::ui_done("Found {ui_value(length(message_items))} NEWS-worthy entries.")
  paste0(paste(message_items, collapse = "\n"), "\n\n")
}

#' @importFrom usethis ui_todo ui_done
#' @noRd
update_news <- function() {
  ui_todo("Scraping Git commit messages")
  news      <- readLines("NEWS.md", encoding = "UTF-8")
  changelog <- add_news(get_top_commits())
  ui_done("Adding changelog to {ui_value('NEWS.md')}")
  usethis:::write_utf8("NEWS.md", c(news[1], "", changelog, news[-1]))
}

#' @importFrom fs is_dir dir_delete
#' @importFrom git2r commit
#' @importFrom pkgdown build_site
#' @noRd
update_pkgdown <- function() {
  if ( fs::is_dir("docs/dir") ) {
    fs::dir_delete("docs/dir")
  }
  pkgdown::build_site(preview = FALSE)
  git2r::commit(get_repo(), message = "Update pkgdown", all = TRUE)
}

#' @importFrom desc desc_get_version
#' @importFrom git2r tag
#' @noRd
update_tag <- function(tag = NULL) {
  if ( is.null(tag) ) {
    tag <- paste0("v", as.character(desc::desc_get_version()))
  }
  if ( !grepl("^v", tag) ) tag <- paste("v", tag)
  git2r::tag(get_repo(), name = tag, message = paste("Release of", tag))
}

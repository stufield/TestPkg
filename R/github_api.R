
#' Hit the GitHub API
#'
#' @param path String of the endpoint destination
#' @examples
#' # My GitHub user account
#' github_api("/users/stufield")
#'
#' # This package repo
#' github_api("/repos/stufield/TestPkg")
#' @author Stu Field
#' @importFrom httr status_code user_agent GET modify_url content
#' @export
github_api <- function(path) {
  url  <- httr::modify_url("https://api.github.com", path = path)
  resp <- httr::GET(url, httr::user_agent("http://github.com/stufield"))

  if ( httr::http_type(resp) != "application/json" ) {
    rlang::signal("API did not return json", "error")
  }

  parsed <- jsonlite::fromJSON(httr::content(resp, "text"), simplifyVector = FALSE)

  if ( httr::status_code(resp) != 200 ) {
    rlang::signal(
      stringr::str_glue(
        "GitHub API request failed:
        [{status_code(resp)}]
        {parsed$message}
        <{parsed$documentation_url}>"
        ), "error")
  }

  structure(
    list(
      content  = parsed,
      path     = path,
      response = resp
    ),
    class = "github_api"
  )

}


#' @noRd
#' @export
print.github_api <- function(x, ...) {
  cat("<GitHub: ", x$path, ">\n", sep = "")
  utils::str(x$content)
  invisible(x)
}

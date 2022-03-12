#' Hit the GitHub API
#'
#' @param path String of the endpoint destination
#' @examples
#' # My GitHub user account
#' tryCatch(github_api("/users/stufield"), error = function(e) e)
#'
#' # This package repo
#' tryCatch(github_api("/repos/stufield/TestPkg"), error = function(e) e)
#' @author Stu Field
#' @importFrom httr status_code user_agent GET modify_url content http_type
#' @export
github_api <- function(path) {
  url  <- modify_url("https://api.github.com", path = path)
  resp <- GET(url, user_agent("http://github.com/stufield"))

  if ( http_type(resp) != "application/json" ) {
    stop("API did not return json", call. = FALSE)
  }

  parsed <- jsonlite::fromJSON(content(resp, "text"), simplifyVector = FALSE)

  if ( status_code(resp) != 200 ) {
    stop(
      "GitHub API request failed:\n",
      "[", status_code(resp), "]\n",
      parsed$message, "\n<",
      parsed$documentation_url, ">",
      call. = FALSE
    )
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

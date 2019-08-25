# ------------------------------------------
# We do not want to import all of dplyr,
# so we must re-export the dplyr::filter() generic so that it is
# available on the search path for proper method dispatch.
# So we importFrom first, then re-export
# ------------------------------------------

#' Use the `dplyr::filter()` S3 generic
#'
#' See \code{dplyr::\link[dplyr]{filter}} for details.
#'
#' @name filter
#' @keywords internal
#' @examples
#' filter(iris, Species == "versicolor")
#' filter(iris, Species == "versicolor" & Petal.Width < 4)
#' @importFrom dplyr filter
#' @export
#' @usage filter(.data, ..., .preserve = FALSE)
NULL

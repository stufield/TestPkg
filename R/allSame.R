
#' All Values Same
#'
#' Returns a logical as to whether all values *within* a vector
#' are identical. This function does NOT compare two independent
#' vectors. Please use \code{link{all.equal}},
#' for such a purpose combined with \code{\link{isTRUE}}.
#'
#' @rdname allSame
#' @param x A vector of values. Can be one of the following objects
#' classes: `numeric`, `character`, `factor`, or `logical`.
#' @return Logical. `TRUE` or `FALSE`.
#' @author Stu Field
#' @seealso \code{\link{isTRUE}}, \code{\link{all.equal}}
#' @examples
#' allSame(1:4)
#' allSame(rep(5, 10))
#' allSame(rep("A", 10))
#' allSame(letters)
#' allSame(c(TRUE, TRUE, TRUE))
#' allSame(c(TRUE, TRUE, FALSE))
#' @export
allSame <- function(x) UseMethod("allSame")


#' S3 allSame method for numeric
#'
#' @noRd
#' @export
allSame.numeric <- function(x) {
  # if integer
  if ( all(floor(x) == x, na.rm = TRUE) ) {
    isTRUE(diff(range(x, na.rm = TRUE)) == 0)
  } else {
    isTRUE(diff(range(x, na.rm = TRUE)) < .Machine$double.eps^0.5)
  }
}

#' S3 allSame method for character
#'
#' @noRd
#' @export
allSame.character <- function(x) {
  isTRUE(all(vapply(x, function(i) i == x[1L], NA)))
}

#' S3 allSame method for factor
#'
#' @noRd
#' @export
allSame.factor <- function(x) {
  allSame(as.character(x))
}

#' S3 allSame method for logical
#'
#' @noRd
#' @export
allSame.logical <- function(x) {
  allSame(as.numeric(x))
}

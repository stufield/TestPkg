
#' Calculate Concordance Correlation Coefficient
#'
#' Calculate the concordance correlation coefficient (CCC)
#' and it's significance value from two vectors of related data.
#'
#' @param x A numeric vector of values to compare against `y`.
#' @param y A numeric vector of values to compare against `x`.
#' @return A list of the following:
#' \item{rho.c }{The concordance correlation coefficient (CCC).}
#' \item{ci95 }{The 95 percent confidence intervals of the CCC.}
#' \item{Z }{The z-score of the CCC.}
#' \item{p.value }{The p-value corresponding to the Z-score.}
#' @author Stu Field
#' @seealso \code{\link[stats]{cor}}, \code{\link[stats]{pnorm}}
#' @references Lawrence Lin, Biometrics (45): 255-268.
#' @examples
#' calcCCC(rnorm(100), rnorm(100))
#' v <- rnorm(100)
#' calcCCC(v, v + 1)
#' @importFrom stats cor sd var qnorm pnorm
#' @export calcCCC
calcCCC <- function(x, y) {
  stopifnot(length(x) == length(y), is.numeric(x), is.numeric(y))

  k <- length(x)
  sdx <- sd(x)
  sdy <- sd(y)
  rho <- stats::cor(x, y, method = "pearson")
  v <- sdx / sdy # scale shift
  sx2 <- var(x) * (k - 1) / k
  sy2 <- var(y) * (k - 1) / k
  u <- ( mean(x) - mean(y) ) / ( (sx2 * sy2)^0.25 ) # location shift relative to scale
  Cb <- ( (v + 1 / v + u^2) / 2 )^-1
  pc <- rho * Cb

  sep <- sqrt( ( (1 - rho^2 ) * pc^2 * ( 1 - pc^2 ) / rho^2 +
    (2 * pc^3 * (1 - pc) * u^2 / rho) - 0.5 * pc^4 * u^4 / rho^2) / (k - 2) )

  Z    <- 0.5 * log( (1 + pc) / (1 - pc) )
  pval <- 2 * stats::pnorm(-abs(Z))
  set  <- sep / ( 1 - (pc^2) )
  N.   <- 1 - (0.05 / 2) # default 95%
  up   <- Z + stats::qnorm(N.) * set
  lo   <- Z - stats::qnorm(N.) * set
  lo   <- (exp(2 * lo) - 1) / (exp(2 * lo) + 1)
  up   <- (exp(2 * up) - 1) / (exp(2 * up) + 1)
  list(rho.c = pc, ci95 = c(lower = lo, upper = up), Z = Z, p.value = pval)
}

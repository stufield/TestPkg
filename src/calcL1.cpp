#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix calcL1(List e) {
  Function glm("glmnet");
  Function as("as");
  List model = glm(e["x"], e["y"], Named("family") = "binomial");
  NumericMatrix out = as(model["beta"], "matrix");
  int n = out.size();
  for (int i = 0; i < n; i++) {
    if (out[i] != 0) out[i] = 1;
  }
  return out;
}


/***R
# nr <- 6
# m  <- matrix(sample(c(rep(0,9), 1), nr^2, replace = T), nrow = nr)
# m <- as(m, "dgCMatrix")

e <- list()
e$x <- matrix(rnorm(100*20),100,20)
e$y <- sample(0:1, 100, replace = T)
f = function(x) matrix(as.numeric(as.logical(x)), nrow = 20)
bench::mark(
  orig = sum(f(glmnet(e$x, e$y, family = "binomial")$beta)),
  rcpp = sum(calcL1(e)),
  iterations = 1000
)
#W <- .calcW(e$p, e$alpha, e$Pw, e$kernel)
#first_half <- glmnet::glmnet(e$x1, e$y1, family = "binomial",
#                             lambda = e$lambda.seq,
#                             standardize = e$standardize,
#                             penalty.factor = W)$beta %>%
#  as.logical() %>% as.numeric()
#second_half <- glmnet::glmnet(e$x2, e$y2, family = "binomial",
#                              lambda = e$lambda.seq,
#                              standardize = e$standardize,
#                              penalty.factor = W)$beta %>%
#  as.logical() %>% as.numeric()
#first_half_mat  <- matrix(first_half, nrow = e$p)
#second_half_mat <- matrix(second_half, nrow = e$p)
#return(first_half_mat + second_half_mat)
*/


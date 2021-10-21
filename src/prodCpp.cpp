#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double prod_cpp(NumericVector x) {
  int n = x.size();
  double out = 1;
  for(int i = 0; i < n; ++i) {
    out *= x[i];
  }
  return out;
}

// [[Rcpp::export]]
NumericVector prodMat(NumericMatrix x) {
  int p = x.ncol();
  NumericVector out(p);
  for (int i = 0; i < p; ++i) {
    NumericVector v = x( _ , i);   // grab the ith column
    v = v[ v > 0];
    v.push_front(1);   // ensure first value = 1
    out[i] = prod_cpp(v);
  }
  return out;
}

/***R
prodMat(matrix(1:25, ncol = 5))
*/

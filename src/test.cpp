#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
void test_() {
  NumericVector x(5);
  x[0] = 1;
  x = x / 0;
  Rcout << "This is x: " << x << "\n";
}

/***R
test_()
*/

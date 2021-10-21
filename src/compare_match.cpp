#include <numeric>
#include <unordered_map>
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerVector match_(const CharacterVector& x,
                     const CharacterVector& table) {

  typedef std::unordered_map<SEXP,int> MAP ;
  //typedef MAP::value_type VALUE ;

  // populate the hash
  MAP hash ;
  int n = table.size() ;
  SEXP* ptr = get_string_ptr(table) ;
  for( int i = 0 ; i < n; i++) {
    hash.insert( std::make_pair<SEXP,int>(ptr[i], i + 1) ) ;
  }

  n = x.size() ;
  IntegerVector result(n) ;
  ptr = get_string_ptr(x) ;
  MAP::const_iterator end = hash.end() ;
  for( int i = 0; i < n; i++) {
    MAP::const_iterator it = hash.find(ptr[i]) ;
    if ( it == end ) { // no match
      result[i] = NA_INTEGER ;
    } else {
      result[i] = it->second ;
    }
  }
  return result - 1;
}

// [[Rcpp::export]]
IntegerVector match2( const CharacterVector& x, const CharacterVector& table) {
  IntegerVector res = match(x, table);
  return res - 1;
}

/*** R
  set.seed(101)
  x <- sample(LETTERS, 100000, replace = TRUE)
  bnch <- bench::mark(
    match_ = match_(x, LETTERS),
    match2 = match2(x, LETTERS),
    iterations = 1000
  )
  bnch
  summary(bnch, relative = TRUE)
*/

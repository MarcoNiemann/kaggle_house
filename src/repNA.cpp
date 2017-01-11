#include "repNA.hpp"

//' Remove \code{NA} from \code{NumericVector}
//'
//' This function removes \code{NA} values from \code{NumericVector}s.
//'
//' This function accepts a numeric input vector and then replaces each
//' contained \code{NA} value with the \code{int} specified in the parameter
//' \code{rep}.
//'
//' @param  xin \code{NumericVector} potentially containing removable \code{NA}
//'             values.
//' @param  rep \code{int} value to replace \code{NA}s occuring in \code{xin}.
//'
//' @return \code{NumericVector} that is equal to \code{xin} except the
//'         \code{NA} values which have been replaced with \code{rep}.
//'
//' @examples
//' KaggleHouse:::cpp_rep_na_num(c(1, 2, NA), 3)
//'
// [[Rcpp::export]]
NumericVector cpp_rep_na_num( NumericVector xin, int rep ){

  for( int i = 0; i < xin.size(); i++ ){
    if(NumericVector::is_na(xin[i])) {
      xin[i] = rep ;
    }
  }
  return xin ;
}

//' Remove \code{NA} from \code{CharacterVector}
//'
//' This function removes \code{NA} values from \code{CharacterVector}s.
//'
//' This function accepts a character input vector and then replaces each
//' contained \code{NA} value with the \code{std::string} specified in the
//' parameter \code{rep}.
//'
//' @param  xin \code{CharacterVector} potentially containing removable
//'             \code{NA} values.
//' @param  rep \code{std::string} value to replace \code{NA}s occuring in
//'             \code{xin}.
//'
//' @return \code{CharacterVector} that is equal to \code{xin} except the
//'         \code{NA} values which have been replaced with \code{rep}.
//'
//' @examples
//' KaggleHouse:::cpp_rep_na_chr(c("a", "b", NA), "c")
//'
// [[Rcpp::export]]
CharacterVector cpp_rep_na_chr( CharacterVector xin, std::string rep ){

  for( int i = 0; i < xin.size(); i++ ){
    if(CharacterVector::is_na(xin[i])) {
      xin[i] = rep ;
    }
  }
  return xin ;
}
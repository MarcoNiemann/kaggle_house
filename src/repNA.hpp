#define REPNA_HPP

#include <Rcpp.h>
using namespace Rcpp;

NumericVector cpp_rep_na_num( NumericVector xin, int rep );
CharacterVector cpp_rep_na_chr( CharacterVector xin, std::string rep );

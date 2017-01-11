#define UNIVARIATEDATA_HPP

#include <Rcpp.h>
using namespace Rcpp;

NumericMatrix cpp_bind(NumericVector a, NumericVector b);
Rcpp::List cpp_countValueOccurrences(Rcpp::DataFrame x);
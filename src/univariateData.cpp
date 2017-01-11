#include "univariateData.hpp"

//' Combine two \code{NumericVector}s into a \code{NumericMatrix}
//'
//' This function combines two given \code{NumericVector}s into a
//' \code{NumericMatrix}. It thereby provides similar functionality as
//' \code{\link{base::cbind}}.
//'
//' @param a \code{NumericVector} representing the first column.
//' @param b \code{NumericVector} representing the second column.
//'
//' @return \code{NumericMatrix} being the combination of the two
//'         \code{NumericVector}s \code{a} and \code{b}.
// [[Rcpp::export]]
NumericMatrix cpp_bind(NumericVector a, NumericVector b) {
  NumericMatrix out = no_init_matrix(a.size(), 2);
  out(_, 0) = a;
  out(_, 1) = b;

  return out;
}

//' Convert Value Occurences to Points
//'
//' This function converts a \code{data.frame} of feature characteristic
//' occurences to plottable points. For that the characteristic value is used
//' as the x-value. To visualize multiple occurences, each of them is assigned
//' a slightly higher y-position, so that multiple occurences of one
//' characteristic will later on stack in the plotted graph.
//'
//' @param x \code{data.frame} with the results of the \code{table} command on
//'           a given vector. \code{x} should contain the counts per
//'           characteristic of a specific feature.
//'
//' @return \code{List} including plottable point positions representing the
//'         counts of feature characteristics.
//'
//' @examples
//'   val <- rep(1:5, c(10, 20, 30, 40, 50))
//'   df <- as.data.frame(table(val))
//'   KaggleHouse:::cpp_valueOccurrencesToPoints(df)
// [[Rcpp::export]]
Rcpp::List cpp_valueOccurrencesToPoints(Rcpp::DataFrame x) {
  Rcpp::List results(x.nrows());

  for(int i = 0; i < x.nrows(); i++) {
    double y = 0.0;

    NumericVector columnOne = x[0];
    NumericVector columnTwo = x[1];

    NumericVector col1(columnTwo[i]);
    NumericVector col2(columnTwo[i]);

    for(int j = 0; j < columnTwo[i]; j ++) {
      col1(j) = columnOne[i];
      col2(j) = y;
      y = y + 0.1;
    }

    results[i] = cpp_bind(col1, col2);
  }

  return results;
}

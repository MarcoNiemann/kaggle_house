#include "regex.hpp"

//' Extract Selector Name From Variable Name
//'
//' This function extract the selector name (e.g. 'var') from a variable name
//' like 'data$var'.
//'
//' This function basically takes variables names of the form
//' \code{(containingObject)$(selector)} as an input. With a regex command the
//' last selector - meaning the substring behind the last '$' - will be
//' extracted and returned. In case that the input parameter \code{x} does not
//' contain a selector, the empty string will be returned.
//'
//' @param x \code{std::string} containing the selector name (usually in the
//'          form \code{(containingObject)$(selector)}).
//'
//' @return \code{std::string} containing the extracted \code{selector} part.
//'         In case no selector can be found, the an empty string is returned.
//'
//' @examples
//'   # The outcome should be simply 'SalePrice'
//'   KaggleHouse:::cpp_regex_selector_name(data_train_na$SalePrice)
//'
// [[Rcpp::export]]
std::string cpp_regex_selector_name(const std::string x) {
  std::regex rgx("\\$([^\\$]*)$");
  std::smatch match;
  std::string y = "";

  while(std::regex_search(x, match, rgx)) {
    return match[match.size() - 1].str();
  }

  return "";
}



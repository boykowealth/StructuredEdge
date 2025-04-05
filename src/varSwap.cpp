#include <Rcpp.h>
#include <cmath>
#include <vector>
#include <string>

using namespace Rcpp;

double variance_swap_payoff(double variance_strike, double realized_variance, double nominal) {
  return nominal * (realized_variance - variance_strike);
}

// [[Rcpp::export]]
DataFrame varianceSwap(double variance_strike, double realized_variance_start, 
                       std::string position_str, double nominal) {
  int position;
  if (position_str == "Long" || position_str == "long") {
    position = 1; // Long position
  } else if (position_str == "Short" || position_str == "short") {
    position = -1; // Short position
  } else {
    Rcpp::stop("Invalid position. Use 'Long' for long position or 'Short' for short position.");
  }
  
  double realized_variance_min = -1.0;  // -100% normalized
  double realized_variance_max = 1.0;   // +100% normalized
  double realized_variance_step = 0.0001; // Step size of 1 basis point (bps)
  
  std::vector<double> normalized_variances, swap_values;
  
  for (double normalized_variance = realized_variance_min; normalized_variance <= realized_variance_max; normalized_variance += realized_variance_step) {
    double realized_variance = realized_variance_start * (1.0 + normalized_variance);
    normalized_variances.push_back(normalized_variance);
    swap_values.push_back(position * variance_swap_payoff(variance_strike, realized_variance, nominal)); 
  }
  
  return DataFrame::create(
    _["Spot"] = normalized_variances,
    _["Payoff"] = swap_values
  );
}

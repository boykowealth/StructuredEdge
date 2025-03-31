#include <Rcpp.h>
#include <cmath>
#include <vector>
#include <string>

using namespace Rcpp;

// Function for calculating the present value of variance swap cash flows
double variance_swap_present_value(double variance_strike, double realized_variance, double nominal) {
  // Payoff is the difference between realized variance and variance strike, scaled by notional
  return nominal * (realized_variance - variance_strike);
}

// [[Rcpp::export]]
DataFrame varianceSwap(double variance_strike, double realized_variance_start, 
                       std::string position_str, double nominal) {
  // Convert position input to 1 for "Long" and -1 for "Short"
  int position;
  if (position_str == "Long" || position_str == "long") {
    position = 1; // Long position
  } else if (position_str == "Short" || position_str == "short") {
    position = -1; // Short position
  } else {
    Rcpp::stop("Invalid position. Use 'Long' for long position or 'Short' for short position.");
  }
  
  // Define the fixed range for realized variance (-100% to +100%)
  double realized_variance_min = -1.0;  // -100% normalized
  double realized_variance_max = 1.0;   // +100% normalized
  double realized_variance_step = 0.0001; // Step size of 1 basis point (bps)
  
  std::vector<double> normalized_variances, swap_values;
  
  for (double normalized_variance = realized_variance_min; normalized_variance <= realized_variance_max; normalized_variance += realized_variance_step) {
    double realized_variance = realized_variance_start * (1.0 + normalized_variance); // Convert normalized variance back to actual realized variance
    normalized_variances.push_back(normalized_variance);
    swap_values.push_back(position * variance_swap_present_value(variance_strike, realized_variance, nominal)); // Adjusted by position
  }
  
  return DataFrame::create(
    _["Spot"] = normalized_variances,
    _["Payoff"] = swap_values
  );
}
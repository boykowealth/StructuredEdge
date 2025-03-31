#include <Rcpp.h>
#include <cmath>
#include <vector>
#include <string>

using namespace Rcpp;

// Function for calculating FRA prices
double fra_price(double r1, double r2, double t1, double t2, double nominal) {
  double discount_factor = exp(-r2 * t2);
  double forward_rate = (r2 * t2 - r1 * t1) / (t2 - t1); // Implied forward rate
  return nominal * discount_factor * forward_rate;
}

// [[Rcpp::export]]
DataFrame irForward(double r1, double r2, double t1, double t2, double nominal, std::string position_str) {
  // Convert position input to 1 for "Long" and -1 for "Short"
  int position;
  if (position_str == "Long" || position_str == "long") {
    position = 1; // Long position
  } else if (position_str == "Short" || position_str == "short") {
    position = -1; // Short position
  } else {
    Rcpp::stop("Invalid position. Use 'Long' for long position or 'Short' for short position.");
  }
  
  // Define the fixed range for rates (-100% to +100%)
  double r_min = -1.0;  // -100% normalized
  double r_max = 1.0;   // +100% normalized
  double r_step = 0.0001; // Step size of 1 basis point (bps)
  
  std::vector<double> normalized_rates, fra_values;
  
  for (double normalized_rate = r_min; normalized_rate <= r_max; normalized_rate += r_step) {
    double r_curr = r1 * (1.0 + normalized_rate); // Convert normalized rate back to actual rate
    normalized_rates.push_back(normalized_rate);
    fra_values.push_back(position * fra_price(r1, r_curr, t1, t2, nominal)); // Adjusted by position
  }
  
  return DataFrame::create(
    _["Spot"] = normalized_rates,
    _["Payoff"] = fra_values
  );
}
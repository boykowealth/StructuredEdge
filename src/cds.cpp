#include <Rcpp.h>
#include <cmath>
#include <vector>
#include <string>

using namespace Rcpp;

// Function to calculate the payoff of a credit default swap (without discounting)
double cds_payoff(double credit_spread, double default_probability, double recovery_rate, 
                  double notional) {
  // Protection leg: Payoff in case of default
  double protection_leg = notional * (1.0 - recovery_rate) * default_probability;
  
  // Premium leg: Regular payments based on the credit spread
  double premium_leg = credit_spread * notional;
  
  // Return net payoff (protection leg - premium leg)
  return protection_leg - premium_leg;
}

// [[Rcpp::export]]
DataFrame creditDefaultSwap(double credit_spread, double default_probability_start, double recovery_rate, 
                            double period_length, double discount_rate, std::string position_str, double nominal) {
  // Convert position input to 1 for "Long" and -1 for "Short"
  int position;
  if (position_str == "Long" || position_str == "long") {
    position = 1; // Long position (protection buyer)
  } else if (position_str == "Short" || position_str == "short") {
    position = -1; // Short position (protection seller)
  } else {
    Rcpp::stop("Invalid position. Use 'Long' for long position or 'Short' for short position.");
  }
  
  // Define the fixed range for default probabilities (-100% to +100%)
  double default_probability_min = -1.0;  // -100% normalized
  double default_probability_max = 1.0;   // +100% normalized
  double default_probability_step = 0.0001; // Step size of 1 basis point (bps)
  
  std::vector<double> normalized_probabilities, swap_values;
  
  for (double normalized_probability = default_probability_min; normalized_probability <= default_probability_max; normalized_probability += default_probability_step) {
    double default_probability = default_probability_start * (1.0 + normalized_probability); // Convert normalized probability back to actual probability
    normalized_probabilities.push_back(normalized_probability);
    swap_values.push_back(position * cds_payoff(credit_spread, default_probability, recovery_rate, nominal)); // Adjusted by position
  }
  
  return DataFrame::create(
    _["Spot"] = normalized_probabilities,
    _["Payoff"] = swap_values
  );
}

#include <Rcpp.h>
#include <cmath>
#include <vector>
#include <string>

using namespace Rcpp;

// Function for calculating the present value of credit default swap cash flows
double cds_present_value(double credit_spread, double default_probability, double recovery_rate, 
                         double notional, double period_length, double discount_rate) {
  // Protection leg: Payoff in case of default
  double protection_leg = notional * (1.0 - recovery_rate) * default_probability;
  
  // Premium leg: Regular payments based on the credit spread
  double premium_leg = credit_spread * period_length * notional;
  
  // Discount both legs
  double discount_factor = exp(-discount_rate * period_length);
  return discount_factor * (protection_leg - premium_leg);
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
  
  // Define a fixed range for default probabilities (0% to 200%)
  double default_probability_min = default_probability_start * 0.0; // Start at 0% of the initial probability
  double default_probability_max = default_probability_start * 2.0; // Range up to 200% of the initial probability
  int num_steps = 20000; // Standardize the number of rows to 20,001
  double default_probability_step = (default_probability_max - default_probability_min) / num_steps;
  
  std::vector<double> normalized_probabilities, swap_values;
  
  for (int i = 0; i <= num_steps; ++i) {
    double default_probability = default_probability_min + i * default_probability_step; // Increment default probability
    double normalized_probability = (default_probability / default_probability_start) - 1.0; // Decimal deviation from the initial probability
    normalized_probabilities.push_back(normalized_probability);
    swap_values.push_back(position * cds_present_value(credit_spread, default_probability, recovery_rate, nominal, period_length, discount_rate)); // Adjusted by position
  }
  
  return DataFrame::create(
    _["Spot"] = normalized_probabilities,
    _["Price"] = swap_values
  );
}
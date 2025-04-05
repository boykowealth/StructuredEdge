#include <Rcpp.h>
#include <cmath>
#include <vector>
#include <string>

using namespace Rcpp;

double swap_present_value(double fixed_rate, double floating_rate, double notional, double period_length, double discount_rate) {
  double discount_factor = exp(-discount_rate * period_length); 
  double fixed_leg = fixed_rate * period_length * notional;  
  double floating_leg = floating_rate * period_length * notional; 
  return discount_factor * (floating_leg - fixed_leg);  
}

// [[Rcpp::export]]
DataFrame interestRateSwap(double fixed_rate, double T, double floating_rate, 
                           double discount_rate, std::string position_str, double nominal) {
  // Convert position input to 1 for "Long" and -1 for "Short" --> 1 Payer of Fixed, -1 Payer of Floating
  int position;
  if (position_str == "Long" || position_str == "long") {
    position = 1; 
  } else if (position_str == "Short" || position_str == "short") {
    position = -1; 
  } else {
    Rcpp::stop("Invalid position. Use 'Long' for long position or 'Short' for short position.");
  }
  
  double floating_rate_min = -1.0;  // -100% normalized
  double floating_rate_max = 1.0;   // +100% normalized
  double floating_rate_step = 0.0001; // Step size of 1 basis point (bps)
  
  std::vector<double> normalized_rates, swap_values;
  
  for (double normalized_rate = floating_rate_min; normalized_rate <= floating_rate_max; normalized_rate += floating_rate_step) {
    double floating_rate_curr = floating_rate * (1.0 + normalized_rate);
    normalized_rates.push_back(normalized_rate);
    swap_values.push_back(position * swap_present_value(fixed_rate, floating_rate_curr, nominal, T, discount_rate));
  }
  
  return DataFrame::create(
    _["Spot"] = normalized_rates,
    _["Payoff"] = swap_values
  );
}
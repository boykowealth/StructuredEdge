#include <Rcpp.h>
#include <cmath>
#include <vector>
#include <string>

using namespace Rcpp;

// Function to calculate the payoff of an exchange rate swap (without discounting)
double exchange_rate_swap_payoff(double fixed_rate, double exchange_rate, double notional, double period_length) {
  double fixed_leg = fixed_rate * period_length * notional;    // Fixed cash flow in one currency
  double floating_leg = exchange_rate * period_length * notional; // Floating cash flow based on the exchange rate
  return floating_leg - fixed_leg;         // Net cash flow (Payoff)
}

// [[Rcpp::export]]
DataFrame exchangeRateSwap(double fixed_rate, double period_length, double exchange_rate, 
                           double discount_rate, std::string position_str, double nominal) {
  // Convert position input to 1 for "Long" and -1 for "Short"
  int position;
  if (position_str == "Long" || position_str == "long") {
    position = 1; // Long position
  } else if (position_str == "Short" || position_str == "short") {
    position = -1; // Short position
  } else {
    Rcpp::stop("Invalid position. Use 'Long' for long position or 'Short' for short position.");
  }
  
  // Define the fixed range for exchange rates (-100% to +100%)
  double exchange_rate_min = -1.0;  // -100% normalized
  double exchange_rate_max = 1.0;   // +100% normalized
  double exchange_rate_step = 0.0001; // Step size of 1 basis point (bps)
  
  std::vector<double> normalized_rates, swap_values;
  
  for (double normalized_rate = exchange_rate_min; normalized_rate <= exchange_rate_max; normalized_rate += exchange_rate_step) {
    double exchange_rate_curr = exchange_rate * (1.0 + normalized_rate); // Convert normalized rate back to actual exchange rate
    normalized_rates.push_back(normalized_rate);
    swap_values.push_back(position * exchange_rate_swap_payoff(fixed_rate, exchange_rate_curr, nominal, period_length)); // Adjusted by position
  }
  
  return DataFrame::create(
    _["Spot"] = normalized_rates,
    _["Payoff"] = swap_values
  );
}

#include <Rcpp.h>
#include <cmath>
#include <vector>
#include <string>

using namespace Rcpp;

// Function to calculate the present value of an exchange rate swap
double exchange_rate_swap_present_value(double fixed_rate, double exchange_rate, double notional, double period_length, double discount_rate) {
  double discount_factor = exp(-discount_rate * period_length); // Discount factor for the period
  double fixed_leg = fixed_rate * period_length * notional;    // Fixed cash flow in one currency
  double floating_leg = exchange_rate * period_length * notional; // Floating cash flow based on the exchange rate
  return discount_factor * (floating_leg - fixed_leg);         // Net cash flow, discounted
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
  
  // Range of exchange rates
  double exchange_rate_min = exchange_rate * 0.0;  // Start at 0% of the initial exchange rate
  double exchange_rate_max = exchange_rate * 2.0001;  // Range up to 200% of the initial exchange rate
  double exchange_rate_step = 0.0001100001;
  
  std::vector<double> normalized_rates, swap_values;
  
  for (double exchange_rate_curr = exchange_rate_min; exchange_rate_curr <= exchange_rate_max; exchange_rate_curr += exchange_rate_step) {
    double normalized_rate = (exchange_rate_curr / exchange_rate) - 1.0; // Decimal deviation from the initial exchange rate
    normalized_rates.push_back(normalized_rate);
    swap_values.push_back(position * exchange_rate_swap_present_value(fixed_rate, exchange_rate_curr, nominal, period_length, discount_rate)); // Adjusted by position
  }
  
  return DataFrame::create(
    _["Spot"] = normalized_rates,
    _["Price"] = swap_values
  );
}
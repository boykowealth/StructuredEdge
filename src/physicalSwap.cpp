#include <Rcpp.h>
#include <cmath>
#include <vector>
#include <string>

using namespace Rcpp;

// Function to calculate the payoff of a commodity swap (without discounting)
double commodity_swap_payoff(double fixed_price, double spot_price, double notional, double period_length) {
  double fixed_leg = fixed_price * period_length * notional;    // Fixed cash flow
  double floating_leg = spot_price * period_length * notional;  // Floating cash flow based on spot price
  return floating_leg - fixed_leg;          // Net cash flow (Payoff)
}

// [[Rcpp::export]]
DataFrame physicalSwap(double fixed_price, double period_length, double spot_price, 
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
  
  // Define the fixed range for spot prices (-100% to +100%)
  double spot_price_min = -1.0;  // -100% normalized
  double spot_price_max = 1.0;   // +100% normalized
  double spot_price_step = 0.0001; // Step size of 1 basis point (bps)
  
  std::vector<double> normalized_prices, swap_values;
  
  for (double normalized_price = spot_price_min; normalized_price <= spot_price_max; normalized_price += spot_price_step) {
    double spot_price_curr = spot_price * (1.0 + normalized_price); // Convert normalized price back to actual spot price
    normalized_prices.push_back(normalized_price);
    swap_values.push_back(position * commodity_swap_payoff(fixed_price, spot_price_curr, nominal, period_length)); // Adjusted by position
  }
  
  return DataFrame::create(
    _["Spot"] = normalized_prices,
    _["Payoff"] = swap_values
  );
}

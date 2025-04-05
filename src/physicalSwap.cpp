#include <Rcpp.h>
#include <cmath>
#include <vector>
#include <string>

using namespace Rcpp;

double commodity_swap_payoff(double fixed_price, double spot_price, double notional, double period_length) {
  double fixed_leg = fixed_price * period_length * notional;  
  double floating_leg = spot_price * period_length * notional; 
  return floating_leg - fixed_leg;   
}

// [[Rcpp::export]]
DataFrame physicalSwap(double fixed_price, double period_length, double spot_price, 
                       double discount_rate, std::string position_str, double nominal) {
  int position;
  if (position_str == "Long" || position_str == "long") {
    position = 1; // Long position
  } else if (position_str == "Short" || position_str == "short") {
    position = -1; // Short position
  } else {
    Rcpp::stop("Invalid position. Use 'Long' for long position or 'Short' for short position.");
  }
  
  double spot_price_min = -1.0;  // -100% normalized
  double spot_price_max = 1.0;   // +100% normalized
  double spot_price_step = 0.0001; // Step size of 1 basis point (bps)
  
  std::vector<double> normalized_prices, swap_values;
  
  for (double normalized_price = spot_price_min; normalized_price <= spot_price_max; normalized_price += spot_price_step) {
    double spot_price_curr = spot_price * (1.0 + normalized_price); 
    normalized_prices.push_back(normalized_price);
    swap_values.push_back(position * commodity_swap_payoff(fixed_price, spot_price_curr, nominal, period_length)); 
  }
  
  return DataFrame::create(
    _["Spot"] = normalized_prices,
    _["Payoff"] = swap_values
  );
}

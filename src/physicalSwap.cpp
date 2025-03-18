#include <Rcpp.h>
#include <cmath>
#include <vector>

using namespace Rcpp;

// Function for calculating fixed and floating values in a commodity swap
double fixed_leg(double fixed_price, double quantity) {
  return fixed_price * quantity;
}

double floating_leg(double spot_price, double quantity) {
  return spot_price * quantity;
}

// [[Rcpp::export]]
DataFrame physicalSwap(double quantity, double fixed_price, double spot_price) {
  // Automatically determine the spot price range
  double spot_price_min = spot_price * 0.8;
  double spot_price_max = spot_price * 1.2;
  double spot_price_step = 0.5;
  
  std::vector<double> spot_prices, fixed_leg_values, floating_leg_values;
  
  for (double spot = spot_price_min; spot <= spot_price_max; spot += spot_price_step) {
    spot_prices.push_back(spot);
    fixed_leg_values.push_back(fixed_leg(fixed_price, quantity));
    floating_leg_values.push_back(floating_leg(spot, quantity));
  }
  
  return DataFrame::create(
    _["Spot"] = spot_prices,
    _["Fixed"] = fixed_leg_values,
    _["Floating"] = floating_leg_values
  );
}

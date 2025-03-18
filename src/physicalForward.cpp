#include <Rcpp.h>
#include <cmath>
#include <vector>

using namespace Rcpp;

// Function for calculating forward prices with storage costs
double physical_delivery_forward_price(double S, double T, double r, double c) {
  return S * exp((r + c) * T);
}

// [[Rcpp::export]]
DataFrame physForwardContract(double S, double T, double r, double c) {
  double S_min = S * 0.8;
  double S_max = S * 1.2;
  double S_step = 0.5;
  
  std::vector<double> spot_prices, forward_values;
  
  for (double S_curr = S_min; S_curr <= S_max; S_curr += S_step) {
    spot_prices.push_back(S_curr);
    forward_values.push_back(physical_delivery_forward_price(S_curr, T, r, c));
  }
  
  return DataFrame::create(
    _["Spot"] = spot_prices,
    _["Forward"] = forward_values
  );
}

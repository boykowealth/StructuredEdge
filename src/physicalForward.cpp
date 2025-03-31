#include <Rcpp.h>
#include <cmath>
#include <vector>
#include <string>

using namespace Rcpp;

// Function for calculating forward prices with storage costs
double physical_delivery_forward_price(double S, double T, double r, double c) {
  return S * exp((r + c) * T);
}

// [[Rcpp::export]]
DataFrame physForwardContract(double S, double T, double r, double c, std::string position_str, double nominal) {
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
  double S_min = -1.0;  // -100% normalized
  double S_max = 1.0;   // +100% normalized
  double S_step = 0.0001; // Step size of 1 basis point (bps)
  
  std::vector<double> normalized_spots, forward_values;
  
  for (double normalized_spot = S_min; normalized_spot <= S_max; normalized_spot += S_step) {
    double S_curr = S * (1.0 + normalized_spot); // Convert normalized spot back to price
    normalized_spots.push_back(normalized_spot);
    forward_values.push_back(position * nominal * physical_delivery_forward_price(S_curr, T, r, c)); // Adjusted by position and nominal
  }
  
  return DataFrame::create(
    _["Spot"] = normalized_spots,
    _["Payoff"] = forward_values
  );
}
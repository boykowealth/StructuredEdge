#include <Rcpp.h>
#include <cmath>
#include <vector>
#include <string>

using namespace Rcpp;

// Function for calculating exchange rate forward prices with foreign and domestic interest rates
double exchange_rate_forward_price(double S, double T, double rd, double rf) {
  return S * exp((rd - rf) * T); // Incorporates domestic and foreign interest rates
}

// [[Rcpp::export]]
DataFrame exchangeForward(double S, double T, double rd, double rf, std::string position_str, double nominal) {
  // Convert position input to 1 for "Long" and -1 for "Short"
  int position;
  if (position_str == "Long" || position_str == "long") {
    position = 1; // Long position
  } else if (position_str == "Short" || position_str == "short") {
    position = -1; // Short position
  } else {
    Rcpp::stop("Invalid position. Use 'Long' for long position or 'Short' for short position.");
  }
  
  double S_min = S * 0.0;  // Start at 0% of the original spot price
  double S_max = S * 2.0;  // Range up to 200% of the original spot price
  double S_step = 0.01;    // Divide range into small increments
  
  std::vector<double> normalized_spots, forward_values;
  
  for (double S_curr = S_min; S_curr <= S_max; S_curr += S_step) {
    double normalized_spot = (S_curr / S) - 1.0; // Calculate decimal deviation from original spot price
    normalized_spots.push_back(normalized_spot);
    forward_values.push_back(position * nominal * exchange_rate_forward_price(S_curr, T, rd, rf)); // Adjusted by position and nominal
  }
  
  return DataFrame::create(
    _["Spot"] = normalized_spots,
    _["Price"] = forward_values
  );
}
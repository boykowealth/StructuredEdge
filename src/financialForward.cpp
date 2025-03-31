#include <Rcpp.h>
#include <cmath>
#include <vector>
#include <string>

using namespace Rcpp;

// Function for calculating forward price at initial spot
double forward_price(double S, double T, double r) {
  return S * exp(r * T); // Forward price at initial spot (S)
}

// [[Rcpp::export]]
DataFrame finForwardContract(double S, double T, double r, std::string position_str, double nominal) {
  // Convert position input to 1 for "Long" and -1 for "Short"
  int position;
  if (position_str == "Long" || position_str == "long") {
    position = 1; // Long position
  } else if (position_str == "Short" || position_str == "short") {
    position = -1; // Short position
  } else {
    Rcpp::stop("Invalid position. Use 'Long' for long position or 'Short' for short position.");
  }
  
  // Compute the initial forward price at S (0% spot change)
  double forward_initial = forward_price(S, T, r);
  
  // Define the fixed range for spot prices (-100% to +100%)
  double S_min = -1.0;  // -100% normalized
  double S_max = 1.0;   // +100% normalized
  double S_step = 0.0001; // Step size of 1 basis point (bps)
  
  std::vector<double> normalized_spots, payoffs;
  
  for (double normalized_spot = S_min; normalized_spot <= S_max; normalized_spot += S_step) {
    // Calculate the current spot price based on normalization
    double S_curr = S * (1.0 + normalized_spot);
    normalized_spots.push_back(normalized_spot);
    
    // Payoff = (Spot Price - Initial Forward Price) * Position * Nominal
    double payoff = position * nominal * (S_curr - forward_initial);
    payoffs.push_back(payoff);
  }
  
  return DataFrame::create(
    _["Spot"] = normalized_spots,
    _["Payoff"] = payoffs
  );
}

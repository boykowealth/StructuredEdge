#include <Rcpp.h>
#include <cmath>
#include <vector>
#include <string>

using namespace Rcpp;

double physical_delivery_forward_price(double S, double T, double r, double c) {
  return S * exp((r + c) * T); // Includes storage costs in forward price calculation
}

// [[Rcpp::export]]
DataFrame physForwardContract(double S, double T, double r, double c, std::string position_str, double nominal) {
  int position;
  if (position_str == "Long" || position_str == "long") {
    position = 1; 
  } else if (position_str == "Short" || position_str == "short") {
    position = -1; 
  } else {
    Rcpp::stop("Invalid position. Use 'Long' for long position or 'Short' for short position.");
  }
  
  double forward_price_initial = physical_delivery_forward_price(S, T, r, c);
  
  double S_min = -1.0;  // -100% normalized
  double S_max = 1.0;   // +100% normalized
  double S_step = 0.0001; // Step size of 1 basis point (bps)
  
  std::vector<double> normalized_spots, payoffs;
  
  for (double normalized_spot = S_min; normalized_spot <= S_max; normalized_spot += S_step) {
    double S_curr = S * (1.0 + normalized_spot);
    normalized_spots.push_back(normalized_spot);
    
    double payoff = position * nominal * (S_curr - forward_price_initial);
    payoffs.push_back(payoff);
  }
  
  return DataFrame::create(
    _["Spot"] = normalized_spots,
    _["Payoff"] = payoffs
  );
}

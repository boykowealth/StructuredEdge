#include <Rcpp.h>
#include <cmath>
#include <vector>
#include <string>

using namespace Rcpp;

double exchange_rate_forward_price(double S, double T, double rd, double rf) {
  return S * exp((rd - rf) * T); // Forward price based on interest rate differential
}

// [[Rcpp::export]]
DataFrame exchangeForward(double S, double T, double rd, double rf, std::string position_str, double nominal) {
  int position;
  if (position_str == "Long" || position_str == "long") {
    position = 1; // Long position
  } else if (position_str == "Short" || position_str == "short") {
    position = -1; // Short position
  } else {
    Rcpp::stop("Invalid position. Use 'Long' for long position or 'Short' for short position.");
  }
  
  double forward_initial = exchange_rate_forward_price(S, T, rd, rf);
  
  double S_min = -1.0;  // -100% normalized
  double S_max = 1.0;   // +100% normalized
  double S_step = 0.0001; // Step size of 1 basis point (bps)
  
  std::vector<double> normalized_spots, payoffs;
  
  for (double normalized_spot = S_min; normalized_spot <= S_max; normalized_spot += S_step) {
    double S_curr = S * (1.0 + normalized_spot);
    normalized_spots.push_back(normalized_spot);
    
    double payoff = position * nominal * (S_curr - forward_initial);
    payoffs.push_back(payoff);
  }
  
  return DataFrame::create(
    _["Spot"] = normalized_spots,
    _["Payoff"] = payoffs
  );
}

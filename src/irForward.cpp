#include <Rcpp.h>
#include <cmath>
#include <vector>
#include <string>

using namespace Rcpp;

double fra_forward_rate(double r1, double r2, double t1, double t2) {
  return (r2 * t2 - r1 * t1) / (t2 - t1); // Implied forward rate
}

// [[Rcpp::export]]
DataFrame irForward(double r1, double r2, double t1, double t2, double nominal, std::string position_str) {
  int position;
  if (position_str == "Long" || position_str == "long") {
    position = 1; 
  } else if (position_str == "Short" || position_str == "short") {
    position = -1; 
  } else {
    Rcpp::stop("Invalid position. Use 'Long' for long position or 'Short' for short position.");
  }
  
  double forward_rate_initial = fra_forward_rate(r1, r2, t1, t2);
  
  double r_min = -1.0;  // -100% normalized
  double r_max = 1.0;   // +100% normalized
  double r_step = 0.0001; // Step size of 1 basis point (bps)
  
  std::vector<double> normalized_rates, payoffs;
  
  for (double normalized_rate = r_min; normalized_rate <= r_max; normalized_rate += r_step) {
    double r_curr = r1 * (1.0 + normalized_rate);
    normalized_rates.push_back(normalized_rate);
    
    double actual_forward_rate = fra_forward_rate(r1, r_curr, t1, t2);
    double payoff = position * nominal * (actual_forward_rate - forward_rate_initial) * exp(-r_curr * t2);
    payoffs.push_back(payoff);
  }
  
  return DataFrame::create(
    _["Spot"] = normalized_rates,
    _["Payoff"] = payoffs
  );
}

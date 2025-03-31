#include <Rcpp.h>
#include <cmath>
#include <vector>
#include <string>

using namespace Rcpp;

// Standard normal cumulative distribution function
double norm_cdf(double x) {
  return 0.5 * std::erfc(-x / std::sqrt(2));
}

// Black-Scholes option pricing formula with cost of carry
double black_scholes_price(std::string option_type, double S, double K, double T, double r, double sigma, double b) {
  double d1 = (std::log(S / K) + (b + 0.5 * sigma * sigma) * T) / (sigma * std::sqrt(T));
  double d2 = d1 - sigma * std::sqrt(T);
  
  if (option_type == "Call" || option_type == "call") {
    return S * std::exp((b - r) * T) * norm_cdf(d1) - K * std::exp(-r * T) * norm_cdf(d2); // Call price
  } else if (option_type == "Put" || option_type == "put") {
    return K * std::exp(-r * T) * norm_cdf(-d2) - S * std::exp((b - r) * T) * norm_cdf(-d1); // Put price
  } else {
    Rcpp::stop("Invalid option type. Use 'Call' for Call or 'Put' for Put.");
  }
}

// [[Rcpp::export]]
DataFrame blackScholes(double S, double K, double T, double r, double sigma, double b, std::string option_type, std::string position_str, double nominal, double option_cost) {
  // Convert position input to 1 for "Long" and -1 for "Short"
  int position;
  if (position_str == "Long" || position_str == "long") {
    position = 1; // Long position
  } else if (position_str == "Short" || position_str == "short") {
    position = -1; // Short position
  } else {
    Rcpp::stop("Invalid position. Use 'Long' for long position or 'Short' for short position.");
  }
  
  double S_min = -1.0;  // Start at -100% of the original spot price
  double S_max = 1.0;   // Range up to +100% of the original spot price
  double S_step = 0.0001; // Step size of 1 basis point (bps)
  
  std::vector<double> normalized_spots, black_scholes_prices;
  
  for (double normalized_spot = S_min; normalized_spot <= S_max; normalized_spot += S_step) {
    double S_curr = S * (1.0 + normalized_spot); // Convert normalized spot back to price
    double option_value = black_scholes_price(option_type, S_curr, K, T, r, sigma, b); // Option value from Black-Scholes formula
    double net_value = (option_value - option_cost) * nominal * position; // Payoff calculation
    normalized_spots.push_back(normalized_spot);
    black_scholes_prices.push_back(net_value);
  }
  
  return DataFrame::create(
    _["Spot"] = normalized_spots,
    _["Payoff"] = black_scholes_prices
  );
}
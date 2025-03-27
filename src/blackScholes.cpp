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
DataFrame blackScholes(double S, double K, double T, double r, double sigma, double b, std::string option_type, std::string position_str, double nominal) {
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
  double S_step = 0.01; // Divide range into bps
  
  std::vector<double> normalized_spots, black_scholes_prices;
  
  for (double S_curr = S_min; S_curr <= S_max; S_curr += S_step) {
    double normalized_spot = (S_curr / S) - 1.0; // Calculate decimal deviation from original spot
    normalized_spots.push_back(normalized_spot);
    black_scholes_prices.push_back(position * nominal * black_scholes_price(option_type, S_curr, K, T, r, sigma, b)); // Adjusted by position
  }
  
  return DataFrame::create(
    _["Spot"] = normalized_spots,
    _["Price"] = black_scholes_prices
  );
}
#include <Rcpp.h>
#include <cmath>
#include <vector>
#include <string>

using namespace Rcpp;

// Function for binomial tree option pricing
double binomial_tree_price(std::string option_type, double S, double K, double T, double r, double u, double d, double p, int steps) {
  std::vector<double> option_values(steps + 1, 0.0);
  
  // Compute terminal option prices
  for (int i = 0; i <= steps; ++i) {
    double stock_price = S * std::pow(u, steps - i) * std::pow(d, i);
    if (option_type == "Call" || option_type == "call") {
      option_values[i] = std::max(0.0, stock_price - K); // Terminal payoff for Call
    } else if (option_type == "Put" || option_type == "put") {
      option_values[i] = std::max(0.0, K - stock_price); // Terminal payoff for Put
    } else {
      Rcpp::stop("Invalid option type. Use 'Call' for Call or 'Put' for Put.");
    }
  }
  
  // Backward induction through the tree
  for (int step = steps - 1; step >= 0; --step) {
    for (int i = 0; i <= step; ++i) {
      option_values[i] = exp(-r * T / steps) * (p * option_values[i] + (1 - p) * option_values[i + 1]);
    }
  }
  
  return option_values[0]; // Return the option price at the root node
}

// [[Rcpp::export]]
DataFrame binomialTree(double S, double K, double T, double r, double u, double d, double p, int steps, std::string option_type, std::string position_str, double nominal, double option_cost) {
  // Convert position input to 1 for "Long" and -1 for "Short"
  int position;
  if (position_str == "Long" || position_str == "long") {
    position = 1; // Long position
  } else if (position_str == "Short" || position_str == "short") {
    position = -1; // Short position
  } else {
    Rcpp::stop("Invalid position. Use 'Long' for long position or 'Short' for short position.");
  }
  
  double S_min = -1.0;  // Start at -100% of the original spot price (normalized)
  double S_max = 1.0;   // Range up to +100% of the original spot price (normalized)
  double S_step = 0.0001; // Step size of 1 basis point (bps)
  
  std::vector<double> normalized_spots, binomial_tree_prices;
  
  for (double normalized_spot = S_min; normalized_spot <= S_max; normalized_spot += S_step) {
    double S_curr = S * (1.0 + normalized_spot); // Convert normalized spot back to price
    double option_value = binomial_tree_price(option_type, S_curr, K, T, r, u, d, p, steps); // Option value from tree
    double net_value = (option_value - option_cost) * nominal * position; // Payoff calculation
    normalized_spots.push_back(normalized_spot);
    binomial_tree_prices.push_back(net_value);
  }
  
  return DataFrame::create(
    _["Spot"] = normalized_spots,
    _["Payoff"] = binomial_tree_prices
  );
}
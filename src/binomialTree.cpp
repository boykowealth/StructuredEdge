#include <Rcpp.h>
#include <cmath>
#include <vector>

using namespace Rcpp;

// Function for binomial tree option pricing
double binomial_tree_price(char option_type, double S, double K, double T, double r, double u, double d, double p, int steps) {
  std::vector<double> option_values(steps + 1, 0.0);
  
  // Compute terminal option prices
  for (int i = 0; i <= steps; ++i) {
    double stock_price = S * std::pow(u, steps - i) * std::pow(d, i);
    if (option_type == 'C' || option_type == 'c') {
      option_values[i] = std::max(0.0, stock_price - K);
    } else if (option_type == 'P' || option_type == 'p') {
      option_values[i] = std::max(0.0, K - stock_price);
    } else {
      Rcpp::stop("Invalid option type. Use 'C' for Call or 'P' for Put.");
    }
  }
  
  // Backward induction through the tree
  for (int step = steps - 1; step >= 0; --step) {
    for (int i = 0; i <= step; ++i) {
      option_values[i] = exp(-r * T / steps) * (p * option_values[i] + (1 - p) * option_values[i + 1]);
    }
  }
  
  return option_values[0];
}

// [[Rcpp::export]]
DataFrame binomialTree(double S, double K, double T, double r, double u, double d, double p, int steps) {
  double S_min = S * 0.8;
  double S_max = S * 1.2;
  double S_step = 0.5;
  
  std::vector<double> spot_prices, call_prices, put_prices;
  
  for (double S_curr = S_min; S_curr <= S_max; S_curr += S_step) {
    spot_prices.push_back(S_curr);
    call_prices.push_back(binomial_tree_price('C', S_curr, K, T, r, u, d, p, steps));
    put_prices.push_back(binomial_tree_price('P', S_curr, K, T, r, u, d, p, steps));
  }
  
  return DataFrame::create(
    _["Spot"] = spot_prices,
    _["Call"] = call_prices,
    _["Put"] = put_prices
  );
}

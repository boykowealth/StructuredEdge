#include <Rcpp.h>
#include <cmath>
#include <vector>

using namespace Rcpp;

// Standard normal cumulative distribution function
double norm_cdf(double x) {
  return 0.5 * std::erfc(-x / std::sqrt(2));
}

// Black-Scholes option pricing formula with cost of carry
double black_scholes_price(char option_type, double S, double K, double T, double r, double sigma, double b) {
  double d1 = (std::log(S / K) + (b + 0.5 * sigma * sigma) * T) / (sigma * std::sqrt(T));
  double d2 = d1 - sigma * std::sqrt(T);
  
  if (option_type == 'C' || option_type == 'c') {
    return S * std::exp((b - r) * T) * norm_cdf(d1) - K * std::exp(-r * T) * norm_cdf(d2);
  } else if (option_type == 'P' || option_type == 'p') {
    return K * std::exp(-r * T) * norm_cdf(-d2) - S * std::exp((b - r) * T) * norm_cdf(-d1);
  } else {
    Rcpp::stop("Invalid option type. Use 'C' for Call or 'P' for Put.");
  }
}

// [[Rcpp::export]]
DataFrame blackScholes(double S, double K, double T, double r, double sigma, double b) {
  double S_min = S * 0.8;
  double S_max = S * 1.2;
  double S_step = 0.5;
  
  std::vector<double> spot_prices, call_prices, put_prices;
  
  for (double S_curr = S_min; S_curr <= S_max; S_curr += S_step) {
    spot_prices.push_back(S_curr);
    call_prices.push_back(black_scholes_price('C', S_curr, K, T, r, sigma, b));
    put_prices.push_back(black_scholes_price('P', S_curr, K, T, r, sigma, b));
  }
  
  return DataFrame::create(
    _["Spot"] = spot_prices,
    _["Call"] = call_prices,
    _["Put"] = put_prices
  );
}

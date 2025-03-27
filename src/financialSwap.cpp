#include <Rcpp.h>
#include <cmath>
#include <vector>
#include <string>

using namespace Rcpp;

// Function for calculating the present value of swap cash flows
double swap_present_value(double fixed_rate, double floating_rate, double notional, double period_length, double discount_rate) {
  double discount_factor = exp(-discount_rate * period_length); // Discount factor for the period
  double fixed_leg = fixed_rate * period_length * notional;     // Fixed cash flow
  double floating_leg = floating_rate * period_length * notional; // Floating cash flow
  return discount_factor * (floating_leg - fixed_leg);          // Net cash flow, discounted
}

// [[Rcpp::export]]
DataFrame interestRateSwap(double fixed_rate, double T, double floating_rate, 
                           double discount_rate, std::string position_str, double nominal) {
  // Convert position input to 1 for "Long" and -1 for "Short" --> 1 Payer of Fixed, -1 Payer of Floating
  int position;
  if (position_str == "Long" || position_str == "long") {
    position = 1; // Long position
  } else if (position_str == "Short" || position_str == "short") {
    position = -1; // Short position
  } else {
    Rcpp::stop("Invalid position. Use 'Long' for long position or 'Short' for short position.");
  }
  
  // Range of floating rates
  double floating_rate_min = floating_rate * 0.0;  // Start at 0% of the initial floating rate
  double floating_rate_max = floating_rate * 2.0001;  // Range up to 200% of the initial floating rate
  double floating_rate_step = 0.000003;
  
  std::vector<double> normalized_rates, swap_values;
  
  for (double floating_rate_curr = floating_rate_min; floating_rate_curr <= floating_rate_max; floating_rate_curr += floating_rate_step) {
    double normalized_rate = (floating_rate_curr / floating_rate) - 1.0; // Decimal deviation from the initial floating rate
    normalized_rates.push_back(normalized_rate);
    swap_values.push_back(position * swap_present_value(fixed_rate, floating_rate_curr, nominal, T, discount_rate)); // Adjusted by position
  }
  
  return DataFrame::create(
    _["Spot"] = normalized_rates,
    _["Price"] = swap_values
  );
}
#include <Rcpp.h>
#include <cmath>
#include <vector>

using namespace Rcpp;

// Function for calculating forward interest rate
double forward_rate(double r1, double r2, double t1, double t2) {
  return ((1 + r2 * t2) / (1 + r1 * t1) - 1) / (t2 - t1);
}

// [[Rcpp::export]]
DataFrame irForward(double r1, double t1, double t2, double t_min, double t_max, double t_step) {
  std::vector<double> time_points, forward_rates;
  
  for (double t_curr = t_min; t_curr <= t_max; t_curr += t_step) {
    double r2 = r1 + t_curr * 0.001; // Example term structure, modify as needed
    time_points.push_back(t_curr);
    forward_rates.push_back(forward_rate(r1, r2, t1, t_curr));
  }
  
  return DataFrame::create(
    _["Time"] = time_points,
    _["Forward"] = forward_rates
  );
}

#include <Rcpp.h>
#include <cmath>
#include <random>
using namespace Rcpp;

// [[Rcpp::export]]
DataFrame simulate_gbm_single(double num_steps, double S0, double mu, double sigma, double T) {
  if (num_steps <= 0 || T <= 0) {
    stop("num_steps and T must be greater than 0.");
  }
  
  double dt = T / num_steps;
  
  std::random_device rd;
  std::mt19937 gen(rd());
  std::normal_distribution<> d(0.0, 1.0);
  
  NumericVector results(num_steps + 1);
  
  results[0] = S0;
  
  for (int step = 1; step <= num_steps; step++) {
    double dz = d(gen); 
    double St = results[step - 1] * exp((mu - 0.5 * sigma * sigma) * dt + sigma * sqrt(dt) * dz);
    results[step] = St;
  }
  
  NumericVector time(num_steps + 1);
  for (int i = 0; i <= num_steps; i++) {
    time[i] = i * dt;
  }
  
  if (time.size() != results.size()) {
    stop("Vector length mismatch between time and results.");
  }
  
  DataFrame df = DataFrame::create(Named("Time") = time, Named("Price") = results);
  
  return df;
}
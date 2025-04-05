#include <Rcpp.h>
#include <cmath>
#include <random>
using namespace Rcpp;

// [[Rcpp::export]]
DataFrame simulate_ou_single(int num_steps, double X0, double mu, double theta, double sigma, double T) {
  
  if (num_steps <= 0 || T <= 0) {
    stop("num_steps and T must be greater than 0.");
  }
  
  double dt = T / num_steps;
  
  std::random_device rd;
  std::mt19937 gen(rd());
  std::normal_distribution<> d(0.0, 1.0);
  
  NumericVector results(num_steps + 1);
  
  results(0) = X0;
  
  for (int step = 1; step <= num_steps; step++) {
    double dz = d(gen); 
    double Xt = results(step - 1) + theta * (mu - results(step - 1)) * dt + sigma * sqrt(dt) * dz;
    results(step) = Xt;
  }
  
  NumericVector time(num_steps + 1);
  for (int i = 0; i <= num_steps; i++) {
    time(i) = i * dt;
  }
  
  DataFrame df = DataFrame::create(Named("Time") = time, Named("Price") = results);
  
  return df;
}

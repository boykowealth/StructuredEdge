#include <Rcpp.h>
#include <cmath>
#include <random>
using namespace Rcpp;

// [[Rcpp::export]]
DataFrame simulate_ou_single(int num_steps, double X0, double mu, double theta, double sigma, double T) {
  
  if (num_steps <= 0 || T <= 0) {
    stop("num_steps and T must be greater than 0.");
  }
  
  // Define the time step (assuming uniform step size)
  double dt = T / num_steps;
  
  // Create a random number generator for Brownian motion (Normal distribution)
  std::random_device rd;
  std::mt19937 gen(rd());
  std::normal_distribution<> d(0.0, 1.0);
  
  // Vector to store the results of the simulation
  NumericVector results(num_steps + 1);
  
  // Set the initial value
  results(0) = X0;
  
  // Simulate the OU process
  for (int step = 1; step <= num_steps; step++) {
    double dz = d(gen);  // Brownian motion increment
    double Xt = results(step - 1) + theta * (mu - results(step - 1)) * dt + sigma * sqrt(dt) * dz;
    results(step) = Xt;
  }
  
  // Create the time vector for the DataFrame
  NumericVector time(num_steps + 1);
  for (int i = 0; i <= num_steps; i++) {
    time(i) = i * dt;
  }
  
  // Create the DataFrame
  DataFrame df = DataFrame::create(Named("Time") = time, Named("Value") = results);
  
  return df;
}

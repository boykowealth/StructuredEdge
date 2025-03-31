#include <Rcpp.h>
#include <cmath>
#include <random>
using namespace Rcpp;

// [[Rcpp::export]]
DataFrame simulate_ouj_single(int num_steps, double S0, double mu, double theta, double sigma, 
                              double Jump_prob, double Jump_mean,
                              double sigma_JumpStdv, double T) {
  
  if (num_steps <= 0 || T <= 0) {
    stop("num_steps and T must be greater than 0.");
  }
  // Define the time step (assuming uniform step size)
  double dt = T / num_steps;
  
  // Random number generators
  std::random_device rd;
  std::mt19937 gen(rd());
  std::normal_distribution<> d(0.0, 1.0);  // For Brownian motion
  std::poisson_distribution<> poisson_dist(Jump_prob * dt);  // For Poisson-distributed jumps
  std::lognormal_distribution<> lognorm_d(0, sigma_JumpStdv);  // For lognormal jump sizes
  
  // Vector to store the results of the simulation
  NumericVector results(num_steps + 1);
  
  // Set the initial value
  results(0) = S0;
  
  // Simulate the OUJ process (OU with Lévy noise)
  for (int step = 1; step <= num_steps; step++) {
    double dz = d(gen);  // Brownian motion increment
    double z_t = poisson_dist(gen);  // Poisson-distributed jump indicator
    double epsilon_2 = lognorm_d(gen);  // Lognormal jump size
    
    // Mean reversion term
    double drift_term = theta * (mu - Jump_prob * Jump_mean - results(step - 1));
    
    // Update the process value based on the OUJ model
    double dS = drift_term * results(step - 1) * dt + sigma * results(step - 1) * dz * sqrt(dt) + z_t * epsilon_2;
    results(step) = results(step - 1) + dS;
  }
  
  // Create the time vector for the DataFrame
  NumericVector time(num_steps + 1);
  for (int i = 0; i <= num_steps; i++) {
    time(i) = i * dt;
  }
  
  // Create the DataFrame
  DataFrame df = DataFrame::create(Named("Time") = time, Named("Price") = results);
  
  return df;
}

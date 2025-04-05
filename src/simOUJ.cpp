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
  double dt = T / num_steps;
  
  std::random_device rd;
  std::mt19937 gen(rd());
  std::normal_distribution<> d(0.0, 1.0);  // For Brownian motion
  std::poisson_distribution<> poisson_dist(Jump_prob * dt);  // For Poisson-distributed jumps
  std::lognormal_distribution<> lognorm_d(0, sigma_JumpStdv);  // For lognormal jump sizes
  
  NumericVector results(num_steps + 1);
  
  results(0) = S0;
  
  for (int step = 1; step <= num_steps; step++) {
    double dz = d(gen);  
    double z_t = poisson_dist(gen); 
    double epsilon_2 = lognorm_d(gen);
    
    double drift_term = theta * (mu - Jump_prob * Jump_mean - results(step - 1));
    
    double dS = drift_term * results(step - 1) * dt + sigma * results(step - 1) * dz * sqrt(dt) + z_t * epsilon_2;
    results(step) = results(step - 1) + dS;
  }
  
  NumericVector time(num_steps + 1);
  for (int i = 0; i <= num_steps; i++) {
    time(i) = i * dt;
  }
  
  DataFrame df = DataFrame::create(Named("Time") = time, Named("Price") = results);
  
  return df;
}

//
// This Stan program defines a simple model, with a
// vector of values 'y' modeled as normally distributed
// with mean 'mu' and standard deviation 'sigma'.
//
// Learn more about model development with Stan at:
//
//    http://mc-stan.org/users/interfaces/rstan.html
//    https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
//

// The input data is a vector 'y' of length 'N'.
data {
  int<lower=1> K;            // Number of categories
  int<lower=1> Trials[K];         // Array of number of trials
  int<lower=0> Successes[K];    // Array of counts for each category
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  real<lower=0> theta_1;
  
  real<lower=0, upper =1> theta_2; // proportion always casual
  
  
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
    
    theta_1 ~ exponential(1);
    
    theta_2 ~ beta(1,1);
    
    for (k in 1:K) {
          Successes[k] ~ binomial(Trials[k], theta_2 + (1-theta_2)*(1-k^(-theta_1)));

    }
}

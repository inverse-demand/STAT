poisson_mle <- function(data_set){
  
  (1/nrow(data_set))*sum(data_set)
  
  
  
}

#loglikeliood of poisson

poisson_log_likelihood = function(lambda, x, n){ # function for Poisson log-likelihood
  return(-n * lambda + sum(x) * log(lambda) + sum(log(factorial(x))))
}


lambda <- seq(0.01, 9.99, 0.01)

plot(lambda, poisson_log_likelihood(lambda = lambda, x = data_set, n = 100), type='l',
     main = 'Loglikelihood',
     ylab = 'log-likelihood of lambda')
abline(v = poisson_mle(data_set))
abline(h = max(poisson_log_likelihood(lambda = lambda,
                                      x = data_set, n = 100)))
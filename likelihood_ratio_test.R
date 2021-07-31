likelihood_ratio <- function(x, n, pi){
  
  gllr <- x*log((n*pi)/x)+(n-x)*log((n*(1-pi))/(n-x))
  
  ratio <- -2*gllr
  z <- 1-pchisq(ratio, df = 1)
  return(z)
  
}

x <- likelihood_ratio(x = 26, n = 30, pi = .7)

1-pchisq(x, df = 1)



-2.42
4.84
-.5*.0278

-.5*-2.42

## look at chi-squared probability table
# .05 at 1 deg of freedom is 3.841*-.5

qchisq(1-.05, 1)*-.5


likelihood_gllr <- function(x, n, pi){
  
  gllr <- x*log((n*pi)/x)+(n-x)*log((n*(1-pi))/(n-x))
  
  return(gllr)
  
}

likelihood_ratio_interval <- function(x, n){
  
  
  ms <- data.frame(stringsAsFactors = FALSE)
  for(i in 1:1000){
    
     pi = .001*i
    
     gllr <- data.frame(likelihood_gllr(x, n, pi = pi))
     gllr$pi <- pi
     
     ms<-rbind(ms,gllr)
    
  }
  
  return(ms)
  
}

check <- likelihood_ratio_interval(x = 26, n = 30)
plot(check$pi,check$likelihood_gllr.x..n..pi...pi.)

likelihood_gllr(x = 26, n = 30, pi = .986)*2
likelihood_ratio(x = 26, n = 30, pi = .986)

# GLLR needs to be -1.92


for(i in 1:20){print(i)}

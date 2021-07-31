score.test <- function(p, pi, n){
  
  test <- ((p - pi)-1/(2*n))/(sqrt(pi*(1-pi)/n))
  test <- pnorm(q = score, lower.tail = FALSE)*2
  return(test)
  
}

score <- score.test(p = .8667, pi = .70, n = 30)
score <- score.test(p = .7, pi = .5333, n = 15)
score

pnorm(q = score, lower.tail = FALSE)*2

score.test_interval <- function(p,n,z){
  
  
  lower <- p*(n/(n+z^2))+.5*(z^2/(n+z^2))-z*sqrt((1/(n+z^2))*(p*(1-p)*(n/(n+z^2))+.5*.5*(z^2/(n+z^2))))
  upper <- p*(n/(n+z^2))+.5*(z^2/(n+z^2))+z*sqrt((1/(n+z^2))*(p*(1-p)*(n/(n+z^2))+.5*.5*(z^2/(n+z^2))))
  
  return(c(lower,upper))
  
}

score.test_interval(p=.64,n=350,z=1.96)
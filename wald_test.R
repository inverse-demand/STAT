wald.test <- function(p, pi, n){
  
  test <- ((p - pi)-1/(2*n))/(sqrt(p*(1-p)/n))
  #test <- (p - pi)/(sqrt(p*(1-p)/n))
  test <- pnorm(q = test, lower.tail = FALSE)*2
  
  return(test)
  
}

score <- wald.test(p = .8667, pi = .70, n = 30)
score <- wald.test(p = .92, pi = .75, n = 25)
score <- wald.test(p = .7, pi = .53333, n = 15)
#score <- wald.test(p = .53333, pi = .7, n = 15)

score
pnorm(q = score, lower.tail = FALSE)
pnorm(q = score, lower.tail = FALSE)*2


wald.test_interval <- function(p,z,n, approx = FALSE){
  
  if(approx == TRUE){
    lower <- p - z*sqrt(p*(1-p)/n)#-1/(2*n)
    upper <- p + z*sqrt(p*(1-p)/n)#+1/(2*n)
  } else if(approx == FALSE){
    
    lower <- p - z*sqrt(p*(1-p)/n)-1/(2*n)
    upper <- p + z*sqrt(p*(1-p)/n)+1/(2*n)
    
  } else{print("Not Viable")}
  
  return(c(lower,upper))
  
}

wald.test_interval(.92, 1.96, 25)
wald.test_interval(.8666667, 1.96, 30)
wald.test_interval(.64, 1.96, 350, approx = TRUE)


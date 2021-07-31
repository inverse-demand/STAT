like.ci<-function(n,x,alpha=0.05,df=1,ncp=0,l=100){
  
  cutoff<--0.5*qchisq((1-alpha),df=df,ncp=ncp)
  
  
  pi<-as.matrix(seq(0,1,length.out=l))
  
  GLLR.vec<-x*log((n*pi/x))+((n-x)*log((n*(1-pi))/(n-x)))
  
  test<-GLLR.vec>=cutoff
  like.U<-max(pi[test])
  like.L<-min(pi[test])
  
  plot(pi,GLLR.vec,ylim=c((cutoff-5),0),ylab="GLLR")
  #browser()
  
  segments(0,cutoff,like.L,cutoff)
  segments(like.L,(cutoff-5),like.L,cutoff)
  
  
  segments(0,cutoff,like.U,cutoff,lwd=2)
  segments(like.U,(cutoff-5),like.U,cutoff,lwd=2)
  title("Likelihood Intervals for a Proportion")	
  
  
  out<-cbind(like.L,like.U)
  return(out)
  
  #return(like.L,like.U)
  
  
}

like.ci(n = 30, x = 26)
like.ci(n = 350, x = 224)
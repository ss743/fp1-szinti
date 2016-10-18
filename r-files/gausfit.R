source("functions.R")

gausfit <- function(input,bereich,weighted=FALSE,sig0=0,N0=0){ #--- Fitten der Gaußfunktion
  
  thegaussian <- y ~ C + N*exp(-(x-mu)^2/(2*sig^2))
  
  daten=input[bereich[1]:bereich[2],]
  ymin=min(daten$y)
  if(N0==0){
    ymax=max(daten$y)
  } else {
    ymax=N0
  }
  mu0 =daten$x[which.max(daten$y)]
  if(sig0==0)
  {
    #sig0=(daten$x[bereich[2]]-daten$x[bereich[1]])/3
    sig0=(bereich[2]-bereich[1])/3
  }
  err=daten$sy

  #plot(function(x){ymin + ymax*exp(-(x-mu0)^2/(2*sig0^2))},bereich[1],bereich[2],add=TRUE,col="green")
  
    
  if(weighted)
    fit = nls(thegaussian,daten,weights=1/err^2,start=list(C=ymin,N=ymax,mu=mu0,sig=sig0))
  else
    fit = nls(thegaussian,daten,start=list(C=ymin,N=ymax,mu=mu0,sig=sig0))
  
  return(fit)
  
}

gaus2 <- function(input,bereich,weighted=FALSE,sig0=0,N0=0){ #--- Fitten der Gaußfunktion
  
  thegaussian <- y ~ C + N*exp(-(x-mu)^2/(2*sig^2))
  
  daten=subset(input,x>=bereich[1] & x <= bereich[2])#input[bereich[1]:bereich[2],]
  ymin=min(daten$y)
  if(N0==0){
    ymax=max(daten$y)
  } else {
    ymax=N0
  }
  mu0 =daten$x[which.max(daten$y)]
  if(sig0==0)
  {
    #sig0=(daten$x[bereich[2]]-daten$x[bereich[1]])/3
    sig0=(bereich[2]-bereich[1])/3
  }
  err=daten$sy
  
  #plot(function(x){ymin + ymax*exp(-(x-mu0)^2/(2*sig0^2))},bereich[1],bereich[2],add=TRUE,col="green")
  
  
  if(weighted)
    fit = nls(thegaussian,daten,weights=1/err^2,start=list(C=ymin,N=ymax,mu=mu0,sig=sig0))
  else
    fit = nls(thegaussian,daten,start=list(C=ymin,N=ymax,mu=mu0,sig=sig0))
  
  return(fit)
  
}


plotgaus <- function(fit,bereich,log=""){ #--- Plotten der gefitteten Gaußfunktion in vorhandenen Graph
  fitdata <- summary(fit)$parameters
  
  N<-fitdata["N","Estimate"]
  C<-fitdata["C","Estimate"]
  mu<-fitdata["mu","Estimate"]
  sig<-fitdata["sig","Estimate"]
  
  plot (function(x){C + N*exp(-(x-mu)^2/(2*sig^2))},bereich[1],bereich[2],add=TRUE,col="red",log=log)
  
}

printfitdata <- function(fit,title=""){ #--- Ausgabe der Gaußfit-Daten
  fitdata <- summary(fit)$parameters
  
  mu<-fitdata["mu","Estimate"]
  smu<-fitdata["mu","Std. Error"]
  sig<-fitdata["sig","Estimate"]
  ssig<-fitdata["sig","Std. Error"]
  
  cat(title)
  cat("\n")
  
  cat(" mu    = ")
  cat(mu)
  cat("+-")
  cat(smu)
  cat("\n")
  
  cat(" sigma = ")
  cat(sig)
  cat("+-")
  cat(ssig)
  cat("\n")

}

getresult <- function(fit){
  fitdata <- summary(fit)$parameters
  
  mu<-fitdata["mu","Estimate"]
  smu<-fitdata["mu","Std. Error"]
  sig<-fitdata["sig","Estimate"]
  ssig<-fitdata["sig","Std. Error"]
  
  return(roundfunc(c(mu,abs(sig/2))))
  
}

library("plotrix")

read <- function(filename){
  
  return (read.table(paste("../data/",filename,".TKA",sep="")))
  
}

draw <- function(x,y,add=FALSE,col="black",log=""){
  if(add){
    points(x,y,bty="l",cex=0.6,pch=4,col=col)
  } else {
  plot(x,y,bty="l",cex=0.6,pch=4,col=col,log=log)
  grid()
  }
  
}

drawCI <- function(x,y,sy,col="black",log="",ylim=c(10^-4,10^2),xlab="Kanal",ylab=expression(Zählrate / s^-1)){

  plotCI(x,y,ui=y+sy,li=((y-sy)*(y>sy)+0.000001*(y<=sy)),bty="l",cex=0.6,pch=4,col=col,log=log,ylim=ylim,sfrac=0.005,xlab=xlab,ylab=ylab,scol="darkgrey")
  grid()
  
}

drawCIx <- function(x,y,sx,sy,col="black",log="",ylim=c(10^-4,10^2),xlab="Kanal",ylab=expression(Zählrate / s^-1)){
  
  lowlim=y-sy
  if(log=="y"){
    lowlim=((y-sy)*(y>sy)+0.000001*(y<=sy))
  }
  plotCI(x,y,ui=y+sy,li=lowlim,bty="l",cex=0.6,pch=4,col=col,log=log,ylim=ylim,sfrac=0.005,xlab=xlab,ylab=ylab,scol="darkgrey")
  plotCI(x,y,uiw=sx,err="x",cex=0.6,pch=4,col=col,log=log,bty="l",ylim=ylim,sfrac=0.005,xlab=xlab,ylab=ylab,add=TRUE,scol="darkgrey")
  grid()
  
}



roundfunc <- function(vals){
  x=vals[1]
  xerr=vals[2]
  n=0
  for(i in -20:20){
    a=round(xerr,i)*10^i
    if(a==1){
      n=i+1
      return(c(round(x,n),round(xerr,n)))
    }
    if(a==2){
      if(xerr*10^i<1.95){
        n=i+1
      } else {
        n=i
      }
      return(c(round(x,n),round(xerr,n)))
    }
    if(a>2){
      n=i
      return(c(round(x,n),round(xerr,n)))
    }
  }
  return(vals)
  
}
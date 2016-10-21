library("plotrix")

read <- function(filename){
  
  return (read.table(paste("../data/",filename,".TKA",sep="")))
  
}

readTxt <- function(filename){
  
  return (read.table(paste("../data/",filename,".txt",sep=""),skip=10))
  
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

  plot(x,y,bty="l",cex=0.6,pch=4,col=col,log=log,ylim=ylim,xlab=xlab,ylab=ylab)
  condition=1#(y<=sy)
  lowlim=y-sy
  if(log=="y"){
    lowlim=((y-sy)*(y>sy)+0.000001*(y<=sy))
  }
  arrows(x,y*condition,x,lowlim*condition,cex=0.6,pch=4,bty="l",ylim=ylim,xlab=xlab,ylab=ylab,col="darkgrey",length=0.05,angle=90)
  arrows(x,y*condition,x,(y+sy)*condition,cex=0.6,pch=4,bty="l",ylim=ylim,xlab=xlab,ylab=ylab,col="darkgrey",length=0.05,angle=90)
  points(x,y,bty="l",cex=0.6,pch=4,col=col,log=log,ylim=ylim,xlab=xlab,ylab=ylab)
  grid()
  
}

drawCIx <- function(x,y,sx,sy,col="black",scol="darkgrey",barsize=0.05,vbarsize=0.005,log="",ylim=c(10^-4,10^2),xlab="Kanal",ylab=expression(Zählrate / s^-1)){
  condition=1#(y<=sy)
  lowlim=y-sy
  if(log=="y"){
    lowlim=((y-sy)*(y>sy)+0.000001*(y<=sy))
  }
  #lowlim=y
#  plotCI(x,y*(y>sy),ui=y+sy,li=lowlim,bty="l",cex=0.6,pch=4,col=col,log=log,ylim=ylim,sfrac=0.005,xlab=xlab,ylab=ylab,scol="darkgrey")
  plotCI(x,y*condition,uiw=sx,err="x",bty="l",cex=0.6,pch=4,col=col,log=log,ylim=ylim,sfrac=vbarsize,xlab=xlab,ylab=ylab,scol=scol)
  #plotCI(x,y*condition,uiw=sx,err="x",cex=0.6,pch=4,col=col,log=log,bty="l",ylim=ylim,sfrac=0.005,xlab=xlab,ylab=ylab,add=TRUE,scol="darkgrey")
  arrows(x,y*condition,x,lowlim*condition,cex=0.6,pch=4,bty="l",ylim=ylim,xlab=xlab,ylab=ylab,col=scol,length=barsize,angle=90)
  arrows(x,y*condition,x,(y+sy)*condition,cex=0.6,pch=4,bty="l",ylim=ylim,xlab=xlab,ylab=ylab,col=scol,length=barsize,angle=90)
  #points(x,(y-sy)*condition,cex=0.6,pch=4,log=log,bty="l",ylim=ylim,sfrac=0.005,xlab=xlab,ylab=ylab,col="green")
  #points(x,(lowlim),cex=0.6,pch=4,log=log,bty="l",ylim=ylim,sfrac=0.005,xlab=xlab,ylab=ylab,col="green")
  plotCI(x,y*condition,uiw=sx,err="x",bty="l",cex=0.6,pch=4,col=col,log=log,ylim=ylim,sfrac=vbarsize,xlab=xlab,ylab=ylab,scol=scol,add=TRUE)
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
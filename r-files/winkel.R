source("gausfit.R")
source("functions.R")

winkel=c(-90,-65,-40,-15,-12.5,-10,-7.5,-5,-2.5,0,2.5,5,7.5,10,12.5,15,40,65,90)
time=c(179797,179982,180061,179443,179961,179984,180067,180026,179953,180218,179838,179994,179951,180206,180119,180052,180040,180042,179988)
counts=c(188,193,180,265,264,277,528,1846,3350,4048,4202,3475,2306,665,275,247,216,199,180)
scounts=sqrt(counts)

drawCI(winkel,counts/time,scounts/time,ylim=c(0,0.025),xlab="Winkel / °")
fit=gaus2(data.frame(x=winkel,y=counts/time,sy=scounts/time),weighted=TRUE,sig0=10)
plotgaus2(fit,c(-90,90))
print(getresult(fit))
printfitdata(fit)
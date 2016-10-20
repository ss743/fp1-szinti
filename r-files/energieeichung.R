source("functions.R")
source("gausfit.R")
source("linearfit.R")

par(mar=c(5,5,1,1))

europium   = read("Europium")[[1]]
cobalt     = read("Cobalt")[[1]]
natrium    = read("Natrium")[[1]]

untergrund = read("Untergrund")[[1]]

thorium    = read("Thorium")[[1]]

teuropium=europium[2]
tcobalt=cobalt[2]
tnatrium=natrium[2]
tuntergrund=untergrund[2]
tthorium=thorium[2]

europium  [1:2] = c(0,0)
cobalt    [1:2] = c(0,0)
natrium   [1:2] = c(0,0)
untergrund[1:2] = c(0,0)
thorium   [1:2] = c(0,0)

seuropium   = sqrt(europium)
scobalt     = sqrt(cobalt)
snatrium    = sqrt(natrium)
suntergrund = sqrt(untergrund)
sthorium    = sqrt(thorium)


channels   = c(1:8192)

untergrund=untergrund/tuntergrund
suntergrund=suntergrund/tuntergrund

drawCI(channels,untergrund,suntergrund,log="y",xlab="Kanal",ylim=c(10^-5,10^0))

europium_c = europium/teuropium - untergrund
cobalt_c   = cobalt  /tcobalt   - untergrund
natrium_c  = natrium /tnatrium  - untergrund
thorium_c  = thorium /tthorium  - untergrund

seuropium_c = sqrt((seuropium/teuropium)^2+(suntergrund)^2)
scobalt_c   = sqrt((scobalt/tcobalt)^2    +(suntergrund)^2)
snatrium_c  = sqrt((snatrium/tnatrium)^2  +(suntergrund)^2)
sthorium_c  = sqrt((sthorium/tthorium)^2  +(suntergrund)^2)

na_fitgrenzen=array(c(1100,1300,2670,3100,3800,4200),dim=c(2,3))
co_fitgrenzen=array(c(2500,2750,2850,3200,5400,6000),dim=c(2,3))
eu_fitgrenzen=array(c(250,350,700,900,520,680,1700,1900,2100,2300,2350,2650,3000,3300,60,140,150,250),dim=c(2,9))
try({
  na_fit1=gausfit(data.frame(x=channels,y=natrium_c,sy=snatrium_c),na_fitgrenzen[,1],weighted=TRUE)
  na_fit2=gausfit(data.frame(x=channels,y=natrium_c,sy=snatrium_c),na_fitgrenzen[,2],weighted=TRUE)
  na_fit3=gausfit(data.frame(x=channels,y=natrium_c,sy=snatrium_c),na_fitgrenzen[,3],weighted=TRUE)
})
try({
  co_fit1=gausfit(data.frame(x=channels,y=cobalt_c,sy=scobalt_c),co_fitgrenzen[,1],weighted=TRUE)
  co_fit2=gausfit(data.frame(x=channels,y=cobalt_c,sy=scobalt_c),co_fitgrenzen[,2],weighted=TRUE)
  co_fit3=gausfit(data.frame(x=channels,y=cobalt_c,sy=scobalt_c),co_fitgrenzen[,3],weighted=TRUE)
})
try({
  eu_fit1=gausfit(data.frame(x=channels,y=europium_c,sy=seuropium_c),eu_fitgrenzen[,1],weighted=TRUE)
  eu_fit2=gausfit(data.frame(x=channels,y=europium_c,sy=seuropium_c),eu_fitgrenzen[,2],weighted=TRUE)
  eu_fit3=gausfit(data.frame(x=channels,y=europium_c,sy=seuropium_c),eu_fitgrenzen[,3],weighted=TRUE)
  eu_fit4=gausfit(data.frame(x=channels,y=europium_c,sy=seuropium_c),eu_fitgrenzen[,4],weighted=TRUE)
  eu_fit5=gausfit(data.frame(x=channels,y=europium_c,sy=seuropium_c),eu_fitgrenzen[,5],weighted=TRUE)
  eu_fit6=gausfit(data.frame(x=channels,y=europium_c,sy=seuropium_c),eu_fitgrenzen[,6],weighted=TRUE)
  eu_fit7=gausfit(data.frame(x=channels,y=europium_c,sy=seuropium_c),eu_fitgrenzen[,7],weighted=TRUE)
  eu_fit8=gausfit(data.frame(x=channels,y=europium_c,sy=seuropium_c),eu_fitgrenzen[,8],weighted=TRUE)
  eu_fit9=gausfit(data.frame(x=channels,y=europium_c,sy=seuropium_c),eu_fitgrenzen[,9],weighted=TRUE)
})


drawCI(channels,europium_c,seuropium_c,log="y")
plotgaus(eu_fit1,eu_fitgrenzen[,1])
plotgaus(eu_fit2,eu_fitgrenzen[,2])
plotgaus(eu_fit3,eu_fitgrenzen[,3])
plotgaus(eu_fit4,eu_fitgrenzen[,4])
plotgaus(eu_fit5,eu_fitgrenzen[,5])
plotgaus(eu_fit6,eu_fitgrenzen[,6])
plotgaus(eu_fit7,eu_fitgrenzen[,7])
plotgaus(eu_fit8,eu_fitgrenzen[,8],col="pink")
plotgaus(eu_fit9,eu_fitgrenzen[,9],col="pink")

drawCI(channels,cobalt_c,scobalt_c,log="y")
plotgaus(co_fit1,co_fitgrenzen[,1])
plotgaus(co_fit2,co_fitgrenzen[,2])
plotgaus(co_fit3,co_fitgrenzen[,3],col="pink")

drawCI(channels,natrium_c,snatrium_c,log="y")
plotgaus(na_fit1,na_fitgrenzen[,1])
plotgaus(na_fit2,na_fitgrenzen[,2])
plotgaus(na_fit3,na_fitgrenzen[,3],col="pink")


eu1=getresult(eu_fit1)
eu2=getresult(eu_fit2)
eu3=getresult(eu_fit3)
eu4=getresult(eu_fit4)
eu5=getresult(eu_fit5)
eu6=getresult(eu_fit6)
eu7=getresult(eu_fit7)
eu8=getresult(eu_fit8)
eu9=getresult(eu_fit9)
co1=getresult(co_fit1)
co2=getresult(co_fit2)
co3=getresult(co_fit3)
na1=getresult(na_fit1)
na2=getresult(na_fit2)
na3=getresult(na_fit3)

print(getresult(eu_fit1))
print(getresult(eu_fit2))
print(getresult(eu_fit3))
print(getresult(eu_fit4))
print(getresult(eu_fit5))
print(getresult(eu_fit6))
print(getresult(eu_fit7))
print(getresult(eu_fit8))
print(getresult(eu_fit9))

print(getresult(co_fit1))
print(getresult(co_fit2))
print(getresult(co_fit3))

print(getresult(na_fit1))
print(getresult(na_fit2))
print(getresult(na_fit3))

channel  = c(eu1[1],eu2[1],co1[1],co2[1],na1[1],na2[1])
schannel = c(eu1[2],eu2[2],co1[2],co2[2],na1[2],na2[2])
energy   = c(122,344,1173.2,1332.5,511,1274.6)

drawCI(energy,channel,schannel,ylim=c(min(channel),max(channel)),xlab="Energie / keV", ylab="Kanal")

lin=linearfit(data.frame(x=energy,y=channel,sy=schannel),weighted=TRUE)
plotlinear(lin,c(1,8192))
linres=getlinresults(lin)
print(linres)
#plotlindata(lin)


energyCh = 1/linres[1]*channels+linres[3]/linres[1]
senergyCh = sqrt((linres[2]*energyCh/linres[1])^2+(linres[4]/linres[1])^2)

drawCIx(energyCh,untergrund,senergyCh,suntergrund,log="y",xlab="Energie / keV",ylim=c(10^-5,10^0))
un_fitgrenzen=array(c(1400,1600,2500,2720),dim=c(2,2))

try({
  un_fit1=gaus2(data.frame(x=energyCh,y=untergrund,sy=suntergrund),un_fitgrenzen[,1],weighted=TRUE)
  plotgaus(un_fit1,un_fitgrenzen[,1])
  print(getresult(un_fit1))
})
try({
  un_fit2=gaus2(data.frame(x=energyCh,y=untergrund,sy=suntergrund),un_fitgrenzen[,2],weighted=TRUE)
  plotgaus(un_fit2,un_fitgrenzen[,2])
  print(getresult(un_fit2))
})


#drawCIx(energyCh,thorium_c,senergyCh,sthorium_c,log="y",xlab="Energie / keV",ylim=c(10^-5,10^0))
#drawCI(channels,thorium_c,sthorium_c,log="y",xlab="Energie / Kanäle",ylim=c(10^-5,10^0))

n=1024
#rawCI(channels[1:n],thorium_c[1:n],sthorium_c[1:n],xlab="Energie / Kanäle",ylim=c(10^-2,10^0))
drawCIx(energyCh[1:n],thorium_c[1:n],senergyCh[1:n],sthorium_c[1:n],log="y",xlab="Energie / keV",ylim=c(5*10^-2,10^0))

th_fitgrenzen=array(c(60,84,90,130,160,200,250,277,288,340,355,410,420,480,520,580,590,670,700,775,825,915,1400,1800,1900,2500,2520,2750),dim=c(2,14))

try({
  th_fit1=gaus2(data.frame(x=energyCh,y=thorium_c,sy=sthorium_c),th_fitgrenzen[,1],weighted=TRUE)
  plotgaus(th_fit1,th_fitgrenzen[,1])
  print(getresult(th_fit1))
})
try({
  th_fit2=gaus2(data.frame(x=energyCh,y=thorium_c,sy=sthorium_c),th_fitgrenzen[,2],weighted=TRUE)
  plotgaus(th_fit2,th_fitgrenzen[,2])
  print(getresult(th_fit2))
})
try({
  th_fit3=gaus2(data.frame(x=energyCh,y=thorium_c,sy=sthorium_c),th_fitgrenzen[,3],weighted=TRUE)
  plotgaus(th_fit3,th_fitgrenzen[,3])
  print(getresult(th_fit3))
})
try({
  th_fit4=gaus2(data.frame(x=energyCh,y=thorium_c,sy=sthorium_c),th_fitgrenzen[,4],weighted=TRUE)
  plotgaus(th_fit4,th_fitgrenzen[,4])
  print(getresult(th_fit4))
})
try({
  th_fit5=gaus2(data.frame(x=energyCh,y=thorium_c,sy=sthorium_c),th_fitgrenzen[,5],weighted=TRUE)
  plotgaus(th_fit5,th_fitgrenzen[,5])
  print(getresult(th_fit5))
})
try({
  th_fit6=gaus2(data.frame(x=energyCh,y=thorium_c,sy=sthorium_c),th_fitgrenzen[,6],weighted=TRUE)
  plotgaus(th_fit6,th_fitgrenzen[,6])
  print(getresult(th_fit6))
})
try({
  th_fit7=gaus2(data.frame(x=energyCh,y=thorium_c,sy=sthorium_c),th_fitgrenzen[,7],weighted=TRUE)
  plotgaus(th_fit7,th_fitgrenzen[,7])
  print(getresult(th_fit7))
})


m=2048

n=1024
#rawCI(channels[n:m],thorium_c[n:m],sthorium_c[n:m],xlab="Energie / Kanäle",ylim=c(10^-2,10^0))
drawCIx(energyCh[n:m],thorium_c[n:m],senergyCh[n:m],sthorium_c[n:m],log="y",xlab="Energie / keV",ylim=c(10^-2,10^-1))

try({
  th_fit8=gaus2(data.frame(x=energyCh,y=thorium_c,sy=sthorium_c),th_fitgrenzen[,8],weighted=TRUE)
  plotgaus(th_fit8,th_fitgrenzen[,8])
  print(getresult(th_fit8))
})

try({
  th_fit9=gaus2(data.frame(x=energyCh,y=thorium_c,sy=sthorium_c),th_fitgrenzen[,9],weighted=TRUE)
  plotgaus(th_fit9,th_fitgrenzen[,9])
  print(getresult(th_fit9))
})
try({
  th_fit10=gaus2(data.frame(x=energyCh,y=thorium_c,sy=sthorium_c),th_fitgrenzen[,10],weighted=TRUE)
  plotgaus(th_fit10,th_fitgrenzen[,10])
  print(getresult(th_fit10))
})
try({
  th_fit11=gaus2(data.frame(x=energyCh,y=thorium_c,sy=sthorium_c),th_fitgrenzen[,11],weighted=TRUE)
  plotgaus(th_fit11,th_fitgrenzen[,11])
  print(getresult(th_fit11))
})

#m=4096
n=8192
#rawCI(channels[m:n],thorium_c[m:n],sthorium_c[m:n],xlab="Energie / Kanäle",ylim=c(10^-2,10^0))
drawCIx(energyCh[m:n],thorium_c[m:n],senergyCh[m:n],sthorium_c[m:n],xlab="Energie / keV",ylim=c(10^-5,0.5*10^-2))

try({
  th_fit12=gaus2(data.frame(x=energyCh,y=thorium_c,sy=sthorium_c),th_fitgrenzen[,12],weighted=TRUE)
  plotgaus(th_fit12,th_fitgrenzen[,12])
  print(getresult(th_fit12))
})
try({
  th_fit13=gaus2(data.frame(x=energyCh,y=thorium_c,sy=sthorium_c),th_fitgrenzen[,13],weighted=TRUE)
  plotgaus(th_fit13,th_fitgrenzen[,13])
  print(getresult(th_fit13))
})
try({
  th_fit14=gaus2(data.frame(x=energyCh,y=thorium_c,sy=sthorium_c),th_fitgrenzen[,14],weighted=TRUE)
  plotgaus(th_fit14,th_fitgrenzen[,14])
  print(getresult(th_fit14))
})
m=1
n=8192
pre=""
suf=""

cat("\n\n")
starttableKanal()
printfitdata(co_fit1,paste(pre,getresult(co_fit1)[1],suf,sep=""))
printfitdata(co_fit2,paste(pre,getresult(co_fit2)[1],suf,sep=""))
printfitdata(co_fit3,paste(pre,getresult(co_fit3)[1],suf,sep=""))
endtable("Fitdaten der Cobaltpeaks","cobalttable")

cat("\n\n")
starttableKanal()
printfitdata(na_fit1,paste(pre,getresult(na_fit1)[1],suf,sep=""))
printfitdata(na_fit2,paste(pre,getresult(na_fit2)[1],suf,sep=""))
printfitdata(na_fit3,paste(pre,getresult(na_fit3)[1],suf,sep=""))
endtable("Fitdaten der Natriumpeaks","natriumtable")

cat("\n\n")
starttableKanal()
printfitdata(eu_fit8,paste(pre,getresult(eu_fit8)[1],suf,sep=""))
printfitdata(eu_fit9,paste(pre,getresult(eu_fit9)[1],suf,sep=""))
printfitdata(eu_fit1,paste(pre,getresult(eu_fit1)[1],suf,sep=""))
printfitdata(eu_fit3,paste(pre,getresult(eu_fit3)[1],suf,sep=""))
printfitdata(eu_fit2,paste(pre,getresult(eu_fit2)[1],suf,sep=""))
printfitdata(eu_fit4,paste(pre,getresult(eu_fit4)[1],suf,sep=""))
printfitdata(eu_fit5,paste(pre,getresult(eu_fit5)[1],suf,sep=""))
printfitdata(eu_fit6,paste(pre,getresult(eu_fit6)[1],suf,sep=""))
printfitdata(eu_fit7,paste(pre,getresult(eu_fit7)[1],suf,sep=""))
endtable("Fitdaten der Europiumpeaks","europiumtable")

cat("\n\n")
starttable()
printfitdata(th_fit1,paste(pre,getresult(th_fit1)[1],suf,sep=""))
printfitdata(th_fit2,paste(pre,getresult(th_fit2)[1],suf,sep=""))
printfitdata(th_fit3,paste(pre,getresult(th_fit3)[1],suf,sep=""))
printfitdata(th_fit4,paste(pre,getresult(th_fit4)[1],suf,sep=""))
printfitdata(th_fit5,paste(pre,getresult(th_fit5)[1],suf,sep=""))
printfitdata(th_fit6,paste(pre,getresult(th_fit6)[1],suf,sep=""))
printfitdata(th_fit7,paste(pre,getresult(th_fit7)[1],suf,sep=""))
printfitdata(th_fit8,paste(pre,getresult(th_fit8)[1],suf,sep=""))
printfitdata(th_fit9,paste(pre,getresult(th_fit9)[1],suf,sep=""))
printfitdata(th_fit10,paste(pre,getresult(th_fit10)[1],suf,sep=""))
printfitdata(th_fit11,paste(pre,getresult(th_fit11)[1],suf,sep=""))
printfitdata(th_fit12,paste(pre,getresult(th_fit12)[1],suf,sep=""))
printfitdata(th_fit13,paste(pre,getresult(th_fit13)[1],suf,sep=""))
printfitdata(th_fit14,paste(pre,getresult(th_fit14)[1],suf,sep=""))
endtable("Fitdaten der Thoriumpeaks","thoriumtable")

cat("\n\n")
starttable()
printfitdata(un_fit1,paste(pre,getresult(un_fit1)[1],suf,sep=""))
printfitdata(un_fit2,paste(pre,getresult(un_fit2)[1],suf,sep=""))
endtable("Fitdaten der Untergrundpeaks","untergrundtable")

drawCIx(energyCh[m:n],thorium_c[m:n],senergyCh[m:n],sthorium_c[m:n],log="y",xlab="Energie / keV",ylim=c(10^-5,10^0))
th_fit=c(getresult(th_fit1)[1],
         getresult(th_fit2)[1],
         getresult(th_fit3)[1],
         getresult(th_fit4)[1],
         getresult(th_fit5)[1],
         getresult(th_fit6)[1],
         getresult(th_fit7)[1],
         getresult(th_fit8)[1],
         getresult(th_fit9)[1],
         getresult(th_fit10)[1],
         getresult(th_fit11)[1],
         getresult(th_fit12)[1],
         getresult(th_fit13)[1],
         getresult(th_fit14)[1])
for(a in th_fit){
  abline(v=a,col="pink")
}

color="pink"

plotgaus(th_fit1,th_fitgrenzen[,1],col=color)
plotgaus(th_fit2,th_fitgrenzen[,2],col=color)
plotgaus(th_fit3,th_fitgrenzen[,3],col=color)
plotgaus(th_fit4,th_fitgrenzen[,4],col=color)
plotgaus(th_fit5,th_fitgrenzen[,5],col=color)
plotgaus(th_fit6,th_fitgrenzen[,6],col=color)
plotgaus(th_fit7,th_fitgrenzen[,7],col=color)
plotgaus(th_fit8,th_fitgrenzen[,8],col=color)
plotgaus(th_fit9,th_fitgrenzen[,9],col=color)
plotgaus(th_fit10,th_fitgrenzen[,10],col=color)
plotgaus(th_fit11,th_fitgrenzen[,11],col=color)
plotgaus(th_fit12,th_fitgrenzen[,12],col=color)
plotgaus(th_fit13,th_fitgrenzen[,13],col=color)
plotgaus(th_fit14,th_fitgrenzen[,14],col=color)


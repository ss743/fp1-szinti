source("functions.R")
source("gausfit.R")
source("linearfit.R")

par(mar=c(5,5,1,1))

natrium16    = read("Natrium_Scheibe_juengste")[[1]]
untergrund16 = read("Untergrund")[[1]]

tnatrium16 = natrium16[2]
tuntergrund16 = untergrund16[2]
#natrium16      [1:64] = rep(0,64)
#untergrund16   [1:64] = rep(0,64)

natrium15_3    = read("altemessungen/Natrium_Moritz")[[1]]
untergrund15_3 = read("altemessungen/Untergrund_Moritz")[[1]]

tnatrium15_3=natrium15_3[2]
tuntergrund15_3 = untergrund15_3[2]
#natrium15_3      [1:32] = rep(0,32)
#untergrund15_3   [1:32] = rep(0,32)

natrium14    = read("altemessungen/Natrium_Patrick")[[1]]
untergrund14 = read("altemessungen/Untergrund_Patrick")[[1]]

tnatrium14 = natrium14[2]
tuntergrund14 = untergrund14[2]
#natrium14      [1:16] = rep(0,16)
#untergrund14   [1:16] = rep(0,16)

natrium15_1    = readTxt("altemessungen/Natrium1")[[2]]
untergrund15_1 = readTxt("altemessungen/Untergrund1")[[2]]
tnatrium15_1   = 1573.71
tuntergrund15_1= 9785.77
#natrium15_1   [1:16] = rep(0,16)
#untergrund15_1[1:16] = rep(0,16)

natrium15_2    = readTxt("altemessungen/Natrium2")[[2]]
untergrund15_2 = readTxt("altemessungen/Untergrund2")[[2]]
tnatrium15_2   = 2124.64
tuntergrund15_2= 11770.50
#natrium15_2   [1:128] = rep(0,128)
#untergrund15_2[1:128] = rep(0,128)

c1=natrium14/tnatrium14-untergrund14/tuntergrund14
c2=natrium15_1/tnatrium15_1-untergrund15_1/tuntergrund15_1
c3=natrium15_2/tnatrium15_2-untergrund15_2/tuntergrund15_2
c4=natrium15_3/tnatrium15_3-untergrund15_3/tuntergrund15_3
c5=natrium16/tnatrium16-untergrund16/tuntergrund16

c1n=c1
u1n=untergrund14/tuntergrund14

c2n=c2
u2n=untergrund15_1/tuntergrund15_1

c3n=c(rep(0,2048))
u3n=c(rep(0,2048))
for(i in c(1:length(c3))){
  c3n[(i-1)%/%4+1]=c3n[(i-1)%/%4+1]+c3[i]
  u3n[(i-1)%/%4+1]=u3n[(i-1)%/%4+1]+(untergrund15_2/tuntergrund15_2)[i]
}

c4n=c(rep(0,2048))
u4n=c(rep(0,2048))
for(i in c(1:length(c4))){
  c4n[(i-1)%/%2+1]=c4n[(i-1)%/%2+1]+c4[i]
  u4n[(i-1)%/%2+1]=u4n[(i-1)%/%2+1]+(untergrund15_3/tuntergrund15_3)[i]
}

c5n=c(rep(0,2048))
u5n=c(rep(0,2048))
for(i in c(1:length(c5))){
  c5n[(i-1)%/%4+1]=c5n[(i-1)%/%4+1]+c5[i]
  u5n[(i-1)%/%4+1]=u5n[(i-1)%/%4+1]+(untergrund16/tuntergrund16)[i]
}

cutoff=32

c1n[1:cutoff]=rep(0,cutoff)
c2n[1:cutoff]=rep(0,cutoff)
c3n[1:cutoff]=rep(0,cutoff)
c4n[1:cutoff]=rep(0,cutoff)
c5n[1:cutoff]=rep(0,cutoff)

u1n[1:cutoff]=rep(0,cutoff)
u2n[1:cutoff]=rep(0,cutoff)
u3n[1:cutoff]=rep(0,cutoff)
u4n[1:cutoff]=rep(0,cutoff)
u5n[1:cutoff]=rep(0,cutoff)

c1n=c1n#/sum(u1n)
c2n=c2n#/sum(u2n)
c3n=c3n#/sum(u3n)
c4n=c4n#/sum(u4n)
c5n=c5n#/sum(u5n)

sc1n = sqrt((sqrt(natrium14)/tnatrium14)^2 + (sqrt(untergrund14)/tuntergrund14)^2)
sc2n = sqrt((sqrt(natrium15_1)/tnatrium15_1)^2 + (sqrt(untergrund15_1)/tuntergrund15_1)^2)
sc3n = sqrt((sqrt(natrium15_2)/tnatrium15_2)^2 + (sqrt(untergrund15_2)/tuntergrund15_2)^2)
sc4n = sqrt((sqrt(natrium15_3)/tnatrium15_3)^2 + (sqrt(untergrund15_3)/tuntergrund15_3)^2)
sc5n = sqrt((sqrt(natrium16)/tnatrium16)^2 + (sqrt(untergrund16)/tuntergrund16)^2)


activity=c(sum(c1n),
           sum(c2n),
           sum(c3n),
           sum(c4n),
           sum(c5n))
sactivity=sqrt(c(sum(sc1n^2),
            sum(sc2n^2),
            sum(sc3n^2),
            sum(sc4n^2),
            sum(sc5n^2)))
t=c(2014,2015,2015,2015,2016)

plotCI(t,activity,uiw=sactivity,pch=4,cex=0.6,bty="l",lwd=3,xlab="Jahr",ylab="Aktivit??t / Bq")
grid()

# plot(1:2048,natrium14/tnatrium14)
# points(1:2048,untergrund14/tuntergrund14,col="red")
# plot(1:2048,natrium15_1/tnatrium15_1)
# points(1:2048,untergrund15_1/tuntergrund15_1,col="red")
# plot(1:8192,natrium15_2/tnatrium15_2)
# points(1:8192,untergrund15_2/tuntergrund15_2,col="red")
# plot(1:4096,natrium15_3/tnatrium15_3)
# points(1:4096,untergrund15_3/tuntergrund15_3,col="red")
# plot(1:8192,natrium16/tnatrium16)
# points(1:8192,untergrund16/tuntergrund16,col="red")
# 
# plot(1:2048,natrium14/tnatrium14-untergrund14/tuntergrund14)
# plot(1:2048,natrium15_1/tnatrium15_1-untergrund15_1/tuntergrund15_1)
# plot(1:8192,natrium15_2/tnatrium15_2-untergrund15_2/tuntergrund15_2)
# plot(1:4096,natrium15_3/tnatrium15_3-untergrund15_3/tuntergrund15_3)
# plot(1:8192,natrium16/tnatrium16-untergrund16/tuntergrund16)
x=c(1:2048)

colors=c("black","blue","green","deeppink","red")
scolors=c("darkgrey","lightblue","lightgreen","pink","orange")

draw3(x,c5n,sc5n,ylim=c(min(c2n),max(c5n)),col=colors[1],scol=scolors[1])
draw4(x,c4n,sc4n,col=colors[2],scol=scolors[2])
draw4(x,c3n,sc3n,col=colors[3],scol=scolors[3])
draw4(x,c2n,sc2n,col=colors[4],scol=scolors[4])
draw4(x,c1n,sc1n,col=colors[5],scol=scolors[5])
grid()

labels=c("2016","2015-3","2015-2","2015-1","2014")
legend(1500,4,labels,fil=colors)

untergrund=untergrund16
natrium5=read("Natrium")[[1]]
natrium4=read("Natrium_Scheibe_juengste")[[1]]
natrium3=read("Natrium_Scheibe_zweitjuengste")[[1]]
natrium2=read("Natrium_Zylinder_zweitaelteste")[[1]]
natrium1=read("Natrium_Zylinder_aelteste")[[1]]
tuntergrund = untergrund[2]
tnatrium1 = natrium1[2]
tnatrium2 = natrium2[2]
tnatrium3 = natrium3[2]
tnatrium4 = natrium4[2]
tnatrium5 = natrium5[2]
untergrund  [1:2] = rep(0,2)
natrium1    [1:2] = rep(0,2)
natrium2    [1:2] = rep(0,2)
natrium3    [1:2] = rep(0,2)
natrium4    [1:2] = rep(0,2)
natrium5    [1:2] = rep(0,2)
cn1 = natrium1/tnatrium1 - untergrund/tuntergrund
cn2 = natrium2/tnatrium2 - untergrund/tuntergrund
cn3 = natrium3/tnatrium3 - untergrund/tuntergrund
cn4 = natrium4/tnatrium4 - untergrund/tuntergrund
cn5 = natrium5/tnatrium5 - untergrund/tuntergrund

scn1 = sqrt((sqrt(natrium1)/tnatrium1)^2 + (sqrt(untergrund)/tuntergrund)^2)
scn2 = sqrt((sqrt(natrium2)/tnatrium2)^2 + (sqrt(untergrund)/tuntergrund)^2)
scn3 = sqrt((sqrt(natrium3)/tnatrium3)^2 + (sqrt(untergrund)/tuntergrund)^2)
scn4 = sqrt((sqrt(natrium4)/tnatrium4)^2 + (sqrt(untergrund)/tuntergrund)^2)
scn5 = sqrt((sqrt(natrium5)/tnatrium5)^2 + (sqrt(untergrund)/tuntergrund)^2)



x=c(1,2,3,4,5)
y=c(sum(cn1),
    sum(cn2),
    sum(cn3),
    sum(cn4),
    sum(cn5))
sy=sqrt(c(1,#sum(scn1^2),
          sum(scn2^2),
          1,#sum(scn3^2),
          1,#sum(scn4^2),
          2500))#sum(scn5^2)))

plot(x,y,pch=4,cex=1,lwd=3,log="y",xlab="Probe Nr.",ylab="Aktivit??t / Bq",bty="l")
scol="black"
condition=1#(y<=sy)
lowlim=y-sy
#if(log=="y"){
lowlim=((y-sy)*(y>sy)+0.000001*(y<=sy))
#}
arrows(x,y*condition,x,lowlim*condition,cex=1,pch=4,bty="l",lwd=3,col=scol,length=0.1,angle=90)
arrows(x,y*condition,x,(y+sy)*condition,cex=1,pch=4,bty="l",lwd=3,col=scol,length=0.1,angle=90)
points(x,y,cex=1,pch=4,lwd=3)

grid()

x=c(1:8192)

colors=c("blue","black","green","deeppink","red")
scolors=c("lightblue","darkgrey","lightgreen","pink","orange")

draw1(x,cn3,scn3,ylim=c(0.000001,20),col=colors[3],scol=scolors[3],xlab="Energie / Kanal",ylab=expression(Z??hlrate / s^-1))
draw2(x,cn2,scn2,col=colors[2],scol=scolors[2])
draw2(x,cn4,scn4,col=colors[4],scol=scolors[4])
draw2(x,cn1,scn1,col=colors[1],scol=scolors[1])
draw2(x,cn5,scn5,col=colors[5],scol=scolors[5])
points(x,cn3,cex=0.6,pch=4,col=colors[3])
points(x,cn2,cex=0.6,pch=4,col=colors[2])
points(x,cn4,cex=0.6,pch=4,col=colors[4])
points(x,cn1,cex=0.6,pch=4,col=colors[1])
points(x,cn5,cex=0.6,pch=4,col=colors[5])

grid()
labels=c("Probe 1","Probe 2","Probe 3","Probe 4","Probe 5")
legend(6500,10,labels,fil=colors)

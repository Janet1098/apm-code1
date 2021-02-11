install.packages("jrvfinance")
library("jrvFinance")

CleanP=read.csv("/Users/mac/Desktop/book4.csv")
CleanP

Date = c("2021-01-18","2021-01-19","2021-01-20","2021-01-21","2021-01-22","2021-01-25","2021-01-26","2021-01-27","2021-01-28","2021-01-29")

for (i in c(1:10)) 
{
  for (j in c(1:10))
  {
    Yield[i,j]=bond.yields(Date[j],CleanP$Maturity.Date[i],CleanP$Coupon[i],freq=2,CleanP[i,j],convention = c("30/360","ACT/ACT","ACT/360","30/360E"),comp.freq=2,redemption_value = 100)
  }
}
Yield

DirtyP=read.csv("/Users/mac/Desktop/Book6.csv")
DirtyP

for (i in c(1:10))
{
  for (j in c(1:10))
  {
    p=DirtyP[i,j]
    c=DirtyP$Coupon[i]/2 
    PV2=0 
    FV=100
    for (k in c(1:length(c)))
      PV2=PV2+c*(1+Spots[k,j]/2)^(-2*c[k]) 
    PV2=0 
    p2=p-PV2
    Spots[i,j]=2*((p2/(c+FV))^(-1/(2*DirtyP$Maturity[1,3]))-1)
  }
}
Sports


for (i in c(1:10))
{
  for (j in c(1:10))
  {
    YieldsR[j,i]=approx(CleanP$Months.to.Maturity,Yield[[i]],xout=6*j,method = "linear",ties=mean)$y
  }
}
YieldsR

plot(seq(6,60,6),YieldsR$`s`,type="l",ylim=c(0.0005,0.005), col="red",xlab="Maturity by Months",ylab="Yield", main ="Yield Curve in 5 years")
lines(seq(6,60,6),YieldsR$`s.1`,col="yellow")
lines(seq(6,60,6),YieldsR$`s.2`,col="green")
lines(seq(6,60,6),YieldsR$`s.3`,col="blue")
lines(seq(6,60,6),YieldsR$`s.4`,col="black")
lines(seq(6,60,6),YieldsR$`s.5`,col="grey")
lines(seq(6,60,6),YieldsR$`s.6`,col="darkred")
lines(seq(6,60,6),YieldsR$`s.7`,col="orange")
lines(seq(6,60,6),YieldsR$`s.8`,col="purple")
lines(seq(6,60,6),YieldsR$`s.9`,col="darkblue")
legend("topleft",Date,lty=c(1,1), lwd=c(2,2),cex=.5, bty = "n", col=c("red","yellow","green","blue","black","grey","darkred","orange","purple","darkblue"))


for (i in c(1:10))
{
  for (j in c(1:10))
  {
    SpotsR[j,i]=approx(DirtyP$Months.to.Maturity,Spots[[i]],xout=6*j,method = "linear",ties=mean)$y}
}
SpotsR

plot(seq(6,60,6),SpotsR$`s`,type="l", ylim=c(-0.02,0.07), col="red",xlab="Maturity in months",ylab="Spots Rate", main =" Spots Curve in 5 years")
lines(seq(6,60,6),SpotsR$`s.1`,col="yellow")
lines(seq(6,60,6),SpotsR$`s.2`,col="green")
lines(seq(6,60,6),SpotsR$`s.3`,col="blue")
lines(seq(6,60,6),SpotsR$`s.4`,col="lightgreen")
lines(seq(6,60,6),SpotsR$`s.5`,col="grey")
lines(seq(6,60,6),SpotsR$`s.6`,col="darkred")
lines(seq(6,60,6),SpotsR$`s.7`,col="orange")
lines(seq(6,60,6),SpotsR$`s.8`,col="lightblue")
lines(seq(6,60,6),SpotsR$`s.9`,col="lightyellow")
legend("topright", Date,lty=c(1,1), lwd=c(2,2),cex=1, bty = "n",col=c("red","yellow","green","blue","lightgreen","grey","darkred","orange","lightblue","lightyellow"))


for (j in c(1:4))
{
  for (i in c(1:10))
  {
    yr=(1+SpotsR[2*j,i]/2)^(2*j)
    yr1_fwd=(1+SpotsR[2+2*j,i]/2)^(2+2*j)
    fwd[j,i]=2*((yr1_fwd/yr)^(1/2)-1)
  }
}

plot(seq(1,4),fwd$`s`,type="l",ylim=c(-0.05,0.016), col="blue",xlab="Following 4 years",ylab="Forward Rate", main ="Forward Curves")
lines(seq(1,4),fwd$`s.1`,col="green")
lines(seq(1,4),fwd$`s.2`,col="red")
lines(seq(1,4),fwd$`s.3`,col="blueviolet")
lines(seq(1,4),fwd$`s.4`,col="violet")
lines(seq(1,4),fwd$`s.5`,col="yellowgreen")
lines(seq(1,4),fwd$`s.6`,col="sienna")
lines(seq(1,4),fwd$`s.7`,col="powderblue")
lines(seq(1,4),fwd$`s.8`,col="gold")
lines(seq(1,4),fwd$`s.9`,col="orange")
legend("topleft",Dates,lty=c(1,1), lwd=c(2,2),cex=0.5, bty = "n", col=c("blue","green","red","blueviolet","violet","yellowgreen","sienna","powderblue","gold","orange"))


y1=y2=y3=y4=y5=vector("numeric",9)

for (i in c(1:10))
{
  y1[i]=log(YieldsR[2,i]/YieldsR[2,i+1])
  y2[i]=log(YieldsR[4,i]/YieldsR[4,i+1])
  y3[i]=log(YieldsR[6,i]/YieldsR[6,i+1])
  y4[i]=log(YieldsR[8,i]/YieldsR[8,i+1])
  y5[i]=log(YieldsR[10,i]/YieldsR[10,i+1])
}

yields=data.frame(y1,y2,y3,y4,y5)
covariance1=cov(yields,yields)
yields
covariance1

f11=f12=f13=f14=vector("numeric",9)

for(i in c(1:9))
{
  f11[i]=log(fwd[1,i]/fwd[1,i+1])
  f12[i]=log(fwd[2,i]/fwd[2,i+1])
  f13[i]=log(fwd[3,i]/fwd[3,i+1])
  f14[i]=log(fwd[4,i]/fwd[4,i+1])
}

fwds=data.frame(f11,f12,f13,f14)
covariance2=cov(fwds,fwds)
covariance2

eigen1=eigen(covariance1,symmetric=TRUE,only.values = FALSE)
eigen1
eigen2=eigen(covariance2,symmetric=TRUE,only.values = FALSE)
eigen2



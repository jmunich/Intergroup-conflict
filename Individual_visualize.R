par(mfcol=c(5,2))
a<-c(6,10)
b<-c(10,6)
for(k in 1:2){
props<-seq(.1,1,.2)


for(n in 1:length(props)){
rge<-seq(0,1,.00001)

alpha<-a[k]
beta<-b[k]
curve(dbeta(x,alpha,beta),lwd=2, xlab = "Contribution proportion belief", ylab = "Probability", xlim=c(0,1), main=paste("Beliefs at proportions alpha=", alpha, " and beta=", beta))

PropCa<-props[n]
xs<-seq(0,PropCa,.0001)
cory<-c(0,dbeta(xs,alpha,beta),0)
corx<-c(0,seq(0,PropCa,.0001),PropCa)
polygon(corx,cory,col='skyblue')

lines(c(PropCa,PropCa),c(dbeta(0,alpha,beta),max(dbeta(rge,alpha,beta))),lwd=2,col="red")

exx<-integrate(function(x){dbeta(x,alpha,beta)},lower=0,upper=PropCa)$value
ex<-function(x){x*dbeta(x,alpha,beta)/exx}

ep<-1-integrate(ex,lower=0,upper=PropCa)$value
et<-integrate(ex,lower=0,upper=PropCa)$value

Ua<-(pbeta(PropCa,alpha,beta)*(2-(PropCa)-(et)))+((1-pbeta(PropCa,alpha,beta))*(1-PropCa))
Ga<-(pbeta(PropCa,alpha,beta)*(1-(et)))

#lines(c(Ua,Ua),c(dbeta(0,alpha,beta),max(dbeta(rge,alpha,beta))),lwd=2,col="red", lty="dashed")
lines(c(ep,ep),c(dbeta(0,alpha,beta),max(dbeta(rge,alpha,beta))),lwd=2,col="green", lty="dotted")
lines(c(et,et),c(dbeta(0,alpha,beta),max(dbeta(rge,alpha,beta))),lwd=2,col="grey", lty="dotdash")
lines(c(Ga,Ga),c(dbeta(0,alpha,beta),max(dbeta(rge,alpha,beta))),lwd=2,col="red", lty="dashed")
}
}
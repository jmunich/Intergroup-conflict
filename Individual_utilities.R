belsa<-c(5,5,10)
belsb<-c(10,5,5)

for(o in 1:length(belsa)){
### Set the ratio of belief parameters here
al<-belsa[o]
bet<-belsb[o]
par(mfrow=c(4,2), new=FALSE)

for(j in 1:4){
### Alpha and beta are parameters of a beta distribution, indicating player's belief about 
### proportion of endowment E invested by the opponent. Ci shows possible contributions by
### the player.
alpha<-al*(j^3)
beta<-bet*(j^3)
E<-30
Ci<-c(seq(0,E,.1))

### Utility of the defender is a function of his contribution. His probability of winning 
### equals the cummulative probability of the beta distribution at point Ci (defender contribution).
### This indicates all the cases when expected contribution of the aggressor is bellow his contribution.


Ud<-pbeta(Ci/E,alpha,beta)*(E-(Ci))

### Utility of the defender contains the same probability of winning and also the probability of loss
### which is 1-CDF(Ci). In case of loss, the attacker expects to gain only what remains from his
### endowment. In case of win, he gains also the endowement of the defender, minus the contribution of the defender.
### Defender contribution is expressed as the expectation of the beta distribution bellow the aggressor contribution,
### that is, the integral of all possible contributions with which the defender loses, 
### multiplied by their respected probabilities.


expect<-c()
for(i in 1:length(Ci)){
exx<-integrate(function(x){dbeta(x,alpha,beta)},lower=0,upper=Ci[i]/E)$value
if(exx>0)
{
ex<-function(x){x*dbeta(x,alpha,beta)/exx}
expect[i]<-integrate(ex,lower=0,upper=Ci[i]/E)$value
}
if(exx==0){
expect[i]<-0  
}
}

Ua<-(pbeta(Ci/E,alpha,beta)*((2*E)-(Ci)-(expect*E)))+((1-pbeta(Ci/E,alpha,beta))*(E-(Ci)))

### The following plots show the utility of defender (left) and 
### aggressor (right) at varying levels of uncertainty.

plot(Ci, Ud, type="l", main=paste("alpha=",alpha,"beta=",beta), ylab="Utility Defender", xlab="Contribution Defender")
plot(Ci, Ua, type="l", main=paste("alpha=",alpha,"beta=",beta), ylab="Utility Aggressor", xlab="Contribution Aggressor")
}

  
### Group size can affect the uncertainty of expected contribution. When one persons decides on the proportion
### of endowement E to invest, one value is drawn from the beta distribution. If n people each decide on proportion
### of their E/n, then n values are drawn. Following the central limit theorem, the more people make the decision,
### the closer the sum of their contributions is to the central value.

par(mfrow=c(1,1))
cl<-c("blue","purple","red", "orange")
for(k in 1:4){
  alpha<-al*(k^3)
  beta<-bet*(k^3)
curve(dbeta(x,alpha,beta),col = cl[k], xlab = "Contribution proportion belief", ylab = "Probability", xlim=c(0,1), ylim=c(0,30), main=paste("Beliefs at proportions alpha=", al, " and beta=", bet))
par(new=TRUE)
}
}

alpha<-40
beta<-40
UFa<-function(x){
  exxx<-integrate(function(y){dbeta(y,alpha,beta)},lower=0,upper=x/E)$value
  if(exxx>0){
    exp<-function(z){z*dbeta(z,alpha,beta)/exxx}
    expectv<-integrate(exp,lower=0,upper=x/E)$value
  }
  if(exxx==0){
    expectv<-0  
  }
  (pbeta(x/E,alpha,beta)*((2*E)-(x)-(expectv*E)))+((1-pbeta(x/E,alpha,beta))*(E-(x)))
}
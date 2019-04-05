alpha_i<-2
beta_i<-2
alpha_o<-2
beta_o<-2
n<-3
E<-10
rounds<-12

######################

belief_i<-function(x){dbeta(x,alpha_i,beta_i)}
belief_o<-function(x){dbeta(x,alpha_o,beta_o)}

threshold<-function(pl,ing){(((n-1)/n)*ing)+(((n-1)/n)*pl)}
joint_distribution<-function(x,y){(belief_i(x)*belief_o(y))/4}

par(mfrow=c(1,2))

emdbook::curve3d(joint_distribution, from=c(0,0), to=c(1,1), xlab="In-group contribution", ylab="out-group contribution", zlab="likelihood", sys3d="contour")

par(mfrow=c(3,2))

curve(belief_i,lwd=2, xlab = "Contribution belief ingroup (Ag)", ylab ="Likelihood", xlim=c(0,1), main=paste("Beliefs at proportions alpha=", round(alpha_i,3), " and beta=", round(beta_i,3)))
curve(belief_o,lwd=2, xlab = "Contribution belief outgroup (Def)", ylab = "Likelihood", xlim=c(0,1), main=paste("Beliefs at proportions alpha=", round(alpha_o,3), " and beta=", round(beta_o,3)))

Pwin<-function(C){
  joint_distribution2<-function(x,y){belief_i(x)*belief_o(y)*(threshold(C,x)>y)}
  pracma::integral2(joint_distribution2, xmin=0, xmax=1, ymin=0, ymax=1)$`Q`
}

Ewin<-function(C){
  joint_distribution2<-function(x,y){belief_i(x)*belief_o(y)*(threshold(C,x)>y)}
  joint_distribution3<-function(x,y){belief_i(x)*belief_o(y)*y*(threshold(C,x)>y)}
  pracma::integral2(joint_distribution3, xmin=0, xmax=1, ymin=0, ymax=1)$`Q`/pracma::integral2(joint_distribution2, xmin=0, xmax=1, ymin=0, ymax=1)$`Q`
}

Ua<-function(x){(Pwin(x)*((2*E-(E*Ewin(x)))-(E*x)))+((1-Pwin(x))*(E*(1-x)))}

Ud<-function(x){Pwin(x)*(E-(E*x))}

Cagg<-(optimize(Ua,interval=c(0,1), maximum=TRUE)$`maximum`*E)
Cdef<-(optimize(Ud,interval=c(0,1), maximum=TRUE)$`maximum`*E)

alpha_o_s_ag<-alpha_o
beta_o_s_ag<-beta_o
alpha_i_s_ag<-alpha_i
beta_i_s_ag<-beta_i

alpha_i_s_def<-alpha_i
beta_i_s_def<-beta_i
alpha_o_s_def<-alpha_o
beta_o_s_def<-beta_o
############################################################################

Ag_cons<-c(Cagg,rep(0,(rounds-1)))
Def_cons<-c(Cdef,rep(0,(rounds-1)))

for(i in 1:(rounds-1)){

alpha_o_s_ag<-alpha_o_s_ag+(Cdef/E)
beta_o_s_ag<-beta_o_s_ag+((E-Cdef)/E)
alpha_i_s_ag<-alpha_i_s_ag+(Cagg/E)
beta_i_s_ag<-beta_i_s_ag+((E-Cagg)/E)

alpha_i_s_def<-alpha_i_s_def+(Cdef/E)
beta_i_s_def<-beta_i_s_def+((E-Cdef)/E)
alpha_o_s_def<-alpha_o_s_def+(Cagg/E)
beta_o_s_def<-beta_o_s_def+((E-Cagg)/E)


belief_i<-function(x){dbeta(x,alpha_i_s_ag,beta_i_s_ag)}
belief_o<-function(x){dbeta(x,alpha_o_s_ag,beta_o_s_ag)}

curve(belief_i,lwd=2, xlab = "Contribution belief ingroup (Ag)", ylab = "Likelihood", xlim=c(0,1), main=paste("Beliefs at proportions alpha=", round(alpha_i_s_ag,3), " and beta=", round(beta_i_s_ag,3)))
curve(belief_o,lwd=2, xlab = "Contribution belief outgroup (Def)", ylab = "Likelihood", xlim=c(0,1), main=paste("Beliefs at proportions alpha=", round(alpha_o_s_ag,3), " and beta=", round(beta_o_s_ag,3)))

threshold<-function(pl,ing){(((n-1)/n)*ing)+(((n-1)/n)*pl)}
joint_distribution<-function(x,y){(belief_i(x)*belief_o(y))/4}

Pwin<-function(C){
joint_distribution2<-function(x,y){belief_i(x)*belief_o(y)*(threshold(C,x)>y)}
pracma::integral2(joint_distribution2, xmin=0, xmax=1, ymin=0, ymax=1)$`Q`
}

Ewin<-function(C){
joint_distribution2<-function(x,y){belief_i(x)*belief_o(y)*(threshold(C,x)>y)}
joint_distribution3<-function(x,y){belief_i(x)*belief_o(y)*y*(threshold(C,x)>y)}
pracma::integral2(joint_distribution3, xmin=0, xmax=1, ymin=0, ymax=1)$`Q`/pracma::integral2(joint_distribution2, xmin=0, xmax=1, ymin=0, ymax=1)$`Q`
}

Ua<-function(x){(Pwin(x)*((2*E-(E*Ewin(x)))-(E*x)))+((1-Pwin(x))*(E*(1-x)))}

#####################################

belief_i<-function(x){dbeta(x,alpha_i_s_def,beta_i_s_def)}
belief_o<-function(x){dbeta(x,alpha_o_s_def,beta_o_s_def)}

threshold<-function(pl,ing){(((n-1)/n)*ing)+(((n-1)/n)*pl)}
joint_distribution<-function(x,y){(belief_i(x)*belief_o(y))/4}


Pwin<-function(C){
joint_distribution2<-function(x,y){belief_i(x)*belief_o(y)*(threshold(C,x)>y)}
pracma::integral2(joint_distribution2, xmin=0, xmax=1, ymin=0, ymax=1)$`Q`
}

Ewin<-function(C){
joint_distribution2<-function(x,y){belief_i(x)*belief_o(y)*(threshold(C,x)>y)}
joint_distribution3<-function(x,y){belief_i(x)*belief_o(y)*y*(threshold(C,x)>y)}
pracma::integral2(joint_distribution3, xmin=0, xmax=1, ymin=0, ymax=1)$`Q`/pracma::integral2(joint_distribution2, xmin=0, xmax=1, ymin=0, ymax=1)$`Q`
}

Ud<-function(x){Pwin(x)*(E-(E*x))}

Cagg<-optimize(Ua,interval=c(0,1), maximum=TRUE)$`maximum`*E
Cdef<-optimize(Ud,interval=c(0,1), maximum=TRUE)$`maximum`*E

Ag_cons[i+1]<-Cagg
Def_cons[i+1]<-Cdef
}

par(mfrow=c(1,2))
plot(c(1:rounds), Ag_cons,main="Aggressors", xlab="Round", ylab="Contributions")
lines(c(1:rounds), Ag_cons)

plot(c(1:rounds), Def_cons,main="Defenders", xlab="Round", ylab="Contributions")
lines(c(1:rounds), Def_cons)

par(mfrow=c(1,1))
plot(c(1:rounds), Ag_cons,main=paste("Red=Aggressors, Blue=Defenders, Endowment=",E,", players=",n), xlab="Round", ylab="Contributions",ylim=c(0,max(c(Ag_cons,Def_cons))),col="red",pch=24, bg="red")
#plot(c(1:rounds), Ag_cons,main="Red=Aggressors, Blue=Defenders", xlab="Round", ylab="Contributions",ylim=c(0,E),col="red",pch=24, bg="red")
points(c(1:rounds), Def_cons, col="blue",pch=25, bg="blue")
lines(c(1:rounds), Ag_cons, col="red")
lines(c(1:rounds), Def_cons, col="blue")


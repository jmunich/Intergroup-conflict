alpha_i<-8
beta_i<-4
alpha_o<-8
beta_o<-4
n<-6
E<-10
C<-.6

belief_i<-function(x){dbeta(x,alpha_i,beta_i)}
belief_o<-function(x){dbeta(x,alpha_o,beta_o)}

threshold<-function(pl,ing){(((n-1)/n)*ing)+(((n-1)/n)*pl)}
joint_distribution<-function(x,y){(belief_i(x)*belief_o(y))/4}

#emdbook::curve3d(joint_distribution, from=c(0,0), to=c(1,1), sys3d="contour")

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

Uas<-c()
a<-c(1:100)/100
for(i in 1:length(a)){
  Uas[i]<-Ua(a[i])
}

Uds<-c()
a<-c(1:100)/100
for(i in 1:length(a)){
  Uds[i]<-Ud(a[i])
}

plot(a, Uas, type='l', xlab="Contribution proportion", ylab="Utility", main=paste("Aggressor \n alpha_i=",alpha_i,"beta_i=",beta_i,"\n alpha_o=", alpha_o, "beta_o=", beta_o))
plot(a, Uds, type='l', xlab="Contribution proportion", ylab="Utility", main=paste("Defender \n alpha_i=",alpha_i,"beta_i=",beta_i,"\n alpha_o=", alpha_o, "beta_o=", beta_o))



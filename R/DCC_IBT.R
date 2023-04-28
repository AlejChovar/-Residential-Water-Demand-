## ------------------ ##

# Title: Estimating Residential Water Demand  under Systematic Shifts between Uniform Price (UP) and Increasing blokc Tariffs (IBT)
# Dataset: Residential water consumption at household level with demographics variables at census district level.
# Date: 24 abril 2023
# Author: A M. Chovar Vera, F.A. Vasqu??z-Lav??n, and R.Ponce Oliva
# email: achovarv@bcentral.cl and fvasquez@udd.cl

# This script is able to reproduce any IBT estimation for summer months of the paper. 

## ------------------ ##

# define the log function
ln_f <-function(x){
  y <- log((x<1e-307)*1e-307 + (x>=1e-307)*x) 
  return(y)
}

# define the exponential function 
exp_f <-function(x){
  y <- exp((x< -700)*-700 + (abs(x)<=700)*x + (x>=700)*700)
  return(y)
}

func<-function(beta,data){
  alpha<-beta[17]
  mu<-beta[18]
  s_n<-abs(beta[19])
  s_e<-abs(beta[20])
  
  s_ne<-sqrt(s_n^(2)+s_e^(2))
  
  rho<-s_n/s_ne
  
  rhs<-(beta[1] + beta[2]*pp_  +beta[3]*tem + beta[4]*media_nt + beta[5]*terremoto + beta[6]*com_2 + beta[7]*com_3 + beta[8]*com_4 + beta[9]*com_5 + beta[10]*com_6 + beta[11]*com_7 + beta[12]*com_8 + beta[13]*com_9 + beta[14]*com_10 + beta[15]*com_11 + beta[16]*com_1)
  
  t1<-(lnw1-rhs-alpha*logp1-mu*y1_1)/s_n
  t2<-(lnw1-rhs-alpha*logp2-mu*y2_1)/s_n
  
  s1<-(logx1-rhs-alpha*logp1-mu*y1_1)/s_ne  
  s2<-(logx1-rhs-alpha*logp2-mu*y2_1)/s_ne
  
  u1<-(logx1-lnw1)/s_e
  
  m1<-(lnw1-rhs-alpha*logp2-mu*y2_1)/s_n
  
  r1<-(t1-rho*s1)/sqrt(1-rho^2)
  r2<-(t2-rho*s2)/sqrt(1-rho^2)
  
  n2 <- (m1-rho*s2)/sqrt(1-rho^2)
  
  #Log-likelihood
  logl<-sum(ln_f((exp_f(-(s1^2/2))/(s_ne))*pnorm(r1) +
                   (exp_f(-(s2^2/2))/(s_ne))*(pnorm(r2)-pnorm(n2)) +
                   (exp_f(-(u1^2/2))/(s_e))*(pnorm(m1)-pnorm(t1))))
  
  
  return(-logl)
} 

startv <-c(const=1,pp=0,tem=0,nt=0,terremoto=0,com_2=0,com_3=0,com_4=0,com_5=0,com_6=0,com_7=0,com_8=0,com_9=0,com_10=0,com_11=0,com_1=0,price=-0.05,income=0.05,s_n=0.1,s_e=0.1) 
mod_dcc <- optim(startv,func,method="BFGS",hessian=TRUE,dat=data,control=list(maxit=1000))
mod_dcc
MFisher<-solve(mod_dcc$hessian)

## Standar Error
se <-sqrt(diag(MFisher))
se

#t-statistic
t<-mod_dcc$par/se
t

# p-value
pval<-2*(1-pt(abs(t),nrow(data)-length(startv)))
pval

# results table
mod_dcc$par
betas<-matrix(mod_dcc$par)
results<-cbind(betas,se,t,pval)
colnames(results)<-c("beta","se","t","p-value")
print(results,digits=3)

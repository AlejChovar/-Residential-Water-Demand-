## ------------------ ##

# Title: Estimating Residential Water Demand  under Systematic Shifts between Uniform Price (UP) and Increasing blokc Tariffs (IBT)
# Dataset: Residential water consumption at household level with demographics variables at census district level.
# Date: 24 abril 2023
# Author: A M. Chovar Vera, F.A. Vasqu??z-Lav??n, and R.Ponce Oliva
# email: achovarv@bcentral.cl and fvasquez@udd.cl

# This script is able to reproduce the DCC model when all the database is used, that is, observations from summer and non-summer months.

## ------------------ ##

##### Create the log-likelihood for the DCC model
# this function estimate the full DCC 
func<-function(beta,data){ 
  alpha<-beta[17]
  mu<-beta[18]
  s_n<-abs(beta[19])
  s_e<-abs(beta[20])
  
  s_ne<-sqrt(s_n^(2)+s_e^(2))
  
  rho<-s_n/s_ne
  rhs<-(beta[1] + beta[2]*pp_  +beta[3]*tem + beta[4]*media_nt + beta[5]*terremoto + beta[6]*com_2 + beta[7]*com_3 + beta[8]*com_4 + beta[9]*com_5 + beta[10]*com_6 + beta[11]*com_7 + beta[12]*com_8 + beta[13]*com_9 + beta[14]*com_10 + beta[15]*com_11 + beta[16]*com_1)

  t1<-(lnw1-rhs- alpha*logp1 - mu*y1_1)/s_n
  t2<-(lnw1-rhs- alpha*logp2 - mu*y2_1)/s_n
  
  s1<-(logx1-rhs-alpha*logp1 - mu*y1_1)/s_ne  
  s2<-(logx1-rhs-alpha*logp2 - mu*y2_1)/s_ne
  
  s<-(logx1-rhs-alpha*logp - mu*y)/s_ne 
  
  u1<-(logx1-lnw1)/s_e
  
  r1<-(t1-rho*s1)/sqrt(1-rho^2)
  r2<-(t2-rho*s2)/sqrt(1-rho^2)
  
  #Log-likelihood
  sum( log((1/s_ne)*dnorm(s1))*(1-tariff_str) +
         log((1/s_ne)*dnorm(s1)*pnorm(r1) +
               (1/s_ne)*dnorm(s2)*(1-pnorm(r2)) +
               (1/s_e)*dnorm(u1)*(pnorm(t2)-pnorm(t1)))*tariff_str
  )

} 

gr<-Deriv(func,c(beta="1",beta="2",beta="3",beta="4",beta="5",beta="6",beta="7",beta="8",beta="9",beta="10",beta="11",beta="12",beta="13",beta="14",beta="15",beta="16",beta="17",beta="18",beta="19",beta="20"))

#VECTOR OF INITIAL VALUES FOR MLE
startv <-c(const=1,pp=0,tem=0,nt=0,terremoto=0,com_2=0,com_3=0,com_4=0,com_5=0,com_6=0,com_7=0,com_8=0,com_9=0,com_10=0,com_11=0,com_1=0,price=-0.05,income=0.05,s_n=0.9,s_e=0.9) 

#evaluating the function:
func(startv)
gr(startv)
# optimization startup time
time1<-Sys.time()
#using MAXLIK
mod_dcc<-maxLik(logLik= func,grad=gr,start=startv,data=data,method="BFGS",control=list(printLevel=1,iterlim=1000))
# optimization end time
time2<-Sys.time()
#duration of the optimization process
difftime(time2,time1)
#Results 
summary(mod_dcc)

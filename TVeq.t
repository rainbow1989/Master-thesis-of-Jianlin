TVeq.t=function(theta,data){
  u=data[,1]
  v=data[,2]
  t=dim(data)[1]
  tau = rep(1,t)
  psi=rep(1,t)
  tt = cor(data,method="kendall")
  psi[1]= tt[1,2]
  for (i in 2:t){
    if(i<=10){
      psi[i]= (1-exp(-(theta[1] + theta[2]*psi[i-1]+theta[3]*mean(qt(u[1:i-1],df=theta[4])*qt(v[1:i-1],df=theta[4])))))*(1+exp(-(theta[1] +theta[2]*psi[i-1]+theta[3]*mean(qt(u[1:i-1],df=theta[4])*qt(v[1:i-1],df=theta[4])))))^(-1)
    }
    else{
      
      psi[i]= (1-exp(-(theta[1] + theta[2]*psi[i-1]+theta[3]*mean(qt(u[(i-10):i-1],df=theta[4])*qt(v[(i-10):i-1],df=theta[4])))))*(1+exp(-(theta[1] +theta[2]*psi[i-1]+theta[3]*mean(qt(u[(i-10):i-1],df=theta[4])*qt(v[(i-10):i-1],df=theta[4])))))^(-1)
    }
    if(is.nan(psi[i])==1){
      psi[i]=-0.9999999
    }else if(psi[i]>0.9999999){
      psi[i]=0.9999999              ####to avoid reach the bound and cause error
    }else if(psi[i]<(-0.9999999)) {
      psi[i]=-0.9999999
    }else{
      psi[i]=psi[i]
    }
    
  }
  return(psi)
}

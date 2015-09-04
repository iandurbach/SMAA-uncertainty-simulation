# Function to apply MAUT model to get ranking of alternatives

maut<-function(gdata,nI,J,K,ta,la,u_c){
  
  u=array(rep(0,nI*J*K),c(nI,J,K))  
  wutil=array(rep(0,nI*J*K),c(nI,J,K))
  U_k=array(rep(0,nI*J),c(nI,J))
  
  w=sort(wts2(J),decreasing=T)
  
  for(j in 1:J)
  {
    cutoffs = c(0,ta[j],1)
    values = c(0,la[j],1)
    xt = as.vector(gdata[,j,])
    xt_pvf = smaa.pvf(xt,cutoffs=cutoffs,values=values,outOfBounds="clip")
    u[,j,] = matrix(xt_pvf,nI,K)
    wutil[,j,] = u[,j,] * w[j]
  }
  
  for(m in 1:nI)
  {
    U_k[m,] <- apply(wutil[m,,],1,sum)
  }
  value_f <- apply(U_k,1,sum)/K
  
  if(u_c > 0){
    utility = -exp(-u_c * value_f)
  } else if (u_c < 0){
    utility = exp(-u_c * value_f)
  } else { utility = value_f}

  list(utility=utility,w=w)
}
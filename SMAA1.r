# SMAA-1: uncertainty represented by the theoretical probability distributions used to 
# generate the "true" observed values. See SMAA-Boot.r for more details.

SMAA1<-function(nI,J,runs,input,dstbn,ta,la)
  {
  
  x=array(rep(0,nI*J*runs),c(runs,nI,J))
  # Generate array of criterion values (Normal SMAA)  
  if (dstbn=="normal"){
    for (i in 1:nI){
      for (j in 1:J){
        x[,i,j]=rnorm(runs,input[i,j,4],input[i,j,5])
      }
    }
  }
  if (dstbn=="skew"){
    for (i in 1:nI){
      for (j in 1:J){
        x[,i,j]=input[i,j,4]+rgamma(runs,shape=input[i,j,5],scale=input[i,j,6])-input[i,j,5]*input[i,j,6]
      }
    }
  }
  
  # Standardize data by column (criteria) across all generated values
  for (j in 1:J){
    x[,,j]=(x[,,j]-min(x[,,j]))/(max(x[,,j])-min(x[,,j]))
  }
  
  # Generate matrix of weights (inter-criteria importance)
  w=wts(nC, runs)
  
  # Compute utilities
  u=array(rep(0,runs*nI*J),c(runs,nI,J))  
  for(j in 1:J)
  {
    cutoffs = c(0,ta[j],1)
    values = c(0,la[j],1)
    xt = as.vector(x[,,j])
    xt_pvf = smaa.pvf(xt,cutoffs=cutoffs,values=values,outOfBounds="clip")
    u[,,j] = matrix(xt_pvf,runs,nI)
  }
  
  # Run SMAA
  b = smaa(u,w)$ra
  b = unclass(b)
  b

}
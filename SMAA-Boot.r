# SMAA-Boot: uncertainty represented by the empirical probability distributions i.e.
# precisely the same probability distribution used in the MAUT results. Note that MAUT uses
# discrete draws from a continuous distribution. Thus, the discrete dbn may not match the 
# theoretical dbn exactly. SMAA-Boot uses the empirical discrete dbn, SMAA1.r uses the 
# theoretical dbn.

SMAABoot<-function(nI,J,runs,input,dstbn,ta,la)
{
  
  x=array(rep(0,nI*J*runs),c(runs,nI,J))
  # Generate bootstrap array of criterion values  
  for (i in 1:nI){
    for (j in 1:J){
      x[,i,j]=sample(input[i,j,],runs,replace=TRUE)
    }
  }
  
  # Generate matrix of weights (inter-criteria importance)
  w=wts(nC,runs)
  
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
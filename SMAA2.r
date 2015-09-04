# SMAA-u2: uncertainty represented by expected values (i.e. ignored)

SMAA2 <- function(nI,J,runs,input,dstbn,ta,la)
{
  
  # Assign input to x (Deterministic SMAA)
  x=input
  x=as.matrix(x)
  
  # Generate matrix of weights (inter-criteria importance)
  w=wts(nC, runs)
  
  # Compute utilities
  u=array(rep(0,runs*nI*J),c(runs,nI,J)) 
  ut=array(rep(0,nI*J),c(nI,J))  
  for(j in 1:J)
  {
    cutoffs = c(0,ta[j],1)
    values = c(0,la[j],1)
    xt = as.vector(x[,j])
    xt_pvf = smaa.pvf(xt,cutoffs=cutoffs,values=values,outOfBounds="clip")
    ut[,j] = xt_pvf
  }
  for(i in 1:runs){u[i,,] = ut}
  
  # Run SMAA
  b = smaa(u,w)$ra
  b = unclass(b)
  b
  
}
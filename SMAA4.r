# SMAA-u4: Uncertainty represented by 3 quantiles, with fixed quantile weights 
# (Keefer-Bodily)

SMAA4<-function(nI,J,runs,input,dstbn,ta,la){
  
  # Assign input to x (Deterministic SMAA)
  x1=input$v1
  x2=input$v2
  x3=input$v3
  x=array(rep(0,3*nI*J),c(3,nI,J)) 
  x[1,,] = x1
  x[2,,] = x2
  x[3,,] = x3
  
  # Generate matrix of weights (inter-criteria importance)
  w=wts(nC, runs)
  
  # Compute utilities
  ut=array(rep(0,3*nI*J),c(3,nI,J)) 
  u=array(rep(0,runs*nI*J),c(runs,nI,J)) 
  for(j in 1:J)
  {
    cutoffs = c(0,ta[j],1)
    values = c(0,la[j],1)
    xt = as.vector(x[,,j])
    xt_pvf = smaa.pvf(xt,cutoffs=cutoffs,values=values,outOfBounds="clip")
    ut[,,j] = matrix(xt_pvf,3,nI)
  }
  ut = 0.185*ut[1,,] + 0.63*ut[2,,] + 0.185*ut[3,,]
  for(i in 1:runs){u[i,,] = ut}
  
  # Run SMAA
  b = smaa(u,w)$ra
  b = unclass(b)
  b
}
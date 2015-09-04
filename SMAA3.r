# SMAA-u3: uncertainty represented by expected values and variances

SMAA3<-function(nI,j,runs,input,dstbn,ta,la,riskp){
  
  # Assign input to x (Deterministic SMAA)
  x1=input$v1
  x2=input$v2
  
  x1=as.matrix(x1)
  x2=as.matrix(x2)
  
  # Generate matrix of weights (inter-criteria importance)
  w=wts(nC, runs)
  
  # Compute utilities
  u=array(rep(0,runs*nI*J),c(runs,nI,J)) 
  ut=array(rep(0,nI*J),c(nI,J))  
  temp_x = c()
  for(j in 1:J)
  {
    cutoffs = c(0,ta[j],1)
    values = c(0,la[j],1)
    xt = as.vector(x1[,j])
    xt_pvf = smaa.pvf(xt,cutoffs=cutoffs,values=values,outOfBounds="clip")
    ut[,j] = xt_pvf
    
    utildif=max(ut[,j])-min(ut[,j])
    vardif=max(x2[,j])-min(x2[,j])
    temp_x[j]=(1/riskp)*utildif/vardif
  }
  ut = ut - t(t(x2)*temp_x)
  for(i in 1:runs){u[i,,] = ut}
  
  # Run SMAA
  b = smaa(u,w)$ra
  b = unclass(b)
  b
  
}
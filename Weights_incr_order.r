# Weights Function - generates random weight vector with weights that sum to 1

wts<-function(nC,runs)
{
  nJ = length(tw)
  
  ## constraints for weight restrictions
  # w > 0
  lower.w = rep(0,nJ)
  
  At = matrix(0,nrow=nJ,ncol=nJ)
  diag(At) = 1
  A = rbind(-At)
  
  d = rep("<=", nJ)
  userConstr = list(constr=A, rhs=-lower.w, dir=d)
  
  # ordinal
  if(nC>0){
    c1 = ordinalConstraint(nJ, 2, 1)
    for(j2 in 3:nJ){
      c2 = ordinalConstraint(nJ, j2, 1)
      c1 = mergeConstraints(c1,c2)  
    }
    
    if(nC>1){
      for(j1 in 2:nC){   
        for(j2 in (j1+1):nJ){
          c2 = ordinalConstraint(nJ, j2, j1)
          c1 = mergeConstraints(c1,c2)
        }
      }
    }
    
    c1 = eliminateRedundant(c1)    
    
    userConstr = mergeConstraints(userConstr,c1)
    
  }
  
  transform = simplex.createTransform(nJ)
  constr = simplex.createConstraints(transform, userConstr)
  seedPoint = createSeedPoint(constr, homogeneous=TRUE)
  samples = har(seedPoint, constr, N=runs*thin.f, thin=thin.f, homogeneous=TRUE, transform=transform)$samples
  return(samples)
}

# weights standardized to sum to 1
wts2<-function(num){
  q=runif(num+1,0,1)
  q[1]=0
  q[num+1]=1
  q=sort(q)
  w=rep(0,num)
  for (i in 1:num){	w[i]=q[i+1]-q[i] }
  w
}

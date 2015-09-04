# Function to apply errors to summaries

applyerror<-function(gsum,nI,J,e){
  esum=array(rep(0,nI*J*11),c(nI,J,11))
  x<-runif(nI*J*11,-e,e)
  dim(x)<-c(nI,J,11)
  esum<-(1+x)*gsum
  esum
}
# Function for generating data and calculating initial summaries

gendata<-function(nI,J,K,nd,dstbn,par1){
    
    nondom=nd
    
    # Lower and upper limits for mean, std. dev. and skewness 
    # mean
    ml=0.2
    mu=0.8
    # low st. dev.
    lvl=0.02
    lvu=0.04
    # high st. dev.
    hvl=0.08
    hvu=0.1
    # positive skewness
    lskew=0.5
    uskew=1
  
    # nInitialize arrays
    # Generated Data
    gdata=array(rep(0,nI*J*K),c(nI,J,K))
    # Standardized (generated) data
    stdata=array(rep(0,nI*J*K),c(nI,J,K))
    # Summary of generated data
    gsum=array(rep(0,nI*J*11),c(nI,J,11))
    
    # Generate means from uniform distribution
    gsum[,,1]=runif(nI*J,ml,mu)
    
    # For non-dominated alternatives, standardize criteria means with sum of squares equal to 1
    if (nondom=="T"){ 
      gsum_t = gsum[,,1]^2
      gsum[,,1]=sqrt(gsum_t/apply(gsum_t,1,sum))
    }
    
    # Generate low or high variances from uniform distribution
    if (par1=="lowvar"){gsum[,,2]=runif(nI*J,lvl,lvu)}else{gsum[,,2]=runif(nI*J,hvl,hvu)}
    
  if (dstbn=="normal"){
        # Generate data from normal distribution
        gdata<-rnorm(nI*J*K,gsum[,,1],gsum[,,2])
        dim(gdata)<-c(nI,J,K)
  } else if (dstbn=="skew"){  
    # Generate skewness
    gsum[,,3]=runif(nI*J,lskew,uskew)
  
    for (i in 1:nI){
      for (j in 1:J){
        # Gamma shape parameter
        gshape=(2/gsum[i,j,3])^2
        # Gamma scale parameter
        gscale=sqrt((gsum[i,j,2])^2/gshape)
        # Generate data from Gamma distbn [mean + ~G(0,gshape,gscale)] 
        gdata[i,j,]=gsum[i,j,1]+rgamma(K,shape=gshape,scale=gscale)-gshape*gscale
      }
    }
  }
    
  for (j in 1:J){
    # Standardize all data values on each criterion to lie between 0 and 1
    stdata[,j,]=(gdata[,j,]-min(gdata[,j,]))/(max(gdata[,j,])-min(gdata[,j,]))
  }
    
  # Populate gsum for generated data from normal distribution
  if (dstbn=="normal"){
    for (m in 1:nI){
      for (n in 1:J){
        gsum[m,n,4]=mean(gdata[m,n,])                   # normal mean parameter
        gsum[m,n,5]=sd(gdata[m,n,])                     # normal stdev parameter  
        gsum[m,n,7]=mean(stdata[m,n,])                  # mean of st. data
        gsum[m,n,8]=var(stdata[m,n,])                   # variance of st. data
        gsum[m,n,9]=quantile(stdata[m,n,],probs=0.05)   # l. quantile of st. data
        gsum[m,n,10]=quantile(stdata[m,n,],probs=0.5)   # m. quantile of st. data
        gsum[m,n,11]=quantile(stdata[m,n,],probs=0.95)  # u. quantile of st. data
      }
    }
  } else if (dstbn=="skew"){ 
    for (m in 1:nI){
      for (n in 1:J){
        gsum[m,n,4]=mean(gdata[m,n,])                   # gamma mean parameter  
        skewness = mean((gdata[m,n,]-mean(gdata[m,n,]))^3)/(var(gdata[m,n,])^1.5)
        gsum[m,n,5]=(2/skewness)^2         # gamma shape parameter
        gsum[m,n,6]=sqrt(var(gdata[m,n,])/gsum[m,n,5])  # gamma scale parameter
        gsum[m,n,7]=mean(stdata[m,n,])                  # mean of st. data
        gsum[m,n,8]=var(stdata[m,n,])                   # variance of st. data
        gsum[m,n,9]=quantile(stdata[m,n,],probs=0.05)   # l. quantile of st. data
        gsum[m,n,10]=quantile(stdata[m,n,],probs=0.5)   # m. quantile of st. data
        gsum[m,n,11]=quantile(stdata[m,n,],probs=0.95)  # u. quantile of st. data
      }
    }
  }
  list(stdata=stdata,gsum=gsum)
}

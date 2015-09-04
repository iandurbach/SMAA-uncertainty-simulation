runsimMain<-function(K,nd,wrange,runs,sims,simname,identifier){
  
  output_array<-c('Computer','SimulationDate','nI','J','K','non_dominated','dstbn','variance','wr','thinning_factor','tau','lambda',
                  'u_c','e','model','runs','mautbest','mautworst','r1_scor','r1_util_loss','r1_mro4mautbest',
                  'r1_mautbestintop3','r1_mautbestintop5','r1_mautro4modelbest','r1_modelbestintop3','r1_modelbestintop5',
                  'r3_scor','r3_util_loss','r3_mro4mautbest','r3_mautbestintop3','r3_mautbestintop5','r3_mautro4modelbest',
                  'r3_modelbestintop3','r3_modelbestintop5','r5_scor','r5_util_loss','r5_mro4mautbest','r5_mautbestintop3',
                  'r5_mautbestintop5','r5_mautro4modelbest','r5_modelbestintop3','r5_modelbestintop5','timetaken')
  
  computer = 'local'
  ulambda = NA
  alpha = NA
  beta = NA
  
  wr <<- 0.5
  
  for (nI in c(9,19)){
    nI <<- nI
    for (J in c(6,12)){
      J <<- J
      nC <<- wr * J
      print(J)
      for (dstbn in c("normal","skew")){
        dstbn <<- dstbn
        for (par1 in c("highvar","lowvar")){
          par1 <<- par1
          for (utau in c("high","low","linear")){
            utau <<- utau
            for (u_c in c(-2,0,2))
            {
              u_c <<- u_c
              for (e in c(0,0.1,0.2)){
                e <<- e
                for (s in 1:sims)
                {
                  
                  if(utau == "low"){
                    ta = runif(J,0.2,0.4)
                    la = runif(J,0.6,0.8)
                  } else if(utau == "high"){
                    ta = runif(J,0.6,0.8)
                    la = runif(J,0.2,0.4)
                  } else if(utau == "linear"){
                    ta = runif(J,0.48,0.52)
                    la = runif(J,0.48,0.52)
                  }
                  
                  d1<-gendata(nI,J,K,nd,dstbn,par1)
                  tmaut<-maut(d1$stdata,nI,J,K,ta,la,u_c)
                  tw<<-tmaut$w
                  egsum<-applyerror(d1$gsum,nI,J,e)
                  
                  input0=d1$stdata
                  input1=egsum
                  input2=egsum[,,7]
                  input3=list(v1=as.matrix(egsum[,,7]),v2=as.matrix(egsum[,,8]))
                  input4=list(v1=as.matrix(egsum[,,9]),v2=as.matrix(egsum[,,10]),v3=as.matrix(egsum[,,11]))
                  
                  time0=system.time(newraim<-SMAABoot(nI,J,runs,input0,dstbn,ta,la),gcFirst=FALSE)
                  output0=t(c(computer,simname,nI,J,K,nd,dstbn,par1,wr,thin.f,utau,ulambda,u_c,e,"SMAABoot",runs))
                  output0=c(output0,outputmeasures(newraim,tmaut$utility),time0[3])
                  #                     write(output0,file=paste("testsim_output",identifier,".csv",sep=""),ncolumns=44,append="T", sep=",")
                  output_array<-cbind(output_array,output0)
                  
                  time1=system.time(newraim<-SMAA1(nI,J,runs,input1,dstbn,ta,la),gcFirst=FALSE)
                  output1=t(c(computer,simname,nI,J,K,nd,dstbn,par1,wr,thin.f,utau,ulambda,u_c,e,"SMAA1",runs))
                  output1=c(output1,outputmeasures(newraim,tmaut$utility),time1[3])
                  #                     write(output1,file=paste("testsim_output",identifier,".csv",sep=""),ncolumns=44,append="T", sep=",")
                  output_array<-cbind(output_array,output1)
                  
                  time2=system.time(newraim<-SMAA2(nI,J,runs,input2,dstbn,ta,la),gcFirst=FALSE)
                  output2=t(c(computer,simname,nI,J,K,nd,dstbn,par1,wr,thin.f,utau,ulambda,u_c,e,"SMAA2",runs))
                  output2=c(output2,outputmeasures(newraim,tmaut$utility),time2[3])
                  #                     write(output2,file=paste("testsim_output",identifier,".csv",sep=""),ncolumns=44,append="T", sep=",")
                  output_array<-cbind(output_array,output2)
                  
                  time3=system.time(newraim<-SMAA3(nI,J,runs,input3,dstbn,ta,la,riskp=4),gcFirst=FALSE)
                  output3=t(c(computer,simname,nI,J,K,nd,dstbn,par1,wr,thin.f,utau,ulambda,u_c,e,"SMAA3",runs))
                  output3=c(output3,outputmeasures(newraim,tmaut$utility),time3[3])
                  #                     write(output3,file=paste("testsim_output",identifier,".csv",sep=""),ncolumns=44,append="T", sep=",")
                  output_array<-cbind(output_array,output3)
                  
                  time4=system.time(newraim<-SMAA4(nI,J,runs,input4,dstbn,ta,la),gcFirst=FALSE)
                  output4=t(c(computer,simname,nI,J,K,nd,dstbn,par1,wr,thin.f,utau,ulambda,u_c,e,"SMAA4",runs))
                  output4=c(output4,outputmeasures(newraim,tmaut$utility),time4[3])
                  #                     write(output4,file=paste("testsim_output",identifier,".csv",sep=""),ncolumns=44,append="T", sep=",")
                  output_array<-cbind(output_array,output4)
                  
                }
              }              
            }
          }
        }
      }
    }
  }
  
  write.csv(t(output_array),file=paste("output_array_",identifier,".csv",sep=""))
  
}
outputmeasures<-function(raim,truemaut){

modelRO1=raim[,1]
modelRO3=raim[,1]+raim[,2]+raim[,3]
modelRO5=raim[,1]+raim[,2]+raim[,3]+raim[,4]+raim[,5]

smodelRO1=sort(modelRO1,decreasing=TRUE,index.return=TRUE)
smodelRO3=sort(modelRO3,decreasing=TRUE,index.return=TRUE)
smodelRO5=sort(modelRO5,decreasing=TRUE,index.return=TRUE)

mautRO=sort(truemaut,decreasing=TRUE,index.return=TRUE)

# record min and max of MAUT utility (to get an idea of how close the best and worst alternatives are)
mautbest=mautRO$x[1]
mautworst=tail(mautRO$x,1)
output=c(mautbest,mautworst)

  for (l in 1:3){
     # assign relevant model rank order measure and calculate correlation between true and model rank orders
     if (l==1){
      modelRO=smodelRO1
      scor=cor(truemaut,modelRO1,method="spearman")
     }
     else if(l==2){
      modelRO=smodelRO3
      scor=cor(truemaut,modelRO3,method="spearman")
     }
     else if (l==3){
      modelRO=smodelRO5
      scor=cor(truemaut,modelRO5,method="spearman")
     }

    # utility loss
    sel=truemaut[modelRO$ix[1]]
    ul=(mautbest-sel)/(mautbest-mautworst)

    # rank of MAUT best in model rank order
    mro4mautbest=match(mautRO$ix[1],modelRO$ix)
    # is MAUT best in in top3 ranks of model rank order?
    if(mro4mautbest<4){mautbestintop3=1}
    else{mautbestintop3=0}
    # is MAUT best in in top5 ranks of model rank order?
    if(mro4mautbest<6){mautbestintop5=1}
    else{mautbestintop5=0}
    # rank of model's best in MAUT rank order
    mautro4modelbest=match(modelRO$ix[1],mautRO$ix)
    # is model best in in top3 ranks of MAUT rank order?
    if(mautro4modelbest<4){modelbestintop3=1}
    else{modelbestintop3=0}
    # is model best in in top5 ranks of MAUT rank order?
    if(mautro4modelbest<6){modelbestintop5=1}
    else{modelbestintop5=0}
    
    newoutput=c(scor,ul,mro4mautbest,mautbestintop3,mautbestintop5,mautro4modelbest,modelbestintop3,modelbestintop5)
    output=c(output,newoutput)
  }
  output
}
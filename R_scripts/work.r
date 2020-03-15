#This script creates a self-efficacy similarity network
setwd("E:/Dropbox/IND/ASSIST-ME/Self-Efficacy Networks/")
setwd("~/Dropbox/Forskning/ASSIST-ME/Self-Efficacy Networks")

source("~/Dropbox/ScriptsAndPrograms/PER-R/Creating Networks/Backbone Extraction/backboneExtraction.r")
source("~/Dropbox/ScriptsAndPrograms/PER-R/NetworkMeasures/Segregation/segregation27June2017.r")
mydata<-read.csv("dataset.csv",sep=";")
mydata$X<-c(1:length(mydata$Name))
#mydata[is.na(mydata)] <- 0
names(mydata)[25]<-"post48"
names(mydata)[26]<-"post49"

library(igraph)

#Respondent similarity network

resp<-paste("R",c(1:length(mydata$X)),sep="")
probs<-function(mydata,n){
  a<-as.numeric(names(table(mydata[,n])))
  p<-as.numeric(table(mydata[,n],useNA = "always")/length(resp))
  x<-c(1:length(resp))
  for(i in 1:length(a)){
    x[which(mydata[,n]==a[i])]<-p[i]
  }
  x[which(is.na(mydata[,n]))]<-p[6]
  return(x)
}

names(mydata[3:14]) #Columns for pre
names(mydata[14:26]) #Columns for post
pre<-mydata[3:14]
post<-mydata[14:26]

pmat<-function(data){
pmat<-matrix(0,ncol=length(data),nrow=length(data[,1]))
for(j in 1:length(data)){
  pmat[,j]<-probs(data,j)  
  
}
infmat<--log2(pmat)
return(infmat)
}

simRes<-function(i,j,infmat,d){
  y<-infmat[i,]
  overlap<-sum(y[which(d[i,1:12]==d[j,1:12])])
  sinfi<-sum(infmat[i,])
  sinfj<-sum(infmat[j,])
  sim<-2*overlap/(sinfi+sinfj)
  return(sim)
}

simResk<-function(k,inf,d){
    simVec<-vector()
  for(i in 1:length(resp)){
    simVec[i]<-simRes(k,i,inf,d)
  }
  return(simVec)
}

simMatrix<-function(d){
  inf<-pmat(d)
  similarityMatrix<-matrix(data=0,ncol=length(d[,1]),nrow=length(d[,1]))
  for(i in 1:length(resp)){
    similarityMatrix[,i]<-simResk(i,inf,d)  
    
  }
  return(similarityMatrix)
}

preSim<-simMatrix(pre)
postSim<-simMatrix(post)
respTot<-resp
respT<-resp[mydata$control=="n"]
respC<-resp[mydata$control=="y"]

resp<-respT
preSimT<-simMatrix(pre[mydata$control=="n",])
postSimT<-simMatrix(post[mydata$control=="n",])

resp<-respC
preSimC<-simMatrix(pre[mydata$control=="y",])
postSimC<-simMatrix(post[mydata$control=="y",])

preNet<-graph.adjacency(preSim,diag=F,weighted=T)
postNet<-graph.adjacency(postSim,diag=F,weighted=T)
preNetT<-graph.adjacency(preSimT,diag=F,weighted=T)
postNetT<-graph.adjacency(postSimT,diag=F,weighted=T)

preNetC<-graph.adjacency(preSimC,diag=F,weighted=T)
postNetC<-graph.adjacency(postSimC,diag=F,weighted=T)
V(preNet)$id<-resp
V(postNet)$id<-resp
V(preNetT)$id<-respT
V(postNetT)$id<-respT
V(preNetC)$id<-respC
V(postNetC)$id<-respC


preNetBB<-backboneNetwork(preNet,0.015,2)
postNetBB<-backboneNetwork(postNet,0.015,2)
preNetBBT<-backboneNetwork(preNetT,0.015,2)
postNetBBT<-backboneNetwork(postNetT,0.015,2)
preNetBBC<-backboneNetwork(preNetC,0.015,2)
postNetBBC<-backboneNetwork(postNetC,0.015,2)
write.graph(preNetBB,"preNetBB.net",format=c("pajek"))
write.graph(postNetBB,"postNetBB.net",format=c("pajek"))


#Question Similarity Network

probsQ<-function(mydata,n){
  a<-as.numeric(names(table(as.numeric(mydata[n,1:12]))))
  p<-table(as.numeric(mydata[n,1:12]))/12
  x<-c(1:12)
  for(i in 1:length(a)){
    x[which(mydata[n,1:12]==a[i])]<-p[[i]]
  }
  return(x)
}

names(pre[1:12])
pmatQ<-matrix(0,ncol=93,nrow=12)
for(j in 1:93){
  pmatQ[,j]<-probsQ(post,j)  
  
}
infmatQ<--log2(pmatQ)
md<-t(post[1:12])

simResQ<-function(i,j){
  y<-infmatQ[i,]
  overlap<-sum(y[which(md[i,]==md[j,])]) 
  sinfi<-sum(infmatQ[i,])
  sinfj<-sum(infmatQ[j,])
  sim<-2*overlap/(sinfi+sinfj)
  return(sim)
}

simReskQ<-function(k){
  simVec<-vector()
  for(i in 1:12){
    simVec[i]<-simResQ(k,i)
  }
  return(simVec)
}

simMatrixQ<-matrix(data=0,ncol=12,nrow=12)
for(i in 1:12){
  simMatrixQ[,i]<-simReskQ(i)  
  
}


preQ<-graph.adjacency(simMatrixQ,diag=F,weighted=T,mode = "undirected")
postQ<-graph.adjacency(simMatrixQ,diag=F,weighted=T,mode = "undirected")
V(preQ)$id<-names(mydata[2:13])
V(postQ)$id<-names(mydata[14:25])

preQBB<-backboneNetwork(preQ,0.05,2)
postQBB<-backboneNetwork(postQ,0.05,2)
write.graph(preQBB,"preQBB.net",format=c("pajek"))
write.graph(postQBB,"postQBB.net",format=c("pajek"))


#analysis of gains
g38<-mydata$post38-mydata$pre38
g39<-mydata$post39-mydata$pre39
g40<-mydata$post40-mydata$pre40
g41<-mydata$post41-mydata$pre41
g42<-mydata$post42-mydata$pre42
g43<-mydata$post43-mydata$pre43
g44<-mydata$post44-mydata$pre44
g45<-mydata$post45-mydata$pre45
g46<-mydata$post46-mydata$pre46
g47<-mydata$post47-mydata$pre47
g48<-mydata$post48-mydata$pre48
g49<-mydata$post48-mydata$pre49

tt<-function(g){
test<-t.test(g[which(mydata$control=="y")],g[which(mydata$control=="n")])
return(test)
}

  
#Segregation
attributes<-data.frame(ID,control=mydata$control,country=mydata$country,yearsTeaching=mydata$yearsTeaching,physics=mydata$Physics,chemistry=mydata$Chemistry,technology=mydata$Technology,biology=mydata$Biology,mathematics=mydata$Math,integrated=mydata$Integrated)
attributes$subjects<-do.call(paste,c(attributes[,5:10],sep=""))
icpre<-infomap.community(preNetBB)
icpost<-infomap.community(postNetBB)
attributes$icpre<-icpre$membership
attributes$icpost<-icpost$membership

resampleX(icpre$membership,x = mydata$control,100,2)

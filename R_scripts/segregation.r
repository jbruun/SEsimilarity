segregation<-function(memb,x){
  #calculate the weighted cross entropy, the segregation,
  #as described in AUTHORS
  #The functiion provided here is more general than the functions in the supplemental material
  #of that paper. 
  #memb and x should be of equal length, and memb should be a membership vector from a community detection algorithm
  df<-data.frame(memb,as.numeric(as.factor(x)))
  par<-as.numeric(table(x))
  tot<-sum(par)
  q<-par/tot
  N<-length(unique(memb)) #number of modules in memb
  n<-length(memb) # number of nodes
  #create placeholder data.frame!
S<-vector()
for(i in 1:N){
  m<-sum(table(df[which(df[,1]==i),2]))
p<-table(df[which(df[,1]==i),2])/sum(table(df[which(df[,1]==i),2]))
pp<-as.numeric(p)
pq<-q[as.numeric(names(p))]
weight<-m/n
S[i]<-weight*sum(pp*log2(pp/pq),na.rm=T)
}
S<-S
  return(S)
  
}

entropy<-function(x){
  par<-as.numeric(table(x))
  tot<-sum(par)
  q<-par/tot
  H<--sum(q*log2(q))
  return(H)
}

segMax<-function(memb,x, mode,iter){
  #memb is a membership vector and x is a corresponding attribute vector
  m<-length(unique(memb))
  #a<-as.data.frame(table(V(g)$membership))
  #N<-a$Freq # number of students in the group
  #choose which attribute to calculate segregation on
  
  #The segregation
  Seg<-entropy(x)
  df<-data.frame(memb,x)
  
  Frame<-matrix(0,nrow=m,ncol=iter)
  #Frame2<-matrix(0,nrow=m,ncol=iter)
  for (i in 1:iter){
    y<-sample(x,length(memb),replace=F)
    Frame[,i]<-segregation(memb,y)
  } 
  
  if (mode==1){
    #groupwise segregation Z-scores
    tFrame<-as.data.frame(t(Frame))
    mFrame<-sapply(tFrame,mean)
    mFrame<-as.vector(mFrame)
    sdFrame<-sapply(tFrame,sd)
    sdFrame<-as.vector(sdFrame)
    Z<-(Seg-mFrame)/(sdFrame)
    
  }
  else {
    #total segregation as used in paper
    ranMix<-colSums(Frame)
    ranMix<-ranMix/length(x)
    gMix<-sum(Seg)/length(x)
    Z<-(gMix-mean(ranMix))/(sd(ranMix))
  }
  return(Z)
}

#RESAMPLING

resampleX<-function(memb,x,mode,iter){
  #memb is a membership vector and x is a corresponding attribute vector
  m<-length(unique(memb))
  #a<-as.data.frame(table(V(g)$membership))
  #N<-a$Freq # number of students in the group
  #choose which attribute to calculate segregation on
  
  #The segregation
  Seg<-segregation(memb,x)
  df<-data.frame(memb,x)
  
  Frame<-matrix(0,nrow=m,ncol=iter)
  #Frame2<-matrix(0,nrow=m,ncol=iter)
  for (i in 1:iter){
    y<-sample(x,length(memb),replace=F)
    Frame[,i]<-segregation(memb,y)
  } 
  
  if (mode==1){
    #groupwise segregation Z-scores
    tFrame<-as.data.frame(t(Frame))
    mFrame<-sapply(tFrame,mean)
    mFrame<-as.vector(mFrame)
    sdFrame<-sapply(tFrame,sd)
    sdFrame<-as.vector(sdFrame)
    Z<-(Seg-mFrame)/(sdFrame)
    
  }
  else {
    #total segregation as used in paper
    ranMix<-colSums(Frame)
    ranMix<-ranMix/length(x)
    gMix<-sum(Seg)/length(x)
    Z<-(gMix-mean(ranMix))/(sd(ranMix))
  }
  return(Z)
}

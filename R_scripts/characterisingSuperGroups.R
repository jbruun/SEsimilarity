##
library(pgirmess)
library(PMCMRplus)

setwd("~/Dropbox/articles/SE_IJSE/Self-Efficacy Networks")
mydata<-read.csv("dataset.csv",sep=";")
names(mydata)[25]<-"post48"
names(mydata)[26]<-"post49"
newdata<-mydata[mydata$control=="n",]
#load supergroups
respT<-newdata[15:26]
respT[is.na(respT)]<-0
respTpre<-newdata[3:14]
respTpre[is.na(respTpre)]<-0
anMat<-function(data,memb,letter){
sgax<-matrix(0,nrow=12,ncol=7)
sgax[,1]<-c(38:49)
N<-length(which(memb==letter))
colnames(sgax)<-c("Q","1 - Disagree",2:4,"5 - Agree", "NA")
sgax[1,2]<-length(which(respT[memb==letter,]$post38==1))/N
sgax[1,3]<-length(which(respT[memb==letter,]$post38==2))/N
sgax[1,4]<-length(which(respT[memb==letter,]$post38==3))/N
sgax[1,5]<-length(which(respT[memb==letter,]$post38==4))/N
sgax[1,6]<-length(which(respT[memb==letter,]$post38==5))/N
sgax[1,7]<-length(which(respT[memb==letter,]$post38==0))/N

sgax[2,2]<-length(which(respT[memb==letter,]$post39==1))/N
sgax[2,3]<-length(which(respT[memb==letter,]$post39==2))/N
sgax[2,4]<-length(which(respT[memb==letter,]$post39==3))/N
sgax[2,5]<-length(which(respT[memb==letter,]$post39==4))/N
sgax[2,6]<-length(which(respT[memb==letter,]$post39==5))/N
sgax[2,7]<-length(which(respT[memb==letter,]$post39==0))/N

sgax[3,2]<-length(which(respT[memb==letter,]$post40==1))/N
sgax[3,3]<-length(which(respT[memb==letter,]$post40==2))/N
sgax[3,4]<-length(which(respT[memb==letter,]$post40==3))/N
sgax[3,5]<-length(which(respT[memb==letter,]$post40==4))/N
sgax[3,6]<-length(which(respT[memb==letter,]$post40==5))/N
sgax[3,7]<-length(which(respT[memb==letter,]$post40==0))/N

sgax[4,2]<-length(which(respT[memb==letter,]$post41==1))/N
sgax[4,3]<-length(which(respT[memb==letter,]$post41==2))/N
sgax[4,4]<-length(which(respT[memb==letter,]$post41==3))/N
sgax[4,5]<-length(which(respT[memb==letter,]$post41==4))/N
sgax[4,6]<-length(which(respT[memb==letter,]$post41==5))/N
sgax[4,7]<-length(which(respT[memb==letter,]$post41==0))/N

sgax[5,2]<-length(which(respT[memb==letter,]$post42==1))/N
sgax[5,3]<-length(which(respT[memb==letter,]$post42==2))/N
sgax[5,4]<-length(which(respT[memb==letter,]$post42==3))/N
sgax[5,5]<-length(which(respT[memb==letter,]$post42==4))/N
sgax[5,6]<-length(which(respT[memb==letter,]$post42==5))/N
sgax[5,7]<-length(which(respT[memb==letter,]$post42==0))/N

sgax[6,2]<-length(which(respT[memb==letter,]$post43==1))/N
sgax[6,3]<-length(which(respT[memb==letter,]$post43==2))/N
sgax[6,4]<-length(which(respT[memb==letter,]$post43==3))/N
sgax[6,5]<-length(which(respT[memb==letter,]$post43==4))/N
sgax[6,6]<-length(which(respT[memb==letter,]$post43==5))/N
sgax[6,7]<-length(which(respT[memb==letter,]$post43==0))/N

sgax[7,2]<-length(which(respT[memb==letter,]$post44==1))/N
sgax[7,3]<-length(which(respT[memb==letter,]$post44==2))/N
sgax[7,4]<-length(which(respT[memb==letter,]$post44==3))/N
sgax[7,5]<-length(which(respT[memb==letter,]$post44==4))/N
sgax[7,6]<-length(which(respT[memb==letter,]$post44==5))/N
sgax[7,7]<-length(which(respT[memb==letter,]$post44==0))/N

sgax[8,2]<-length(which(respT[memb==letter,]$post45==1))/N
sgax[8,3]<-length(which(respT[memb==letter,]$post45==2))/N
sgax[8,4]<-length(which(respT[memb==letter,]$post45==3))/N
sgax[8,5]<-length(which(respT[memb==letter,]$post45==4))/N
sgax[8,6]<-length(which(respT[memb==letter,]$post45==5))/N
sgax[8,7]<-length(which(respT[memb==letter,]$post45==0))/N

sgax[9,2]<-length(which(respT[memb==letter,]$post46==1))/N
sgax[9,3]<-length(which(respT[memb==letter,]$post46==2))/N
sgax[9,4]<-length(which(respT[memb==letter,]$post46==3))/N
sgax[9,5]<-length(which(respT[memb==letter,]$post46==4))/N
sgax[9,6]<-length(which(respT[memb==letter,]$post46==5))/N
sgax[9,7]<-length(which(respT[memb==letter,]$post46==0))/N

sgax[10,2]<-length(which(respT[memb==letter,]$post47==1))/N
sgax[10,3]<-length(which(respT[memb==letter,]$post47==2))/N
sgax[10,4]<-length(which(respT[memb==letter,]$post47==3))/N
sgax[10,5]<-length(which(respT[memb==letter,]$post47==4))/N
sgax[10,6]<-length(which(respT[memb==letter,]$post47==5))/N
sgax[10,7]<-length(which(respT[memb==letter,]$post47==0))/N

sgax[11,2]<-length(which(respT[memb==letter,]$post48==1))/N
sgax[11,3]<-length(which(respT[memb==letter,]$post48==2))/N
sgax[11,4]<-length(which(respT[memb==letter,]$post48==3))/N
sgax[11,5]<-length(which(respT[memb==letter,]$post48==4))/N
sgax[11,6]<-length(which(respT[memb==letter,]$post48==5))/N
sgax[11,7]<-length(which(respT[memb==letter,]$post48==0))/N

sgax[12,2]<-length(which(respT[memb==letter,]$post49==1))/N
sgax[12,3]<-length(which(respT[memb==letter,]$post49==2))/N
sgax[12,4]<-length(which(respT[memb==letter,]$post49==3))/N
sgax[12,5]<-length(which(respT[memb==letter,]$post49==4))/N
sgax[12,6]<-length(which(respT[memb==letter,]$post49==5))/N
sgax[12,7]<-length(which(respT[memb==letter,]$post49==0))/N
return(sgax)
}

anMatPre<-function(data,memb,letter){
  sgax<-matrix(0,nrow=12,ncol=7)
  sgax[,1]<-c(38:49)
  N<-length(which(memb==letter))
  colnames(sgax)<-c("Q","1 - Disagree",2:4,"5 - Agree", "NA")
  sgax[1,2]<-length(which(data[memb==letter,]$pre38==1))/N
  sgax[1,3]<-length(which(data[memb==letter,]$pre38==2))/N
  sgax[1,4]<-length(which(data[memb==letter,]$pre38==3))/N
  sgax[1,5]<-length(which(data[memb==letter,]$pre38==4))/N
  sgax[1,6]<-length(which(data[memb==letter,]$pre38==5))/N
  sgax[1,7]<-length(which(data[memb==letter,]$pre38==0))/N
  
  sgax[2,2]<-length(which(data[memb==letter,]$pre39==1))/N
  sgax[2,3]<-length(which(data[memb==letter,]$pre39==2))/N
  sgax[2,4]<-length(which(data[memb==letter,]$pre39==3))/N
  sgax[2,5]<-length(which(data[memb==letter,]$pre39==4))/N
  sgax[2,6]<-length(which(data[memb==letter,]$pre39==5))/N
  sgax[2,7]<-length(which(data[memb==letter,]$pre39==0))/N
  
  sgax[3,2]<-length(which(data[memb==letter,]$pre40==1))/N
  sgax[3,3]<-length(which(data[memb==letter,]$pre40==2))/N
  sgax[3,4]<-length(which(data[memb==letter,]$pre40==3))/N
  sgax[3,5]<-length(which(data[memb==letter,]$pre40==4))/N
  sgax[3,6]<-length(which(data[memb==letter,]$pre40==5))/N
  sgax[3,7]<-length(which(data[memb==letter,]$pre40==0))/N
  
  sgax[4,2]<-length(which(data[memb==letter,]$pre41==1))/N
  sgax[4,3]<-length(which(data[memb==letter,]$pre41==2))/N
  sgax[4,4]<-length(which(data[memb==letter,]$pre41==3))/N
  sgax[4,5]<-length(which(data[memb==letter,]$pre41==4))/N
  sgax[4,6]<-length(which(data[memb==letter,]$pre41==5))/N
  sgax[4,7]<-length(which(data[memb==letter,]$pre41==0))/N
  
  sgax[5,2]<-length(which(data[memb==letter,]$pre42==1))/N
  sgax[5,3]<-length(which(data[memb==letter,]$pre42==2))/N
  sgax[5,4]<-length(which(data[memb==letter,]$pre42==3))/N
  sgax[5,5]<-length(which(data[memb==letter,]$pre42==4))/N
  sgax[5,6]<-length(which(data[memb==letter,]$pre42==5))/N
  sgax[5,7]<-length(which(data[memb==letter,]$pre42==0))/N
  
  sgax[6,2]<-length(which(data[memb==letter,]$pre43==1))/N
  sgax[6,3]<-length(which(data[memb==letter,]$pre43==2))/N
  sgax[6,4]<-length(which(data[memb==letter,]$pre43==3))/N
  sgax[6,5]<-length(which(data[memb==letter,]$pre43==4))/N
  sgax[6,6]<-length(which(data[memb==letter,]$pre43==5))/N
  sgax[6,7]<-length(which(data[memb==letter,]$pre43==0))/N
  
  sgax[7,2]<-length(which(data[memb==letter,]$pre44==1))/N
  sgax[7,3]<-length(which(data[memb==letter,]$pre44==2))/N
  sgax[7,4]<-length(which(data[memb==letter,]$pre44==3))/N
  sgax[7,5]<-length(which(data[memb==letter,]$pre44==4))/N
  sgax[7,6]<-length(which(data[memb==letter,]$pre44==5))/N
  sgax[7,7]<-length(which(data[memb==letter,]$pre44==0))/N
  
  sgax[8,2]<-length(which(data[memb==letter,]$pre45==1))/N
  sgax[8,3]<-length(which(data[memb==letter,]$pre45==2))/N
  sgax[8,4]<-length(which(data[memb==letter,]$pre45==3))/N
  sgax[8,5]<-length(which(data[memb==letter,]$pre45==4))/N
  sgax[8,6]<-length(which(data[memb==letter,]$pre45==5))/N
  sgax[8,7]<-length(which(data[memb==letter,]$pre45==0))/N
  
  sgax[9,2]<-length(which(data[memb==letter,]$pre46==1))/N
  sgax[9,3]<-length(which(data[memb==letter,]$pre46==2))/N
  sgax[9,4]<-length(which(data[memb==letter,]$pre46==3))/N
  sgax[9,5]<-length(which(data[memb==letter,]$pre46==4))/N
  sgax[9,6]<-length(which(data[memb==letter,]$pre46==5))/N
  sgax[9,7]<-length(which(data[memb==letter,]$pre46==0))/N
  
  sgax[10,2]<-length(which(data[memb==letter,]$pre47==1))/N
  sgax[10,3]<-length(which(data[memb==letter,]$pre47==2))/N
  sgax[10,4]<-length(which(data[memb==letter,]$pre47==3))/N
  sgax[10,5]<-length(which(data[memb==letter,]$pre47==4))/N
  sgax[10,6]<-length(which(data[memb==letter,]$pre47==5))/N
  sgax[10,7]<-length(which(data[memb==letter,]$pre47==0))/N
  
  sgax[11,2]<-length(which(data[memb==letter,]$pre48==1))/N
  sgax[11,3]<-length(which(data[memb==letter,]$pre48==2))/N
  sgax[11,4]<-length(which(data[memb==letter,]$pre48==3))/N
  sgax[11,5]<-length(which(data[memb==letter,]$pre48==4))/N
  sgax[11,6]<-length(which(data[memb==letter,]$pre48==5))/N
  sgax[11,7]<-length(which(data[memb==letter,]$pre48==0))/N
  
  sgax[12,2]<-length(which(data[memb==letter,]$pre49==1))/N
  sgax[12,3]<-length(which(data[memb==letter,]$pre49==2))/N
  sgax[12,4]<-length(which(data[memb==letter,]$pre49==3))/N
  sgax[12,5]<-length(which(data[memb==letter,]$pre49==4))/N
  sgax[12,6]<-length(which(data[memb==letter,]$pre49==5))/N
  sgax[12,7]<-length(which(data[memb==letter,]$pre49==0))/N
  return(sgax)
}

##############SUPERGROUP PRE POST ANSWERS AND DIFFERENCES#######################################
sgAPre<-anMatPre(respTpre,supergroups,"A")
sgAPost<-anMat(respT,supergroups,"A")
sgADiff<-sgAPost-sgAPre
sgBPre<-anMatPre(respTpre,supergroups,"B")
sgBPost<-anMat(respT,supergroups,"B")
sgBDiff<-sgBPost-sgBPre
sgCPre<-anMatPre(respTpre,supergroups,"C")
sgCPost<-anMat(respT,supergroups,"C")
sgCDiff<-sgCPost-sgCPre




par(mfrow=c(3,2))
barplot(rbind(sgAPost[1,2:7],sgBPost[1,2:7],sgCPost[1,2:7]),beside=T,main="Q38: Find better ways to teach using FA",xlab="Teacher response",ylab="Frequency",legend=c("A","B","C"))
barplot(rbind(sgAPost[2,2:7],sgBPost[2,2:7],sgCPost[2,2:7]),beside=T,main="Q39R: Not difficult to integrate FA ",xlab="Teacher response",ylab="Frequency")
barplot(rbind(sgAPost[3,2:7],sgBPost[3,2:7],sgCPost[3,2:7]),beside=T,main="Q40: Know necessary steps",xlab="Teacher response",ylab="Frequency")
barplot(rbind(sgAPost[4,2:7],sgBPost[4,2:7],sgCPost[4,2:7]),beside=T,main="Q41R: Effective in monitoring student work",xlab="Teacher response",ylab="Frequency")
barplot(rbind(sgAPost[5,2:7],sgBPost[5,2:7],sgCPost[5,2:7]),beside=T,main="Q42R: Teaching will be effective ",xlab="Teacher response",ylab="Frequency")
barplot(rbind(sgAPost[6,2:7],sgBPost[6,2:7],sgCPost[6,2:7]),beside=T,main="Q43: Student background can be overcome",xlab="Teacher response",ylab="Frequency")
par(mfrow=c(3,2))
barplot(rbind(sgAPost[7,2:7],sgBPost[7,2:7],sgCPost[7,2:7]),beside=T,main="Q44: Low-achieving student progress due to FA",xlab="Teacher response",ylab="Frequency",legend=c("A","B","C"))
barplot(rbind(sgAPost[8,2:7],sgBPost[8,2:7],sgCPost[8,2:7]),beside=T,main="Q45: Understand FA enough to be effective ",xlab="Teacher response",ylab="Frequency")
barplot(rbind(sgAPost[9,2:7],sgBPost[9,2:7],sgCPost[9,2:7]),beside=T,main="Q46R: Increased effort leads to change ",xlab="Teacher response",ylab="Frequency")
barplot(rbind(sgAPost[10,2:7],sgBPost[10,2:7],sgCPost[10,2:7]),beside=T,main="Q47R: Not difficult to explain content ",xlab="Teacher response",ylab="Frequency")
barplot(rbind(sgAPost[11,2:7],sgBPost[11,2:7],sgCPost[11,2:7]),beside=T,main="Q48: Able to answer student questions",xlab="Teacher response",ylab="Frequency")
barplot(rbind(sgAPost[12,2:7],sgBPost[12,2:7],sgCPost[12,2:7]),beside=T,main="Q49R: Do not wonder necessary skills",xlab="Teacher response",ylab="Frequency")

#PRE
par(mfrow=c(3,2))
barplot(rbind(sgAPre[1,2:7],sgBPre[1,2:7],sgCPre[1,2:7]),beside=T,main="Q38: Find better ways to teach using FA",xlab="Teacher response",ylab="Frequency",legend=c("A","B","C"))
barplot(rbind(sgAPre[2,2:7],sgBPre[2,2:7],sgCPre[2,2:7]),beside=T,main="Q39R: Not difficult to integrate FA ",xlab="Teacher response",ylab="Frequency")
barplot(rbind(sgAPre[3,2:7],sgBPre[3,2:7],sgCPre[3,2:7]),beside=T,main="Q40: Know necessary steps",xlab="Teacher response",ylab="Frequency")
barplot(rbind(sgAPre[4,2:7],sgBPre[4,2:7],sgCPre[4,2:7]),beside=T,main="Q41R: Effective in monitoring student work",xlab="Teacher response",ylab="Frequency")
barplot(rbind(sgAPre[5,2:7],sgBPre[5,2:7],sgCPre[5,2:7]),beside=T,main="Q42R: Teaching will be effective ",xlab="Teacher response",ylab="Frequency")
barplot(rbind(sgAPre[6,2:7],sgBPre[6,2:7],sgCPre[6,2:7]),beside=T,main="Q43: Student background can be overcome",xlab="Teacher response",ylab="Frequency")
par(mfrow=c(3,2))
barplot(rbind(sgAPre[7,2:7],sgBPre[7,2:7],sgCPre[7,2:7]),beside=T,main="Q44: Low-achieving student progress due to FA",xlab="Teacher response",ylab="Frequency",legend=c("A","B","C"))
barplot(rbind(sgAPre[8,2:7],sgBPre[8,2:7],sgCPre[8,2:7]),beside=T,main="Q45: Understand FA enough to be effective ",xlab="Teacher response",ylab="Frequency")
barplot(rbind(sgAPre[9,2:7],sgBPre[9,2:7],sgCPre[9,2:7]),beside=T,main="Q46R: Increased effort leads to change ",xlab="Teacher response",ylab="Frequency")
barplot(rbind(sgAPre[10,2:7],sgBPre[10,2:7],sgCPre[10,2:7]),beside=T,main="Q47R: Not difficult to explain content ",xlab="Teacher response",ylab="Frequency")
barplot(rbind(sgAPre[11,2:7],sgBPre[11,2:7],sgCPre[11,2:7]),beside=T,main="Q48: Able to answer student questions",xlab="Teacher response",ylab="Frequency")
barplot(rbind(sgAPre[12,2:7],sgBPre[12,2:7],sgCPre[12,2:7]),beside=T,main="Q49R: Do not wonder necessary skills",xlab="Teacher response",ylab="Frequency")

#Differences between post and pre. 
par(mfrow=c(3,2))
barplot(rbind(sgADiff[1,2:7],sgBDiff[1,2:7],sgCDiff[1,2:7]),beside=T,main="Q38: Find better ways to teach using FA",xlab="Teacher response",ylab="Frequency",legend=c("A","B","C"))
barplot(rbind(sgADiff[2,2:7],sgBDiff[2,2:7],sgCDiff[2,2:7]),beside=T,main="Q39R: Not difficult to integrate FA ",xlab="Teacher response",ylab="Frequency")
barplot(rbind(sgADiff[3,2:7],sgBDiff[3,2:7],sgCDiff[3,2:7]),beside=T,main="Q40: Know necessary steps",xlab="Teacher response",ylab="Frequency")
barplot(rbind(sgADiff[4,2:7],sgBDiff[4,2:7],sgCDiff[4,2:7]),beside=T,main="Q41R: Effective in monitoring student work",xlab="Teacher response",ylab="Frequency")
barplot(rbind(sgADiff[5,2:7],sgBDiff[5,2:7],sgCDiff[5,2:7]),beside=T,main="Q42R: Teaching will be effective ",xlab="Teacher response",ylab="Frequency")
barplot(rbind(sgADiff[6,2:7],sgBDiff[6,2:7],sgCDiff[6,2:7]),beside=T,main="Q43: Student background can be overcome",xlab="Teacher response",ylab="Frequency")
par(mfrow=c(3,2))
barplot(rbind(sgADiff[7,2:7],sgBDiff[7,2:7],sgCDiff[7,2:7]),beside=T,main="Q44: Low-achieving student progress due to FA",xlab="Teacher response",ylab="Frequency",legend=c("A","B","C"))
barplot(rbind(sgADiff[8,2:7],sgBDiff[8,2:7],sgCDiff[8,2:7]),beside=T,main="Q45: Understand FA enough to be effective ",xlab="Teacher response",ylab="Frequency")
barplot(rbind(sgADiff[9,2:7],sgBDiff[9,2:7],sgCDiff[9,2:7]),beside=T,main="Q46R: Increased effort leads to change ",xlab="Teacher response",ylab="Frequency")
barplot(rbind(sgADiff[10,2:7],sgBDiff[10,2:7],sgCDiff[10,2:7]),beside=T,main="Q47R: Not difficult to explain content ",xlab="Teacher response",ylab="Frequency")
barplot(rbind(sgADiff[11,2:7],sgBDiff[11,2:7],sgCDiff[11,2:7]),beside=T,main="Q48: Able to answer student questions",xlab="Teacher response",ylab="Frequency")
barplot(rbind(sgADiff[12,2:7],sgBDiff[12,2:7],sgCDiff[12,2:7]),beside=T,main="Q49R: Do not wonder necessary skills",xlab="Teacher response",ylab="Frequency")

###############

#setwd("~/Dropbox/articles/SE_IJSE")
#sga<-as.matrix(read.csv("superGroupAnswersA.csv",sep = ";"))
#sgb<-as.matrix(read.csv("superGroupAnswersB.csv",sep = ";"))
#sgc<-as.matrix(read.csv("superGroupAnswersC.csv",sep = ";"))

colnames(sga)<-c("Q","1 - Disagree",2,3,4,"5 - Agree")
colnames(sgb)<-c("Q","1 - Disagree",2,3,4,"5 - Agree")
colnames(sgc)<-c("Q","1 - Disagree",2,3,4,"5 - Agree")

Sa<-vector()
for(i in 1:12){
  Sa[i]<--sum(sga[i,2:7]*log2(sga[i,2:7]),na.rm=T)
  
}

Sb<-vector()
for(i in 1:12){
  Sb[i]<--sum(sgb[i,2:7]*log2(sgb[i,2:7]),na.rm=T)
  
}

Sc<-vector()
for(i in 1:12){
  Sc[i]<--sum(sgc[i,2:7]*log2(sgc[i,2:7]),na.rm=T)
  
}

Smax<--log2(1/6)

summary(aov(c(Sa,Sb,Sc)~c(rep("A",12),rep("B",12),rep("C",12))))
boxplot2(c(Sa,Sb,Sc)~c(rep("A",12),rep("B",12),rep("C",12)))
TukeyHSD(aov(c(Sa,Sb,Sc)~c(rep("A",12),rep("B",12),rep("C",12))))



kruskal.test(newdata$post38 ~ as.factor(supergroups))
kruskalmc(newdata$post38, supergroups)
all<-c(newdata$post38,newdata$post39,newdata$post40,newdata$post41,newdata$post42,
       newdata$post43,newdata$post44,newdata$post45,newdata$post46,newdata$post47,
       newdata$post48,newdata$post49)
sg<-rep(supergroups,12)
kruskalmc(all, sg)
supergroups<-as.factor(supergroups)
kruskal.test(newdata$post38 ~ supergroups)
kruskal.test(newdata$post39 ~ supergroups)
kruskal.test(newdata$post40 ~ supergroups)
kruskal.test(newdata$post41 ~ supergroups)
kruskal.test(newdata$post42 ~ supergroups)
kruskal.test(newdata$post43 ~ supergroups)
kruskal.test(newdata$post44 ~ supergroups)
kruskal.test(newdata$post45 ~ supergroups)
kruskal.test(newdata$post46 ~ supergroups)
kruskal.test(newdata$post47 ~ supergroups)
kruskal.test(newdata$post48 ~ supergroups)
kruskal.test(newdata$post49 ~ supergroups)

ans <- kwAllPairsConoverTest(newdata$post38 ~ supergroups,p.adjust.method="single-step")
summary(ans)
ans <- kwAllPairsDunnTest(newdata$post38 ~ supergroups,p.adjust.method="bonferroni",dist="Chisquare")
summary(ans)
ans38 <- kwAllPairsNemenyiTest(newdata$post38 ~ supergroups, dist="Chisquare")
summary(ans38)
ans39 <- kwAllPairsNemenyiTest(newdata$post39 ~ supergroups, dist="Chisquare")
summary(ans39)
ans40 <- kwAllPairsNemenyiTest(newdata$post40 ~ supergroups, dist="Chisquare")
summary(ans40)
ans41 <- kwAllPairsNemenyiTest(newdata$post41 ~ supergroups, dist="Chisquare")
summary(ans41)
ans42 <- kwAllPairsNemenyiTest(newdata$post42 ~ supergroups, dist="Chisquare")
summary(ans42)
ans43 <- kwAllPairsNemenyiTest(newdata$post43 ~ supergroups, dist="Chisquare")
summary(ans43)

ans44 <- kwAllPairsNemenyiTest(newdata$post44 ~ supergroups, dist="Chisquare")
summary(ans44)
ans45 <- kwAllPairsNemenyiTest(newdata$post45 ~ supergroups, dist="Chisquare")
summary(ans45)
ans46 <- kwAllPairsNemenyiTest(newdata$post46 ~ supergroups, dist="Chisquare")
summary(ans46)
ans47 <- kwAllPairsNemenyiTest(newdata$post47 ~ supergroups, dist="Chisquare")
summary(ans47)
ans48 <- kwAllPairsNemenyiTest(newdata$post48 ~ supergroups, dist="Chisquare")
summary(ans48)
ans49 <- kwAllPairsNemenyiTest(newdata$post49 ~ supergroups, dist="Chisquare")
summary(ans49)

par(mfrow=c(3,2))
barplot(rbind(sga[1,2:7],sgb[1,2:7],sgc[1,2:7]),beside=T,main="Q38: Find better ways to teach using FA",xlab="Teacher response",ylab="Frequency",legend=c("A","B","C"))
barplot(rbind(sga[2,2:7],sgb[2,2:7],sgc[2,2:7]),beside=T,main="Q39R: Not difficult to integrate FA ",xlab="Teacher response",ylab="Frequency")
barplot(rbind(sga[3,2:7],sgb[3,2:7],sgc[3,2:7]),beside=T,main="Q40: Know necessary steps",xlab="Teacher response",ylab="Frequency")
barplot(rbind(sga[4,2:7],sgb[4,2:7],sgc[4,2:7]),beside=T,main="Q41R: Effective in monitoring student work",xlab="Teacher response",ylab="Frequency")
barplot(rbind(sga[5,2:7],sgb[5,2:7],sgc[5,2:7]),beside=T,main="Q42R: Teaching will be effective ",xlab="Teacher response",ylab="Frequency")
barplot(rbind(sga[6,2:7],sgb[6,2:7],sgc[6,2:7]),beside=T,main="Q43: Student background can be overcome",xlab="Teacher response",ylab="Frequency")
par(mfrow=c(3,2))
barplot(rbind(sga[7,2:7],sgb[7,2:7],sgc[7,2:7]),beside=T,main="Q44: Low-achieving student progress due to FA",xlab="Teacher response",ylab="Frequency",legend=c("A","B","C"))
barplot(rbind(sga[8,2:7],sgb[8,2:7],sgc[8,2:7]),beside=T,main="Q45: Understand FA enough to be effective ",xlab="Teacher response",ylab="Frequency")
barplot(rbind(sga[9,2:7],sgb[9,2:7],sgc[9,2:7]),beside=T,main="Q46R: Increased effort leads to change ",xlab="Teacher response",ylab="Frequency")
barplot(rbind(sga[10,2:7],sgb[10,2:7],sgc[10,2:7]),beside=T,main="Q47R: Not difficult to explain content ",xlab="Teacher response",ylab="Frequency")
barplot(rbind(sga[11,2:7],sgb[11,2:7],sgc[11,2:7]),beside=T,main="Q48: Able to answer student questions",xlab="Teacher response",ylab="Frequency")
barplot(rbind(sga[12,2:7],sgb[12,2:7],sgc[12,2:7]),beside=T,main="Q49R: Do not wonder necessary skills",xlab="Teacher response",ylab="Frequency")


##TERTILES HYPOTHESIS
kruskal.test(newdata$post38 ~ tert)
kruskal.test(newdata$post39 ~ tert)
kruskal.test(newdata$post40 ~ tert)
kruskal.test(newdata$post41 ~ tert)
kruskal.test(newdata$post42 ~ tert)
kruskal.test(newdata$post43 ~ tert)
kruskal.test(newdata$post44 ~ tert)
kruskal.test(newdata$post45 ~ tert)
kruskal.test(newdata$post46 ~ tert)
kruskal.test(newdata$post47 ~ tert)
kruskal.test(newdata$post48 ~ tert)
kruskal.test(newdata$post49 ~ tert)
ans40t <- kwAllPairsNemenyiTest(newdata$post40 ~ tert, dist="Chisquare")
summary(ans40t)
ans49t <- kwAllPairsNemenyiTest(newdata$post49 ~ tert, dist="Chisquare")
summary(ans49t)


par(mfrow=c(1,3))
barplot(rbind(sga[3,2:7],sg1[3,2:7]),beside=T,main="Q40: Know necessary steps",xlab="Teacher response",ylab="Frequency",legend=c("Super Group A","Lower tertile"))
barplot(rbind(sgc[3,2:7],sg2[3,2:7]),beside=T,main="Q40: Know necessary steps",xlab="Teacher response",ylab="Frequency",legend=c("Super Group C","Middle tertile"))
barplot(rbind(sgb[3,2:7],sg3[3,2:7]),beside=T,main="Q40: Know necessary steps",xlab="Teacher response",ylab="Frequency",legend=c("Super Group B","Upper tertile"))
par(mfrow=c(3,2))

par(mfrow=c(1,3))
barplot(rbind(sga[12,2:7],sg1[12,2:7]),beside=T,main="Q49: Have necessary skills",xlab="Teacher response",ylab="Frequency",legend=c("Super Group A","Lower tertile"))
barplot(rbind(sgc[12,2:7],sg2[12,2:7]),beside=T,main="Q49: Have necessary skills",xlab="Teacher response",ylab="Frequency",legend=c("Super Group C","Middle tertile"))
barplot(rbind(sgb[12,2:7],sg3[12,2:7]),beside=T,main="Q49: Have necessary skills",xlab="Teacher response",ylab="Frequency",legend=c("Super Group B","Upper tertile"))
par(mfrow=c(3,2))



par(mfrow=c(1,3))
barplot(rbind(sga[3,2:7],sg1[3,2:7]),beside=T,main="Q40: Know necessary steps",xlab="Teacher response",ylab="Frequency",legend=c("Super Group A","Lower tertile"))
barplot(rbind(sgc[3,2:7],sg2[3,2:7]),beside=T,main="Q40: Know necessary steps",xlab="Teacher response",ylab="Frequency",legend=c("Super Group C","Middle tertile"))
barplot(rbind(sgb[3,2:7],sg3[3,2:7]),beside=T,main="Q40: Know necessary steps",xlab="Teacher response",ylab="Frequency",legend=c("Super Group B","Upper tertile"))
par(mfrow=c(3,2))

par(mfrow=c(1,3))
barplot(rbind(sga[12,2:7],sg1[12,2:7]),beside=T,main="Q49: Have necessary skills",xlab="Teacher response",ylab="Frequency",legend=c("Super Group A","Lower tertile"))
barplot(rbind(sgc[12,2:7],sg2[12,2:7]),beside=T,main="Q49: Have necessary skills",xlab="Teacher response",ylab="Frequency",legend=c("Super Group C","Middle tertile"))
barplot(rbind(sgb[12,2:7],sg3[12,2:7]),beside=T,main="Q49: Have necessary skills",xlab="Teacher response",ylab="Frequency",legend=c("Super Group B","Upper tertile"))
par(mfrow=c(3,2))

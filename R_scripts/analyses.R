#This script creates self-efficacy similarity networks
#It contains most of the analysis done in the main article. 



library(igraph)
library(PMCMRplus)
library(effsize)
library(rcompanion)
library(gplots)

source("R_scripts/backboneExtraction.r")
source("R_scripts/segregation.r")


mydata<-read.csv("Data/dataset.csv",sep=";")
mydata$X<-c(1:length(mydata$Name))



#Respondent similarity network
  #Name respondents
resp<-paste("R",c(1:length(mydata$X)),sep="") 
  #Function for calculating frequencies (here equated with probabilities) of responses
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
  #Identify which columns contain pre and which contain post responses. 
names(mydata[3:14]) #Columns for pre
names(mydata[15:26]) #Columns for post
post<-mydata[15:26] #Responses to post survey questions
post[is.na(post)]<-100 #R does not like to perform calculations with NAs
pre<-mydata[3:14] # We will compare pre community structure with post community structure
pre[is.na(pre)]<-100


  #Function which transforms frequencies/probabilities to information (bits)
pmat<-function(data){
pmat<-matrix(0,ncol=length(data),nrow=length(data[,1]))
for(j in 1:length(data)){
  pmat[,j]<-probs(data,j)  
  
}
infmat<--log2(pmat)
return(infmat)
}

  #Function for calculating similarities between respondents
  #The function uses Lin's (1998) information theoretical measure. 
simRes<-function(i,j,infmat,d){
  y<-infmat[i,]
  overlap<-sum(y[which(d[i,1:12]==d[j,1:12])])
  sinfi<-sum(infmat[i,])
  sinfj<-sum(infmat[j,])
  sim<-2*overlap/(sinfi+sinfj)
  return(sim)
}

  #Function for calculating similarity between k'th respondent and everyone else
simResk<-function(k,inf,d){
    simVec<-vector()
  for(i in 1:length(resp)){
    simVec[i]<-simRes(k,i,inf,d)
  }
  return(simVec)
}

  #Function for making similarity matrix. 
simMatrix<-function(d){
  inf<-pmat(d)
  similarityMatrix<-matrix(data=0,ncol=length(d[,1]),nrow=length(d[,1]))
  for(i in 1:length(resp)){
    similarityMatrix[,i]<-simResk(i,inf,d)  
    
  }
  return(similarityMatrix)
}

  #Functions are now used to create a post similarity network
postSim<-simMatrix(post)
respTot<-resp
respT<-resp[mydata$control=="n"]
respC<-resp[mydata$control=="y"]

resp<-respT
preSimT<-simMatrix(pre[mydata$control=="n",])
postSimT<-simMatrix(post[mydata$control=="n",])

preNetT<-graph.adjacency(preSimT,diag=F,weighted=T)
postNetT<-graph.adjacency(postSimT,diag=F,weighted=T)
V(preNetT)$id<-respT
V(postNetT)$id<-respT

#Extract backbone networks
preNetBBT<-backboneNetwork(preNetT,0.0176,2) #Alpha value set to minimum value that keeps the network connected
postNetBBT<-backboneNetwork(postNetT,0.0335,2)# This is the network we focus on in the article.
write.graph(postNetBBT,"postNetBBT.net",format=c("pajek"))
write.graph(preNetBBT,"preNetBBT.net",format=c("pajek"))


#1000 infomaps
#CODE FOR WRITING 1000 INFOMAPS IN INFOMAP FOLDER:
#for i in {1..1000}; do ./pathToNetwork/[name].net /pathtoFolder/1000Infomaps[name]/
#--out-name $i -2 --clu --map -s$i; done
files<-vector()
for (i in 1:1000){
  files[i]<-paste("Data/1000InfomapsPreBBT/",i,".clu",sep = "")
  
}
#setwd("..") #go to where the files are
csvs<-lapply(files,read.csv,skip=2,header=F,sep="")

a<-matrix(data=0,nrow=1000,ncol=64)
for (i in 1:1000){
  a[i,]<-csvs[[i]][ order(csvs[[i]][,1]),2 ]
  
}

comp<-function(j){
  nmi<-vector()
  for(i in 1:1000){
    nmi[i]<-compare(a[i,],a[j,],method="nmi")  
    
  }
  return(nmi)  
} 

nmiM<-matrix(data=0,nrow=1000,ncol=1000)
for(i in 1:1000){
  nmiM[i,]<-comp(i)
  
}

mean(nmiM) #Normalized mutual information of pre-cluster solutions
sd(nmiM) #Standard deviation of NMI

files<-vector()
for (i in 1:1000){
  files[i]<-paste("Data/1000InfomapsPostBBT/",i,".clu",sep = "")
  
}
csvs<-lapply(files,read.csv,skip=2,header=F,sep="")

a<-matrix(data=0,nrow=1000,ncol=64)
for (i in 1:1000){
  a[i,]<-csvs[[i]][ order(csvs[[i]][,1]),2 ]
  
}

comp<-function(j){
  nmi<-vector()
  for(i in 1:1000){
    nmi[i]<-compare(a[i,],a[j,],method="nmi")  
    
  }
  return(nmi)  
} 

nmiM<-matrix(data=0,nrow=1000,ncol=1000)
for(i in 1:1000){
  nmiM[i,]<-comp(i)
  
}

mean(nmiM)#Normalized mutual information of post-cluster solutions
sd(nmiM)#Standard deviation of NMI


#Get most frequent groupings
postBBTgroup<-read.csv("Data/1000InfomapsPostBBT/1.clu",skip=2,header=F,sep="") #for us it was the first grouping.
postBBTgroup<-postBBTgroup[order(postBBTgroup$V1),]
preBBTgroup<-read.csv("Data/1000InfomapsPreBBT/2.clu",skip=2,header=F,sep="") #for us it was the first grouping.
preBBTgroup<-preBBTgroup[order(preBBTgroup$V1),]
compare(preBBTgroup$V2,postBBTgroup$V2,method="nmi") 

attributesT<-read.csv("attributesT.csv")

#ANALYSIS
attributes<-data.frame(mydata$ID,control=mydata$control,country=mydata$country,yearsTeaching=mydata$yearsTeaching,physics=mydata$Physics,chemistry=mydata$Chemistry,technology=mydata$Technology,biology=mydata$Biology,mathematics=mydata$Math,integrated=mydata$Integrated)
attributes$subjects<-do.call(paste,c(attributes[,5:10],sep=""))

before<-rowMeans(mydata[,3:14],na.rm=T)
after<-rowMeans(mydata[,15:26],na.rm=T)

beforeT<-before[mydata$control=="n"]
afterT<-after[mydata$control=="n"]

beforeC<-before[mydata$control=="y"]
afterC<-after[mydata$control=="y"]

attributesT<-attributes[mydata$control=="n",]

attributesT$preGroup<-preBBTgroup$V2
attributesT$postGroup<-postBBTgroup$V2
attributesT$preScore<-beforeT
attributesT$postScore<-afterT

#BETWEEN AND WITHIN GROUP SIMILARITY
#post BBT network

gSimT<-matrix(data=0,ncol=12,nrow=12)
groupSim<-function(i){
  groupsimVec<-vector()
for (j in 1:12)
  groupsimVec[j]<-mean(postSimT[which(attributesT$postGroup==i),which(attributesT$postGroup==j)])
return(groupsimVec)
}
for (k in 1:12){
gSimT[k,]<-groupSim(k)
}

gsim<-list()
for (i in 1:12){
  gsim[[i]]<-postSimT[which(attributesT$postGroup==i),which(attributesT$postGroup==i)][upper.tri(postSimT[which(attributesT$postGroup==i),which(attributesT$postGroup==i)])]
}

gdiag<-vector()
for (i in 1:12){
  gdiag[i]<-mean(gsim[[i]])
}

supergroups<-vector(length = 64)
supergroups[attributesT$postGroup==5|attributesT$postGroup==7|attributesT$postGroup==9|attributesT$postGroup==11|attributesT$postGroup==12]<-"A"
supergroups[attributesT$postGroup==1|attributesT$postGroup==2|attributesT$postGroup==3|attributesT$postGroup==4]<-"B"
supergroups[attributesT$postGroup==6|attributesT$postGroup==8|attributesT$postGroup==10]<-"C"


supergroupsWithC<-rep("D",93)
supergroupsWithC[attributesT$mydata.ID[as.character(supergroups)=="A"]]<-"A"
supergroupsWithC[attributesT$mydata.ID[supergroups=="B"]]<-"B"
supergroupsWithC[attributesT$mydata.ID[supergroups=="C"]]<-"C"

#REPEATED MEASURES ANOVA
resp<-c(1:93)
preT<-data.frame(respondent=resp[mydata$control=="n"],group=supergroups,score=beforeT,condition="pre")
preC<-data.frame(respondent=resp[mydata$control=="y"],group="D",score=beforeC,condition="pre")
pre<-rbind(preT,preC)

postT<-data.frame(respondent=resp[mydata$control=="n"],group=supergroups,score=afterT,condition="post")
postC<-data.frame(respondent=resp[mydata$control=="y"],group="D",score=afterC,condition="post")
post<-rbind(postT,postC)
tot<-rbind(pre,post)
tot<-tot[-c(70,77,163,170),]
se.aov <- with(tot, aov(score ~ condition * group + Error(respondent / (condition * group))))
summary(se.aov)

treat<-rbind(preT,postT)
set.aov <- with(treat, aov(score ~ condition * group + Error(respondent / (condition * group))))
summary(set.aov)

#ANCOVA
plot(beforeT[supergroups=="A"],afterT[supergroups=="A"],xlim=c(1,5),ylim=c(1,5))
points(beforeT[supergroups=="B"],afterT[supergroups=="B"],xlim=c(1,5),ylim=c(1,5),col="red")
points(beforeT[supergroups=="C"],afterT[supergroups=="C"],xlim=c(1,5),ylim=c(1,5),col="blue")

cor.test(beforeT[supergroups=="A"],afterT[supergroups=="A"])
cor.test(beforeT[supergroups=="B"],afterT[supergroups=="B"])
cor.test(beforeT[supergroups=="C"],afterT[supergroups=="C"])
cor.test(beforeC,afterC)

tot<-data.frame(resp,group=pre$group,prescore=pre$score,postscore=post$score)
assump1<-aov(postscore~prescore++group+group:prescore,data=tot)
summary(assump1) #Assumption 1: Equality of slopes-interation is not significant

plot(tot$prescore,tot$postscore,col=tot$group,xlab="prescore",ylab="postscore")

#The linear models (except D) have slopes of 0.3-0.4 with std err of 0.1 and above. 
summary(lm(tot$postscore[tot$group=='A']~tot$prescore[tot$group=='A']))
summary(lm(tot$postscore[tot$group=='B']~tot$prescore[tot$group=='B']))
summary(lm(tot$postscore[tot$group=='C']~tot$prescore[tot$group=='C']))
summary(lm(tot$postscore[tot$group=='D']~tot$prescore[tot$group=='D']))

#Equality of groups on the covariate is violated. 
summary(aov(prescore~group, data=tot))

#Homogeneity of variance seems ot be supported
library(car)
library(gplots)
leveneTest(postscore~group,center=mean,data=tot)

#ANOVA BOOTSTRAP
library(boot)

# function to obtain regression weights
bs <- function(formula, data, indices) {
  d <- data[indices,] # allows boot to select sample
  fit <- aov(formula, data=d)
  return(coef(fit))
}
postSG<-data.frame(score=attributesT$postScore,supergroups)
# bootstrapping with 1000 replications
results <- boot(data=postSG, statistic=bs,
                R=10000, formula=score~supergroups)

# view results
results
plot(results, index=1) # A?
plot(results, index=2) # B?
plot(results, index=3) # C?

# get 95% confidence intervals
boot.ci(results, type="bca", index=1) # intercept
boot.ci(results, type="bca", index=2) # wt
boot.ci(results, type="bca", index=3) # disp 

aov.x<-aov(afterT~supergroups)
aov.y<-aov(beforeT~supergroups)
summary(aov(attributesT$postScore~supergroups))
boxplot2(afterT~supergroups)
TukeyHSD(aov(afterT~supergroups))

#ANOVAS


newdata<-mydata[mydata$control=="n",]
summary(aov(newdata$pre38~supergroups))
TukeyHSD(aov(newdata$pre38~supergroups))
summary(aov(newdata$post38~supergroups))
TukeyHSD(aov(newdata$post38~supergroups))
summary(aov(newdata$pre39~supergroups))
TukeyHSD(aov(newdata$pre39~supergroups))
summary(aov(newdata$post39~supergroups))
TukeyHSD(aov(newdata$post39~supergroups))
summary(aov(newdata$pre40~supergroups))
TukeyHSD(aov(newdata$pre40~supergroups))
summary(aov(newdata$post40~supergroups))
TukeyHSD(aov(newdata$post40~supergroups))
summary(aov(newdata$pre41~supergroups))
TukeyHSD(aov(newdata$pre41~supergroups))
summary(aov(newdata$post41~supergroups))
TukeyHSD(aov(newdata$post41~supergroups))
summary(aov(newdata$pre42~supergroups))
TukeyHSD(aov(newdata$pre42~supergroups))
summary(aov(newdata$post42~supergroups))
TukeyHSD(aov(newdata$post42~supergroups))
summary(aov(newdata$pre43~supergroups))
TukeyHSD(aov(newdata$pre43~supergroups))
summary(aov(newdata$post43~supergroups))
TukeyHSD(aov(newdata$post43~supergroups))
summary(aov(newdata$pre44~supergroups))
TukeyHSD(aov(newdata$pre44~supergroups))
summary(aov(newdata$post44~supergroups))
TukeyHSD(aov(newdata$post44~supergroups))
summary(aov(newdata$pre45~supergroups))
TukeyHSD(aov(newdata$pre45~supergroups))
summary(aov(newdata$post45~supergroups))
TukeyHSD(aov(newdata$post45~supergroups))

summary(aov(newdata$pre46~supergroups))
TukeyHSD(aov(newdata$pre46~supergroups))
summary(aov(newdata$post46~supergroups))
TukeyHSD(aov(newdata$post46~supergroups))

summary(aov(newdata$pre47~supergroups))
TukeyHSD(aov(newdata$pre47~supergroups))
summary(aov(newdata$post47~supergroups))
TukeyHSD(aov(newdata$post47~supergroups))

summary(aov(newdata$pre48~supergroups))
TukeyHSD(aov(newdata$pre48~supergroups))
summary(aov(newdata$post48~supergroups))
TukeyHSD(aov(newdata$post48~supergroups))

summary(aov(newdata$pre49~supergroups))
TukeyHSD(aov(newdata$pre49~supergroups))
summary(aov(newdata$post49~supergroups))
TukeyHSD(aov(newdata$post49~supergroups))

#####
plot(density(afterC,na.rm = T))
plot(density(attributesT$preScore[supergroups=="A"]))
plot(density(attributesT$preScore[supergroups=="B"]))
plot(density(attributesT$preScore[supergroups=="C"]))
plot(density(attributesT$preScore,na.rm=T))

preMeans<-vector()
preUnc<-vector()
preMeans[1]<-mean(beforeC,na.rm=T)
preUnc[1]<-1.96*sd(beforeC,na.rm=T)/sqrt(length(afterC))
preMeans[2]<-mean(attributesT$preScore[supergroups=="A"],na.rm=T)
preUnc[2]<-1.96*sd(attributesT$preScore[supergroups=="A"],na.rm=T)/sqrt(length(supergroups[supergroups=="A"]))
preMeans[3]<-mean(attributesT$preScore[supergroups=="B"],na.rm=T)
preUnc[3]<-1.96*sd(attributesT$preScore[supergroups=="B"],na.rm=T)/sqrt(length(supergroups[supergroups=="B"]))
preMeans[4]<-mean(attributesT$preScore[supergroups=="C"],na.rm=T)
preUnc[4]<-1.96*sd(attributesT$preScore[supergroups=="C"],na.rm=T)/sqrt(length(supergroups[supergroups=="C"]))
preMeans[5]<-mean(attributesT$preScore,na.rm=T)
preUnc[5]<-1.96*sd(attributesT$preScore,na.rm=T)/sqrt(length(supergroups))


postMeans<-vector()
postUnc<-vector()
postMeans[1]<-mean(afterC,na.rm=T)
postUnc[1]<-1.96*sd(afterC,na.rm=T)/sqrt(length(afterC))
postMeans[2]<-mean(attributesT$postScore[supergroups=="A"],na.rm=T)
postUnc[2]<-1.96*sd(attributesT$postScore[supergroups=="A"],na.rm=T)/sqrt(length(supergroups[supergroups=="A"]))
postMeans[3]<-mean(attributesT$postScore[supergroups=="B"],na.rm=T)
postUnc[3]<-1.96*sd(attributesT$postScore[supergroups=="B"],na.rm=T)/sqrt(length(supergroups[supergroups=="B"]))
postMeans[4]<-mean(attributesT$postScore[supergroups=="C"],na.rm=T)
postUnc[4]<-1.96*sd(attributesT$postScore[supergroups=="C"],na.rm=T)/sqrt(length(supergroups[supergroups=="C"]))
postMeans[5]<-mean(attributesT$postScore,na.rm=T)
postUnc[5]<-1.96*sd(attributesT$postScore,na.rm=T)/sqrt(length(supergroups))

means<-data.frame(preMeans,postMeans)
uncs<-data.frame(preUnc,postUnc)
x<-c(1.2,1.8)
plot(x,means[1,],xlim=c(1,2),ylim=c(1,5),pch=1,axes=F,ann=F)
arrows(x, as.numeric(means[1,])-as.numeric(uncs[1,]), x, as.numeric(means[1,])+as.numeric(uncs[1,]), length=0.05, angle=90, code=3)
segments(x[1], means[1,1],x[2],means[1,2])
lines(x,means[5,], type="o", pch=2, lty=5)
arrows(x, as.numeric(means[5,])-as.numeric(uncs[5,]), x, as.numeric(means[5,])+as.numeric(uncs[5,]), length=0.05, angle=90, code=3)
axis(1, at=c(1.2,1.8), lab=c("Pre","Post"),cex=1.5)
axis(2, las=1, at=1:5,cex=1)
legend(1, 5, c("Control","All Participants"), pch=1:2, lty=c(1,5))
box()

plot(x,means[2,],xlim=c(1,2),ylim=c(1,5), type="o", pch=5, lty=2,axes=F,ann=F)
arrows(x, as.numeric(means[2,])-as.numeric(uncs[2,]), x, as.numeric(means[2,])+as.numeric(uncs[2,]), length=0.05, angle=90, code=3)
segments(x[1], means[1,1],x[2],means[1,2])
segments(x[1], means[1,1]-uncs[1,1],x[2],means[1,2]-uncs[1,2])
segments(x[1], means[1,1]+uncs[1,1],x[2],means[1,2]+uncs[1,2])

lines(x,means[3,], type="o", pch=6, lty=3)
arrows(x, as.numeric(means[3,])-as.numeric(uncs[3,]), x, as.numeric(means[3,])+as.numeric(uncs[3,]), length=0.05, angle=90, code=3)
lines(x,means[4,], type="o", pch=7, lty=4)
arrows(x, as.numeric(means[4,])-as.numeric(uncs[4,]), x, as.numeric(means[4,])+as.numeric(uncs[4,]), length=0.05, angle=90, code=3)

axis(1, at=c(1.2,1.8), lab=c("Pre","Post"))
axis(2, las=1, at=1:5)
legend(1, 5, c("Super group A","Super group B", "Super group C"), pch=5:7, lty=2:4)


box()

newdata<-mydata[mydata$control=="n",]
Q38prem<-vector()
Q38preu<-vector()
Q38prem[1]<-mean(newdata$pre38[supergroups=="A"],na.rm=T)
Q38prem[2]<-mean(newdata$pre38[supergroups=="B"],na.rm=T)
Q38prem[3]<-mean(newdata$pre38[supergroups=="C"],na.rm=T)

Q38preu[1]<-1.96*sd(newdata$pre38[supergroups=="A"],na.rm=T)/sqrt(sqrt(length(supergroups[supergroups=="A"])))
Q38preu[2]<-1.96*sd(newdata$pre38[supergroups=="B"],na.rm=T)/sqrt(sqrt(length(supergroups[supergroups=="B"])))
Q38preu[3]<-1.96*sd(newdata$pre38[supergroups=="C"],na.rm=T)/sqrt(sqrt(length(supergroups[supergroups=="C"])))

Q38postm<-vector()
Q38postu<-vector()
Q38postm[1]<-mean(newdata$post38[supergroups=="A"],na.rm=T)
Q38postm[2]<-mean(newdata$post38[supergroups=="B"],na.rm=T)
Q38postm[3]<-mean(newdata$post38[supergroups=="C"],na.rm=T)

Q38postu[1]<-1.96*sd(newdata$post38[supergroups=="A"],na.rm=T)/sqrt(sqrt(length(supergroups[supergroups=="A"])))
Q38postu[2]<-1.96*sd(newdata$post38[supergroups=="B"],na.rm=T)/sqrt(sqrt(length(supergroups[supergroups=="B"])))
Q38postu[3]<-1.96*sd(newdata$post38[supergroups=="C"],na.rm=T)/sqrt(sqrt(length(supergroups[supergroups=="C"])))

means<-data.frame(Q38prem,Q38postm)
uncs<-data.frame(Q38preu,Q38postu)
x<-c(1.05,1.65)
plot(x,means[1,],xlim=c(1,2),ylim=c(1,5), type="o", pch=5, lty=2,axes=F,ann=F)
arrows(x, as.numeric(means[1,])-as.numeric(uncs[1,]), x, as.numeric(means[1,])+as.numeric(uncs[1,]), length=0.05, angle=90, code=3)

lines(x+.03,means[2,], type="o", pch=6, lty=3)
arrows(x+.03, as.numeric(means[2,])-as.numeric(uncs[2,]), x+.03, as.numeric(means[2,])+as.numeric(uncs[2,]), length=0.05, angle=90, code=3)
lines(x-.03,means[3,], type="o", pch=7, lty=4)
arrows(x-.03, as.numeric(means[3,])-as.numeric(uncs[3,]), x-.03, as.numeric(means[3,])+as.numeric(uncs[3,]), length=0.05, angle=90, code=3)

axis(1, at=x, lab=c("Pre","Post"))
axis(2, las=1, at=1:5)
title(main="Q38")
legend(1.75, 5, c("Super group A","Super group B", "Super group C"), pch=5:7, lty=2:4)

###Q39###
Q39prem<-vector()
Q39preu<-vector()
Q39prem[1]<-mean(newdata$pre39[supergroups=="A"],na.rm=T)
Q39prem[2]<-mean(newdata$pre39[supergroups=="B"],na.rm=T)
Q39prem[3]<-mean(newdata$pre39[supergroups=="C"],na.rm=T)

Q39preu[1]<-1.96*sd(newdata$pre39[supergroups=="A"],na.rm=T)/sqrt(sqrt(length(supergroups[supergroups=="A"])))
Q39preu[2]<-1.96*sd(newdata$pre39[supergroups=="B"],na.rm=T)/sqrt(sqrt(length(supergroups[supergroups=="B"])))
Q39preu[3]<-1.96*sd(newdata$pre39[supergroups=="C"],na.rm=T)/sqrt(sqrt(length(supergroups[supergroups=="C"])))

Q39postm<-vector()
Q39postu<-vector()
Q39postm[1]<-mean(newdata$post39[supergroups=="A"],na.rm=T)
Q39postm[2]<-mean(newdata$post39[supergroups=="B"],na.rm=T)
Q39postm[3]<-mean(newdata$post39[supergroups=="C"],na.rm=T)

Q39postu[1]<-1.96*sd(newdata$post39[supergroups=="A"],na.rm=T)/sqrt(sqrt(length(supergroups[supergroups=="A"])))
Q39postu[2]<-1.96*sd(newdata$post39[supergroups=="B"],na.rm=T)/sqrt(sqrt(length(supergroups[supergroups=="B"])))
Q39postu[3]<-1.96*sd(newdata$post39[supergroups=="C"],na.rm=T)/sqrt(sqrt(length(supergroups[supergroups=="C"])))

means<-data.frame(Q39prem,Q39postm)
uncs<-data.frame(Q39preu,Q39postu)
x<-c(1.05,1.65)
plot(x,means[1,],xlim=c(1,2),ylim=c(1,5), type="o", pch=5, lty=2,axes=F,ann=F)
arrows(x, as.numeric(means[1,])-as.numeric(uncs[1,]), x, as.numeric(means[1,])+as.numeric(uncs[1,]), length=0.05, angle=90, code=3)

lines(x+.03,means[2,], type="o", pch=6, lty=3)
arrows(x+.03, as.numeric(means[2,])-as.numeric(uncs[2,]), x+.03, as.numeric(means[2,])+as.numeric(uncs[2,]), length=0.05, angle=90, code=3)
lines(x-.03,means[3,], type="o", pch=7, lty=4)
arrows(x-.03, as.numeric(means[3,])-as.numeric(uncs[3,]), x-.03, as.numeric(means[3,])+as.numeric(uncs[3,]), length=0.05, angle=90, code=3)

axis(1, at=x, lab=c("Pre","Post"))
axis(2, las=1, at=1:5)
title(main="Q39")
legend(1.75, 5, c("Super group A","Super group B", "Super group C"), pch=5:7, lty=2:4)

###Q40###
Q40prem<-vector()
Q40preu<-vector()
Q40prem[1]<-mean(newdata$pre40[supergroups=="A"],na.rm=T)
Q40prem[2]<-mean(newdata$pre40[supergroups=="B"],na.rm=T)
Q40prem[3]<-mean(newdata$pre40[supergroups=="C"],na.rm=T)

Q40preu[1]<-1.96*sd(newdata$pre40[supergroups=="A"],na.rm=T)/sqrt(sqrt(length(supergroups[supergroups=="A"])))
Q40preu[2]<-1.96*sd(newdata$pre40[supergroups=="B"],na.rm=T)/sqrt(sqrt(length(supergroups[supergroups=="B"])))
Q40preu[3]<-1.96*sd(newdata$pre40[supergroups=="C"],na.rm=T)/sqrt(sqrt(length(supergroups[supergroups=="C"])))

Q40postm<-vector()
Q40postu<-vector()
Q40postm[1]<-mean(newdata$post40[supergroups=="A"],na.rm=T)
Q40postm[2]<-mean(newdata$post40[supergroups=="B"],na.rm=T)
Q40postm[3]<-mean(newdata$post40[supergroups=="C"],na.rm=T)

Q40postu[1]<-1.96*sd(newdata$post40[supergroups=="A"],na.rm=T)/sqrt(sqrt(length(supergroups[supergroups=="A"])))
Q40postu[2]<-1.96*sd(newdata$post40[supergroups=="B"],na.rm=T)/sqrt(sqrt(length(supergroups[supergroups=="B"])))
Q40postu[3]<-1.96*sd(newdata$post40[supergroups=="C"],na.rm=T)/sqrt(sqrt(length(supergroups[supergroups=="C"])))

means<-data.frame(Q40prem,Q40postm)
uncs<-data.frame(Q40preu,Q40postu)
x<-c(1.05,1.65)
plot(x,means[1,],xlim=c(1,2),ylim=c(1,5), type="o", pch=5, lty=2,axes=F,ann=F)
arrows(x, as.numeric(means[1,])-as.numeric(uncs[1,]), x, as.numeric(means[1,])+as.numeric(uncs[1,]), length=0.05, angle=90, code=3)

lines(x+.03,means[2,], type="o", pch=6, lty=3)
arrows(x+.03, as.numeric(means[2,])-as.numeric(uncs[2,]), x+.03, as.numeric(means[2,])+as.numeric(uncs[2,]), length=0.05, angle=90, code=3)
lines(x-.03,means[3,], type="o", pch=7, lty=4)
arrows(x-.03, as.numeric(means[3,])-as.numeric(uncs[3,]), x-.03, as.numeric(means[3,])+as.numeric(uncs[3,]), length=0.05, angle=90, code=3)

axis(1, at=x, lab=c("Pre","Post"))
axis(2, las=1, at=1:5)
title(main="Q40")
legend(1.75, 5, c("Super group A","Super group B", "Super group C"), pch=5:7, lty=2:4)

###Q41###
Q41prem<-vector()
Q41preu<-vector()
Q41prem[1]<-mean(newdata$pre41[supergroups=="A"],na.rm=T)
Q41prem[2]<-mean(newdata$pre41[supergroups=="B"],na.rm=T)
Q41prem[3]<-mean(newdata$pre41[supergroups=="C"],na.rm=T)

Q41preu[1]<-1.96*sd(newdata$pre41[supergroups=="A"],na.rm=T)/sqrt(sqrt(length(supergroups[supergroups=="A"])))
Q41preu[2]<-1.96*sd(newdata$pre41[supergroups=="B"],na.rm=T)/sqrt(sqrt(length(supergroups[supergroups=="B"])))
Q41preu[3]<-1.96*sd(newdata$pre41[supergroups=="C"],na.rm=T)/sqrt(sqrt(length(supergroups[supergroups=="C"])))

Q41postm<-vector()
Q41postu<-vector()
Q41postm[1]<-mean(newdata$post41[supergroups=="A"],na.rm=T)
Q41postm[2]<-mean(newdata$post41[supergroups=="B"],na.rm=T)
Q41postm[3]<-mean(newdata$post41[supergroups=="C"],na.rm=T)

Q41postu[1]<-1.96*sd(newdata$post41[supergroups=="A"],na.rm=T)/sqrt(sqrt(length(supergroups[supergroups=="A"])))
Q41postu[2]<-1.96*sd(newdata$post41[supergroups=="B"],na.rm=T)/sqrt(sqrt(length(supergroups[supergroups=="B"])))
Q41postu[3]<-1.96*sd(newdata$post41[supergroups=="C"],na.rm=T)/sqrt(sqrt(length(supergroups[supergroups=="C"])))

means<-data.frame(Q41prem,Q41postm)
uncs<-data.frame(Q41preu,Q41postu)
x<-c(1.05,1.65)
plot(x,means[1,],xlim=c(1,2),ylim=c(1,5), type="o", pch=5, lty=2,axes=F,ann=F)
arrows(x, as.numeric(means[1,])-as.numeric(uncs[1,]), x, as.numeric(means[1,])+as.numeric(uncs[1,]), length=0.05, angle=90, code=3)

lines(x+.03,means[2,], type="o", pch=6, lty=3)
arrows(x+.03, as.numeric(means[2,])-as.numeric(uncs[2,]), x+.03, as.numeric(means[2,])+as.numeric(uncs[2,]), length=0.05, angle=90, code=3)
lines(x-.03,means[3,], type="o", pch=7, lty=4)
arrows(x-.03, as.numeric(means[3,])-as.numeric(uncs[3,]), x-.03, as.numeric(means[3,])+as.numeric(uncs[3,]), length=0.05, angle=90, code=3)

axis(1, at=x, lab=c("Pre","Post"))
axis(2, las=1, at=1:5)
title(main="Q41")
legend(1.75, 5, c("Super group A","Super group B", "Super group C"), pch=5:7, lty=2:4)

###Q42###
Q42prem<-vector()
Q42preu<-vector()
Q42prem[1]<-mean(newdata$pre42[supergroups=="A"],na.rm=T)
Q42prem[2]<-mean(newdata$pre42[supergroups=="B"],na.rm=T)
Q42prem[3]<-mean(newdata$pre42[supergroups=="C"],na.rm=T)

Q42preu[1]<-1.96*sd(newdata$pre42[supergroups=="A"],na.rm=T)/sqrt(sqrt(length(supergroups[supergroups=="A"])))
Q42preu[2]<-1.96*sd(newdata$pre42[supergroups=="B"],na.rm=T)/sqrt(sqrt(length(supergroups[supergroups=="B"])))
Q42preu[3]<-1.96*sd(newdata$pre42[supergroups=="C"],na.rm=T)/sqrt(sqrt(length(supergroups[supergroups=="C"])))

Q42postm<-vector()
Q42postu<-vector()
Q42postm[1]<-mean(newdata$post42[supergroups=="A"],na.rm=T)
Q42postm[2]<-mean(newdata$post42[supergroups=="B"],na.rm=T)
Q42postm[3]<-mean(newdata$post42[supergroups=="C"],na.rm=T)

Q42postu[1]<-1.96*sd(newdata$post42[supergroups=="A"],na.rm=T)/sqrt(sqrt(length(supergroups[supergroups=="A"])))
Q42postu[2]<-1.96*sd(newdata$post42[supergroups=="B"],na.rm=T)/sqrt(sqrt(length(supergroups[supergroups=="B"])))
Q42postu[3]<-1.96*sd(newdata$post42[supergroups=="C"],na.rm=T)/sqrt(sqrt(length(supergroups[supergroups=="C"])))

means<-data.frame(Q42prem,Q42postm)
uncs<-data.frame(Q42preu,Q42postu)
x<-c(1.05,1.65)
plot(x,means[1,],xlim=c(1,2),ylim=c(1,5), type="o", pch=5, lty=2,axes=F,ann=F)
arrows(x, as.numeric(means[1,])-as.numeric(uncs[1,]), x, as.numeric(means[1,])+as.numeric(uncs[1,]), length=0.05, angle=90, code=3)

lines(x+.03,means[2,], type="o", pch=6, lty=3)
arrows(x+.03, as.numeric(means[2,])-as.numeric(uncs[2,]), x+.03, as.numeric(means[2,])+as.numeric(uncs[2,]), length=0.05, angle=90, code=3)
lines(x-.03,means[3,], type="o", pch=7, lty=4)
arrows(x-.03, as.numeric(means[3,])-as.numeric(uncs[3,]), x-.03, as.numeric(means[3,])+as.numeric(uncs[3,]), length=0.05, angle=90, code=3)

axis(1, at=x, lab=c("Pre","Post"))
axis(2, las=1, at=1:5)
title(main="Q42")
legend(1.75, 5, c("Super group A","Super group B", "Super group C"), pch=5:7, lty=2:4)

###Q43###
Q43prem<-vector()
Q43preu<-vector()
Q43prem[1]<-mean(newdata$pre43[supergroups=="A"],na.rm=T)
Q43prem[2]<-mean(newdata$pre43[supergroups=="B"],na.rm=T)
Q43prem[3]<-mean(newdata$pre43[supergroups=="C"],na.rm=T)

Q43preu[1]<-1.96*sd(newdata$pre43[supergroups=="A"],na.rm=T)/sqrt(sqrt(length(supergroups[supergroups=="A"])))
Q43preu[2]<-1.96*sd(newdata$pre43[supergroups=="B"],na.rm=T)/sqrt(sqrt(length(supergroups[supergroups=="B"])))
Q43preu[3]<-1.96*sd(newdata$pre43[supergroups=="C"],na.rm=T)/sqrt(sqrt(length(supergroups[supergroups=="C"])))

Q43postm<-vector()
Q43postu<-vector()
Q43postm[1]<-mean(newdata$post43[supergroups=="A"],na.rm=T)
Q43postm[2]<-mean(newdata$post43[supergroups=="B"],na.rm=T)
Q43postm[3]<-mean(newdata$post43[supergroups=="C"],na.rm=T)

Q43postu[1]<-1.96*sd(newdata$post43[supergroups=="A"],na.rm=T)/sqrt(sqrt(length(supergroups[supergroups=="A"])))
Q43postu[2]<-1.96*sd(newdata$post43[supergroups=="B"],na.rm=T)/sqrt(sqrt(length(supergroups[supergroups=="B"])))
Q43postu[3]<-1.96*sd(newdata$post43[supergroups=="C"],na.rm=T)/sqrt(sqrt(length(supergroups[supergroups=="C"])))

means<-data.frame(Q43prem,Q43postm)
uncs<-data.frame(Q43preu,Q43postu)
x<-c(1.05,1.65)
plot(x,means[1,],xlim=c(1,2),ylim=c(1,5), type="o", pch=5, lty=2,axes=F,ann=F)
arrows(x, as.numeric(means[1,])-as.numeric(uncs[1,]), x, as.numeric(means[1,])+as.numeric(uncs[1,]), length=0.05, angle=90, code=3)

lines(x+.03,means[2,], type="o", pch=6, lty=3)
arrows(x+.03, as.numeric(means[2,])-as.numeric(uncs[2,]), x+.03, as.numeric(means[2,])+as.numeric(uncs[2,]), length=0.05, angle=90, code=3)
lines(x-.03,means[3,], type="o", pch=7, lty=4)
arrows(x-.03, as.numeric(means[3,])-as.numeric(uncs[3,]), x-.03, as.numeric(means[3,])+as.numeric(uncs[3,]), length=0.05, angle=90, code=3)

axis(1, at=x, lab=c("Pre","Post"))
axis(2, las=1, at=1:5)
title(main="Q43")
legend(1.75, 5, c("Super group A","Super group B", "Super group C"), pch=5:7, lty=2:4)

###Q44###
Q44prem<-vector()
Q44preu<-vector()
Q44prem[1]<-mean(newdata$pre44[supergroups=="A"],na.rm=T)
Q44prem[2]<-mean(newdata$pre44[supergroups=="B"],na.rm=T)
Q44prem[3]<-mean(newdata$pre44[supergroups=="C"],na.rm=T)

Q44preu[1]<-1.96*sd(newdata$pre44[supergroups=="A"],na.rm=T)/sqrt(sqrt(length(supergroups[supergroups=="A"])))
Q44preu[2]<-1.96*sd(newdata$pre44[supergroups=="B"],na.rm=T)/sqrt(sqrt(length(supergroups[supergroups=="B"])))
Q44preu[3]<-1.96*sd(newdata$pre44[supergroups=="C"],na.rm=T)/sqrt(sqrt(length(supergroups[supergroups=="C"])))

Q44postm<-vector()
Q44postu<-vector()
Q44postm[1]<-mean(newdata$post44[supergroups=="A"],na.rm=T)
Q44postm[2]<-mean(newdata$post44[supergroups=="B"],na.rm=T)
Q44postm[3]<-mean(newdata$post44[supergroups=="C"],na.rm=T)

Q44postu[1]<-1.96*sd(newdata$post44[supergroups=="A"],na.rm=T)/sqrt(sqrt(length(supergroups[supergroups=="A"])))
Q44postu[2]<-1.96*sd(newdata$post44[supergroups=="B"],na.rm=T)/sqrt(sqrt(length(supergroups[supergroups=="B"])))
Q44postu[3]<-1.96*sd(newdata$post44[supergroups=="C"],na.rm=T)/sqrt(sqrt(length(supergroups[supergroups=="C"])))

means<-data.frame(Q44prem,Q44postm)
uncs<-data.frame(Q44preu,Q44postu)
x<-c(1.05,1.65)
plot(x,means[1,],xlim=c(1,2),ylim=c(1,5), type="o", pch=5, lty=2,axes=F,ann=F)
arrows(x, as.numeric(means[1,])-as.numeric(uncs[1,]), x, as.numeric(means[1,])+as.numeric(uncs[1,]), length=0.05, angle=90, code=3)

lines(x+.03,means[2,], type="o", pch=6, lty=3)
arrows(x+.03, as.numeric(means[2,])-as.numeric(uncs[2,]), x+.03, as.numeric(means[2,])+as.numeric(uncs[2,]), length=0.05, angle=90, code=3)
lines(x-.03,means[3,], type="o", pch=7, lty=4)
arrows(x-.03, as.numeric(means[3,])-as.numeric(uncs[3,]), x-.03, as.numeric(means[3,])+as.numeric(uncs[3,]), length=0.05, angle=90, code=3)

axis(1, at=x, lab=c("Pre","Post"))
axis(2, las=1, at=1:5)
title(main="Q44")
legend(1.75, 5, c("Super group A","Super group B", "Super group C"), pch=5:7, lty=2:4)

###Q45###
Q45prem<-vector()
Q45preu<-vector()
Q45prem[1]<-mean(newdata$pre45[supergroups=="A"],na.rm=T)
Q45prem[2]<-mean(newdata$pre45[supergroups=="B"],na.rm=T)
Q45prem[3]<-mean(newdata$pre45[supergroups=="C"],na.rm=T)

Q45preu[1]<-1.96*sd(newdata$pre45[supergroups=="A"],na.rm=T)/sqrt(sqrt(length(supergroups[supergroups=="A"])))
Q45preu[2]<-1.96*sd(newdata$pre45[supergroups=="B"],na.rm=T)/sqrt(sqrt(length(supergroups[supergroups=="B"])))
Q45preu[3]<-1.96*sd(newdata$pre45[supergroups=="C"],na.rm=T)/sqrt(sqrt(length(supergroups[supergroups=="C"])))

Q45postm<-vector()
Q45postu<-vector()
Q45postm[1]<-mean(newdata$post45[supergroups=="A"],na.rm=T)
Q45postm[2]<-mean(newdata$post45[supergroups=="B"],na.rm=T)
Q45postm[3]<-mean(newdata$post45[supergroups=="C"],na.rm=T)

Q45postu[1]<-1.96*sd(newdata$post45[supergroups=="A"],na.rm=T)/sqrt(sqrt(length(supergroups[supergroups=="A"])))
Q45postu[2]<-1.96*sd(newdata$post45[supergroups=="B"],na.rm=T)/sqrt(sqrt(length(supergroups[supergroups=="B"])))
Q45postu[3]<-1.96*sd(newdata$post45[supergroups=="C"],na.rm=T)/sqrt(sqrt(length(supergroups[supergroups=="C"])))

means<-data.frame(Q45prem,Q45postm)
uncs<-data.frame(Q45preu,Q45postu)
x<-c(1.05,1.65)
plot(x,means[1,],xlim=c(1,2),ylim=c(1,5), type="o", pch=5, lty=2,axes=F,ann=F)
arrows(x, as.numeric(means[1,])-as.numeric(uncs[1,]), x, as.numeric(means[1,])+as.numeric(uncs[1,]), length=0.05, angle=90, code=3)

lines(x+.03,means[2,], type="o", pch=6, lty=3)
arrows(x+.03, as.numeric(means[2,])-as.numeric(uncs[2,]), x+.03, as.numeric(means[2,])+as.numeric(uncs[2,]), length=0.05, angle=90, code=3)
lines(x-.03,means[3,], type="o", pch=7, lty=4)
arrows(x-.03, as.numeric(means[3,])-as.numeric(uncs[3,]), x-.03, as.numeric(means[3,])+as.numeric(uncs[3,]), length=0.05, angle=90, code=3)

axis(1, at=x, lab=c("Pre","Post"))
axis(2, las=1, at=1:5)
title(main="Q45")
legend(1.75, 5, c("Super group A","Super group B", "Super group C"), pch=5:7, lty=2:4)


###Q46###
Q46prem<-vector()
Q46preu<-vector()
Q46prem[1]<-mean(newdata$pre46[supergroups=="A"],na.rm=T)
Q46prem[2]<-mean(newdata$pre46[supergroups=="B"],na.rm=T)
Q46prem[3]<-mean(newdata$pre46[supergroups=="C"],na.rm=T)

Q46preu[1]<-1.96*sd(newdata$pre46[supergroups=="A"],na.rm=T)/sqrt(sqrt(length(supergroups[supergroups=="A"])))
Q46preu[2]<-1.96*sd(newdata$pre46[supergroups=="B"],na.rm=T)/sqrt(sqrt(length(supergroups[supergroups=="B"])))
Q46preu[3]<-1.96*sd(newdata$pre46[supergroups=="C"],na.rm=T)/sqrt(sqrt(length(supergroups[supergroups=="C"])))

Q46postm<-vector()
Q46postu<-vector()
Q46postm[1]<-mean(newdata$post46[supergroups=="A"],na.rm=T)
Q46postm[2]<-mean(newdata$post46[supergroups=="B"],na.rm=T)
Q46postm[3]<-mean(newdata$post46[supergroups=="C"],na.rm=T)

Q46postu[1]<-1.96*sd(newdata$post46[supergroups=="A"],na.rm=T)/sqrt(sqrt(length(supergroups[supergroups=="A"])))
Q46postu[2]<-1.96*sd(newdata$post46[supergroups=="B"],na.rm=T)/sqrt(sqrt(length(supergroups[supergroups=="B"])))
Q46postu[3]<-1.96*sd(newdata$post46[supergroups=="C"],na.rm=T)/sqrt(sqrt(length(supergroups[supergroups=="C"])))

means<-data.frame(Q46prem,Q46postm)
uncs<-data.frame(Q46preu,Q46postu)
x<-c(1.05,1.65)
plot(x,means[1,],xlim=c(1,2),ylim=c(1,5), type="o", pch=5, lty=2,axes=F,ann=F)
arrows(x, as.numeric(means[1,])-as.numeric(uncs[1,]), x, as.numeric(means[1,])+as.numeric(uncs[1,]), length=0.05, angle=90, code=3)

lines(x+.03,means[2,], type="o", pch=6, lty=3)
arrows(x+.03, as.numeric(means[2,])-as.numeric(uncs[2,]), x+.03, as.numeric(means[2,])+as.numeric(uncs[2,]), length=0.05, angle=90, code=3)
lines(x-.03,means[3,], type="o", pch=7, lty=4)
arrows(x-.03, as.numeric(means[3,])-as.numeric(uncs[3,]), x-.03, as.numeric(means[3,])+as.numeric(uncs[3,]), length=0.05, angle=90, code=3)

axis(1, at=x, lab=c("Pre","Post"))
axis(2, las=1, at=1:5)
title(main="Q46")
legend(1.75, 5, c("Super group A","Super group B", "Super group C"), pch=5:7, lty=2:4)

###Q47###
Q47prem<-vector()
Q47preu<-vector()
Q47prem[1]<-mean(newdata$pre47[supergroups=="A"],na.rm=T)
Q47prem[2]<-mean(newdata$pre47[supergroups=="B"],na.rm=T)
Q47prem[3]<-mean(newdata$pre47[supergroups=="C"],na.rm=T)

Q47preu[1]<-1.96*sd(newdata$pre47[supergroups=="A"],na.rm=T)/sqrt(sqrt(length(supergroups[supergroups=="A"])))
Q47preu[2]<-1.96*sd(newdata$pre47[supergroups=="B"],na.rm=T)/sqrt(sqrt(length(supergroups[supergroups=="B"])))
Q47preu[3]<-1.96*sd(newdata$pre47[supergroups=="C"],na.rm=T)/sqrt(sqrt(length(supergroups[supergroups=="C"])))

Q47postm<-vector()
Q47postu<-vector()
Q47postm[1]<-mean(newdata$post47[supergroups=="A"],na.rm=T)
Q47postm[2]<-mean(newdata$post47[supergroups=="B"],na.rm=T)
Q47postm[3]<-mean(newdata$post47[supergroups=="C"],na.rm=T)

Q47postu[1]<-1.96*sd(newdata$post47[supergroups=="A"],na.rm=T)/sqrt(sqrt(length(supergroups[supergroups=="A"])))
Q47postu[2]<-1.96*sd(newdata$post47[supergroups=="B"],na.rm=T)/sqrt(sqrt(length(supergroups[supergroups=="B"])))
Q47postu[3]<-1.96*sd(newdata$post47[supergroups=="C"],na.rm=T)/sqrt(sqrt(length(supergroups[supergroups=="C"])))

means<-data.frame(Q47prem,Q47postm)
uncs<-data.frame(Q47preu,Q47postu)
x<-c(1.05,1.65)
plot(x,means[1,],xlim=c(1,2),ylim=c(1,5), type="o", pch=5, lty=2,axes=F,ann=F)
arrows(x, as.numeric(means[1,])-as.numeric(uncs[1,]), x, as.numeric(means[1,])+as.numeric(uncs[1,]), length=0.05, angle=90, code=3)

lines(x+.03,means[2,], type="o", pch=6, lty=3)
arrows(x+.03, as.numeric(means[2,])-as.numeric(uncs[2,]), x+.03, as.numeric(means[2,])+as.numeric(uncs[2,]), length=0.05, angle=90, code=3)
lines(x-.03,means[3,], type="o", pch=7, lty=4)
arrows(x-.03, as.numeric(means[3,])-as.numeric(uncs[3,]), x-.03, as.numeric(means[3,])+as.numeric(uncs[3,]), length=0.05, angle=90, code=3)

axis(1, at=x, lab=c("Pre","Post"))
axis(2, las=1, at=1:5)
title(main="Q47")
legend(1.75, 5, c("Super group A","Super group B", "Super group C"), pch=5:7, lty=2:4)



###Q48###
Q48prem<-vector()
Q48preu<-vector()
Q48prem[1]<-mean(newdata$pre48[supergroups=="A"],na.rm=T)
Q48prem[2]<-mean(newdata$pre48[supergroups=="B"],na.rm=T)
Q48prem[3]<-mean(newdata$pre48[supergroups=="C"],na.rm=T)

Q48preu[1]<-1.96*sd(newdata$pre48[supergroups=="A"],na.rm=T)/sqrt(sqrt(length(supergroups[supergroups=="A"])))
Q48preu[2]<-1.96*sd(newdata$pre48[supergroups=="B"],na.rm=T)/sqrt(sqrt(length(supergroups[supergroups=="B"])))
Q48preu[3]<-1.96*sd(newdata$pre48[supergroups=="C"],na.rm=T)/sqrt(sqrt(length(supergroups[supergroups=="C"])))

Q48postm<-vector()
Q48postu<-vector()
Q48postm[1]<-mean(newdata$post48[supergroups=="A"],na.rm=T)
Q48postm[2]<-mean(newdata$post48[supergroups=="B"],na.rm=T)
Q48postm[3]<-mean(newdata$post48[supergroups=="C"],na.rm=T)

Q48postu[1]<-1.96*sd(newdata$post48[supergroups=="A"],na.rm=T)/sqrt(sqrt(length(supergroups[supergroups=="A"])))
Q48postu[2]<-1.96*sd(newdata$post48[supergroups=="B"],na.rm=T)/sqrt(sqrt(length(supergroups[supergroups=="B"])))
Q48postu[3]<-1.96*sd(newdata$post48[supergroups=="C"],na.rm=T)/sqrt(sqrt(length(supergroups[supergroups=="C"])))

means<-data.frame(Q48prem,Q48postm)
uncs<-data.frame(Q48preu,Q48postu)
x<-c(1.05,1.65)
plot(x,means[1,],xlim=c(1,2),ylim=c(1,5), type="o", pch=5, lty=2,axes=F,ann=F)
arrows(x, as.numeric(means[1,])-as.numeric(uncs[1,]), x, as.numeric(means[1,])+as.numeric(uncs[1,]), length=0.05, angle=90, code=3)

lines(x+.03,means[2,], type="o", pch=6, lty=3)
arrows(x+.03, as.numeric(means[2,])-as.numeric(uncs[2,]), x+.03, as.numeric(means[2,])+as.numeric(uncs[2,]), length=0.05, angle=90, code=3)
lines(x-.03,means[3,], type="o", pch=7, lty=4)
arrows(x-.03, as.numeric(means[3,])-as.numeric(uncs[3,]), x-.03, as.numeric(means[3,])+as.numeric(uncs[3,]), length=0.05, angle=90, code=3)

axis(1, at=x, lab=c("Pre","Post"))
axis(2, las=1, at=1:5)
title(main="Q48")
legend(1.75, 5, c("Super group A","Super group B", "Super group C"), pch=5:7, lty=2:4)

###Q49###
Q49prem<-vector()
Q49preu<-vector()
Q49prem[1]<-mean(newdata$pre49[supergroups=="A"],na.rm=T)
Q49prem[2]<-mean(newdata$pre49[supergroups=="B"],na.rm=T)
Q49prem[3]<-mean(newdata$pre49[supergroups=="C"],na.rm=T)

Q49preu[1]<-1.96*sd(newdata$pre49[supergroups=="A"],na.rm=T)/sqrt(sqrt(length(supergroups[supergroups=="A"])))
Q49preu[2]<-1.96*sd(newdata$pre49[supergroups=="B"],na.rm=T)/sqrt(sqrt(length(supergroups[supergroups=="B"])))
Q49preu[3]<-1.96*sd(newdata$pre49[supergroups=="C"],na.rm=T)/sqrt(sqrt(length(supergroups[supergroups=="C"])))

Q49postm<-vector()
Q49postu<-vector()
Q49postm[1]<-mean(newdata$post49[supergroups=="A"],na.rm=T)
Q49postm[2]<-mean(newdata$post49[supergroups=="B"],na.rm=T)
Q49postm[3]<-mean(newdata$post49[supergroups=="C"],na.rm=T)

Q49postu[1]<-1.96*sd(newdata$post49[supergroups=="A"],na.rm=T)/sqrt(sqrt(length(supergroups[supergroups=="A"])))
Q49postu[2]<-1.96*sd(newdata$post49[supergroups=="B"],na.rm=T)/sqrt(sqrt(length(supergroups[supergroups=="B"])))
Q49postu[3]<-1.96*sd(newdata$post49[supergroups=="C"],na.rm=T)/sqrt(sqrt(length(supergroups[supergroups=="C"])))

means<-data.frame(Q49prem,Q49postm)
uncs<-data.frame(Q49preu,Q49postu)
x<-c(1.05,1.65)
plot(x,means[1,],xlim=c(1,2),ylim=c(1,5), type="o", pch=5, lty=2,axes=F,ann=F)
arrows(x, as.numeric(means[1,])-as.numeric(uncs[1,]), x, as.numeric(means[1,])+as.numeric(uncs[1,]), length=0.05, angle=90, code=3)

lines(x+.03,means[2,], type="o", pch=6, lty=3)
arrows(x+.03, as.numeric(means[2,])-as.numeric(uncs[2,]), x+.03, as.numeric(means[2,])+as.numeric(uncs[2,]), length=0.05, angle=90, code=3)
lines(x-.03,means[3,], type="o", pch=7, lty=4)
arrows(x-.03, as.numeric(means[3,])-as.numeric(uncs[3,]), x-.03, as.numeric(means[3,])+as.numeric(uncs[3,]), length=0.05, angle=90, code=3)

axis(1, at=x, lab=c("Pre","Post"))
axis(2, las=1, at=1:5)
title(main="Q49")
legend(1.75, 5, c("Super group A","Super group B", "Super group C"), pch=5:7, lty=2:4)



####LWG###
LWG<-vector()
LWG[mydata$country=="DK"]<-1
LWG[mydata$country=="CH"]<-2
LWG[mydata$country=="FI"]<-3
LWG[mydata$country=="CZ"]<-4
LWG[mydata$country=="D"]<-5
LWG[mydata$country=="F"]<-6

LWG<-LWG[mydata$control=="n"]






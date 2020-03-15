############BOOTSTRAP COMMMENT


############TERTILE PROBLEM##########
quantile(postSG$score, probs=c(0,1/3,2/3,1))
tert<-vector()
tert[postSG$score<=5]<-3
tert[postSG$score<=4]<-2
tert[postSG$score<=3.272727]<-1
compare(tert,supergroups,method="NMI")
compare(tert,attributesT$postGroup,method="NMI")
compare(supergroups,attributesT$postGroup,method="NMI")


NMI_test<-vector()
for(i in 1:10000){
a<-sample(tert,length(tert),replace=F)
b<-postSG$supergroups
#b<-sample(postSG$supergroups,length(postSG$supergroups),replace=F)
NMI_test[i]<-compare(a,b,method="NMI")
}
Z_tSG<-(compare(tert,postSG$supergroups,method="NMI")-mean(NMI_test))/sd(NMI_test)

NMI_test<-vector()
for(i in 1:1000){
  a<-sample(tert,length(tert),replace=F)
  b<-attributesT$postGroup
  #b<-sample(attributesT$postGroup,length(attributesT$postGroup),replace=F)
  NMI_test[i]<-compare(a,b,method="NMI")
}
Z_tPG<-(compare(tert,attributesT$postGroup,method="NMI")-mean(NMI_test))/sd(NMI_test)


NMI_test<-vector()
for(i in 1:1000){
  a<-sample(postSG$supergroups,length(postSG$supergroups),replace=F)
  b<-attributesT$postGroup
  #b<-sample(attributesT$postGroup,length(attributesT$postGroup),replace=F)
  NMI_test[i]<-compare(a,b,method="NMI")
}
Z_SGPG<-(compare(attributesT$postGroup,postSG$supergroups,method="NMI")-mean(NMI_test))/sd(NMI_test)

Z_tSG
Z_tPG
Z_SGPG
par(mfrow=c(3,2))
barplot(rbind(sga.t[1,2:7],sgb.t[1,2:7],sgc.t[1,2:7]),beside=T,main="Q38: Find better ways to teach using FA",xlab="Teacher response",ylab="Frequency",legend=c(1,2,3))
barplot(rbind(sga.t[2,2:7],sgb.t[2,2:7],sgc.t[2,2:7]),beside=T,main="Q39R: Not difficult to integrate FA ",xlab="Teacher response",ylab="Frequency")
barplot(rbind(sga.t[3,2:7],sgb.t[3,2:7],sgc.t[3,2:7]),beside=T,main="Q40: Know necessary steps",xlab="Teacher response",ylab="Frequency")
barplot(rbind(sga.t[4,2:7],sgb.t[4,2:7],sgc.t[4,2:7]),beside=T,main="Q41R: Effective in monitoring student work",xlab="Teacher response",ylab="Frequency")
barplot(rbind(sga.t[5,2:7],sgb.t[5,2:7],sgc.t[5,2:7]),beside=T,main="Q42R: Teaching will be effective ",xlab="Teacher response",ylab="Frequency")
barplot(rbind(sga.t[6,2:7],sgb.t[6,2:7],sgc.t[6,2:7]),beside=T,main="Q43: Student background can be overcome",xlab="Teacher response",ylab="Frequency")
par(mfrow=c(3,2))
barplot(rbind(sga.t[7,2:7],sgb.t[7,2:7],sgc.t[7,2:7]),beside=T,main="Q44: Low-achieving student progress due to FA",xlab="Teacher response",ylab="Frequency",legend=c(1,2,3))
barplot(rbind(sga.t[8,2:7],sgb.t[8,2:7],sgc.t[8,2:7]),beside=T,main="Q45: Understand FA enough to be effective ",xlab="Teacher response",ylab="Frequency")
barplot(rbind(sga.t[9,2:7],sgb.t[9,2:7],sgc.t[9,2:7]),beside=T,main="Q46R: Increased effort leads to change ",xlab="Teacher response",ylab="Frequency")
barplot(rbind(sga.t[10,2:7],sgb.t[10,2:7],sgc.t[10,2:7]),beside=T,main="Q47R: Not difficult to explain content ",xlab="Teacher response",ylab="Frequency")
barplot(rbind(sga.t[11,2:7],sgb.t[11,2:7],sgc.t[11,2:7]),beside=T,main="Q48: Able to answer student questions",xlab="Teacher response",ylab="Frequency")
barplot(rbind(sga.t[12,2:7],sgb.t[12,2:7],sgc.t[12,2:7]),beside=T,main="Q49R: Do not wonder necessary skills",xlab="Teacher response",ylab="Frequency")


tertWithC<-rep(4,93)
tertWithC[attributesT$mydata.ID[tert==1]]<-1
tertWithC[attributesT$mydata.ID[tert==2]]<-2
tertWithC[attributesT$mydata.ID[tert==3]]<-3

S1<-vector()
for(i in 1:12){
  S1[i]<--sum(sg1[i,2:7]*log2(sg1[i,2:7]),na.rm=T)
  
}

S2<-vector()
for(i in 1:12){
  S2[i]<--sum(sg2[i,2:7]*log2(sg2[i,2:7]),na.rm=T)
  
}

S3<-vector()
for(i in 1:12){
  S3[i]<--sum(sg3[i,2:7]*log2(sg3[i,2:7]),na.rm=T)
  
}

Smax<--log2(1/6)

plot(S1/Smax,ylim=c(0,1),pch="1")
points(S2/Smax,pch="2",col="blue")
points(S3/Smax,pch="3",col="red")

plot(Sa/Smax,ylim=c(0,1),pch="A")

###GROUPING PROBLEM
qualitySG<-function(g,memb){
  Q<-modularity(g,membership = memb)
  g1<-induced.subgraph(postNetBBT,vids = which(supergroups=="A"))
  g2<-induced.subgraph(postNetBBT,vids = which(supergroups=="B"))
  g3<-induced.subgraph(postNetBBT,vids = which(supergroups=="C"))
  N_A<-vcount(g1)
  N_B<-vcount(g2)
  N_C<-vcount(g3)
  L_A<-ecount(g1)
  L_B<-ecount(g2)
  L_C<-ecount(g3)
  sim_A<-mean(E(g1)$weight)
  sim_B<-mean(E(g2)$weight)
  sim_C<-mean(E(g3)$weight)
  sd_A<-sd(E(g1)$weight)
  sd_B<-sd(E(g2)$weight)
  sd_C<-sd(E(g3)$weight)
  L_nA<-length(E(postNetBBT)[which(supergroups=="A") %--% which(supergroups!="A")]$weight)
  L_nB<-length(E(postNetBBT)[which(supergroups=="B") %--% which(supergroups!="B")]$weight)
  L_nC<-length(E(postNetBBT)[which(supergroups=="C") %--% which(supergroups!="C")]$weight)
  sim_nA<-mean(E(postNetBBT)[which(supergroups=="A") %--% which(supergroups!="A")]$weight)
  sim_nB<-mean(E(postNetBBT)[which(supergroups=="B") %--% which(supergroups!="B")]$weight)
  sim_nC<-mean(E(postNetBBT)[which(supergroups=="C") %--% which(supergroups!="C")]$weight)
  sd_nA<-sd(E(postNetBBT)[which(supergroups=="A") %--% which(supergroups!="A")]$weight)
  sd_nB<-sd(E(postNetBBT)[which(supergroups=="B") %--% which(supergroups!="B")]$weight)
  sd_nC<-sd(E(postNetBBT)[which(supergroups=="C") %--% which(supergroups!="C")]$weight)
  
  mean_n<-mean(c(E(postNetBBT)[which(supergroups=="A") %--% which(supergroups!="A")]$weight,
               E(postNetBBT)[which(supergroups=="B") %--% which(supergroups!="B")]$weight,
               E(postNetBBT)[which(supergroups=="C") %--% which(supergroups!="C")]$weight))
    sd_n<-sd(c(E(postNetBBT)[which(supergroups=="A") %--% which(supergroups!="A")]$weight,
                 E(postNetBBT)[which(supergroups=="B") %--% which(supergroups!="B")]$weight,
                 E(postNetBBT)[which(supergroups=="C") %--% which(supergroups!="C")]$weight))
  mean_i<-mean(c(E(g1)$weight,E(g2)$weight,E(g3)$weight))
    sd_i<-sd(c(E(g1)$weight,E(g2)$weight,E(g3)$weight))
  ttest<- t.test()
  result<-data.frame(Q,N_A,N_B,N_C,L_A,L_B,L_C,sim_A,sd_A,sim_B,sd_B,sim_C,sd_C,L_nA,L_nB,L_nC,sim_nA,sd_nA,sim_nB,sd_nB,sim_nC,sd_nC, mean_n,sd_n,mean_i,sd_i)
  return(result)
}


supergroups<-vector(length = 64)
supergroups[attributesT$postGroup==5|attributesT$postGroup==7|attributesT$postGroup==9|attributesT$postGroup==12]<-"A"
supergroups[attributesT$postGroup==1|attributesT$postGroup==2|attributesT$postGroup==3|attributesT$postGroup==4]<-"B"
supergroups[attributesT$postGroup==6|attributesT$postGroup==8|attributesT$postGroup==10|attributesT$postGroup==11]<-"C"
supergroups<-as.factor(supergroups)

sol1<-qualitySG(postNetBBT,supergroups)

aov.x<-aov(attributesT$postScore~supergroups)
summary(aov(attributesT$postScore~supergroups))
boxplot2(attributesT$postScore~supergroups)
TukeyHSD(aov(attributesT$postScore~supergroups))

supergroups<-vector(length = 64)
supergroups[attributesT$postGroup==5|attributesT$postGroup==7|attributesT$postGroup==9|attributesT$postGroup==12]<-"A"
supergroups[attributesT$postGroup==1|attributesT$postGroup==2|attributesT$postGroup==3|attributesT$postGroup==4|attributesT$postGroup==11]<-"B"
supergroups[attributesT$postGroup==6|attributesT$postGroup==8|attributesT$postGroup==10]<-"C"
supergroups<-as.factor(supergroups)

sol2<-qualitySG(postNetBBT,supergroups)


aov.x<-aov(attributesT$postScore~supergroups)
summary(aov(attributesT$postScore~supergroups))
boxplot2(attributesT$postScore~supergroups)
TukeyHSD(aov(attributesT$postScore~supergroups))


supergroups<-vector(length = 64)
supergroups[attributesT$postGroup==5|attributesT$postGroup==7|attributesT$postGroup==9|attributesT$postGroup==12|attributesT$postGroup==11]<-"A"
supergroups[attributesT$postGroup==1|attributesT$postGroup==2|attributesT$postGroup==3|attributesT$postGroup==4]<-"B"
supergroups[attributesT$postGroup==6|attributesT$postGroup==8|attributesT$postGroup==10]<-"C"
supergroups<-as.factor(supergroups)
sol3<-qualitySG(postNetBBT,supergroups)

supergroups<-vector(length = 64)
supergroups[attributesT$postGroup==5|attributesT$postGroup==7|attributesT$postGroup==9|attributesT$postGroup==12|attributesT$postGroup==11]<-"A"
supergroups[attributesT$postGroup==1|attributesT$postGroup==2|attributesT$postGroup==3|attributesT$postGroup==4|attributesT$postGroup==8]<-"B"
supergroups[attributesT$postGroup==6|attributesT$postGroup==10]<-"C"
supergroups<-as.factor(supergroups)
sol4<-qualitySG(postNetBBT,supergroups)

supergroups<-vector(length = 64)
supergroups[attributesT$postGroup==5|attributesT$postGroup==7|attributesT$postGroup==12]<-"A"
supergroups[attributesT$postGroup==1|attributesT$postGroup==2|attributesT$postGroup==3|attributesT$postGroup==4|attributesT$postGroup==8]<-"B"
supergroups[attributesT$postGroup==6|attributesT$postGroup==9|attributesT$postGroup==10|attributesT$postGroup==11]<-"C"
supergroups<-as.factor(supergroups)
sol5<-qualitySG(postNetBBT,supergroups)

aov.x<-aov(attributesT$postScore~supergroups)
summary(aov(attributesT$postScore~supergroups))
boxplot2(attributesT$postScore~supergroups)
TukeyHSD(aov(attributesT$postScore~supergroups))

respT<-mydata[attributesT$mydata.ID,]
respT[supergroups=="A",15:26]


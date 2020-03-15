supergroups<-vector(length = 64)
supergroups[attributesT$postGroup==5|attributesT$postGroup==7|attributesT$postGroup==9|attributesT$postGroup==12]<-"A"
supergroups[attributesT$postGroup==1|attributesT$postGroup==2|attributesT$postGroup==3|attributesT$postGroup==4]<-"B"
supergroups[attributesT$postGroup==6|attributesT$postGroup==8|attributesT$postGroup==10|attributesT$postGroup==11]<-"C"
supergroups<-as.factor(supergroups)
modularity(postNetBBT,membership = supergroups)

#0.4854486 Benchmark
#0.4245728 PG6 -> B REJECTED
#0.4532307 PG8 -> B R
#0.4615967 PG10 -> B R
#0.4676486 PG11 -> B R
#0.4555447 PG6 -> A R
#0.4900765 PG11 -> A ACCEPTED!

supergroups<-vector(length = 64)
supergroups[attributesT$postGroup==5|attributesT$postGroup==7|attributesT$postGroup==9|attributesT$postGroup==11|attributesT$postGroup==12]<-"A"
supergroups[attributesT$postGroup==1|attributesT$postGroup==2|attributesT$postGroup==3]<-"B"
supergroups[attributesT$postGroup==6|attributesT$postGroup==8|attributesT$postGroup==10|attributesT$postGroup==4]<-"C"
supergroups<-as.factor(supergroups)
modularity(postNetBBT,membership = supergroups)

#0.4630206 PG6 -> A R
#0.4392577 PG1 -> A R
#0.4747686 PG1 -> C R
#0.4766376 PG4 -> A R
#0.5151299 PG4 -> C ACCEPTED!

supergroups<-vector(length = 64)
supergroups[attributesT$postGroup==5|attributesT$postGroup==7|attributesT$postGroup==9|attributesT$postGroup==11|attributesT$postGroup==12]<-"A"
supergroups[attributesT$postGroup==1|attributesT$postGroup==2|attributesT$postGroup==3]<-"B"
supergroups[attributesT$postGroup==6|attributesT$postGroup==8|attributesT$postGroup==10|attributesT$postGroup==4]<-"C"
supergroups<-as.factor(supergroups)
modularity(postNetBBT,membership = supergroups)

#0.4856266 PG3 -> A R
#0.5044945 PG3 -> C R
#0.5115255 PG11 -> C R
#0.4645336 PG7 -> C R
#0.4676486 PG5 -> B R
#0.4898985 PG2 -> C R
#0.4854041 PG1 -> C R
#0.4571022 PG1 -> A R
#0.49733 PG6-> A R
#0.4916785 PG11 -> B R
#0.4741901 PG6 -> B R
# 0.4645336 PG7 -> C R





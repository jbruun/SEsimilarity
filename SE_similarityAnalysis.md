Similarity network analyses of self-efficacy attributes
================
Jesper Bruun
3/12/2020

## Description

``` r
library(igraph)
```

    ## 
    ## Attaching package: 'igraph'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     decompose, spectrum

    ## The following object is masked from 'package:base':
    ## 
    ##     union

``` r
library(PMCMRplus)
library(effsize)
#library(rcompanion)
library(gplots)
```

    ## 
    ## Attaching package: 'gplots'

    ## The following object is masked from 'package:stats':
    ## 
    ##     lowess

``` r
source("R_scripts/backboneExtraction.r")
source("R_scripts/segregation.r")
```

## Load data

``` r
mydata<-read.csv("Data/dataset.csv",sep=";")
mydata$X<-c(1:length(mydata$Name))
```

## Make respondent similarity networks

``` r
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
```

    ##  [1] "pre38" "pre39" "pre40" "pre41" "pre42" "pre43" "pre44" "pre45" "pre46"
    ## [10] "pre47" "pre48" "pre49"

``` r
names(mydata[15:26]) #Columns for post
```

    ##  [1] "post38" "post39" "post40" "post41" "post42" "post43" "post44" "post45"
    ##  [9] "post46" "post47" "post48" "post49"

``` r
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

plot(postNetT)
```

![](SE_similarityAnalysis_files/figure-gfm/make-networks-1.png)<!-- -->

``` r
plot(postNetBBT)
```

![](SE_similarityAnalysis_files/figure-gfm/make-networks-2.png)<!-- -->

## Including Plots

You can also embed plots, for example:

![](SE_similarityAnalysis_files/figure-gfm/pressure-1.png)<!-- -->

Note that the `echo = FALSE` parameter was added to the code chunk to
prevent printing of the R code that generated the plot.

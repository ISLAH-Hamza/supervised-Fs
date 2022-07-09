# supervised-Fs

### Repository content
This repository contains my implementation of the selection algorithms treated in my end of study project titled: **Supervised feature selection based on association and redundancy**.

This repository will be organized as follows:

- the data  folder contains the real and simulated data sets used to test the algorithms
- the singl-label-Fs folder contains the selection algorithms that process the target variable when it contains only one label
- the multi-labels-Fs folder will contain the FMFS algorithm, which works on data array with several labels.


### Tests of algorithme 
#### 1-Toys data simulation test

The toy data set is constructed in such a way that the important explanatory variables for Y are in the following order V3, V2,V1,V6,V5,V4 the first test will be to calculate the frequency of occurrence of these variables among the items in the selected set we will repeat this process on 100 tables.
For example, we will do the test for the OMICFS method. 

First, we need to call the following libraries

```r
library(infotheo)
library(dplyr)
```

then we calculate the frequency as follows 
```r
result=rep(0,6)
featurs_T=1:6
n=100
p=50

for(i in 1:100){
  dataS=toys(n,p)
  X=select(dataS,-Q) %>% discretize( )
  Y= select(dataS,Q) %>% discretize(nbins=2)
  featurs=OMICFS(X,Y,6)
  index=featurs %in% featurs_T
  
  result[index]=result[index]+1
  
  }

```
the number of occurrences in this case for n=100 and p=50 is:
```r
print(result)
100 100 100  97  75  42
```
we repeat the same process to oder méthodes of selection 

#### 2-Real data test
the data we are going to use are represented in the following table:

| Data |source| n| p |class|
|------|------|---|---|-----|
| covid | Github|  863 |  15 |2|
| Diabetic| UCI |  1151 |  20 |2|
| SCADI | UCI|  70 |  206 |6|
| Divorce | UCI|  170 |  55 |2|

We will apply the selection method on the different data then train the SVM NB and Random Forest classifiers and calculate the accuracy using cross validation.

the following code deals with the case of diabteic data as an example. we must all first import the data and balance it
```r
diabet=read.csv("Data/diabetic.csv",sep=",")
X=dplyr::select(diabet,-Class)
Y=dplyr::select(diabet,Class) 
dataResult=caret::upSample(X,as.factor(unlist(Y)))

X=dplyr::select(dataResult,-Class) %>% infotheo::discretize()
Y=dplyr::select(dataResult,Class) %>% infotheo::discretize(nbins=2)

```
and then train the classification models:
```r

ModelResult=matrix(NA,ncol=3,nrow=ncol(X))
for(i in 1:ncol(X)){
  
  features= OMICFS(X,Y,i)
  data=cbind( X[features] , Y )
  data$Class=as.factor(data$Class)
  
  svmMod <- caret::train(Class ~ ., data = data,
                         method = "svmLinear",
                         tuneLength = 10,
                         trControl = caret::trainControl(method='cv',number = 5)
  )
  
  
  NBMod <- caret::train(Class ~ ., data = data,
                        method = "nb",
                        tuneLength = 10,
                        trControl = caret::trainControl(method='cv',number = 5)
  ) 
  
  rfMod <- caret::train(Class ~ ., data = data,
                        method = "rf",
                        tuneLength = 10,
                        trControl = caret::trainControl(method='cv',number = 5)
  )
  
  
  r=c( mean(svmMod$results$Accuracy), mean(NBMod$results$Accuracy) ,mean(rfMod$results$Accuracy))
  
  
  ModelResult[i,]<-r
  print(ModelResult[i,])
  
}

colnames(ModelResult)=c("svm","nb","rf")
rownames(ModelResult)<-seq(1:nrow(ModelResult))
```
The results obtained are shown in the following table 

|i  |  svm |       nb   |     rf |
|-- |  --- |       ---   |    --- |
|1  |0.5794379 | 0.5937358 |0.6104918|
|2  |0.6169865 |0.6399532 |0.6375117|
|3  |0.6227869 |0.6129475 |0.6297156|
|4  |0.6358381 |0.6338006 |0.6333980|
|5  |0.6350519 |0.6231874 |0.6642715|
|6  |0.6465139 |0.6317397 |0.6679264|
|7  |0.6333657 |0.6370826 |0.6613355|
|8  |0.6382636 |0.6313533 |0.6601964|
|9  |0.6416100 |0.6375343 |0.6489181|
|10 |0.6652794 |0.6358598 |0.6732649|
|11 |0.6694855 |0.6387136 |0.6854393|
|12 |0.6808866 |0.6284460 |0.6892023|
|13 |0.6751455 |0.6280688 |0.6835499|
|14 |0.6849749 |0.6330311 |0.6785818|
|15 |0.6824925 |0.6251957 |0.6883794|
|16 |0.6824891 |0.6300836 |0.7148223|
|17 |0.6759351 |0.6243995 |0.7037681|
|18 |0.6849281 |0.6240097 |0.7010897|
|19 |0.6766945 |0.6215189 |0.7075858|
|20 |0.6816074 |0.6264627 |0.6995627|

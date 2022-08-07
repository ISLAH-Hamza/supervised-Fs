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
we repeat the same process for the other selection methods, then we draw the bar chart to visualize the results

![alt text](https://github.com/ISLAH-Hamza/supervised-Fs/blob/main/imges/toys_tst.png)

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
The results obtained are shown in the following graphs 

![alt text](https://github.com/ISLAH-Hamza/supervised-Fs/blob/main/imges/real_data_tst.png)

***!! for more details see the experimental part in the report***   

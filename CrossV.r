# la validation croisé
library(e1071)  

SpliTokFold<-function(data,kfold){
  N<-nrow(data)
  taille<- N %/% kfold
  set.seed(54)
  alea<-runif(N)
  rang<-rank(alea)
  bloc<-(rang-1) %/% taille+1
  bloc<-as.factor((bloc))
  return(bloc)
}


### validation crosé pour le classifieur svm


SVMCROSSV<-function(data,kfold,target){
  result<-c()
  l=which(colnames(data)==target)
  colnames(data)[l]<-'Y'
  index<-SpliTokFold(data,kfold)
  for(i in seq(kfold)){
  test<-data[index==i,]
  train<-data[index!=i,]
  classifier = svm(formula =train$Y ~ .,
                   data =train,
                   type = 'C-classification',
                   kernel = 'polynomial')
  
  y_pred = predict(classifier, newdata = test)
  d=sum(y_pred==test$Y)/length(test$Y)
  result=append(result,d)
  }
  
   return(mean(result))
}


NBCROSSV<-function(data,kfold,target){
  result<-c()
  l=which(colnames(data)==target)
  colnames(data)[l]<-'Y'
  index<-SpliTokFold(data,kfold)
  for(i in seq(kfold)){
    test<-data[index==i,]
    train<-data[index!=i,]
    classifier = naiveBayes(formula =train$Y ~ .,
                     data =train,
                     type = 'C-classification',
                     kernel = 'polynomial')
    
    y_pred = predict(classifier, newdata = test)
    d=sum(y_pred==test$Y)/length(test$Y)
    result=append(result,d)
  }
  
  return(mean(result))
}



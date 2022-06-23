
Div=read.csv("~/Documents/master/PFE/chapitre 5 éxperimentation/supervised-Fs/Data/covide.csv" , sep=",")
X=dplyr::select(Div,-result)
Y=dplyr::select(Div,result) 
dataResult=caret::upSample(X,as.factor(unlist(Y)))

X=dplyr::select(dataResult,-Class) %>% infotheo::discretize(nbins=j)
Y=dplyr::select(dataResult,Class) %>% infotheo::discretize(nbins=6)
ModelResult=c()

for(i in seq(ncol(X))){
  
  features=OMICFS(X,Y,i)
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
  
  
  r=c( svmMod$results$Accuracy[1],NBMod$results$Accuracy[2],rfMod$results$Accuracy[1])
  
  if(length(ModelResult)==0){ModelResult=t(r) }else{ ModelResult=rbind(ModelResult,r)}
  ModelResult<-as.data.frame(ModelResult)
 print(ModelResult)
  
}

colnames(ModelResult)=c("svm","nb","rf")
rownames(ModelResult)<-seq(1:nrow(ModelResult))
write.csv(ModelResult,"les resultats/OMICFS_covide.csv",row.names = F)


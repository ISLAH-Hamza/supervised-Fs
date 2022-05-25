library(minerva)

MIC=function(fearray,classflag,expfeanum=F){
  
  index=colSums(is.na(fearray))==0
  fearray=as.data.frame(fearray)[index]
  #check the input parameters
  incol=ncol(fearray)
  if(expfeanum==F){ expfeanum=incol }
  else if(incol < expfeanum ){ return(-1) }
  
  #Calculate MIC score
  MICValue=c()
  fearray=round(fearray*10^4)/10^4
  for(i in seq(incol)){
    MICValue[i]<-mine(as.numeric(fearray[,i]),as.numeric(classflag[,1]))$MIC 
  }
  
  #sort
  index=1:incol
  mic<-rbind(index,MICValue)
  mic=mic[,order(mic[2,], decreasing = T )]
  if(expfeanum<incol){
    mic=mic[,1:expfeanum]
  }
  return(mic)
}


OMICFS<-function(X,Y,d){
  MaxRel=MIC(X,Y)
  #supriment les element null
  index=which(MaxRel[2,]==0)
  if(length(index)>0){ MaxRel=MaxRel[,-index] }
  
  #initialisation
  
  RankedFea=MaxRel[1,1]
  MaxRel=MaxRel[,-1]
  
  temporthvector=X[,RankedFea]
  orthvecotr=data.frame(temporthvector/norm(temporthvector,"2"))
  i=1
  while(i < d ){
    temporthvector=matrix(nrow = nrow(X),ncol=ncol(MaxRel))
    for (j in seq(ncol(MaxRel))) {
      temporthvector[,j]=X[,MaxRel[1,j]]
      for(m in 1:i){
        temporthvector[,j]=temporthvector[,j]- c(X[,MaxRel[1,j]] %*% orthvecotr[,m]/norm(orthvecotr[,m],"2"))
      }
      temporthvector[,j]=temporthvector[,j]/norm(temporthvector[,j],"2")
    }
    
    MaxRMinR=MIC(temporthvector,Y) %>% as.data.frame()
    
    max_index=MaxRMinR[1,1]
    orthvecotr[,i+1]=temporthvector[, max_index]
    RankedFea[i+1]=MaxRel[1,max_index]
    MaxRel=MaxRel[,-max_index] %>% as.matrix()
    if(ncol(MaxRel)==0){break}
    i=i+1
  }
   return(RankedFea)
}

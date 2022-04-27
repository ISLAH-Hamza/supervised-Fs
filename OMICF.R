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
    MICValue[i]<-mine(as.numeric(fearray[,i]),as.numeric(Y[,1]))$MIC 
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



OMICFS=function(fearray,classflag,psfeanum,expfeanum){
  
  if (psfeanum<expfeanum){
    return(-1)
  }
  #max relevent featurs
  MaxRel= MIC(fearray,classflag,psfeanum)
  
  #suprimer les element null
  index=which(MaxRel[2,]==0)
  if(length(index)>0){ MaxRel=MaxRel[,-index] }
  
  #initialisation
  len=ncol(MaxRel)
  candfeaflag=rep(1,len-1)
  actexpfeanum=min(expfeanum,len)
  RankedFea=MaxRel[1,1]
  temporthvector=fearray[,RankedFea]
  orthvecotr=data.frame(temporthvector/norm(temporthvector,"2"))
  
  for(i in seq(actexpfeanum-1)){
    index1=0
    temporthvector=matrix(nrow = nrow(fearray),ncol=ncol(fearray))
    for(j in seq(len-1)){
      if(candfeaflag[j]==1){
        index1=index1+1
        temporthvector[,index1]=fearray[,MaxRel[1,j+1]]
        for(m in 1:i){
          temporthvector[,index1]=temporthvector[,index1]- c(fearray[,MaxRel[1,j+1]] %*% orthvecotr[,m]/norm(orthvecotr[,m],"2"))
        }
        temporthvector[,index1]=temporthvector[,index1]/norm(temporthvector[,index1],"2")
      }
    }
    #compute max Relevance and min Redundancy
    MaxRMinR=MIC(temporthvector,classflag,index1) %>% as.data.frame()
    index2=0
    tempFSMICnum=tempFSMICscore=c()
    for(j in seq(len-1)){
      if(candfeaflag[j]==1){
        index2=index2+1
        tempFSMICscore[index2]=MaxRMinR[2,index2]
        tempFSMICnum[index2]=MaxRel[1,j+1];
      }
    }
    max_index=which.max(tempFSMICscore)
    orthvecotr[,i+1]=temporthvector[,max_index]
    RankedFea[i+1]=tempFSMICnum[max_index]
    candfeaflag[which( MaxRel[1,] ==  tempFSMICnum[max_index]-1) ]=0;
    
  }
  
  
  return(RankedFea)  
  
  
}


normlize<-function(e){
  result=(e-mean(e))/sd(e)
  return(result)
}

d=rep(0,6)
V=1:6
#Vm=c("V1","V2","V3","V4","V5","V6")
for(i in 1:100){
  data=toys(100,50)
  data=sapply(data,normlize) %>% as.data.frame()
  X=select(data,-Q)
  Y=select(data,Q)
  result=OMICFS(X,Y,6,6)
  print(i)
  result=result %in% V
  d[result]=d[result]+1
  
}

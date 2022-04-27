library(infotheo)
library(dplyr)

MRMR<-function(Featurs,target,k){
  
  # discritisation des entree
  Featursbins=nrow(unique(round(Featurs*10)))
  Featurs=discretize( Featurs , nbins=Featursbins )
  targetbins=nrow(unique(round(target*10)))
  target=discretize( target,nbins=targetbins)
  
  p=ncol(Featurs)
  I=matrix(nrow = 2,ncol=p)
  S=c()

  if(k>p){ return(-1) }   # le nombre de featur selection doit étre inferieur des nbr de featurs
  if(k==p){return(seq(1:p))}
  for(i in 1:p){
    I[2,i]=mutinformation(Featurs[i],target)
    I[1,i]=i
  } 
  
  index=which.max(I[2,])
  S=c(S,index) 
  I=I[,-index]
  
  m=1
  while(m < k){
     R=c()  
     for(i in 1:ncol(I)){
       s=0
       for(j in S){ s=s+mutinformation(Featurs[j],Featurs[ I[1,i] ]) } 
       R[i]=I[2,i]-s/length(S)
     }
     index=which.max(R)
     S=c(S,I[1,index])
     I=I[,-index]
     m=m+1
  }
  
  return(S)
}


d=rep(0,6)
V=1:6
for(i in 1:100){
  
  data=toys(100,500)
  X=select(data,-Q)
  Y=select(data,Q)
  r=V %in% MRMR(X,Y,6)
  d[r]=d[r]+1
  print(i)
}


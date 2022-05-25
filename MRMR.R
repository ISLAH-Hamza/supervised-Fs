MRMR<-function(Featurs,target,k){
  
  # The notations used
  # Featurs : Independent variables. !! Must not contain target variables
  # target : The variables to predict 
  # k : The number of variables to select
  # S : returned subset of selected features
  # I : The Mutual Information Matrix

  
  #initialization
  p=ncol(Featurs)
  I=matrix(nrow = 2,ncol=p)
  S=c()

  if(k>p){ return(-1) }  
  if(k==p){return(seq(1:p))}
  for(i in 1:p){
    I[2,i]=infotheo::mutinformation(Featurs[i],target)
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
       for(j in S){ s=s+infotheo::mutinformation(Featurs[j],Featurs[ I[1,i] ]) } 
       R[i]=I[2,i]-s/length(S)
     }
     index=which.max(R)
     S=c(S,I[1,index])
     I=I[,-index]
     m=m+1
  }
  
  return(S)
}

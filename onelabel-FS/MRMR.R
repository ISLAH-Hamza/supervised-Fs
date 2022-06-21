MRMR<-function(Featurs,target,k){
  # The notations used
  # Featurs : Independent variables. !! Must not contain target variables
  # target : The variables to predict 
  # k : The number of variables to select
  # S : returned subset of selected features
  # I : The Mutual Information Matrix 
  p=ncol(Featurs)
  
  if(k>p){ return(-1) }  
  if(k==p){return(seq(1:p))}
  
  #initialization
  I=matrix(nrow = 2,ncol=p)
  S=rep(NA,k)
  m=1
  
  #calculte the matrix of mutuel information 
  I<-unlist(lapply(1:p, function(x){infotheo::mutinformation(Featurs[x],target) }))
  I<-rbind(1:p,I)
  
  # Variables selections
  index=which.max(I[2,])
  S[1]=index
  I=I[,-index]
  while(m < k){
     m=m+1
     R=c()  
     for(i in 1:ncol(I)){
       s=unlist(lapply(1:m, function(j){ infotheo::mutinformation(Featurs[j],Featurs[ I[1,i] ])})) 
       R[i]=I[2,i]-sum(s)/length(S)
     }
    
     index=which.max(R)
     S[m]=I[1,index]
     I=I[,-index]
     
  }
  
  return(S)
}

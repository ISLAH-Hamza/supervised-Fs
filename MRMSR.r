library(dplyr)
library(infotheo)

MRMSR<-function(Featurs,target,K){

  # handling inputs
  p<-ncol(Featurs)
  if(K>p){return("number of selecting featurs must be less than number of featurs")}
 
  #discritisation
  Featursbins=nrow(unique(round(Featurs*10)))
  Featurs=discretize( Featurs , nbins=Featursbins )
  
  targetbins=nrow(unique(round(target*10)))
  target=discretize( target,nbins=targetbins)
  
  Featurs_OUT<-J<-R<-c()
  
  H<-entropy(target)
  for(i in 1:p ){
    I<-mutinformation( Featurs[i],target)
    R[i]<-I/H
  }
  index<-which.max(R)
  Fl<-Featurs[index]
  Featurs_OUT<-colnames(Fl)
  Featurs<-Featurs[-index]
  
  m<-1
  while(m < K){
    t<-0
    for(i in seq(length(Featurs))){
      Il<-condinformation(Fl, target,Featurs[i])
      Ii<-condinformation(Featurs[i], target,Fl)
      S<-1-((Il+Ii)/2*H)
      t<-t+S
      J[i]<-R[i]-t/length(Featurs_OUT)
    }
    
    index<-which.max(J)
    Fl<-Featurs[index]
    Featurs_OUT<-append(Featurs_OUT,colnames(Fl))
    Featurs<-Featurs[-index]
    m<-m+1
  }
  return(Featurs_OUT)
}






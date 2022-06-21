######### Probabilistic consistancy #######

f=function(S,C){
  #il faut que S et C doit être discret
  N<-nrow(S)
  Result=1
  while(T){
    index<-c()
    e=S[1,]
    for(i in 1:nrow(S)){
      s<-sum(e==S[i,])
      index<-append(index, s == ncol(S))
    }
    
    Nj<-sum(index)/N
    R<-C[index,]
    U=unique(R)
    d=unlist(lapply(1:length(U),function(i){
      which(U==U[i]) %>% length() %>% append(d,.)
    }))
    sup<-max(d)/N
    
    Result=Result-(Nj-sup)
    lines=which(index==T)
    C<-data.frame(C[-lines,])
    S<- as.matrix(S[-lines,])
    if(nrow(C)<=1){
      return(Result)
    }
  }
}
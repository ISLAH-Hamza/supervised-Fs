#variable collaboration

col<-function(ai,R,C){
  S=unlist(lapply(1:ncol(R), function(x){
    infotheo::interinformation( cbind(ai,R[,x],C)  )
  }))
  return(sum(S[S>0]))
}
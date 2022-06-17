toys<-function(n,p){
  
  Q<-rep(c(1,-1),c(n/2,n/2))
  c<-ceiling((n*0.7)/2)
  b<-(n*0.3)/2
  X<-matrix(rep(0,n,p),nrow=n , ncol=p)
  X<-as.data.frame(X)
  for(i in seq(1,3)){
    X[,i]<-c(rnorm(c,i,1),rnorm(b,0,1),-rnorm(c,i,1),-rnorm(b,0,1))
    
  }
  for(i in seq(4,6)){
    X[,i]<-c(rnorm(c,0,1),rnorm(b,i-3,1),-rnorm(c,0,1),-rnorm(b,i-3,1))
    
  }
  for(i in seq(7,p)){
    X[,i]<-rnorm(n,0,20)
    
  }
  
  data<-cbind(X,Q)
  return(data)
}



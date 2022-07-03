

FMFS=function(X,Y,n,q,b=2){
  
  
  if(ncol(Y)<2){stop("number of labels should be greater than 2")}
  if(b!=1 && b!= 2){ stop("b is invalid")}
  # X : predictive variables
  # Y : target variable
  # n : number of featurs to be selected
  # q : number of labeld combination
  # b : maximum orther dependency
  S=c()

  XH=sapply(1:ncol(X),function(i){ c(i, infotheo::entropy(X[i])) })
  XH=XH[,order(XH[2,])]
  # Xp   highest entropy from X   
  Xp=XH[1,]
  L=sapply(1:ncol(Y),function(i){ c(i, infotheo::entropy(Y[i])) })
  L=L[,order(L[2,])]
  Q=L[1,seq(q)]
  
  J=rep(NA,length(Xp))
  
  for(i in 1:length(Xp)){
    S=sapply(Y[Q] , function(y) { infotheo::mutinformation(X[,i],y)} )
    S=sum(S)
    if(b==2){
      Index <- list()
      i <- 1
      j <- 2
      for(k in 1:(q*(q-1)/2)){
        Index[[k]] <- c(i,j)
        j <- j+1
        if(j>q){ i = i+1; j = i+1;}
      }
      
      R=sapply(Index, function(x){ c ( x,infotheo::mutinformation( Y[,x],X[,i]) ) } )   
      S=S-sum(R)
      
      }
    
    J[i]=S 
    
  }
  index=order(J)
  index=index[1:n]
  return(Xp[index])
  
}



test=c(1,2,3,4,5,11,13,14,15)
d2=d1=rep(0,9)
for(i in 1: 100 ){
dataT=MultiData(100,200)
X=select(dataT, 1:20 )
X=round(X*100)/100 
X=infotheo::discretize(X)
Y=select(dataT,21:24) %>% infotheo::discretize()
Y=cbind(Y,2*Y)
q=2

R2=FMFS(X,Y,9,2,b=2)
R1=FMFS(X,Y,9,2,b=1)

d1=d1+as.numeric(R1 %in% test)
d2=d2+as.numeric(R2 %in% test)
print(d1)
}

SS=c()
for(i in 1:9){
  SS=SS=cat("(V", test[i], ",", d2[i],")",sep="")
}



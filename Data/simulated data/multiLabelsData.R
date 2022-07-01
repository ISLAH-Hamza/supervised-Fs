MultiData<-function(n,p){

if(p<16){stop("the minimum number of predictive variables is 16" )}
X<-matrix(NA,nrow=n,ncol=p);
Y<-matrix(NA,nrow=n,ncol=4);
colnames(Y)=c("Y1","Y2","Y3","Y4")

for(i in 1:10){  X[,i]=runif(n,0,1) }

 # redundant variables 
 X[,11]= (X[,1]-X[,2])/2 
 X[,12]= (X[,1]+X[,2])/2 
 X[,13]= X[,3] + 0.1
 X[,14]= X[,4]-0.2
 X[,15]=2*X[,5]
 # noise 
 for(i in 15:p){ X[,i]=rnorm(n,0,20) } 
 
 #lables
 Y[,1]=as.numeric( X[,1] > X [,2] )
 Y[,2]=as.numeric( X[,4] > X [,3] )
 Y[,3]=as.numeric( Y[,1] + Y[,2] == 1 )
 Y[,4]=as.numeric( X[,5] > 0.8 )
 
 
 return(as.data.frame(cbind(X,Y) ))

}

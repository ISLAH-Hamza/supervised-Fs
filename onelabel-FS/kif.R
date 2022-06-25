# !!!!! important the original concept can be found here: 
# https://github.com/KarimOualkacha/KIF/blob/master/R/KIF_functions.R


KIF <- function(X,y){
  
  if(ncol(X) != 2){ stop("X must contain exactly 2 columns") }
  Label <- unique(unlist(y))
  sampleK <- lapply(1:length(Label), function(i){ X[y==Label[i],]} )
  ncls <- sapply(1:length(Label), function(i){sum(y==Label[i])})
  n <- sum(ncls) ## number of observations
  ClassK <- sapply(1:length(Label), function(i){ ccaPP::corKendall(sampleK[[i]][,1], sampleK[[i]][,2])})
  Class0 <- ccaPP::corKendall(X[,1], X[,2])
  kifM <- sapply(1:length(Label), function(i){abs((ncls[i]/n)*(ClassK[i]-Class0))})
  return(sum(kifM))

}


#  !!! Sample should'nt contain y vairables 
## y must be categorical

KIFall <- function(Sample, y, threshold){
  y <- as.factor(unlist(y))
  p <- ncol(Sample) ## number of predictors
  
  
  ## needed to parallelize the code
  IndexSuite <<- list()
  i <- 1
  j <- 2
  for(k in 1:(p*(p-1)/2)){
      
      IndexSuite[[k]] <- c(i,j)
      j <- j+1
      if(j>p){
        i <- i+1
        j <- i+1
      }
  }
  
  ## Compute the scores and save them
  vecBeindex <- sapply( IndexSuite, function(x){ c(x,KIF(Sample[,x],y) ) })
  vecBeindex=t(vecBeindex)
  vecBeindex= vecBeindex[ order(vecBeindex[,3],decreasing=T),]
  return(vecBeindex[seq(threshold), c(1,2)] )
}

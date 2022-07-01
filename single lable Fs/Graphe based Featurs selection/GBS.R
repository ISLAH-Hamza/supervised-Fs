library(igraph)
library(dplyr)

GBS<-function(Features,C){
  ##########  Initialisation #########
  P<-ncol(Features)
  Arcs<-matrix(NA,ncol = 3, nrow =P*(P-1)/2 )
  index=1
  ###########  Création des Graphes ########
  for(i in 1:P){
    j<-i+1
    while( j <= P){
      ThreeI<-cbind(Features[i],Features[j],C) %>%interinformation()
      Hi<-Features[i]  %>% entropy()
      Hj<-Features[j]  %>% entropy()
      weight=ThreeI / (Hi +Hj )
      Arcs[index,]<-c( i , j ,weight )
      j<-j+1
      index=index+1
    }
  }
  
  colnames(Arcs)=c("from","to"," weight")
  G=graph_from_data_frame(Arcs[,c(1,2)],directed = F)
  E(G)$weight=Arcs[,3]
  
  R=as.numeric( get.edgelist(G)[which.max(E(G)$weight ),] ) #initailiser S par le max des elements.
  #############  Arbre de poids minimal ######
  tree<-igraph::mst(G)  #crée l'arbre couvrant de poid minimal
  SelEdge=E(tree)$weight
  SelEdge=rbind(1:length(SelEdge),SelEdge)
  SelEdgSorted=SelEdge[,order( SelEdge[2,], decreasing = T)]
  
  for(i in 1:length(SelEdge)){
    Tem=R
    Forst= decompose(delete_edges(tree, SelEdgSorted[1,seq(i)]))
    for (j in 1:length(Forst)) {
      TemTree= as.numeric( V(  Forst[[j]]  ))
      N=length(TemTree)
      if(sum(R %in% TemTree )==0){
        colList=rep(NA,N) 
        for( k in 1:N ){
          colList[k]=col(Features[TemTree[k]],Features[R],C )
        }
        Tem=c(Tem,TemTree[ which.max(colList) ] )
      }
    }
    
    # evaluation du méthode.
    if(f(Features[Tem],C) >f(Features[R],C )){
      R=Tem
    }else{
      return(R)
    }
    
  }
}
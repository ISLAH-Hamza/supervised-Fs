library(infotheo)
library(dplyr)
library(optrees)
library(tictoc)
######################## les outils ###########################

f=function(S,C){
   
  S<-discretize(S) %>% as.matrix()
  N<-nrow(S)
  Result<-1
  while(T){
    index<-c()
    e=S[1,]
    for(i in 1:nrow(S)){
      s<-sum(e==S[i,])
      index<-append(index, s == ncol(S))
    }
    Nj<-sum(index)/N
    R<-C[index,]
    d<-c()
    for(e in unique(R)){
      d<-which(R==e) %>% length() %>% append(d,.)
    }
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

col<-function(ai,R,C){
  S=0
  for(aj in R){
    tem<-cbind(ai,aj,C) %>%discretize() %>%interinformation()
    if(tem >0 ){
      S=S+tem
    }}
  return(S)
}


removeMaxEdgeTree<-function(Tree){
  #initialisation
  TreeArc<-as.data.frame(Tree$tree.arcs)    # tree arc should be a data frame
  TreeNode<-Tree$tree.nodes     
  subN1<-c()
  subArc1<-data.frame()
  
  index<-which.max(TreeArc[,3])
  subN1<-TreeArc[index,1]
  TreeNode<-TreeNode[-which(TreeNode==subN1)]
  TreeArc<-TreeArc[-index,]
  i=1
  while(i!=0){
    i=0
    for(j in 1:nrow(TreeArc)){
      isIn<-  !TreeArc[j,1:2] %in% subN1 
      if(sum(isIn)==1){
        elem<-TreeArc[j,c(isIn,F)]
        subN1<-c(subN1,elem)
        TreeNode<-TreeNode[-which(TreeNode==elem)]
        subArc1<-rbind(subArc1,TreeArc[j,])
        TreeArc<-TreeArc[-j,]
        i=1
      }
    }
    
  }
  R1<-list(tree.nodes=TreeNode,tree.arcs=TreeArc)
  R2<-list(tree.nodes=subN1,tree.arcs=subArc1)
  
  
  return(list(R1,R2))
}


####################### la fonction principale ##################################
GBS<-function(Features,C){
  Featuresbins=nrow(unique(round(Features*10)))
  Features=discretize( Features , nbins=Featuresbins )
  
  Cbins=nrow(unique(round(C*10)))
  C=discretize( C , nbins=Cbins )
  # Initialisation
  N<-ncol(Features)
  nodes<-1:N
  arcs<-matrix(ncol = 3)
  
  # création des Graphes
    for(i in 1:N){
      j<-i+1
      while( j <= N){
    
        ThreeI<-cbind(Features[i],Features[j],C) %>%interinformation()
        Hi<-Features[i]  %>% entropy()
        Hj<-Features[j]  %>% entropy()
        weight=ThreeI / (Hi +Hj )
        arcs<-rbind(arcs,c( i , j ,weight ) )
        
       j<-j+1
      }
     
    }
    arcs<-arcs[-1,]
    # Creation d'arbre couvrante de poid minimale
    tree<-getMinimumSpanningTree(nodes,arcs,algorithm ="Kruskal",check.graph=T,show.graph = F, show.data=F)
  
    Forst<-list(tree[1:2])
    # selection des Fs
    while(T){
       if(length(tree$tree.nodes)<=length(Forst)){
          return(tree$tree.nodes)
       }
      #suprimer l'element maximal du arcs
        maxEdg=-1  #la valeur minmale possible d'arret
        for(i in 1:length(Forst)){
        #!!! Verifier ce condition selement tree arcs exist 
        elem<- Forst[[i]]$tree.arcs
        if( nrow(elem)>0){ 
          if(maxEdg < max(elem[,3])){
            maxEdg <- max(elem[,3] )
            index<-i
          }
        }
        }
        subForst<-removeMaxEdgeTree(Forst[[index]])
        Forst<-Forst[-index]
        Forst<-append(Forst,subForst)
       #=====
       R<-c()
       i_line<-which.max(arcs[,3])
       R<-arcs[i_line,1:2]
       Temp<-R  #
       for(i in 1:length(Forst)){
         elem<-Forst[[i]]$tree.nodes
         #intersection entre deux vecteur à cherché
         if(length(intersect(elem,R)) == 0 ){
           S<-R
           maxCol<- -1
           for(a in elem){
              if(col(Features[a],Features[R],C)>= maxCol ){
                  maxCol<-col(Features[a],Features[R],C)
                  r=a
              } 
           }
           Temp<-append(Temp,r)
         }
       }
       
       #Evaluate the selected representative Temp as quality of the groupping F
       if(f(Features[Temp],C) >=f(Features[R],C) & f(Features[Temp],C)!=1){
         R<-Temp
       }else{
         S<-Temp
         return(S)
       }
       
    }
}




d=rep(0,6)
R<-1:6
for(i in 1:100){
  data<-toys(n=50,p=200)
  Features<-X<-data %>% select(-Q)
  C<-Y<-data %>% select(Q)
  tic()
    z=GBS(Features,C)
  toc()
  print(z)
  result=R %in% z
  d[result]<-d[result]+1
  print(i)
}







calc_dist <- function(x,y){
  
  dist = sqrt(sum((x-y)^2))
  return(dist)
}


get_index <- function(clusters,data,linkage){
  
    distance_mat <- matrix(Inf, ncol = length(clusters),nrow =length(clusters) ) 
  
  for (i in 1:length(clusters)){
    for(j in 1:length(clusters) ){
      if(j>i){
        i_vec = c()
        j_vec = c()
        points_i <- clusters[[i]]
        for(c in 1:length(points_i)){
          for(d in 1:length(points_i[[c]])){
            i_vec = c(i_vec,points_i[[c]][d])
          }
        }
        points_j <- clusters[[j]]
        for(c in 1:length(points_j)){
          for(d in 1:length(points_j[[c]])){
            j_vec = c(j_vec,points_j[[c]][d])
          }
        }
        
        clust_d <- c()
          for(c in 1:length(i_vec)){
            for(d in 1:length(j_vec)){
            clust_d = append(clust_d,calc_dist(data[i_vec[c],],data[j_vec[d],]))
          }
          }
       
        if(linkage=='single'){
        
        distance_mat[i,j] <- min(clust_d)
 
        }
        else if(linkage=='complete'){
        
          distance_mat[i,j] <- max(clust_d)
        }
        else if(linkage=='average') {
        
          distance_mat[i,j] <- mean(clust_d)
        }
        else{
          clust_d <- c()
          if(length(i_vec)==1){
            centroid_i = data[i_vec,]
          }
          else{
          centroid_i = colMeans(data[i_vec,])
          }
         
          if(length(j_vec)==1){
            centroid_j = data[j_vec,]
          }
          else{
            centroid_j = colMeans(data[j_vec,])
          }
         
              clust_d = append(clust_d,calc_dist(centroid_i,centroid_j))
              distance_mat[i,j] <- clust_d
           
        }
        
        
        
        }
      
      }
      
  }
    index = which(distance_mat==min(distance_mat),arr.ind=TRUE)
    
 
  
 
  return(list("index"=index,"height"=distance_mat[index[1],index[2]]))
}




HC <- function(data,linkage,labels){
  clusters = list()
  for(i in 1:nrow(data)){
    
    clusters[[i]] = list(i) 
  }
  
  
  
  for(i in 1:nrow(data)-1){
  
    
    param = get_index(clusters,data,linkage)
    index = param$index[1,]
    height = param$height
    
    if(max(index>1)){
      
      display(clusters,index,height,labels)
      if((length(clusters[[min(index)]][[1]])==1) && (length(clusters[[max(index)]][[1]])==1)){
        
        clusters[[min(index)]][[1]] = c(clusters[[min(index)]][[1]],clusters[[max(index)]][[1]])
        
      }
      
      else{
        clusters[[min(index)]] = c(clusters[[min(index)]],clusters[[max(index)]])
        
      }
      
      if(length(clusters)!=1){
        clusters <- clusters[-max(index)]
      }
      
      
    }
    
  
  }
  
  cat("\nFinal Cluster\n")
  print(clusters)
  cat("------------------------\n")

  return(clusters)
}




display <- function(clusters,index,height,labels){
  
  
  disp <- function(something){
    if (length(something)==1){
      something <- something[[1]]
    }
    if (typeof(something)=="list"){
      something <- unlist(something)
    }
    #something <- paste(something, collapse = " ")
    return (something)
  }
  
 
  a <- clusters[[min(index)]]
  b <- clusters[[max(index)]]
  cat("Merging [", disp(a), "] with [", disp(b), "]\n")
  cat("Labels of:[",disp(a),"] [",labels[unlist(a),],"]\n Labels of: [",disp(b),"] [",labels[unlist(b),],"]")
  cat(sprintf("\nHeight:  %f  \n\n",height))
 
}

dataset <- read.table("nci.data.txt")
data <- t(dataset)

labels <- read.table("label.txt")


cat("\n-----------Single Linkage------------\n")
clusters <- HC(data,'single',labels)
cat("\n-----------Complete Linkage------------\n")
HC(data,'complete',labels)
cat("\n-----------Average Linkage------------\n")
HC(data,'average',labels)
cat("\n-----------Centroid Linkage------------\n")
HC(data,'centroid',labels) 


print("k = 5")
k_means <- kmeans(data,5)
print(k_means$cluster)

print("k = 14")
k_means <- kmeans(data,14)
print(k_means$cluster)

print("k = 20")
k_means <- kmeans(data,20)
print(k_means$cluster)

print("k = 25")
k_means <- kmeans(data,25)
print(k_means$cluster)

print("k = 30")
k_means <- kmeans(data,30)
print(k_means$cluster)


print("k = 50")
k_means <- kmeans(data,50)
print(k_means$cluster)


  






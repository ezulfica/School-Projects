hclust_r2 = function(data , col = NULL , kmin , kmax , scaled = F , center = F){
  #col = choose a numeric vector for the column you want to keep 
  #if col is null it will work with your entire dataset
  #kmin, kmax = min and max for the number of cluster you want 
  #scaled = if you want to scale or center your dataset
  
  if ( is.null(col) ){ col = 1:length(data) }
  
  if (scaled == T & center == T){ data[col] = data.frame(scale(data[col])) }
  else if (scaled == F & center == T){data[col] = data.frame(scale(data[col], center = T, scale = F))}
  else if (scaled == T & center == F){data[col] = data.frame(scale(data[col], center = F, scale = T))}
  else {}
  
  TSS = sum(apply(data[col], FUN = function(x){(x - mean(x))**2} , MARGIN = 2)) #Total Sum Square
  data = data[col]
  
  r2 = function(k){
    
    data_ward = hclust(d = dist(data), method = "ward.D2")
    data_hclust = cutree(tree = data_ward, k = k) #clustering our observations in a numeric vector thanks to cutree function
    data["hclust"] = as.factor(data_hclust) #We create a new column were our obs are categorized by the rank of cluster
    
    WSS = function(i){
      data_clust = data[which(data$hclust == i),col]
      sum(apply(data_clust[col], FUN = function(x){(x - mean(x))**2} , MARGIN = 2))
      }
    
    TWSS = sum(sapply(X = unlist(unique(data["hclust"])), FUN = WSS)) #Total Within sum square
    r2 = (TSS - TWSS) / TSS
    }
    
  sapply(X = seq(kmin,kmax), FUN = r2)
  }

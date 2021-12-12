# Random NONOGRAMM
generate_nonogram = function(pattern){
  if (pattern == "random"){
    nonogram_matrix = matrix(sample(x = 0:1, size = ncols*nrows, replace = T, prob = c(1-p, p)), nrow = nrows, ncol = ncols)
    while (sum(rowSums(nonogram_matrix) == 0) != 0 | sum(colSums(nonogram_matrix) == 0) != 0) {
      nonogram_matrix = matrix(sample(x = 0:1, size = ncols*nrows, replace = T, prob = c(1-p,p)), nrow = nrows, ncol = ncols)
    }
    
    rows_clues = list()
    cols_clues = list()

    for (i in 1:nrows){
      r = rle(nonogram_matrix[i,])
      r_clues = r$lengths[r$values != 0]
      rows_clues[[i]] = r_clues
    }
    for (i in 1:ncols){
      r = rle(nonogram_matrix[,i])
      r_clues = r$lengths[r$values != 0]
      cols_clues[[i]] = r_clues
    }
    
    return(list(nonogram = nonogram_matrix, rows_clues = rows_clues, cols_clues = cols_clues))
    
  } else if (pattern == "penguin"){
    
    penguin = list(
      nonogram = c(0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,
                   0,0,0,0,0,0,0,1,1,1,0,0,1,1,1,1,0,0,0,1,1,1,
                   1,1,1,1,0,0,0,0,1,1,0,1,1,0,1,1,0,0,0,0,0,0,
                   0,0,1,0,0,1,1,1,1,0,0,0,0,0,0,0,1,1,0,0,1,1,
                   1,1,1,1,1,0,0,0,1,1,0,0,0,1,1,1,1,1,1,1,1,0,
                   0,1,0,0,0,0,0,0,0,1,1,1,1,1,1,1,0,0,0,0,0,0,
                   0,0,0,0,0,0,1,1) , 
      
      rows_clues = list(2, 2:1, 7, 6, c(1,3), 
                      1:2, 1:2,c(1,3), c(1,3), 
                      1:2, 1:2, c(1,1), c(3,2), c(2,5)
                      ),
      cols_clues = list(1, 1, c(3,4,1), c(6,2,1),
                      c(1,2,1), c(4,2), c(7,2),
                      c(8,1), 7, 2)
    )
    return(penguin)
    
  } else if (pattern == "yinyang"){
      yinyang = list(
        nonogram = NULL, 
        
        rows_clues = list(4,8,10,12,
                       c(12,1), c(2,8,1), c(3,6,1),
                       c(10,1), c(6,2,1), c(6,2,1), 
                       c(4,1), c(3,1), c(2,1), c(1,1), 
                       c(2,2), c(4)
                       ) , 
        cols_clues = list(4,8,10,c(3,7), 
                       c(4,4,1), c(9,1), c(8,1), c(8,1), 
                       c(8,1), c(8,1), c(6,1), c(5,2,1), 
                       c(4,2,1), c(1,1), 
                       c(2,2), 4)
        )
      return(yinyang)
      
  } else if(pattern == "boat"){
      boat = list(
        nonogram = NULL, 
        rows_clues = list(c(6,2), c(1,1,3), c(6,1,1,2), 
                       c(6,4,1,12), c(17,2), c(2,2), 26,
                       c(1,25), 26, 1),
        cols_clues = list(3,c(6,1),c(1,7),c(1,3,3),
                       c(1,3,3), c(1,3,3),c(5,3), c(1,3), 
                       c(1,3),
                       c(2,3), c(3,3), c(2,3), c(2,3),
                       c(1,3), c(1,3), c(1,3), c(2,3), 
                       c(3,1,3), c(2,1,3), c(3,3), c(2,3),
                       c(1,3), c(1,3), c(1,3), c(1,3), c(1,3),
                       c(1,2), c(1,2), 3, 2)
        )
    }
}
  






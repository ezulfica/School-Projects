# Metric function we want to minimize. 
# it takes fives parameters : 
#   a vector representing a result
#   the nonograms clues
#   the numbers of row and cols

count_conflit = function(X, r_clues, c_clues, nrows, ncols){

    if (! "matrix" %in% class(X)) X = matrix(X, nrow = nrows, ncol = ncols) 
    
    NSe_missing_row = rep(0,nrows) #pre-allocated vector to save errors by segment
    N_row_error = rep(0, nrows) #pre-allocated vector to save errors by rows  
    for (i in (1:nrows)){
      r1 = as.numeric(rle(X[i,])$lengths[rle(X[i,])$values == 1]) #existing numbers of cells by segments
      r2 = r_clues[[i]] #required numbers of cells by segments
      N_row_error[i] = sum(r2) - sum(r1) #differences of the existing numbers of cells by row and the required
      max_len = max(length(r1),length(r2)) #maximum number of segment between the require and existing row
      r1 = c(r1, rep(0, max_len - length(r1))) #we increase the size of the vector of length if necessary  
      r2 = c(r2, rep(0, max_len - length(r2))) #we increase the size of the vector of length if necessary  
      NSe_missing_row[i] =  sum(abs(r2 - r1)) #save the diffenrence of length between the required segments and existing
    }
    
    #same step as the rows, for the columns  
    NSe_missing_col = rep(0,ncols)
    N_col_error = rep(0,ncols)
    for (i in (1:ncols)){
      c1 = as.numeric(rle(X[,i])$lengths[rle(X[,i])$values == 1])
      c2 = c_clues[[i]]
      N_col_error[i] = sum(c2) - sum(c1)
      max_len = max(length(c1),length(c2))
      c1 = c(c1, rep(0, max_len - length(c1)))
      c2 = c(c2, rep(0, max_len - length(c2)))
      NSe_missing_col[i] = sum(abs(c2 - c1))
    }
    
    #Looking for the vectorial coordinate of which cells are exceding the rows or cols and which one are missing
    
    which_row_excess = c()
    for (i in which(N_row_error < 0)){
      which_row_excess = c(which_row_excess, 0:(ncols-1) * nrows + i)
    }

    which_col_excess = c()
    for (i in which(N_col_error < 0)){
      which_col_excess = c(which_col_excess, 1:(nrows) + nrows * (i-1))
    }
    
    which_row_need = c()
    for (i in which(N_row_error > 0)){
      which_row_need = c(which_row_need, 0:(ncols-1) * nrows + i)
    }
    
    which_col_need = c()
    for (i in which(N_col_error > 0)){
      which_col_need = c(which_col_need, 1:(nrows) + nrows * (i-1))
    }
    
    #coordinate of which black cells to move
    to_move = union(which_row_excess, which_col_excess)
    to_move = to_move[to_move %in% which(X == 1)]
    
    #coordinate of which white cells it is possible de change in black
    to_add = union(which_row_need, which_col_need)
    to_add = to_add[to_move %in% which(X == 0)]
    
    s = sum((NSe_missing_row), (NSe_missing_col)) #compute the risk
    
    #we return in a list, the risk, the coordinate of which one to move and to change. 
    return(list(s = s, to_move = to_move, to_add = to_add))
}
  

# conflict3 = function(X, nonogram){
#   X = matrix(X, nrow = nrows, ncol = ncols)
#   
#   Ne_b_row = rowSums(X)
#   Ne_b_col = colSums(X)
#   
#   Nr_b_row = sapply(r_clues, sum)
#   Nr_b_col = sapply(c_clues, sum)
#   
#   N_row_error = Nr_b_row - Ne_b_row
#   N_col_error = Nr_b_col - Ne_b_col
#   
#   Se_row = apply(X, 1, function(x) as.numeric(rle(x)$length[rle(x)$values == 1]))
#   Se_col = apply(X, 2, function(x) as.numeric(rle(x)$length[rle(x)$values == 1]))
#   
#   S_row_error = !mapply(identical, Se_row, r_clues)
#   S_col_error = !mapply(identical, Se_col, c_clues)
#   
#   b_edge_row = sapply(apply(X, 1, find_edge), length)
#   b_edge_col = sapply(apply(X,2, find_edge), length)
#   
#   b_bet_row = Ne_b_row - b_edge_row
#   b_bet_col = Ne_b_col - b_edge_col
#   
#   w_sep_row = sapply(apply(X,1, find_separator), length)
#   w_bet_row = ncols - Ne_b_row - w_sep_row
#   w_sep_col = sapply(apply(X,2, find_separator), length)
#   w_bet_col = nrows - Ne_b_col - w_sep_col
#   
#   S_row_length = mapply(function(x,y) length(x) - length(y), r_clues, Se_row)
#   S_col_length = mapply(function(x,y) length(x) - length(y), c_clues, Se_col)
#   
#   r_conflict = (S_row_error) * (
#     (N_row_error < 0) * (S_row_length < 0) * (b_bet_row) + 
#       (N_row_error < 0) * (S_row_length > 0) * (b_edge_row) +
#       (N_row_error > 0)  * (w_bet_row + w_sep_row) +
#       (S_row_length == 0) * (N_row_error < 0) * (Ne_b_row) + 
#       (S_row_length == 0) * (N_row_error > 0) * (w_sep_row) + 
#       (N_row_error == 0) * Ne_b_row 
#   ) 
#   
#   c_conflict =  (S_col_error) * (
#     (N_col_error < 0) * (S_col_length < 0) * (Ne_b_col) + 
#       (N_col_error < 0) * (S_col_length >= 0) * (b_edge_col) +
#       (N_col_error > 0) * (S_col_length >= 0) * (w_bet_col) +
#       (N_col_error > 0) * (S_col_length < 0) * (w_sep_col) +
#       (S_col_length == 0) * (N_col_error < 0) * (Ne_b_col) + 
#       (S_col_length == 0) * (N_col_error > 0) * (w_sep_col) +
#       (N_col_error == 0) * Ne_b_col
#   )
#   
#   s = sum(abs(r_conflict), abs(c_conflict))
#   return(list(s = s))
# }
#
# 
# find_edge = function(x) {
#   edge = c()
#   for (k in 1:length(x)){
#     if (k == 1) { if(x[k] == 1) edge[1] = 1}
#     else if (k > 1 && k < length(x)) { if (x[k] == 1 && (x[k-1] == 0 | x[k+1] == 0)) edge = c(edge, k) }
#     else if (k == length(x))  { if(x[k] == 1 && x[length(x)] == 0) edge = c(edge, length(x))}
#   }
#   return(edge)
# }
# 
# find_separator = function(x){
#   sep = c()
#   for (k in which(x == 0)) {
#     if (k == 1 & x[k+1] == 1) sep = c(sep, k)
#     if (k > 1 && k < length(x)) {if (x[k+1] == 1 | x[k-1] == 1) sep = c(sep, k)}
#     if (k == length(x)) {if(x[k-1] == 1) sep = c(sep, k)}
#   }
#   other = which(x == 0)
#   other = other[!other %in% sep]
#   return(list(sep = sep, other = other))
# }



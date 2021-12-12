# Generate an initial result for solving the nonogram
# The only parameters needed are the nonogram clues. 
# For each list function : 
#1 : we compute the require numbers of black cell and then add them randomly in the grid
#2 : we compute the required segment in order for each row and then move randomly each point by column

generate_initial_state = list(
  
  random_generation = function(r_clues = r_clues, c_clues = c_clues){
    nrows = length(r_clues)
    ncols = length(c_clues)
    nb_element = sum(unlist(r_clues))
    X = sample(x = c(rep(1,nb_element), rep(0, ncols*nrows - nb_element)), size = ncols*nrows)
    return(list(X= matrix(X, nrows, ncols), delta = 0))
  },
  
  byrow_generation = function(r_clues, c_clues){
    nrows = length(r_clues)
    ncols = length(c_clues)
    row_vals = c()

    for (r in r_clues){
      row_value = c()
      for (r_val in r) row_value = c(row_value, rep(1,r_val), 0)
      missing_val = ncols - length(row_value)
      
      if (missing_val >= 0) {
        row_value = c(row_value, rep(0, missing_val)) 
        } else {
          row_value = row_value[1:(ncols)]
        }
      
      row_vals = c(row_vals, row_value)
    }
    
    X = matrix(data = row_vals, nrow = nrows, ncol = ncols, byrow = T)
    return(list(X = X, delta = 0))
    }
)



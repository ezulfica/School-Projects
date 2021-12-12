if (restart) X = X_BEST #If we choose to restart, we use the best state found during the last simulation
X_BEST = X #set the BEST state as the initial one

H = rep(0,n_simul) #pre-allocated vector to save the risk of each iteration
H[1] = count_conflit(X$X, r_clues, c_clues, nrows, ncols)$s #adding the first risk

#text and ggplot model to plot the result as a table
nb_conflit = paste("N = 1\n", "NB Conflits = ", H[1])
p2 = plot_nonogram(X$X, r_clues, c_clues) + ggtitle(label = nb_conflit)

#If we set plot_solution as TRUE
if (plot_solution && !is.null(nonogram$nonogram)) {
  p1 = plot_nonogram(nonogram$nonogram, r_clues, c_clues) + ggtitle("Solution")
  plot_grid(p1,p2) 
  if (is.null(nonogram$nonogram)) print("No solution to plot !") 
  } else print(p2)
    

for (i in 2:n_simul){
  #increasing the temperature if cooling is TRUE
  beta_v = beta0 *log(i-1) * (i>2) * (cooling) + beta0 *(i == 2) * cooling + beta0 * !cooling 
  #changing the initial state
  X = transition(X = X$X, r_clues, c_clues, nrows, ncols, beta_v = beta_v)
  
  #Saving the new risk
  H[i] = H[i-1] + X$delta
  
  if (H[i] < min(H[1:i-1])) X_BEST = X #if we have a better risk, we change our X_BEST
  
  #To plot a result, with or without the solution, every step
  if (i %% step == 0 && H[i] != 0) {
    nb_conflit = paste("N =", i ,"\n", "NB Conflits =", H[i])
    p2 = plot_nonogram(X$X, r_clues, c_clues) + ggtitle(nb_conflit) 
    if (plot_solution && !is.null(nonogram$nonogram)){print(plot_grid(p1, p2))} else {print(p2)}
  }
  
  #When the risk is null, we stop the loop
  if (H[i] == 0 && H[i] != H[i-1]) {
    nb_conflit = paste("N =", i ,"\n", "NB Conflits =", H[i])
    p2 = plot_nonogram(X$X, r_clues, c_clues) + ggtitle(nb_conflit)
    if (plot_solution && !is.null(nonogram$nonogram)) {print(plot_grid(p1, p2))} else {print(p2)}
    break
  }
}




  



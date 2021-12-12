# ------------------------------------------------------------------------------- #
# ------------------------- NP COMPLETE NONOGRAM SOLVER ------------------------- #
# ------------------------------------------------------------------------------- #

#BARTHE ALEXANDRE
#ZULFICAR ERIC
#M2 PROBA STATS UGE
#SIMULATION ET COPULES

# ----------------------------------------------- #
# -------------- Packages required -------------- #
# ----------------------------------------------- #

require(ggplot2)
require(dplyr)
require(gridExtra)
require(cowplot)

# ----------------------------------------------- #
# --------------- Functions import -------------- #
# ----------------------------------------------- #

path = dirname(rstudioapi::getSourceEditorContext()$path) #directory path 
#You only need, every .R files in the same directory

#Looking for all the .R files in the directory with the patterned names
list_source = list.files(
  path = path, 
  pattern = "generate|plot|transition|initial_state|conflict", 
  all.files = T, 
  full.names = T
  )

#Import each file in the environnement
for (file in list_source){
  source(file, encoding = "UTF-8")
}

# ---------------------------------------------------------- #
# ----------------------- Parameters ----------------------- #
# ---------------------------------------------------------- #
set.seed(42)
options(scipen = 999) #to display numbers without scientific pen

#the nonogram is not required to be a square
nrows = 15 #number of rows for the random nonogram
ncols = 15 #number of columns for the random nonogram 
p = 0.45 #the ratio of black square in the random nonogram

# ---------------------------------------------------------- #
# ------------------- Nonogram generator ------------------- #
# ---------------------------------------------------------- #

nonogram = generate_nonogram(pattern = "random")
#pattern = c("random", "penguin", "yinyang" , "boat")
nrows = length(nonogram$rows_clues)
ncols = length(nonogram$cols_clues)
r_clues = nonogram$rows_clues
c_clues = nonogram$cols_clues

# ---------------------------------------------------------- #
# ---------------- Initial state generator ----------------- #
# ---------------------------------------------------------- #

#1 : random
#2 : generate the initial state according to the clues on the rows
#the transition function will change with the value generate
generate = 1
X = generate_initial_state[[generate]](r_clues, c_clues) #the initial_state

# ---------------------------------------------------------- #
# -------------------- SOLVING FUNCTION -------------------- #
# ---------------------------------------------------------- #

final_H = c()
beta0 = 0.3 #the initial temperature, in our case we multiple it and not divide it, so we increase our temperature
n_simul = 1200000 #number of simulation
cooling = T #whether we want to increase the temperature every simulation
plot_solution = T #To plot solution along the result (if we know the solution)
step = 1000 #to plot the result every step, to follow where we stand
restart = F#If we want to re-use the best X found during the last sampling

#RUN THE LOOP FOR SIMULATED ANNEALING
source(file = list.files(path = path, pattern = "solve_nonogram", full.names = T))
final_H = c(final_H, H) #to add the conflic before restarting if needed



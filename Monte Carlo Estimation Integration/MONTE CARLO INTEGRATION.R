# ------------ Paramètres #(ouvrir le script en utf-8)  ------------  #
options(scipen = 999)
n = 10000
a = 1 ; b = 2 ; c = 1 ; d = 3
N = 1000
set.seed(102)
phi = function(x,y) sqrt(cosh(x) + sin(y)**2)*log(1+x*y)
K = phi(2,3)
x = seq(1,2, length = 100)
y = seq(1,3, length = 100)
theta = 2

#calcul integrale de phi (à lancer pour les graphes)
area_v = pracma::quad2d(f = phi, xa = 1,xb = 2, ya = 1, yb = 3) 

#graphe de phi avec persp3d (pas besoin de lancer le package)
rgl::persp3d(x, y, outer(x,y, phi), col = 'red')

# ----- Fonctions liées aux copules : 

#Densité de la copule de Clayton avec pic en (1,1)
density_cop_Clayton = function(u,v,theta){
  y1 = (1+theta)*((1-u)*(1-v))^(-theta-1)
  y2 = (1-u)^(-theta) + (1-v)^(-theta) - 1
  y2 = y2^(-(1/theta) - 2)
  return(y1*y2)
}

#Inverse conditionnelle de la copule de Clayton
F_Clayton_inv_marg = function(u,z,theta){
  z1 = z**(-theta/(theta+1)) - 1
  z2 = z1* u**(-theta) + 1
  return(z2**(-1/theta))
}

#Simulation de U,V selon la Copule
#Ici, comme son souhaite avoir une concentration de point vers le sommet (2,3), on prend
#(1-U, 1-V)
simul_copul_param = function(n,F_inv,theta){
  U = runif(n)
  Z = runif(n)
  return(data.frame(U = 1-U, V = 1-F_inv(U,Z,theta)))
}

# ----------------------------------------------------------- #
# ------------------------ Méthode 1 ------------------------ #
# ----------------------------------------------------------- #

#Simulation des uniformes
U1 = runif(n,a,b)
U2 = runif(n,c,d)
U3 = runif(n, 0, K)

#Estimation de l'intégrale
(d-c)*(b-a)*K*sum(U3 <= phi(U1,U2))/n

#Espérance cumulée pour regarder la convergence
lfgn_1 = cumsum((d-c)*(b-a)*K*(U3 <= phi(U1,U2)))/1:n

#Graphique de la représentation de la loi forte des grands nombres
plot(x = 1:n, 
     y = lfgn_1, 
     ylim = c(area_v-0.5, area_v+0.5), 
     type = "l", 
     col = 1, 
     main = "Convergence vers l'integrale de phi sur [1,2]x[1,3]", 
     ylab = "Integrale", 
     xlab = "n")
abline(h = area_v, col = 3)
legend(x = 7000, y = 5.2, 
         legend=c("Méthode 1", "Intégrale à estimer"), 
         col = c(1,3),
         lty=1:2, 
         cex=0.8)

#Estimation sur 1000 simulation (application du TCL)
#On simule cette fois, n*N uniformes que l'on séparera en N colonnes de n observations
U1_N = runif(n*N,1,2)
U2_N = runif(n*N,1,3)
U3_N = runif(n*N, 0, K)

#Dans chaque colonne on compte le nombre moyen de point d'U3 inférieur à phi(U1, U2)
#On multiplie ensuite cette moyenne par le volume du cube où est définie le graphe notre fonction
TCL_M1 = apply(matrix((U3_N <= phi(U1_N,U2_N)), ncol = N), 2, function(x) mean(x)*(d-c)*(b-a)*K)

#plot de l'histogramme
hist(TCL_M1, main = "Estimation de l'Intégrale de phi avec 1000 simulations (Méthode 1)", 
     xlab = "Intégrale estimée", 
     ylab = "", 
     freq = F, col = "grey")

#Calcul de l'écart-type 
sd(TCL_M1)

# ----------------------------------------------------------- #
# ------------------------ Méthode 2 ------------------------ #
# ----------------------------------------------------------- #

#On reprend les variables U1 et U2 qui sont des uniformes sur [a,b] et [c,d]

#Estimation de l'intégrale
mean(phi(U1,U2))*(b-a)*(d-c)

#Espérance cumulée pour regarder la convergence
lfgn_2 = (d-c)*(b-a)*cumsum(phi(U1,U2))/1:n

#Graphique de la représentation de la loi forte des grands nombres
plot(x = 1:n, 
     y = lfgn_1, 
     ylim = c(area_v-0.5, area_v+0.5), 
     type = "l", 
     col = 1, 
     main = "Convergence vers le volume de phi sur [1,2]x[1,3]", 
     ylab = "Intégrale", 
     xlab = "n")
lines(lfgn_2, col = 4)
abline(h = area_v, col = 3)
legend(x = 7000, y = 5.2, 
       legend=c("Méthode 1", "Méthode 2", "Intégrale à estimer"), 
       col = c(1,4,3),
       lty=1, 
       cex=0.8)

#Estimation sur 1000 simulation (application du TCL)
#On reprend les vecteurs U1_N et U2_N qui sont des uniformes de taille n*N sur notre domaine
#dans une matrice de taille n*N, on calcul phi(u1,u2) puis 
#à chaque colonne on applique la moyenne multipliée à l'aire du domaine
TCL_M2 = apply(matrix(phi(U1_N,U2_N), ncol = N), 2, function(x) mean(x)*(d-c)*(b-a))

#plot de l'histogramme
hist(TCL_M2, main = "Estimation de l'Intégrale de phi avec 1000 simulations (Méthode 2)", 
     xlab = "Intégrale estimée", 
     ylab = "", 
     freq = F, col = "grey")

#Comparaison écart-type entre méthode 2 et méthode 1
sd(TCL_M2) - sd(TCL_M1)

# ----------------------------------------------------------- #
# ------------------------ Méthode 3 ------------------------ #
# ----------------------------------------------------------- #

#Première densité
c_alpha = sinh(2) - sinh(1) #constante de normalisation de l'intégrale
p1 = function(x) cosh(x) / c_alpha * (x<=2 & x>=1) #densité
P1 = function(x) ( (sinh(x) / c_alpha) - sinh(1)/c_alpha )*(x<=2 & x>=1) + 1 *(x>2) #Fonction de répartition
P1_inv = function(x) asinh(c_alpha*x + sinh(1)) * (x<=1 & x>=0) #CDF Inverse

#Seconde densité
p2 = function(x) (1/10)*(x + 3) * (x<=3 & x>=1) #densité
P2 = function(x) ( 1/10 * (x**2/2 + 3*x) - 7/20 )*(x<=3 & x>=1) + 1*(x>3) #CDF
P2_inv = function(x) (sqrt(20*(x+(7/20)) + 9) - 3 ) *(x<=1 & x>=0) #CDF inverse

#Simulation de X et Y où X~p1 et Y~p2 par fonction de répartition inverse
X = P1_inv(runif(n))
Y = P2_inv(runif(n))

#Estimation de l'intégrale
p = p1(X)*p2(Y)
mean(phi(X, Y)/p)

#Espérance cumulée pour regarder la convergence
lfgn_3 = cumsum(phi(X, Y)/p)/1:n

#Graphique de la représentation de la loi forte
plot(x = 1:n, 
     y = lfgn_1, 
     ylim = c(area_v-0.5, area_v+0.5), 
     type = "l", 
     col = 1, 
     main = "Convergence vers le volume de phi sur [1,2]x[1,3]", 
     ylab = "Intégrale", 
     xlab = "n")
lines(lfgn_2, col = 4)
abline(h = area_v, col = 3)
lines(lfgn_3, col = 2)
legend(x = 7000, y = 5.1, 
       legend=c("Méthode 1", "Méthode 2", "Méthode 3", "Intégrale à estimer"), 
       col = c(1,4,2,3),
       lty=1, 
       cex=0.8)


#Estimation sur 1000 simulation (application du TCL)
X_N = P1_inv(runif(n*N))
Y_N = P2_inv(runif(n*N))

#Estimation sur 1000 simulation (application du TCL)
approx = matrix(phi(X_N, Y_N)/(p1(X_N)*p2(Y_N)), ncol = N)
TCL_M3 = apply(X = approx, MARGIN = 2, FUN = mean)

#plot histogramme
hist(TCL_M3, main = "Estimation de l'Intégrale de phi avec 1000 simulations (Méthode 3)", 
     xlab = "Intégrale estimée", 
     ylab = "", 
     freq = F, col = "grey")

#Comparaison méthode 3 et méthode 2
sd(TCL_M3) - sd(TCL_M2)

# ----------------------------------------------------------- #
# -------------------- Méthode 4 - 1 ------------------------ #
# ----------------------------------------------------------- #

#la densité h obtenue grâce au théorème de Sklar, 
h_clayton_uniform = function(x,y){
  density_cop_Clayton(punif(x, a,b), punif(y, c,d), theta) * dunif(x,a,b) * dunif(y,c,d)
}

#graphe de h_clayton_uniform
rgl::persp3d(x, y, outer(x,y, h_clayton_uniform) , col = 'red')

#Simulation du couple U,V ~ Clayton avec pic (1,1)
UV_Clayton = simul_copul_param(n = n, F_Clayton_inv_marg, theta = theta) 

#Simulation de notre couple (U1, U2) où les marges sont des uniformes sur notre domaine 
U2_unif = qunif(UV_Clayton$U,a,b) #Simulation d'une uniforme sur [1,2]
V2_unif = qunif(UV_Clayton$V,c,d) #Simulation d'une uniforme sur [1,3]

#Estimation de l'intégrale
mean(phi(U2_unif,V2_unif)/h_clayton_uniform(U2_unif,V2_unif))

#Espérance cumulée pour regarder la convergence
lfgn_4_unif = cumsum(phi(U2_unif,V2_unif)/h_clayton_uniform(U2_unif,V2_unif))/1:n

#Graphique de la représentation de la loi forte
plot(x = 1:n, 
     y = lfgn_1, 
     ylim = c(area_v-0.5, area_v+0.5), 
     type = "l", 
     col = 1, 
     main = "Convergence vers le volume de phi sur [1,2]x[1,3]", 
     ylab = "Intégrale", 
     xlab = "n")

lines(lfgn_2, col = 4)
lines(lfgn_3, col = 2)
abline(h = area_v, col = 3)
lines(lfgn_4_unif, col = 5)
legend(x = 5900, y = 5.1, 
       legend=c("Méthode 1", "Méthode 2", "Méthode 3", "Méthode 4 - 1", "Intégrale à estimer"), 
       col = c(1,4,2,5,3),
       lty=1, 
       cex=0.8)

#Simulation du couple U,V ~ Clayton avec pic (1,1) de taille n*N
UV_N_Clayton = simul_copul_param(n = n, F_Clayton_inv_marg, theta =  theta)

#Simulation des uniformes [a,b]x[c,d] (application du TCL) - - Méthode 4-1
U2_N_Clayton_unif = qunif(UV_N_Clayton$U,a,b)
V2_N_Clayton_unif = qunif(UV_N_Clayton$V,c,d)

#Estimation sur 1000 simulation (application du TCL) - - Méthode 4-1
hist(TCL_M4_unif, main = "Estimation de l'Intégrale de phi avec 1000 simulations (Méthode 4-1)", 
     xlab = "Intégrale estimée", 
     ylab = "", 
     freq = F, 
     col = "grey")

#Moyenne sur chaque colonne du rapport phi/h 
TCL_M4_unif = apply(
  X = matrix(
    phi(U2_N_Clayton_unif,V2_N_Clayton_unif) * h_clayton_uniform(U2_N_Clayton_unif,V2_N_Clayton_unif)**(-1), 
    ncol = N), 
  MARGIN = 2, 
  FUN = mean)

#Comparaison avec la méthode 3
sd(TCL_M4_unif) - sd(TCL_M3)


# ----------------------------------------------------------- #
# -------------------- Méthode 4 - 2 ------------------------ #
# ----------------------------------------------------------- #

#densité h avec les densité de la méthode 3
h_clayton_m3 = function(x,y){
  density_cop_Clayton(P1(x), P2(y), theta) * p1(x) * p2(y)
}

#graphe de h_clayton_m3
rgl::persp3d(x, y, outer(x,y, h_clayton_m3) , col = 'red')

#Simulation de notre couple (U1, U2) où les marges suivent les densités de la méthode 3
U2_m3 = P1_inv(UV_Clayton$U) #Simulation d'une uniforme sur [1,2]
V2_m3 = P2_inv(UV_Clayton$V) #Simulation d'une uniforme sur [1,3]

#nuage de point de nos variables
plot(U2_m3,V2_m3)

#Estimation de l'intégrale - densité Méthode 3
mean(phi(U2_m3,V2_m3)/h_clayton_m3(U2_m3,V2_m3))

#Espérance cumulée pour regarder la convergence
lfgn_4_m3 = cumsum(phi(U2_m3,V2_m3)/h_clayton_m3(U2_m3,V2_m3))/1:n

#Graphique de la représentation de la loi forte
plot(x = 1:n, 
     y = lfgn_1, 
     ylim = c(area_v-0.5, area_v+0.5), 
     type = "l", 
     col = 1, 
     main = "Convergence vers le volume de phi sur [1,2]x[1,3]", 
     ylab = "Intégrale", 
     xlab = "n")

lines(lfgn_2, col = 4)
lines(lfgn_3, col = 2)
abline(h = area_v, col = 3)
lines(lfgn_4_unif, col = 5)
lines(lfgn_4_m3, col = 6)

legend(x = 5900, y = 5.1, 
       legend=c("Méthode 1", "Méthode 2", "Méthode 3", "Méthode 4 - 1", "Méthode 4 - 2", "Intégrale à estimer"), 
       col = c(1,4,2,5,6,3),
       lty=1, 
       cex=0.8)

#N Simulation des variables ~ p1, p2 (application du TCL) 
U2_N_Clayton_m3 = P1_inv(UV_N_Clayton$U)
V2_N_Clayton_m3 = P2_inv(UV_N_Clayton$V)

#Moyenne sur chaque colonne du rapport phi/h - Méthode 4-2
TCL_M4_m3 = apply(
  X = matrix(
    phi(U2_N_Clayton_m3,V2_N_Clayton_m3) * h_clayton_m3(U2_N_Clayton_m3,V2_N_Clayton_m3)**(-1), 
    ncol = N),
  MARGIN = 2, 
  FUN = mean)

hist(TCL_M4_m3, main = "Estimation de l'Intégrale de phi avec 1000 simulations (Méthode 4-2)", 
     xlab = "Intégrale estimée", 
     ylab = "", 
     freq = F, col = "grey")

#Comparaison avec la méthode 3
sd(TCL_M4_m3) - sd(TCL_M3)

#Comparaison entre méthode 4
sd(TCL_M4_m3) - sd(TCL_M4_unif)


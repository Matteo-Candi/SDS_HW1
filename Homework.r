# Homework 1 of Statistcs for Data Science
# Accademic year: 2022/2023
# Authors: Barba Paolo, Candi Matteo
rm(list=ls())

# Exercise 1 Stopping time   ----------------------------- 
######### Brutti's code############################
M <- 1000             # simulation size
t_data <- rep(NA, M)   # pre-alloc the data structure (simple vector)
for (m in 1:M){
  x <- runif(1)    # sampling the threshold value
  y <- runif(1)    # first process value
  t <- 1
  while (y > x) {
    t <- t + 1
    y <- runif(1)  # no memory
  }
  t_data[m] <- t   # save
}
p_hat <- proportions( table( t_data ) )


# True marginal PMF of T
pT <- function(t)  1/(t*(t+1))

# Plot it zooming in between t = 1 and t = 25
t_seq <- 1:25

# True PMF of T
plot(t_seq, pT(t_seq), 
     type = "h",
     lwd = 4,
     col = "cyan4",
     ylab = expression(p[T]),
     xlab = "t",
     main = "Marginal of the Stopping Time",
     sub = paste("Simulation size:", M))

points(t_seq, pT(t_seq), pch = 21, 
       col = "black", bg = "darkgoldenrod2", cex = 1.5)

# Simulative approximation to the PMF of T
points(t_seq, p_hat[t_seq], pch = 24, 
       col = "black", bg = "yellow", cex = .7)


step_size = 10
ave_step  = seq(1, length(t_data), step_size)
ave_vec = rep(NA, length(ave_step))

t <- 0
for (i in ave_step){
  t <- t + 1
  ave_vec[t] = mean( t_data[1:i] )
}


plot(ave_vec , main = "Average stopping time",
     xlab = "step", ylam = "Averege stopping time",
     sub = "Simulation study", col = "steelblue" , type = "l" , lwd =3)
grid()

#Comments:

#Supponiamo di  dover pagare una cifra c per giocare a questo gioco e si vincono tot euro quanto è il tempo di attesa.Ci si può chiedere qual'è la cifra "giusta" per partecipare a questo gioco.

#In media ci si aspetta di vincere una somma infinita da questo gioco,quindi, in accordo con la teoria tradizionale del valore atteso,ci si può permettere di pagare qualunque cifra c per partecipare. Infatti anche pagando un miliardo per volta, alla lunga, dovrà capitare la volta (con probabilità invero bassissima) di una vincita così strepitosa da ripagare abbondantemente tutte le altre quote investite per ottenere vincite insignificanti.In pratica però, nessuna persona ragionevole è disposta a pagare più di qualche unità per partecipare a questo gioco. Il rifiuto intuitivo a investire cifre importanti nel gioco è ben sostenuto dalla simulazione descritta nel grafico seguente. Ripetendo 10000 volte la serie si ottengono all'inizio vincite mediamente basse (qualche unità). Successivamente la media si alza, in corrispondenza di qualche serie fortunata, per poi diminuire leggermente fino al prossimo colpo fortunato. L'andamento complessivo è senza dubbio crescente, e tenderà matematicamente all'infinito dopo una serie infinita di giocate ma, nelle 10.000 giocate della simulazione, la media delle vincite è arrivata appena al valore di 6.


#Dal punto di vista matematico, non sorgono difficoltà dalla situazione prospettata nel gioco.In altre parole, è perfettamente coerente accettare la possibilità (infinitesima) di una vincita infinita, tale da bilanciare qualunque somma pagata nelle (infinite) volte in cui la vincita risulta insignificante.

#Possibile soluzione, vincita massima fissata a L:




















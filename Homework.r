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












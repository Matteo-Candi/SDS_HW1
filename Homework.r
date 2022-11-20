# Homework 1 of Statistcs for Data Science
# Accademic year: 2022/2023
# Authors: Barba Paolo, Candi Matteo
rm(list=ls())

# Exercise 1 Stopping time   ----------------------------- 
######### First code############################
M <- 10000             # simulation size
t_data <- rep(NA, M)   # pre-allocation the data structure (simple vector)
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
     xlab = "step", ylab = "Averege stopping time",
     sub = "Simulation study", col = "steelblue" , type = "l" , lwd =3)
grid()

#Comments:

#Suppose we have to pay c euros to start this experiment and we win as many dollars as the waiting time.
#The question we want to answer is: What is the "fair" amount in order to play this game. 

#On average you expect to win an infinite amount from this game, so according to traditional expected value theory, you can afford to pay any amount c to play.


#In fact, reapeating this experiment many time, it will have to happen (with very low probability)  to win so much, enough for paying all the expenses incurred before.

#In practice though, no reasonable person is willing to pay a lot of money to play this game. The intuitive refusal to invest large sums in the game is well supported by the simulation described in the following graph. By repeating the series 10,000 times, on average low winnings are obtained at the beginning (a few units). Subsequently the average rises, corresponding to some lucky series, and then decreases slightly until the next lucky hit. The overall trend is undoubtedly growing, and will mathematically tend to infinity after an infinite series of plays but, in the 10,000 plays of the simulation, the average of the winnings has just reached the value of 10.

#From the statistical point of view, no difficulties came from the situation presented in the game. In other words, it is perfectly coherent to accept the  possibility of an infinite win, such as to balance any amount paid in the (infinite) times the win is insignificant.


#Conclusion:
# Ultimately, the decision to play or not to play this game  must depend on each individual's risk, itself dependent on many parameters, such as initial revenue or the amount you are willing to lose.

#If the earnings here are "on average" infinite, you must also have infinite funds and play an infinite number of times to be eligible for certain earnings.

#############Second way (hopefully faster)############

#The main idea is to simulate a uniform(0,1) that will be the probbability's parameter of the geometric distibution. Basically we are doing a mixture model T ~ Geom(u) where U ~ Unif(0,1)


M <- 10000             # simulation size
t_data <- rep(NA, M)   # # pre-allocation the data structure (simple vector)

for(m in 1:M){
  u <- runif(n = 1)
  t_data[m] <- rgeom(n = 1, prob = u)
}

#RE-plotting the true distribution
plot(t_seq, pT(t_seq), 
     type = "h",
     lwd = 4,
     col = "cyan4",
     ylab = expression(p[T]),
     xlab = "t",
     main = "Marginal of the Stopping Time",
     sub = paste("Simulation size:", M))


p_hat <- proportions( table( t_data ) )
points(t_seq, p_hat[t_seq], pch = 24, 
       col = "black", bg = "yellow", cex = .7)

# It works daje, for sure faster !!!


#############Third way (hopefully faster)############

#Thinking as before but more treaky in the code ( as Matteo said)

M <- 10000             # simulation size
t_data <- rep(NA, M)   # # pre-allocation the data structure (simple vector)

stop_simulation <-rgeom(n = M, prob = runif(n = M))

p_hat <- proportions( table( stop_simulation ) )

#RE-RE-plotting the true distribution

plot(t_seq, pT(t_seq), 
     type = "h",
     lwd = 4,
     col = "cyan4",
     ylab = expression(p[T]),
     xlab = "t",
     main = "Marginal of the Stopping Time",
     sub = paste("Simulation size:", M))

points(t_seq, p_hat[t_seq], pch = 24, 
       col = "black", bg = "yellow", cex = .7)


#It WORKSS, Daje matte!!


















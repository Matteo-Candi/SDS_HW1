<<<<<<< HEAD
# Homework 1 of Statistcs for Data Science
# Accademic year: 2022/2023
# Authors: Barba Paolo, Candi Matteo
=======

>>>>>>> de61e67e83316227c3db1d5163a76a15f4e893ea
rm(list=ls())


#############by now the fastest way############

#The idea is to use a mixture model, so we generate m Unif(0,1) and then use them as a parameters of the geometric distributions 
#Thinking as before but more treaky in the code ( as Matteo said)

Sim <- c(100, 1000, 10000, 100000, 1000000, 10000000)

fin <- c()
for (s in Sim){
  beg <- Sys.time()  
  stop_simulations <-  rgeom(n = M, prob = runif(n = M))
  fin <- c(fin , Sys.time() - beg)
  
} 

tab_time <- data.frame (Simulation_size  = c("100", "1000", "10000", "100000", "1000000", "10000000") ,
            Computational_time = fin
)
tab_time



M <- M             # simulation size
# # pre-allocation the data structure (simple vector)

stop_simulation <-rgeom(n = M, prob = runif(n = M))

p_hat <- proportions( table( stop_simulation ))

#RE-RE-plotting the true distribution
pT <- function(t)  1/(t*(t+1))

# Plot it zooming in between t = 1 and t = 25
t_seq <- 1:25


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

legend("topright", legend = c("True model" , "simulation result"),
       col = c("cyan4","yellow"),lty=c(1,NA),pch=c(NA,24), cex=0.7,
       box.lty=0)


step_size = 10
ave_step  = seq(1, length(stop_simulation), step_size)
ave_vec = rep(NA, length(ave_step))

t <- 0
for (i in ave_step){
  t <- t + 1
  ave_vec[t] = mean( stop_simulation[1:i] )
}


plot(ave_vec , main = "Average stopping time",
     xlab = "step", ylab = "Averege stopping time",
     sub = "Simulation study", col = "steelblue" , type = "l" , lwd =3)
grid()
#It WORKSS, Daje matte!! 

#Analyis:

#Suppose we are a Casino and we want to introduce this experiment in our available games. The costumers have to pay an amount of C euros to start this game and he win as many euros as the waiting time.

#The question we are interested to answer is: 
#What is the "fair" amount oumnto in order to play this game?.

#To answer the question, we need to compute the expected value of this random variable. How much you pay to enter the game is the expected value of the random variable.


#On average you expect to win an infinite amount from this game, so according to traditional expected value theory, you can afford to pay any amount C to play.The mathematical result provides as a solution that it is convenient to pay any amount of money to play this game.


#In fact, reapeating this experiment many time, it will have to happen (with very low probability)  to win so much, enough for paying all the expenses incurred before.

#In practice though, no reasonable person is willing to pay a lot of money to play this game. The intuitive refusal to invest large sums in the game is well supported by the simulation described in the following graph. By repeating the series 10,000 times, on average low winnings are obtained at the beginning (a few units). Subsequently the average rises, corresponding to some lucky series, and then decreases slightly until the next lucky hit. The overall trend is undoubtedly growing, and will mathematically tend to infinity after an infinite series of plays but, in the 10,000 plays of the simulation, the average of the winnings has just reached the value of 10.

#From the statistical point of view, no difficulties came from the situation presented in the game. In other words, it is perfectly coherent to accept the  possibility of an infinite win, such as to balance any amount paid in the (infinite) times the win is insignificant.


#Conclusion:
# Ultimately, the decision to play or not to play this game  must depend on each individual's risk, itself dependent on many parameters, such as initial revenue or the amount you are willing to lose.

#If the earnings here are "on average" infinite, you must also have infinite funds and play an infinite number of times to be eligible for certain earnings.

#No casinos would introduce  this game, having the risk of infinitis loss. A possible solution is to force the game to finish with a maximum win of L after the L-esimo trials.

L = 8
stop_simulation[stop_simulation>7] = L
mean(stop_simulation)
tab = proportions(table(stop_simulation))
library(ggplot2)
# Create Data
data=data.frame(tab)
ggplot(data, aes(x=stop_simulation, y=Freq,fill=stop_simulation))+
  geom_bar(stat="identity") +
  theme_minimal() +
  geom_bar(stat="identity", fill="steelblue")+
  xlab("stop time") + ylab("Frequencies") +  guides(fill="none") +
  ggtitle("Barplot of stopping time" ) + 
  scale_y_continuous(labels=scales::percent)  

#Remark:50% of the time the stopping time is equal to 0 and no payoff for the player.
# The percentege of the time when the game is stopped is
data$Freq[data$stop_simulation == 8]

# Let's go further
# Consider a enter cost C greater than the mean ( casinos are unfair!) now we can have a look at the revenue function.

C <- 2 #cost i order to enter the game

costs <- rep(C , M)
revenue_f<- cumsum(costs - stop_simulation)
i <- seq(from = 0 , to = length(revenue_f), by = step_size)
rev = revenue_f[i]


plot(rev,,type = 'l',main = "Revenue function",
     lwd = 2,
     col = "cyan4",
     ylab = expression(p[T]),
     xlab = "t",
     sub = paste("Simulation size:", M))
grid()
################












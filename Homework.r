
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
# # pre-allocation the data structure (simple vector)

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

#No casinos would accept to play this game, having the risk of infinitis loss,a possible solution force the game to finish with a maximum win of L after the L-esimo trials.

L = 8
stop_simulation[stop_simulation>7] = L
#Considering the fact of the maximum revenue fixed to    L = 8 in the simultation study it result a "fair" amount to pay as
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

# Let's have a look further
# Consider a enter cost c greter than the mean ( casinos are unfair!) now eh can have a look at the revenue function.
c <- 2 #cost i order to enter the game

costs <- rep(c , M)

revenue_f<- cumsum(costs - stop_simulation)
i <- seq(from = 0 , to = length(revenue_f), by = step_size)
rev = revenue_f[i]
length(rev)

plot(rev,,type = 'l',main = "Revenue function",
     lwd = 2,
     col = "cyan4",
     ylab = expression(p[T]),
     xlab = "t",
     sub = paste("Simulation size:", M))
grid()



# Exercise 2 --------------------------------------------------------------

rm(list=ls())


library(VGAM)   # Import package for Laplace distribution

# Defining step functions for p_hat 
p_hat_func <- function(x , bins , p_hat ){
  interval <- cut(x, bins, include.lowest = T)
  levels   <- levels(interval)
  f        <- p_hat[interval == levels]
  return(f)
}

# Defining step functions for q_hat

q_hat_func <- function(x, bins, q_hat){
  interval <- cut(x, bins, include.lowest = T)
  levels   <- levels(interval)
  f        <- q_hat[interval == levels]
  return(f)
}



#Define the simulation function

simulation_function <- function(m , sim_size = 100, n=100, h = 1/m , eps = .1 ){
  for(rep in 1:sim_size){  
    m <- 5
    integral_p <- c()       # Pre-allocate the vector of the integral
    integral_q <- c()       # Pre-allocate the vector of the integral
    
    X <- rbeta(n, 10, 10)   # Generating the random sample from the beta
    
    bins <- seq(0, 1, h)    # Set the bins
    
    intervals <- cut(X, bins, include.lowest = T)  # Rename units with the bins they belong
    
    
    pj_hat <- table(intervals) / n              # Finding the frequencies of units inside each bins
    
    
    p_hat <- as.vector(pj_hat / h)              # Computing high of each bin dividing the frequencies for the width of the bin
    
    nu <- rlaplace(m, 0, 2/eps)                 # Generating m values from a Laplacian: one for each bin
    
    
    Dj <- table(intervals) + nu                 # Adding nu to every absolute frequencies of each bin
    
    Dj[Dj < 0] = 0    # Set all the nagative values to 0 t                      
    qj_hat = Dj
    
    # Finding qj_hat dividing max(0, Dj) for the sum of Dj
    if (sum(qj_hat) != 0){
      qj_hat <- qj_hat / sum(qj_hat)} else {qj_hat <- rep(0, length(qj_hat))}
    

    q_hat <- qj_hat / h      # Computing the high of the histogram dividing by the width of the columns
    
    
    # Compute the function to integrate 
    to_integrate_1 <- function(x){return((dbeta(x, 10, 10) - p_hat_func(x,bins = bins , p_hat = p_hat))^2)}
    to_integrate_2 <- function(x){return((dbeta(x, 10, 10) - q_hat_func(x,bins = bins , q_hat = q_hat))^2)}
    # Compute the integral
    p <- integrate( Vectorize(to_integrate_1) , lower = 0 , upper = 1, subdivisions=2000)$value
    q <- integrate( Vectorize(to_integrate_2) , lower = 0 , upper = 1, subdivisions=2000)$value
    integral_p <- c(integral_p, p)
    integral_q <- c(integral_q, q)
    
  }
  mise_p <- mean(integral_p)   #Save the results
  mise_q <- mean(integral_q)   #Save the results
  return(c(mise_p , mise_q))
}



simulation_function(m=3)




M = seq(5, 10 , 2)

results <- sapply(M ,simulation_function)

data = data.frame(results , row.names = c("MISE p_hat" , "MISE q_hat"))  
colnames(data) =  M
data

mixture_fun <- function(x, shape1 = 10) {
  
}


mixture_beta <- function(x, shape_1 = 2 , shape_2 = 15 , shape_3 = 12 , shape_4 = 6 , pi = 0.6 ){
  f <- pi * dbeta(x, shape1 = shape_1 , shape2 = shape_2) + (1 - pi) * dbeta(x, shape1 = shape_3 , shape2 = shape_4)
  return(f)
  
}
curve(mixture_beta(x))


curve(dbeta( x, 2 , 15))

curve(dbeta( x, 12 , 6 ), add = T)


######


simulation_function_2 <- function(m , sim_size = 100, n=100, h = 1/m , eps = .1 ){
  for(rep in 1:sim_size){  
    m <- 5
    integral_p <- c()       # Pre-allocate the vector of the integral
    integral_q <- c()       # Pre-allocate the vector of the integral
    
    X <- rbeta(n, 10, 10)   # Generating the random sample from the beta
    
    bins <- seq(0, 1, h)    # Set the bins
    
    intervals <- cut(X, bins, include.lowest = T)  # Rename units with the bins they belong
    
    
    pj_hat <- table(intervals) / n              # Finding the frequencies of units inside each bins
    
    
    p_hat <- as.vector(pj_hat / h)              # Computing high of each bin dividing the frequencies for the width of the bin
    
    nu <- rlaplace(m, 0, 2/eps)                 # Generating m values from a Laplacian: one for each bin
    
    
    Dj <- table(intervals) + nu                 # Adding nu to every absolute frequencies of each bin
    
    Dj[Dj < 0] = 0    # Set all the nagative values to 0 t                      
    qj_hat = Dj
    
    # Finding qj_hat dividing max(0, Dj) for the sum of Dj
    if (sum(qj_hat) != 0){
      qj_hat <- qj_hat / sum(qj_hat)} else {qj_hat <- rep(0, length(qj_hat))}
    
    
    q_hat <- qj_hat / h      # Computing the high of the histogram dividing by the width of the columns
    
    
    # Compute the function to integrate 
    to_integrate_1 <- function(x){return(( mixture_beta(x) - p_hat_func(x,bins = bins , p_hat = p_hat))^2)}
    to_integrate_2 <- function(x){return(( mixture_beta(x) - q_hat_func(x,bins = bins , q_hat = q_hat))^2)}
    # Compute the integral
    p <- integrate( Vectorize(to_integrate_1) , lower = 0 , upper = 1, subdivisions=2000)$value
    q <- integrate( Vectorize(to_integrate_2) , lower = 0 , upper = 1, subdivisions=2000)$value
    integral_p <- c(integral_p, p)
    integral_q <- c(integral_q, q)
    
  }
  mise_p <- mean(integral_p)   #Save the results
  mise_q <- mean(integral_q)   #Save the results
  return(c(mise_p , mise_q))
}



simulation_function_2(m = 2)





# Exercise 1 Stopping time   ----------------------------- 
rm(list=ls())
set.seed(13112221)

##### FAST Algortithm #####
Sim <- c(100, 1000, 10000, 100000, 1000000, 10000000)  # vector of the size 
fin <- c()    # inizialize the the vector of the result
timing_fun <- function(s){
  beg <- Sys.time()     # starting time
  stop_simulations <-  rgeom(n = s, prob = runif(n = s))   # run the simulation
  fin <- Sys.time() - beg #ending time
  return(fin)
}

results <- sapply(Sim , timing_fun)

tab_time <- data.frame (Simulation_size  = c("100", "1000", "10000", "100000", "1000000", "10000000") , Computational_time = results  )   # creating the dataframe of the results
tab_time
paste("The median speed over M is : " , median(results))
##### Analysis ######


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

##### Computational Analysis #####

Sim <- c(100, 1000, 10000, 100000, 1000000, 10000000 )  # vector of the size 
M <- 100    # Simulation size
diff <- matrix(NA , nrow =  length(Sim) , ncol = M )       # inizialize the the vector of the result


for (j in 1:M){
  i <- 1
  for (s in Sim){    
    stop_simulations <-  rgeom(n = s, prob = runif(n = s))   # run the simulation
    p_hat <- proportions(table(stop_simulations))
    
    diff[i,j] <- sum(pT(t_seq) - p_hat[t_seq], na.rm = T) 
    diff
    i <- i + 1
}
}


save( diff , file='Computational_analysis')



load("Computational_analysis")



par(mfrow = c( 1, 1))
plot(diff[1,] , ylim  = c( -.09 , .09),
     type = "l" , main = "Series of the errors" , 
     lwd = 2 , xlab = "Trials",
     ylab = "Sum of the errors per Simulation Size" ) 


points(diff[2,] , col = 'blue' , type = "l", lwd = 2)
points(diff[3,] , col = 'red', type = "l",  lwd = 2)  
points(diff[4,] , col = 'green', type = "l", lwd = 2)
points(diff[5,] , col = "pink", type = "l",  lwd = 2)

legend("topright",legend = Sim, col = c("black","blue", "red", "green", "pink"), lty = 1 ,merge = TRUE, bg = "lightgray")
grid()


#####

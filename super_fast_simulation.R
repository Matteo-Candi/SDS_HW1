rm(list=ls()) 
library(VGAM)
library(foreach)
library(parallel)
library(doSNOW)




# Exercise_2
set.seed(13112221)  # For reprocibility
##### Step Function for p_hat #####

# Defining step functions for p_hat 
p_hat_func <- function(x , bins , p_hat){
  interval <- cut(x, bins, include.lowest = T)
  return(p_hat[interval])
}

##### Step Function for q_hat #######
q_hat_func <- function(x, bins, q_hat){
  interval <- cut(x, bins, include.lowest = T)
  return(q_hat[interval])
}

##### Mixture distribution of Beta ######
dmixture <- function(x, shape_1 = 2 , shape_2 = 15 , shape_3 = 12 , shape_4 = 6 , pi = 0.6 ){
  f <- pi * dbeta(x, shape1 = shape_1 , shape2 = shape_2) + (1 - pi) * dbeta(x, shape1 = shape_3 , shape2 = shape_4)
  return(f)
}

##### Random sample from Mixture Beta  #####
rmixture <- function(n, shape_1 = 2 , shape_2 = 15 , shape_3 = 12 , shape_4 = 6 , pi = 0.6 ){
  sam <- c()
  u <- runif(n)
  for (x in u){
    if (x < pi) sam <-c(sam, rbeta(1, shape1 = shape_1 , shape2 = shape_2))
    else sam <- c(sam , rbeta(1, shape1 = shape_3 , shape2 = shape_4) )
  }
  return(sam)
}

#####  Simulation Function #######
simulation_function <- function(m , sim_size = 100, n=100, h = 1/m , eps = .1, func='beta'){
  
  print(m)
  
  if(func == 'beta'){
    distr = function(x) dbeta(x,shape1 = 10, shape2 = 10)
    sample_distr = function(n) rbeta(n, 10, 10)} 
  else if(func == 'mixture'){
    distr = dmixture
    sample_distr = function(n) rmixture(n)}
  else{stop("The 'func' input is wrong. Choose between 'beta' or 'mixture'!")}
  
  matr <- matrix(NA , )
  matr_integral <- matrix(NA , nrow = sim_size  , ncol = 3)
  
  for(rep in 1:sim_size){
    
  
    
    X <- sample_distr(n)   # Generating the random sample from the beta
    
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
    
    # Compute the integral
    p <- integrate(function(x) ( distr(x) - p_hat_func(x,bins = bins , p_hat = p_hat))^2 , lower = 0 , upper = 1, subdivisions = 1200)$value
    
    q <- integrate(function(x) (( distr(x) - q_hat_func(x,bins = bins , q_hat = q_hat ))^2), lower = 0 , upper = 1, subdivisions = 1200)$value
    pq <- integrate(function(x) (( p_hat_func(x,bins = bins , p_hat = p_hat ) - q_hat_func(x,bins = bins , q_hat = q_hat ))^2), lower = 0 , upper = 1, subdivisions = 1200)$value
   
    matr_integral[rep,1] <- p 
    matr_integral[rep,2] <- q 
    matr_integral[rep,3] <- pq 

    
  }
  mise_p <- mean(matr_integral[,1])   #Save the results
  mise_q <- mean(matr_integral[,2])   #Save the results
  mise_pq <- mean(matr_integral[,3])
  return(c(mise_p , mise_q, mise_pq))
}


##### Running Simulation  #####
m <- seq(5,50, 1)
simulation_size <- 100
labels <- c("p_hat" , "q_hat", 'pq')

n_cores <- detectCores()
cl <- makeCluster(n_cores, type = 'SOCK')
registerDoSNOW(cl)




# BETA
############
system.time(
 Beta_sim_n100_eps_01 <- foreach(i= m, .combine = 'c', .packages='VGAM') %dopar% {lapply(i, simulation_function, sim_size = simulation_size, n=100 , func='beta', eps = .1)})

beta_n100_eps_01 <- as.data.frame(do.call(rbind, Beta_sim_n100_eps_01 ) , row.names = m )
colnames(beta_n100_eps_01)  <- labels
save(beta_n100_eps_01, file='simulation_data/beta_n100_eps_01.RData')

############
system.time(
  Beta_sim_n100_eps_0001 <- foreach(i= m, .combine = 'c', .packages='VGAM') %dopar% {lapply(i, simulation_function, sim_size = simulation_size, n=100 , func='beta', eps = .001)})

beta_n100_eps_0001 <- as.data.frame(do.call(rbind, Beta_sim_n100_eps_0001 ) , row.names = m )
colnames(beta_n100_eps_0001)  <- labels
save(beta_n100_eps_0001, file='simulation_data/beta_n100_eps_0001.RData')

############
system.time(
  Beta_sim_n1000_eps_01 <- foreach(i= m, .combine = 'c', .packages='VGAM') %dopar% {lapply(i, simulation_function, sim_size = simulation_size, n=1000 , func='beta', eps = .1)})

beta_n1000_eps_01 <- as.data.frame(do.call(rbind, Beta_sim_n1000_eps_01 ) , row.names = m )
colnames(beta_n1000_eps_01)  <- labels
save(beta_n1000_eps_01, file='simulation_data/beta_n1000_eps_01.RData')

############
system.time(
  Beta_sim_n1000_eps_0001 <- foreach(i= m, .combine = 'c', .packages='VGAM') %dopar% {lapply(i, simulation_function, sim_size = simulation_size, n=1000 , func='beta', eps = .001)})

beta_n1000_eps_0001 <- as.data.frame(do.call(rbind, Beta_sim_n1000_eps_0001 ) , row.names = m )
colnames(beta_n1000_eps_0001)  <- labels
save(beta_n1000_eps_0001, file='simulation_data/beta_n1000_eps_0001.RData')



# MIXTURE
############
system.time(
  Mixture_sim_n100_eps_01 <- foreach(i= m, .combine = 'c', .packages='VGAM') %dopar% {lapply(i, simulation_function, sim_size = simulation_size, n=100 , func='mixture', eps = .1)})

mixture_n100_eps_01 <- as.data.frame(do.call(rbind, Mixture_sim_n100_eps_01 ) , row.names = m )
colnames(mixture_n100_eps_01)  <- labels
save(mixture_n100_eps_01, file='simulation_data/mixture_n100_eps_01.RData')

############
system.time(
  Mixture_sim_n100_eps_0001 <- foreach(i= m, .combine = 'c', .packages='VGAM') %dopar% {lapply(i, simulation_function, sim_size = simulation_size, n=100 , func='mixture', eps = .001)})

mixture_n100_eps_0001 <- as.data.frame(do.call(rbind, Mixture_sim_n100_eps_0001 ) , row.names = m )
colnames(mixture_n100_eps_0001)  <- labels
save(mixture_n100_eps_0001, file='simulation_data/mixture_n100_eps_0001.RData')

############
system.time(
  Mixture_sim_n1000_eps_01 <- foreach(i= m, .combine = 'c', .packages='VGAM') %dopar% {lapply(i, simulation_function, sim_size = simulation_size, n=1000 , func='mixture', eps = .1)})

mixture_n1000_eps_01 <- as.data.frame(do.call(rbind, Mixture_sim_n1000_eps_01 ) , row.names = m )
colnames(mixture_n1000_eps_01)  <- labels
save(mixture_n1000_eps_01, file='simulation_data/mixture_n1000_eps_01.RData')

############
system.time(
  Mixture_sim_n1000_eps_0001 <- foreach(i= m, .combine = 'c', .packages='VGAM') %dopar% {lapply(i, simulation_function, sim_size = simulation_size, n=1000 , func='mixture', eps = .001)})

mixture_n1000_eps_0001 <- as.data.frame(do.call(rbind, Mixture_sim_n1000_eps_0001 ), row.names = m)
colnames(mixture_n1000_eps_0001)  <- labels
save(mixture_n1000_eps_0001, file='simulation_data/mixture_n1000_eps_0001.RData')



stopCluster(cl)


qnorm(.95)


vec <-  c(160, 168 , 173, 175, 180, 171, 175, 169, 167, 172, 185)


pnorm(178.25 , 180, 5)¸
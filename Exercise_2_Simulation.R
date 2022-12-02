# Exercise_2
rm(list=ls())       #Clear output
library(VGAM)       # Library for the Laplacian Function
set.seed(13112221)  # For reprocibility
##### Step Function for p_hat #####

# Defining step functions for p_hat 
p_hat_func <- function(x , bins , p_hat ){
  interval <- cut(x, bins, include.lowest = T)
  levels   <- levels(interval)
  f        <- p_hat[interval == levels]
  return(f)
}
##### Step Function for q_hat #######
q_hat_func <- function(x, bins, q_hat){
  interval <- cut(x, bins, include.lowest = T)
  levels   <- levels(interval)
  f        <- q_hat[interval == levels]
  return(f)
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
  
  for(rep in 1:sim_size){  
    
    integral_p <- c()       # Pre-allocate the vector of the integral
    integral_q <- c()       # Pre-allocate the vector of the integral
    
    X <- sample_distr(n)    # Generating the random sample from the beta
    
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
    to_integrate_1 <- function(x) {return(( distr(x) - p_hat_func(x,bins = bins , p_hat = p_hat ))^2)}
    to_integrate_2 <- function(x) {return(( distr(x) - q_hat_func(x,bins = bins , q_hat = q_hat ))^2)}
    
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

##### Running Simulation  #####


m <- seq(5,7)
simulation_size <- 100

Beta_sim_n100_eps_1 <- lapply(m, simulation_function, sim_size = simulation_size, n=100 , func='beta', eps = .1)


Beta_sim_n100_eps_0001 <- lapply(m, simulation_function, sim_size = simulation_size, n=100 , func='beta', eps = 0.001)

Beta_sim_n1000_eps_1 <- lapply(m, simulation_function, sim_size = simulation_size, n=1000 , func='beta', eps = .1)


Beta_sim_n1000_eps_0001 <- lapply(m, simulation_function, sim_size = simulation_size, n=1000 , func='beta', eps = 0.001)




Mixture_sim_n100_eps_1 <- lapply(m, simulation_function, sim_size = simulation_size, n=100 , func='mixture', eps = .1)


Mixture_sim_n100_eps_0001 <- lapply(m, simulation_function, sim_size = simulation_size, n=100 , func='mixture', eps = 0.001)

Mixture_sim_n1000_eps_1 <- lapply(m, simulation_function, sim_size = simulation_size, n=1000 , func='mixture', eps = .1)


Mixture_sim_n1000_eps_0001 <- lapply(m, simulation_function, sim_size = simulation_size, n=1000 , func='mixture', eps = 0.001)





##### Saving into dataset  #####
labels <- c("p_hat" , "q_hat")


### 1
df_n100_eps_1 <-  as.data.frame(do.call(rbind, Beta_sim_n100_eps_1 ) , row.names = m )
colnames(df_n100_eps_1)  <- labels
### 2
df_n100_eps_0001 <-  as.data.frame(do.call(rbind, Beta_sim_n100_eps_0001 ) , row.names = m )
colnames(df_n100_eps_1)  <- labels
### 3
df_n1000_eps_1 <-  as.data.frame(do.call(rbind, Beta_sim_n1000_eps_1 ) , row.names = m )
colnames(df_n100_eps_1)  <- labels
### 4
df_n1000_eps_0001 <-  as.data.frame(do.call(rbind, Beta_sim_n100_eps_1 ) , row.names = m )
colnames(df_n100_eps_1)  <- labels
##### Build Private Dataset

save(df_n100_eps_1, file='df_n100_eps_1.RData')
save(df_n100_eps_0001, file='df_n100_eps_0001.RData')
save(df_n1000_eps_1, file='df_n1000_eps_1.RData')
save(df_n1000_eps_0001, file='df_n1000_eps_0001.RData')






library(readr)

Fitness<- read_csv("Fitness_question.csv")


colnames(Fitness) <- c("index" , "times")
Fitness$index <- seq(1, nrow(Fitness) , 1)


hist(Fitness$times , col = "steelblue",
     main = "Work-out times per month",
     freq = F , border = "white" ,
     xlab = "Number of times" ,
     breaks = 20)
box()


boxplot(Fitness$times , col = "purple3" ,
        boxwex = .5 , main = " Times work-out ",
        ylab = "Frequency", lex.order= T)

summary(Fitness$times)



#####Normalize the data######
low <- min(Fitness$times)
high <- max(Fitness$times)
n <- nrow(Fitness)


Fitness$Norm_times <- round(( Fitness$times - low ) / high , 2 )

Fitness$Norm_times

m <- 20
h <- 1/m

bins <- seq(0, 1, h)    # Set the bins



intervals <- cut(Fitness$Norm_times , bins, include.lowest = T)  # Rename units with the bins they belong
intervals


pj_hat <- table(intervals) / n              # Finding the frequencies of units inside each bins


p_hat <- as.vector(pj_hat / h) 


plot(p_hat, type='n', xlim=c(0,1), ylim = c(0,8))
colors = c("red" ,"blue")  

for(x in 1:(length(bins)-1)){
  print(x)
  segments(bins[x],p_hat[x], bins[x+1],p_hat[x] , col = colors[1])
  segments(bins[x], 0 , bins[x] , p_hat[x] , col = colors[1]) 
  segments(bins[x+1], 0 , bins[x+1] , p_hat[x] , col = colors[1]) 
  segments(0,0,1,0)
}



eps <- 0.001 
m <- 20
nu <- rlaplace(m, 0, 2/eps) # Generating m values from a Laplacian: one for each bin


Dj <- table(intervals) + nu                 # Adding nu to every absolute frequencies of each bin




Dj[Dj < 0] = 0    # Set all the nagative values to 0 t                      
qj_hat = Dj

# Finding qj_hat dividing max(0, Dj) for the sum of Dj
if (sum(qj_hat) != 0){
  qj_hat <- qj_hat / sum(qj_hat)} else {qj_hat <- rep(0, length(qj_hat))}


q_hat <- round(qj_hat / h , 2)     


for(x in 1:(length(bins)-1)){
  print(x)
  segments(bins[x],q_hat[x], bins[x+1],q_hat[x] , col = colors[2])
  segments(bins[x], 0 , bins[x] , q_hat[x] , col = colors[2]) 
  segments(bins[x+1], 0 , bins[x+1] , q_hat[x], col = colors[2] ) 
  segments(0,0,1,0)
}


xx = c(0.3 , 0.9)
p_hat_func <- function(x , bins , p_hat ){
  interval <- cut(x, bins, include.lowest = T)

  return(p_hat[interval])
}

xx= seq(0,1, length.out = n)

hist(p_hat_func(xx, bins, p_hat))

hist(Fitness$Norm_times)

?ecdf


r_p_hat <- function(n){
  xx <- runif(n)
  l <- qemp(p_hat_func(xx , bins , p_hat))
  return(l)
}
r_p_hat




# We use q_hat to return a privatized dataset











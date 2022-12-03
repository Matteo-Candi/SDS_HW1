rm(list=ls())
set.seed(13112221)

library(foreach)
library(parallel)
library(doSNOW)



condition <- function(y_vec , t = 0 , val){
  for (y in y_vec){
    if (y > val){break}
    else t <- t + 1
  }
  return(t)
}
n_cores <- detectCores()
cl <- makeCluster(n_cores, type = 'SOCK')
registerDoSNOW(cl)



stopping_time <- function(M){
  beg <- Sys.time()
  t_data <- rep(NA , M)
  x <- runif(M)
  for (m in 1:M){
    cond <- condition(runif(10) , val = x[m])
    t <- cond + 1
    if (cond != 0)
      while(cond %% 10 == 0){
      cond <- cond + condition(runif(cond) * 4 , val = x[m])
      t <- t + cond
    }
    t_data[m] <- t
  } 
  return(Sys.time() - beg)
}


Sim <- c(100, 1000, 10000, 100000, 1000000, 10000000)  # vector of the size 
fast <- foreach(i= Sim, .combine = 'c') %dopar% {lapply(i, stopping_time)}


stopCluster(cl)




m <-  100000
stopping_time <- function(M){
  beg <- Sys.time()
  t_data <- rep(NA , M)
  x <- runif(M)
  for (m in 1:M){
    cond <- condition(runif(10) , val = x[m])
    t <- cond + 1
    if (cond != 0)
      while(cond %% 10 == 0){
        cond <- cond + condition(runif(cond) * 4 , val = x[m])
        t <- t + cond
      }
    t_data[m] <- t
  } 
  return(t_data)
}


stop <- stopping_time(m) 
p_hat <- proportions(table(stop))

points(t_seq, p_hat[t_seq], pch = 1, 
       col = "bisque", bg = "black", cex = .7)




barplot(prop.table(table(stop)) , xlim = c(1,25))
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

points(t_seq, p_hat[t_seq], pch = 21, 
       col = "black", bg = "bisque", cex = .7)
grid()


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
  return(difftime(Sys.time(), beg, units = "secs"))
}


Sim <- c(100, 1000, 10000, 100000, 1000000, 10000000)  # vector of the size 
fast <- foreach(i= Sim, .combine = 'c') %dopar% {lapply(i, stopping_time)}


stopCluster(cl)


rm(list=ls())

# Setting parameters
n <- 1000
m <- 10
h <- 1/m
eps <- .001

# Generating the random sample from the beta
X <- rbeta(n, 10, 10)

# Set the bins
bins <- seq(0, 1, h)

# Rename units with the bins they belong
intervals <- cut(X, bins, include.lowest = T)

# Finding the frequencies of units inside each bins
pj_hat <- table(intervals) / n

# Computing high of each bin dividing the frequencies for the width of the bin
p_hat <- as.vector(pj_hat / h)
p_hat




# Importing packages to use Laplace function
library(VGAM)

# Generating m values from a Laplacian: one for each bin
nu <- rlaplace(m, 0, 2/eps)
nu

# Adding nu to every absolute frequencies of each bin
Dj <- table(intervals) + nu

# Finding qj_hat dividing max(0, Dj) for the sum of Dj
qj_hat <- c()
for(d in Dj){
  qj_hat <- c(qj_hat, max(d,0))
}
if (sum(qj_hat) != 0){
  qj_hat <- qj_hat / sum(qj_hat)
  } else {qj_hat <- rep(0, length(qj_hat))}

# Computing the high of the histogram dividing by the width of the columns
q_hat <- qj_hat / h



# Plotting p_hat and q_hat
plot(p_hat, col='red', pch=16, xaxt='n', ylab = 'Density', xlab = 'Bins', main='Plot of p_hat and q_hat')
axis(1, at = 0:m, lab=bins)
points(q_hat, col = 'blue', pch = 16)



# Plotting
plot(seq(h,1,h), p_hat, xlim=c(0,1), ylim=c(0, 4), 'n', ylab = 'Density', xlab = 'Bins', main='Plot of p_hat and q_hat')
legend('topright', legend=c('p_hat', 'q_hat', 'Beta(10,10)'), col = c('red', 'blue', 'green'), lwd=3, lty=1)

curve(dbeta(x, 10,10), col='green', lwd = 3, add = T)

p <- 0
for(i in 1:m){
  segments(p ,p_hat[i], p + h, p_hat[i], col = 'red', lwd = 3)
  segments(p + h, p_hat[i], p + h, p_hat[i+1], col='red', lty=3, lwd=3)
  p <- p + h}

q <- 0
for(i in 1:m){
  segments(q ,q_hat[i], q +h, q_hat[i], col = 'blue', lwd = 3)
  segments(q + h, q_hat[i], q + h, q_hat[i+1], col='blue', lty=3, lwd=3)
  q <- q + h}


# Defining step functions for p_hat and q_hat
p_hat_func <- function(x){
  interval <- cut(x, bins, include.lowest = T)
  levels   <- levels(interval)
  f        <- p_hat[interval == levels]
  return(f)
  }

q_hat_func <- function(x){
  interval <- cut(x, bins, include.lowest = T)
  levels   <- levels(interval)
  f        <- q_hat[interval == levels]
  return(f)
  }

# Defining MSE function (value inside integral of MISE)
MSE_p <- function(x) (dbeta(x, 10, 10) - p_hat_func(x))^2
MSE_q <- function(x) (dbeta(x, 10, 10) - q_hat_func(x))^2

# Computing integral
integrate(Vectorize(MSE_p), lower=0, upper=1)
integrate(Vectorize(MSE_q), lower=0, upper=1)






# Simulation --------------------------------------------------------------
# Importing packages to use Laplace function
library(VGAM)

# Defining step functions for p_hat and q_hat
p_hat_func <- function(x, m){
  interval <- cut(x, bins, include.lowest = T)
  levels   <- levels(interval)
  f        <- p_hat[interval == levels]
  return(f)
}

q_hat_func <- function(x){
  interval <- cut(x, bins, include.lowest = T)
  levels   <- levels(interval)
  f        <- q_hat[interval == levels]
  return(f)
}

# Defining MSE function (value inside integral of MISE)
MSE_p <- function(x) (dbeta(x, 10, 10) - p_hat_func(x))^2
MSE_q <- function(x) (dbeta(x, 10, 10) - q_hat_func(x))^2


M <- seq(5, 10, 1)
n <- 100
eps <- .1

mise_p <- c()
mise_q <- c()

for(m in M){
  print(m)
  
  for(rep in 1:1000){
    integral_p <- c()
    integral_q <- c()

    # Setting h
    h <- 1/m
    
    # Generating the random sample from the beta
    X <- rbeta(n, 10, 10)
    
    # Set the bins
    bins <- seq(0, 1, h)
    
    # Rename units with the bins they belong
    intervals <- cut(X, bins, include.lowest = T)
    
    # Finding the frequencies of units inside each bins
    pj_hat <- table(intervals) / n
    
    # Computing high of each bin dividing the frequencies for the width of the bin
    p_hat <- as.vector(pj_hat / h)
    p_hat
    
    
    # Generating m values from a Laplacian: one for each bin
    nu <- rlaplace(m, 0, 2/eps)
    nu
    
    # Adding nu to every absolute frequencies of each bin
    Dj <- table(intervals) + nu
    
    # Finding qj_hat dividing max(0, Dj) for the sum of Dj
    qj_hat <- c()
    for(d in Dj){
      qj_hat <- c(qj_hat, max(d,0))
    }
    if (sum(qj_hat) != 0){
      qj_hat <- qj_hat / sum(qj_hat)} else {qj_hat <- rep(0, length(qj_hat))}
    
    # Computing the high of the histogram dividing by the width of the columns
    q_hat <- qj_hat / h
    
    
    # Computing integral
    p <- integrate(Vectorize(MSE_p), lower=0, upper=1, subdivisions=2000)$value
    q <- integrate(Vectorize(MSE_q), lower=0, upper=1, subdivisions=2000)$value
    integral_p <- c(integral_p, p)
    integral_q <- c(integral_q, q)

  }
  mise_p <- c(mise_p, mean(integral_p))
  mise_q <- c(mise_q, mean(integral_q))
}

par(mfrow=c(1,1))
plot(5:(length(M)+4), mise_p, col='blue', pch=16,ylim=c(0,10), xlab='m', type='l')
points(5:(length(M)+4), mise_q, col='red', pch=16, type='l')

plot(5:(m-1), mise_p, col='blue', pch=16,ylim=c(0,10), xlab='m', type='l')
points(5:(m-1), mise_q, col='red', pch=16, type='l')



par(mfrow=c(2,2))

colors <- c('blue', 'red')


# First chart
load('mise_data/mise_n_100_eps_01_simulationsize_1000.RData')
mise_p_vec <- mise$mise_p
mise_q_vec <- mise$mise_q

plot(5:(length(mise_p_vec)+4), mise_p_vec, ylab='MISE', xlab='m',sub = expression(paste(epsilon,' = ', .1)), type='l', lwd=3, col=colors[1], main='n = 100', ylim=c(0,8))
points(5:(length(mise_q_vec)+4), mise_q_vec, type='l', lwd=3, col=colors[2])
# legend('topleft', legend=c(expression(hat(p)), expression(hat(q))), col=colors, lwd=3)


# Second chart
load('mise_data/mise_n_100_eps_0001_simulationsize_1000.RData')
mise_p_vec <- mise$mise_p
mise_q_vec <- mise$mise_q

plot(5:(length(mise_p_vec)+4), mise_p_vec, ylab='MISE', xlab='m',sub = expression(paste(epsilon,' = ', .001)), type='l', lwd=3, col=colors[1], main='n = 100', ylim=c(0,10))
points(5:(length(mise_q_vec)+4), mise_q_vec, type='l', lwd=3, col=colors[2])
# legend('topleft', legend=c(expression(hat(p)), expression(hat(q))), col=colors, lwd=3)

# Third chart
load('mise_data/mise_n_1000_eps_01_simulationsize_1000.RData')
mise_p_vec <- mise$mise_p
mise_q_vec <- mise$mise_q

plot(5:(length(mise_p_vec)+4), mise_p_vec, ylab='MISE', xlab='m',sub = expression(paste(epsilon,' = ', .1)), type='l', lwd=3, col=colors[1], main='n = 1000', ylim=c(0,3))
points(5:(length(mise_q_vec)+4), mise_q_vec, type='l', lwd=3, col=colors[2])
# legend('topleft', legend=c(expression(hat(p)), expression(hat(q))), col=colors, lwd=3)

# Fourth chart
load('mise_data/mise_n_1000_eps_0001_simulationsize_1000.RData')
mise_p_vec <- mise$mise_p
mise_q_vec <- mise$mise_q

plot(5:(length(mise_p_vec)+4), mise_p_vec, ylab='MISE', xlab='m',sub = expression(paste(epsilon,' = ', .001)), type='l', lwd=3, col=colors[1], main='n = 1000', ylim=c(0,10))
points(5:(length(mise_q_vec)+4), mise_q_vec, type='l', lwd=3, col=colors[2])
# legend('topleft', legend=c(expression(hat(p)), expression(hat(q))), col=colors, lwd=3)








library(VGAM)

M <- seq(5, 50, 1)
n <- 100
eps <- .1
simulation_size <- 1000

p_hat_func <- function(x){
  interval <- cut(x, bins, include.lowest = T)
  levels   <- levels(interval)
  f        <- p_hat[interval == levels]
  return(f)}

q_hat_func <- function(x){
  interval <- cut(x, bins, include.lowest = T)
  levels   <- levels(interval)
  f        <- q_hat[interval == levels]
  return(f)}

MSE_p <- function(x) (dbeta(x, 10, 10) - p_hat_func(x))^2
MSE_q <- function(x) (dbeta(x, 10, 10) - q_hat_func(x))^2

mise_p <- c()
mise_q <- c()

for(m in M){
  print(m)
  
  for(rep in 1:simulation_size){
    
    integral_p <- c()
    integral_q <- c()
    
    h <- 1/m
    
    bins <- seq(0, 1, h)
    
    intervals <- cut(rbeta(n, 10, 10), bins, include.lowest = T)
    p_hat <- as.vector( (table(intervals) / n) / h)
    
    Dj <- table(intervals) + rlaplace(m, 0, 8/eps^2)
    
    qj_hat <- c()
    for(d in Dj){
      qj_hat <- c(qj_hat, max(d,0))}
    if (sum(qj_hat) != 0){
      q_hat <- (qj_hat / sum(qj_hat)) / h} else {q_hat <- rep(0, length(qj_hat))}

    integral_p <- c(integral_p, integrate(Vectorize(MSE_p), lower=0, upper=1, subdivisions=2000)$value)
    integral_q <- c(integral_q, integrate(Vectorize(MSE_q), lower=0, upper=1, subdivisions=2000)$value)
  }
  
  mise_p <- c(mise_p, mean(integral_p))
  mise_q <- c(mise_q, mean(integral_q))
}


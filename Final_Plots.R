par(mfrow=c(2,2), font.main=1)

colors <- c("#2297E6", "#DF536B" )
y_lim <- 12
labels <- c("p_hat" , "q_hat")
eps <- c(.1, .001, .1, .001)
enne <- c(100, 100, 1000, 1000)


### Beta Plot
data <- c('beta_n100_eps_01', 'beta_n100_eps_0001', 'beta_n1000_eps_01', 'beta_n1000_eps_0001')

for(i in 1:4){
  load(paste0('simulation_data/', data[i], '.RData'))
  d <- eval(parse(text = data[i]))
  p_hat <- d$p_hat
  q_hat <- d$q_hat
  
  plot(5:(length(p_hat)+4), p_hat, ylab='MISE', xlab='', sub = bquote(epsilon == .(eps[i])), type='l', lwd=3, col=colors[1], ylim=c(0,y_lim), main = paste('n = ', enne[i]))
  grid()
  mtext(text = 'm',side = 1, line = 2, cex=.8)
  points(5:(length(q_hat)+4), q_hat, type='l', lwd=3, col=colors[2])
}




### Mixture Beta Plot
data <- c('mixture_n100_eps_01', 'mixture_n100_eps_0001', 'mixture_n1000_eps_01', 'mixture_n1000_eps_0001')

for(i in 1:4){
  load(paste0('simulation_data/', data[i], '.RData'))
  d <- eval(parse(text = data[i]))
  p_hat <- d$p_hat
  q_hat <- d$q_hat
  
  plot(5:(length(p_hat)+4), p_hat, ylab='MISE', xlab='', sub = bquote(epsilon == .(eps[i])), type='l', lwd=3, col=colors[1], ylim=c(0,y_lim), main = paste('n = ', enne[i]))
  mtext(text = 'm',side = 1, line = 2, cex=.8)
  points(5:(length(q_hat)+4), q_hat, type='l', lwd=3, col=colors[2])
}





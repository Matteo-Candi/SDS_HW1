par(mfrow=c(2,2), font.main=1)

colors <- c('blue', 'red')
y_lim <- 10
labels <- c("p_hat" , "q_hat")
eps <- c(.1, .001, .1, .001)
enne <- c(100, 100, 1000, 1000)


### Beta Plot
data <- c('beta_n100_eps_01', 'beta_n100_eps_0001', 'beta_n1000_eps_01', 'beta_n1000_eps_0001')

for(i in 1:4){
  load(paste0('mise_data/', data[i], '.RData'))
  d <- eval(parse(text = data[i]))
  p_hat <- d$p_hat
  q_hat <- d$q_hat
  
  plot(5:(length(mise_p_vec)+4), mise_p_vec, ylab='MISE', xlab='', sub = bquote(epsilon == .(eps[i])), type='l', lwd=3, col=colors[1], ylim=c(0,y_lim), main = paste('n = ', enne[i]))
  mtext(text = 'm',side = 1, line = 2, cex=.8)
  points(5:(length(mise_q_vec)+4), mise_q_vec, type='l', lwd=3, col=colors[2])
  }

title("n = 100", line = -2, outer = TRUE)
title("n = 1000", line = -18, outer = TRUE)



### Mixture Beta Plot
data <- c('mixture_n100_eps_01', 'mixture_n100_eps_0001', 'mixture_n1000_eps_01', 'mixture_n1000_eps_0001')

for(i in 1:4){
  load(paste0('mise_data/', data[i], '.RData'))
  d <- eval(parse(text = data[i]))
  p_hat <- d$p_hat
  q_hat <- d$q_hat
  
  plot(5:(length(mise_p_vec)+4), mise_p_vec, ylab='MISE', xlab='', sub = bquote(epsilon == .(eps[i])), type='l', lwd=3, col=colors[1], ylim=c(0,y_lim), main = paste('n = ', enne[i]))
  mtext(text = 'm',side = 1, line = 2, cex=.8)
  points(5:(length(mise_q_vec)+4), mise_q_vec, type='l', lwd=3, col=colors[2])
}

title("n = 100", line = -2, outer = TRUE)
title("n = 1000", line = -18, outer = TRUE)







# 
# 
# # First chart
# load('mise_data/beta_n100_eps_01.RData')
# p_hat <- beta_n100_eps_01$p_hat
# q_hat <- beta_n100_eps_01$q_hat
# plot(5:(length(p_hat)+4), p_hat, ylab='MISE', xlab='m', main = expression(paste(epsilon,' = ', .1)), type='l', lwd=3, col=colors[1], ylim=c(0,y_lim))
# points(5:(length(q_hat)+4), q_hat, type='l', lwd=3, col=colors[2])
# 
# # Second chart
# load('mise_data/beta_n100_eps_0001.RData')
# p_hat <- beta_n100_eps_0001$p_hat
# q_hat <- beta_n100_eps_0001$q_hat
# plot(5:(length(p_hat)+4), p_hat, ylab='MISE', xlab='m', main = expression(paste(epsilon,' = ', .001)), type='l', lwd=3, col=colors[1], ylim=c(0,y_lim))
# points(5:(length(q_hat)+4), q_hat, type='l', lwd=3, col=colors[2])
# 
# # Third chart
# load('mise_data/beta_n1000_eps_01.RData')
# p_hat <- beta_n1000_eps_01$p_hat
# q_hat <- beta_n1000_eps_01$q_hat
# plot(5:(length(p_hat)+4), p_hat, ylab='MISE', xlab='m', main = expression(paste(epsilon,' = ', .1)), type='l', lwd=3, col=colors[1], ylim=c(0,y_lim))
# points(5:(length(q_hat)+4), q_hat, type='l', lwd=3, col=colors[2])
# 
# # Fourth chart
# load('mise_data/beta_n1000_eps_0001.RData')
# p_hat <- beta_n1000_eps_0001$p_hat
# q_hat <- beta_n1000_eps_0001$q_hat
# plot(5:(length(p_hat)+4), p_hat, ylab='MISE', xlab='m', main = expression(paste(epsilon,' = ', .001)), type='l', lwd=3, col=colors[1], ylim=c(0,y_lim))
# points(5:(length(q_hat)+4), q_hat, type='l', lwd=3, col=colors[2])
# 
# 
# 
# title("n = 100", line = -2, outer = TRUE)
# title("n = 1000", line = -18, outer = TRUE)
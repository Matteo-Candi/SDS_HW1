
par(mfrow=c(2,2), font.main=1)

colors <- c('blue', 'red')
y_lim <- 10
labels <- c("p_hat" , "q_hat")

data <- c('mise_n_100_eps_01_simulationsize_100', 'mise_n_100_eps_0001_simulationsize_100', 'mise_n_1000_eps_01_simulationsize_100', 'mise_n_1000_eps_0001_simulationsize_100')
eps <- c(.1, .001, .1, .001)
enne <- c(100, 100, 1000, 1000)

for(i in 1:4){
  load(paste0('mise_data/', data[i], '.RData'))
  d <- eval(parse(text = "mise"))
  mise_p_vec <- d$mise_p
  mise_q_vec <- d$mise_q
  
  plot(5:(length(mise_p_vec)+4), mise_p_vec, ylab='MISE', xlab='', sub = bquote(epsilon == .(eps[i])), type='l', lwd=3, col=colors[1], ylim=c(0,y_lim), main = paste('n = ', enne[i]))
  mtext(text = 'm',side = 1, line = 2, cex=.8)
  points(5:(length(mise_q_vec)+4), mise_q_vec, type='l', lwd=3, col=colors[2])
 
}

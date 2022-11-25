source("Simulation_Function.r")



par(mfrow=c(2,2))

colors <- c('blue', 'red')
y_lim <- 9

# First chart
load('mise_data/mise_n_100_eps_01_simulationsize_100.RData')
mise_p_vec <- mise$mise_p
mise_q_vec <- mise$mise_q

plot(5:(length(mise_p_vec)+4), mise_p_vec, ylab='MISE', xlab='m',sub = expression(paste(epsilon,' = ', .1)), type='l', lwd=3, col=colors[1], main='n = 100', ylim=c(0,y_lim))
points(5:(length(mise_q_vec)+4), mise_q_vec, type='l', lwd=3, col=colors[2])



# Second chart
load('mise_data/mise_n_100_eps_0001_simulationsize_100.RData')
mise_p_vec <- mise$mise_p
mise_q_vec <- mise$mise_q

plot(5:(length(mise_p_vec)+4), mise_p_vec, ylab='MISE', xlab='m',sub = expression(paste(epsilon,' = ', .001)), type='l', lwd=3, col=colors[1], main='n = 100', ylim=c(0,y_lim))
points(5:(length(mise_q_vec)+4), mise_q_vec, type='l', lwd=3, col=colors[2])



# Third chart
load('mise_data/mise_n_1000_eps_01_simulationsize_100.RData')
mise_p_vec <- mise$mise_p
mise_q_vec <- mise$mise_q

plot(5:(length(mise_p_vec)+4), mise_p_vec, ylab='MISE', xlab='m',sub = expression(paste(epsilon,' = ', .1)), type='l', lwd=3, col=colors[1], main='n = 1000', ylim=c(0,y_lim))
points(5:(length(mise_q_vec)+4), mise_q_vec, type='l', lwd=3, col=colors[2])




# Fourth chart
load('mise_data/mise_n_1000_eps_0001_simulationsize_100.RData')
mise_p_vec <- mise$mise_p
mise_q_vec <- mise$mise_q

plot(5:(length(mise_p_vec)+4), mise_p_vec, ylab='MISE', xlab='m',sub = expression(paste(epsilon,' = ', .001)), type='l', lwd=3, col=colors[1], main='n = 1000', ylim=c(0,y_lim))
points(5:(length(mise_q_vec)+4), mise_q_vec, type='l', lwd=3, col=colors[2])




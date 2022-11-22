rm(list=ls())


n <- 1000
m <- 40
h <- 1/m
eps <- .001

X <- rbeta(n, 10, 10)


bins <- seq(0, 1, h)
re <- cut(X, bins, include.lowest = T)
pj_hat <- table(re) / n

p_hat <- as.vector(pj_hat / h)



# histo <- hist(X, freq = F, breaks = c(bins))
# box()
# 
# # p_hat == histo$density



library(VGAM)

nu <- rlaplace(m, 0, 8/eps^2)
Dj <- table(re) + nu

qj_hat <- c()
for(d in Dj){
  qj_hat <- c(qj_hat, max(d,0) / sum(Dj))
}

q_hat <- qj_hat / h




plot(p_hat, col='red', pch=16, xaxt='n')
axis(1, at = 0:m, lab=bins)
points(q_hat, col='blue', pch=16)




# Exercise rush -----------------------------------------------------------


M <- c(100, 1000, 10000, 100000, 1000000, 10000000)
set.seed(123)
l=c()
for(m in 1:100){
  u <- runif(1)
  set.seed(123)
  g <- rgeom(1,u)
  l=c(l,g)

  
}

hist(l , breaks = 1000 , xlim = c(0,10))

set.seed(123)
m <-  100
k <- runif(m)
g <- rgeom(m , u)

hist(g , breaks = 10 , xlim = c(0,10), freq = F)
points(1:10, dgeom(1:10, prob = 0) , add = T , lwd = 3)





# 2) The Exercise ---------------------------------------------------------

curve(dbeta(x,10,10))







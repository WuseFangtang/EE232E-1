library(igraph)
c <- 0.5
n <- seq(100,10000,1)
p <- c/n

ExpectedSize <- function(n,p){
  size <- 0
  for (i in 1:100){
    g = erdos.renyi.game(n, p, directed=F)
    g.components <- clusters(g)
    ix <- which.max(g.components$csize)
    g.giant <- induced.subgraph(g, which(g.components$membership == ix))
    size <- size + length(V(g.giant))
  }
  size<-size/100
  size
}

currSize <- c()
for (i in 1:length(n)){
  print (paste("Calculating ... ", n[i]))
  currSize = c(currSize, ExpectedSize(n[i],p[i]))
}
plot(n, currSize,main = 'GCC size v.s. n', xlab = 'nodes number', ylab ='GCC size')


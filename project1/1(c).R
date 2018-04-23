library(igraph)
prob <- seq(0.0001,0.005,0.0001)
result <- c()
for (val in prob) {
  size <- 0
  for (iter in 1:100){
    g = erdos.renyi.game(1000, val, directed=F)
    g.components <- clusters(g)
    ix <- which.max(g.components$csize)
    g.giant <- induced.subgraph(g, which(g.components$membership == ix))
    size <- size + length(V(g.giant))/1000
  }
  result <- c(result,(size/100))
  print (result)
  print("====================================================")
}
plot(prob,result,type='l',main='Normalized GCC sizes v.s. prob.', xlab = 'prob.', ylab = 'Normalized GCC size')

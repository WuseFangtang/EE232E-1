library(igraph)
prob <- c(0.003, 0.004, 0.01, 0.05, 0.1)

ConnectedProb <- function(prob){
  count <- 0
  for (iter in 1:1000){
    g = erdos.renyi.game(1000, prob, directed=F)
    if (is.connected(g)){count <- count + 1}
  }
  count/1000
}

for (val in prob) {
  print (paste("Probablity for Network connected with prob = ", val, "is",ConnectedProb(val)))
  g = erdos.renyi.game(1000, val, directed=F)
  if (!is.connected(g)){ 
      g.components <- clusters(g)
      ix <- which.max(g.components$csize)
      g.giant <- induced.subgraph(g, which(g.components$membership == ix))
      print("Number of Vertices for GCC:")
      print (length(V(g.giant)))
      d<-diameter(g.giant,directed = FALSE, unconnected =FALSE)
      print (paste("Diameter of GCC with prob = ", val, "is", d))
  }
  print("                                                                 ")
  print("====================================================================             ")
}



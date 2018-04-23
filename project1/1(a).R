library(igraph)
prob <- c(0.003, 0.004, 0.01, 0.05, 0.1)

for (val in prob) {
  g = erdos.renyi.game(1000, val, directed=F)
  print(length(V(g)))
  print (paste("Mean degree with prob = ", val, "is", mean(degree(g))))
  print (paste("Variance of degree with prob = ", val, "is", var(degree(g))))
  plot(degree.distribution(g),main=paste("Degree distribution of the network with prob = ", val),xlab="Degree",ylab="Frequency")
  degreesVector <- degree(g)
  hist(degreesVector, main = paste("Histogram of degressVector with prob = ", val))
  
  print("====================================================")
}


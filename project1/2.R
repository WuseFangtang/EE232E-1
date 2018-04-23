install.packages("pracma", type="binary")
library('igraph')
library('Matrix')
library('pracma')

create_transition_matrix = function (g){
  
  # WARNING: make sure your graph is connected (you might input GCC of your graph)
  
  vs = V(g)
  n = vcount(g)
  adj = as_adjacency_matrix(g)
  adj[diag(rowSums(adj) == 0)] = 1  # handle if the user is using the function for networks with isolated nodes by creating self-edges
  z = matrix(rowSums(adj, , 1))
  
  transition_matrix = adj / repmat(z, 1, n)  # normalize to get probabilities
  
  return(transition_matrix)
}

random_walk = function (g, num_steps, start_node, transition_matrix = NULL){
  if(is.null(transition_matrix))
    transition_matrix = create_transition_matrix(g)
  
  v = start_node
  for(i in 1:num_steps){
    #fprintf('Step %d: %d\n', i, v)  # COMMENT THIS
    PMF = transition_matrix[v, ]
    v = sample(1:vcount(g), 1, prob = PMF)        
  }
  
  return(v)
}

g = erdos.renyi.game(1000, 0.01, directed=F)
distance_matrix = shortest.paths(g, v = V(g), to = V(g))

mean <- c()
var <- c()
for (t in 1:10){
    print (paste('Calculating t = ',t))
    s <- c()
    for (i in 1: 100){
      start_node = round(runif(1,1,1000))
      tail_node = random_walk(g,t,start_node)
      shortest_distance <- distance_matrix[start_node,tail_node]
      s <- c(s, shortest_distance)
    }
    mean <- c(mean, mean(s))
    var <- c(var, mean((s - mean(s))**2))
}
t <- c(1:10)
plot(t, mean,main = 'Mean value of average distance v.s. t', xlab = 't', ylab ='<s(t)>')
plot(t, var,main = 'Variance average distance v.s. t', xlab = 't', ylab ='variance')
length(mean)

library(igraph)
library(corrplot)


setwd("C:/Users/IfyourRtheone/Desktop/UCLA/2018Spring/EE232E Networks/project2/data/google+")
file_names = list.files("gplus/")
file_ids = sub("^([^.]*).*", "\\1", file_names)
ego_node_ids = unique(file_ids) 
ids_circles<-c()
# check if a node has more than two circles
for (id in ego_node_ids) {
  # get the number of circles
  circles_file = paste("gplus/" , id , ".circles" , sep="")
  circles_connect = file(circles_file , open="r")
  circles_content = readLines(circles_connect)
  close(circles_connect)
  
  # check if greater than 2
  if(length(circles_content) > 2)
    ids_circles = c(ids_circles, id)
}


cat("Total Number of Ego Nodes = ", length(ids_circles))
ls = c('109327480479767108490', '115625564993990145546', '101373961279443806744')

for (id in ls) {
  edges_file = paste("gplus/" , id  , ".edges" , sep="") # edge list
  circles_file = paste("gplus/" , id , ".circles" , sep="") # circles list
  
  circles_connect = file(circles_file , open="r")
  circles_content = readLines(circles_connect)
  
  circles = list()
  a = c()
  for (i in 1:length(circles_content)) {
    circle_nodes = strsplit(circles_content[i],"\t")
    circles = c(circles, list(circle_nodes[[1]][-1]))
  }
  
  # a is number of people in every circles
  for (i in 1:length(circles)){
    a <- c(a, length(circles[[i]]))
  }
  
  close(circles_connect)
  
  g = read.graph(edges_file , format = "ncol" , directed=TRUE)  
  g = add.vertices(g, nv = 1, name = id)
  ego_node_index = which(V(g)$name==id) 
  
  add_edge_list = c()
  for (vertex in 1:(vcount(g) - 1)) {
    add_edge_list = c(add_edge_list, c(ego_node_index, vertex))
  }
  
  g = add_edges(g, add_edge_list)
  
  plot(g,layout=layout.fruchterman.reingold,vertex.size=1,vertex.color='red',vertex.label.cex =0,edge.color='red',edge.arrow.size=0.04)
  degIn <- degree(g, mode = 'in')
  hist(degIn, main = paste("Histogram of in-degree distribution with id = ", id))
  degOut <- degree(g,mode = 'out')
  hist(degOut, main = paste("Histogram of out-degree distribution with id = ", id))
  plot(degree.distribution(g, mode = 'in'),main = paste("Plot of in-degree distribution with id = ", id),xlab="Degree",ylab="Frequency")
  plot(degree.distribution(g, mode = 'out'),main = paste("Plot of out-degree distribution with id = ", id),xlab="Degree",ylab="Frequency")

  walktrap_comm = walktrap.community(g)
  
  percentage = vector()
  percentage_circle = vector()
  
  b = c()
  c = array(0, dim = c(max(walktrap_comm$membership), length(circles)))
  total_common_nodes = c()
  # check percentage of match for walktrap community 
  for(m in 1:max(walktrap_comm$membership)){
    
    community_nodes = V(g)$name[which(walktrap_comm$membership == m)]
    temp_common_nodes = c()
    for (n in 1:length(circles)) {
      common_nodes = intersect(community_nodes, circles[[n]])
      total_common_nodes = union(total_common_nodes, common_nodes)
      temp_common_nodes = union(temp_common_nodes, common_nodes)
      c[m,n] = length(common_nodes)
      percent_circle = length(common_nodes)/length(circles[[n]])
      percentage_circle = c(percentage_circle, percent_circle)
    }
    # bi is the number of people in Community i with circle info
    # b <- c(b, sum(c[m,1:length(circles)]))
    
    b <- c(b, length(temp_common_nodes))
  }
  
  # N is the total number of people with circle info
 # N = sum(b)
  N = length(total_common_nodes)
  par(oma=c(0,0,0,0))
  
  cat("Number of Circles: ", length(circles))
  
  c_percentage = matrix(percentage_circle, nrow = max(walktrap_comm$membership), ncol = length(circles))
  colnames(c_percentage) = paste("Circle ",    1:length(circles),     sep="")
  rownames(c_percentage) = paste("Community ",    1:max(walktrap_comm$membership),     sep="")
  corrplot(c_percentage, method="color", cl.lim=c(0,1), tl.cex = 0.7, tl.col = 'black')
  
  node_size = rep(2, vcount(g))
  node_size[ego_node_index] = 4
  plot(walktrap_comm, g, vertex.size = node_size , main = paste("Communities with id = ", id)
     ,asp = 9/16, vertex.label=NA , edge.color = "grey", 
      layout=layout.fruchterman.reingold)
  print(paste("Modularity of g id= ",id, ": ", modularity(g,walktrap_comm$membership)))
  Hc = 0
  Hk = 0
  Hck = 0
  Hkc = 0
  for (i in 1:length(circles)){
    if (a[i] != 0){
      Hc = Hc + a[i]/N * log(a[i]/ N)
    }
  }
  Hc = -Hc
  
  for (i in 1:length(max(walktrap_comm$membership))){
    if (b[i] != 0){
      Hk = Hk + b[i]/N * log(b[i]/ N)
    }
  }
  Hk = -Hk
  
  for (j in 1:length(max(walktrap_comm$membership))){
    for (i in 1:length(circles)){
      if (c[j,i] != 0){
        Hck = Hck + c[j,i]/N * log(c[j,i] / b[j])
      }
    }
  }
  Hck = - Hck
  for (i in 1:length(circles)){
    for (j in 1:length(max(walktrap_comm$membership))){
      if (c[j,i] != 0){
        Hkc = Hkc + c[j,i]/N * log(c[j,i] / a[i])
      }
    }
  }
  Hkc = -Hkc
  
  #homogenity 
  homo = 1 - Hck/Hc
  #Completeness
  comp = 1 - Hkc/Hk
  print(paste("data for id = ", id, "=============================="))
  print(paste("Hk ==",Hk))
  print(paste("Hc ==",Hc))
  print(paste("Hkc ==",Hkc))
  print(paste("Hck ==",Hck))
  print(paste("Homogenity ==",homo))
  print(paste("Completeness ==",comp))
}


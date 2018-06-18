
library(igraph)
library(data.table)

edge_list <- fread("C:\\Users\\IfyourRtheone\\Desktop\\UCLA\\2018Spring\\edge_list.txt", sep='\t', header=FALSE)
names(edge_list) <- c('from','to','weights')

edge_list <- edge_list[duplicated(edge_list[,1:2]) == F]
edge_list <- edge_list[edge_list$from != edge_list$to]

g = graph.data.frame(edge_list[,1:2],directed =TRUE)

degIn <- degree(g, mode = 'in')
hist(degIn, main = "In-degree Distribution of g")
degOut <- degree(g,mode = 'out')
hist(degOut, main = "Out-degree Distribution of g")
plot(degree.distribution(g, mode = 'in'),main = "In-degree Distribution of g",xlab="Degree",ylab="Frequency")
plot(degree.distribution(g, mode = 'out'),main ="Out-degree Distribution of g",xlab="Degree",ylab="Frequency")

pgrank = page.rank(g, directed = TRUE, damping = 0.85)
sorted = sort(pgrank$vector, decreasing = TRUE, index.return = TRUE)
ordered_idx = order(pgrank$vector, decreasing = TRUE)
cat(sorted$x[1:10])
V(g)[ordered_idx[1:10]]

name_list = c("Cruise, Tom", "Watson, Emma (II)", "Clooney, George","Hanks, Tom"
             ,"Johnson, Dwayne (I)","Depp, Johnny","Smith, Will (I)","Streep, Meryl",
             "DiCaprio, Leonardo","Pitt, Brad","Roberts, Eric (I)","Tatasciore, Fred"
             ,"Jeremy, Ron","Trejo, Danny","Flowers, Bess","Hitler, Adolf","Riehle, Richard",
             "Harris, Sam (II)","Jackson, Samuel L.","De Niro, Robert")
idx = c()
for (name in name_list){
  print(name)
  print(which(V(g)$name == name))
  idx = c(idx, which(V(g)$name == name))
}
pgrank$vector[idx]
degree(g, v = V(g)[idx], mode = c("in"))


score = c()
for (i in 1:length(pgrank$vector)){
  score = c(score, pgrank$vector[[i]])
}
pg_data = data.frame(V(g)$name,score)
write.csv (pg_data, file ="data2.csv") 
actor_movie <- fread("actor_num_movies.txt",header = FALSE)

movie_num = c()
for (name in name_list){
  print(name)
  print(which(actor_movie$V1 == name))
  movie_num = c(movie_num,actor_movie$V2[which(actor_movie$V1 == name)])
}
movie_num

name_list2 = c("Cruise, Tom", "Watson, Emma (II)", "Clooney, George","Hanks, Tom"
               ,"Johnson, Dwayne (I)","Depp, Johnny","Smith, Will (I)","Streep, Meryl",
               "DiCaprio, Leonardo","Pitt, Brad")
for (name in name_list2){
  
  idx = which(edge_list$from == name)
  sortidx = sort(edge_list$weights[idx], decreasing = TRUE,index.return = TRUE)
  print(name)
  print(edge_list$to[idx[sortidx$ix[1]]])
  print(edge_list$weights[idx[sortidx$ix[1]]])
  
}

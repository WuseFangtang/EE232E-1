rm(list=ls())
library("igraph")
library(ggplot2)
library(readr)
load("~/Desktop/EE232/project/project5/sf_graph.rda")
Delaunay_edgelist <- read_csv("Delaunay_edgelist.csv", col_names = FALSE)
location <- read_csv("location.csv", col_names = FALSE)
location <- data.frame(name=seq(1, 1880, by=1),x=location$X1,y=location$X2)
g_Delaunay<-graph_from_data_frame(Delaunay_edgelist,directed=FALSE,vertices=location)
g_Delaunay<-simplify(g_Delaunay, remove.multiple = TRUE)

plot(g_Delaunay,vertex.size=1,vertex.label=NA, vertex.color="red",edge.width=0.5)

#get mean time for each edge as weight
pair_vertex_Delaunay=ends(g_Delaunay, E(g_Delaunay), names = FALSE)
for (i in seq(1,5627)) {
  pair <- pair_vertex_Delaunay[i,]
  if(length(E(g)[get.edge.ids(g,pair)])){
    E(g_Delaunay)[get.edge.ids(g_Delaunay,pair)]$weight <- E(g)[get.edge.ids(g,pair)]$weight
  }
  else{
    E(g_Delaunay)[get.edge.ids(g_Delaunay,pair)]$weight <- distances(g,pair[1],pair[2])
  }
}

# get length and mean speed of each edge
E(g_Delaunay)$length <-sqrt(((V(g_Delaunay)[pair_vertex_Delaunay[,1]]$x-V(g_Delaunay)[pair_vertex_Delaunay[,2]]$x)*69)^2+((V(g_Delaunay)[pair_vertex_Delaunay[,1]]$y-V(g_Delaunay)[pair_vertex_Delaunay[,2]]$y)*69)^2)
E(g_Delaunay)$speed<-E(g_Delaunay)$length/E(g_Delaunay)$weight*3600

#calculate traffic flow

car_length = 0.003
saft_time = 2/60

length_Delaunay <- E(g_Delaunay)$length
distances_per_car = car_length + saft_time * E(g_Delaunay)$speed

E(g_Delaunay)$flow <- (E(g_Delaunay)$speed*1 + E(g_Delaunay)$length)*2 / (distances_per_car)

#Question 13
Standford = 1813
UCSC = 1371

Standford_UCSC_max_flow <- (max_flow(g_Delaunay, Standford, UCSC, capacity = E(g_Delaunay)$flow))$value
edge_disjoint_paths(g_Delaunay,Standford,UCSC)
#Standford_UCSC_max_flow = 1651.388 
#disjoint edge=5
#Question 14
library(ggplot2)
library(ggmap)
library(maps)
library(GGally)
names(location) <- c('name','lon','lat')
# get map


map<-get_map(location = c(-122.0779, 37.70999), maptype = "satellite", source = "google", zoom = 8)
# threshold length 20 mile
g_new_Delaunay_length<-subgraph.edges(g_Delaunay,E(g_Delaunay)[E(g_Delaunay)$length<20])
pair_vertex_new_Delaunay=ends(g_new_Delaunay_length, E(g_new_Delaunay_length), names = FALSE)
n1<-data.frame(V(g_new_Delaunay_length)[pair_vertex_new_Delaunay[,1]]$x,V(g_new_Delaunay_length)[pair_vertex_new_Delaunay[,1]]$y,pair_vertex_new_Delaunay[,1],V(g_new_Delaunay_length)[pair_vertex_new_Delaunay[,2]]$x,V(g_new_Delaunay_length)[pair_vertex_new_Delaunay[,2]]$y)
names(n1)<-c('lon','lat','vertex.names','xend','yend')
plot_map <- ggmap(map)+geom_segment(data=n1,aes(xend=xend,yend=yend))+geom_point(data = location,size = 1,color = "red")

# threshold time average of original graph

g_new_Delaunay_time<-subgraph.edges(g_Delaunay,E(g_Delaunay)[E(g_Delaunay)$weight<1666])
pair_vertex_new_Delaunay_time=ends(g_new_Delaunay_time, E(g_new_Delaunay_time), names = FALSE)
n2<-data.frame(V(g_new_Delaunay_time)[pair_vertex_new_Delaunay_time[,1]]$x,V(g_new_Delaunay_time)[pair_vertex_new_Delaunay_time[,1]]$y,pair_vertex_new_Delaunay_time[,1],V(g_new_Delaunay_time)[pair_vertex_new_Delaunay_time[,2]]$x,V(g_new_Delaunay_time)[pair_vertex_new_Delaunay_time[,2]]$y)
names(n2)<-c('lon','lat','vertex.names','xend','yend')
plot_map_time <- ggmap(map)+geom_segment(data=n2,aes(xend=xend,yend=yend))+geom_point(data = location,size = 1,color = "red")

#Question 14
tri<-matrix(triangles(g_new_Delaunay_time), nrow=3)
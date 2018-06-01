library(data.table)
library(igraph)
load('community')

movie_rating <- read.csv('movie_rating(new).txt',encoding='ISO-8859-1')
rownames(movie_rating) <-movie_rating$Movie
movie_rating$X<-NULL
movie_rating$Movie<-NULL

movie_id <- fread("movie_id.txt",sep='\t', header = F)
names(movie_id) <- c("name","id")
rownames(movie_id) <- movie_id$id
movie_id$id <- NULL

edge_list <- fread("movie_edge_list.txt",sep='\t', header = F)
g <- graph_from_data_frame(d=edge_list, vertices=movie_id, directed=F)

getMembership <-function(movie){
	return(membership(fc)[movie])
}

getRate <-function(movie){
	return(movie_rating[movie,])
}

getNeighborsRating<-function(movie){
	neighborNames <- neighbors(g,movie)$name
	criteria <- neighborNames %in% rownames(movie_rating)
	final_neighbor_names <- neighborNames[criteria]
	neighbor_rates <- getRate(final_neighbor_names)
	return(neighbor_rates)
}

getNeighborsRating_Restricted<-function(movie){
	neighborNames <- neighbors(g,movie)$name
	movieMembership <-getMembership(movie)
	criteria <- (neighborNames %in% rownames(movie_rating)) & (getMembership(neighborNames)==movieMembership)
	final_neighbor_names <- neighborNames[criteria]
	neighbor_rates <- getRate(final_neighbor_names)
	return(neighbor_rates)
}

target<-c("Batman v Superman: Dawn of Justice (2016)","Mission: Impossible - Rogue Nation (2015)","Minions (2015)")

batman<-getNeighborsRating(target[1])  #6.3454
mission<-getNeighborsRating(target[2]) #6.2383
minion<-getNeighborsRating(target[3])  #6.7962

batman2<-getNeighborsRating_Restricted(target[1])  #6.3431
mission2<-getNeighborsRating_Restricted(target[2]) #6.2533
minion2<-getNeighborsRating_Restricted(target[3])  #6.8110


getNeighborWeight <-function(movie){
	neighborNames <- neighbors(g,movie)$name
	ids <-sapply(neighborNames, function(x) get.edge.ids(g,c(x,movie)))
	gweights<-edge_attr(g)$V3
	edge_weight<-sapply(ids,function(id) gweights[id])	
	df<-data.frame(neighborNames,edge_weight)
	return(df)
}

getTopKNeighbors <-function(movie,k){
	df<-getNeighborWeight(movie)
	topk_name<-as.character(df[order(-df$edge_weight),]$neighborNames[1:k])
    topk <- getMembership(topk_name)
    return(topk)
}

batman_top5<-getTopKNeighbors(target[1],5)
mission_top5<-getTopKNeighbors(target[2],5)
minion_top5<-getTopKNeighbors(target[3],5)

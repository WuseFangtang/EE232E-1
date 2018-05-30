# initialize
rm(list = ls());
library(data.table)
library(igraph)
library(ggplot2)
library(plyr)

# read list
# this is a super-fast way to read mass dataset
edge_list <- fread("movie_edge_list.txt",sep='\t', header = F)
names(edge_list) <- c("source","target","weight")
movie_id <- fread("movie_id.txt",sep='\t', header = F)
names(movie_id) <- c("name","id")
g <- graph_from_data_frame(d=edge_list, vertices=movie_id, directed=F);

# plot(g,vertex.size=1, vertex.color="red",
#   edge.width=0.5,edge.arrow.size=0.1);

fc <- cluster_fast_greedy(g)
#load("community")
movie_genre <- fread("movie_genre.txt",sep='\t', header = F)
movie_genre <- subset(movie_genre, select = c("V1","V3"))
names(movie_genre)<-c("name","genre")

#community should be in form fc[1]$`1`
genre_dis <-function(movie_genre, community) {
	clean_community <- c();
	for (i in community){
 		clean_community <- append(clean_community,i)
	}
	genre <- movie_genre[movie_genre$name %in% clean_community,genre];
	genre_hist <- count(genre)
	genre_hist[,2]  <- genre_hist[,2] / sum(genre_hist$freq);
	genre_hist <- as.data.frame(genre_hist);
	names(genre_hist) <- c('x','Freq')
	return (genre_hist);
}

#q7 part 1 genre distribution
for (a in 1:29) {
	genre_hist<-genre_dis(movie_genre,fc[a])
	ggplot(data=genre_hist, aes(x = x, y = Freq, label=paste(x, round(Freq, 2))))+geom_point()+ geom_text(size=2, hjust = 1, vjust = 2)
	ggsave(paste("genre_hist_",a,".png",sep=""), width = 12, height = 7, dpi = 300)
}
#q7 part 2 community_graph
for (a in c(17,21,23,24,25,26)){
	clean_community <- c();
	for (i in fc[a]){
 		clean_community <- append(clean_community,i)
	}
	community_graph <- induced_subgraph(g, clean_community);
	png(filename=paste("community_",a,".png",sep=""),width = 4, height = 4, units = 'in', res = 300)
	plot(community_graph,vertex.size=1,vertex.label=NA, vertex.color="red",
    	edge.width=0.5,edge.arrow.size=0.1);
	dev.off()
}

#q8(a) based on q7 part1 

#q8(b)
genre_score <-function(movie_genre, community) {
	clean_community <- c();
	for (i in community){
 		clean_community <- append(clean_community,i)
	}
	genre <- movie_genre[movie_genre$name %in% clean_community,genre];
	genre_hist <- count(genre); #numbers of movies belonging to genres in the community
	genre_hist_all <- count(movie_genre$genre); #fraction of movies belonging to genres in the community
	genre_hist_freq <- genre_hist; #fraction of movies belonging to genres in whole dataset
	genre_hist_freq[,2]  <- genre_hist_freq[,2] / sum(genre_hist_freq$freq);
	genre_hist_freq <- as.data.frame(genre_hist_freq);
	genre_hist_all[,2]  <- genre_hist_all[,2] / sum(genre_hist_all$freq);
	names(genre_hist_freq) <- c('name','Freq')
	names(genre_hist) <- c('name','Freq')
	names(genre_hist_all) <- c('name','Freq')
	max_score <- 0
	max_genre <- ""
	for ( i in genre_hist$name){
		c <- genre_hist[genre_hist$name==i,2];
		p <- genre_hist_freq[genre_hist_freq$name == i, 2]
		q <- genre_hist_all[genre_hist_all$name == i, 2]
		score <- log(c)*p/q;
		if (score > max_score){
			max_score <- score;
			max_genre <- i;
		}
	}
	return (max_genre)
}

for (a in 1:29) {
	print (paste(a ,genre_score(movie_genre,fc[a])))
	
}

#q8(c) Example group 26
actor_list <- list(c("A Schoolboy Error Production (2009)",   "Joiner, Craig",  "McKay, Reuben",  "Noble, Graeme",  "Noble, John-William",   "Sandison, Martin" ,     "Taylor, Stuart (X)"),c("Be My Valentine (2011)","Joiner, Craig", "McKay, Reuben", "Moir, Shaun" ,  "Noble, Graeme", "Noble, John-William"  , "Simpson, Julia (II)"),c("Booze Culture (2012)","Hislop, Tom","McKay, Reuben","Noble, John-William","Kilpatrick, Kayleigh","McKay, Hannah"),c("Boycie (2011)","Hislop, Tom","Joiner, Craig","McKay, Reuben","Moir, Shaun","Noble, Graeme","Noble, John-William","Sandison, Martin","Taylor, Stuart (X)","McKay, Hannah","Simpson, Julia (II)"),c("Call of Babylon (2012)", "Hislop, Tom","Joiner, Craig","McKay, Reuben","Moir, Shaun","Noble, Graeme","Noble, John-William","Sandison, Martin","Taylor, Stuart (X)","Kilpatrick, Kayleigh","McKay, Hannah"),c("Fear of the Park (2010)","McKay, Reuben","Moir, Shaun","Noble, Graeme","Noble, John-William","Simpson, Julia (II)"),c("Inner Joy of a Broken Heart (2011)","Hislop, Tom","McKay, Reuben","Taylor, Stuart (X)","Kilpatrick, Kayleigh","McKay, Hannah","Simpson, Julia (II)"),c("Is This It? (2012)","Hislop, Tom","Joiner, Craig","McKay, Reuben","Noble, Graeme","Noble, John-William","Taylor, Stuart (X)","Kilpatrick, Kayleigh","McKay, Hannah","Simpson, Julia (II)"),c("Legion of Evil (2010)","Joiner, Craig","McKay, Reuben","Moir, Shaun","Noble, Graeme","Noble, John-William","Sandison, Martin","Taylor, Stuart (X)","McKay, Hannah"),c("Life of a Spy (2012)","Hislop, Tom","McKay, Reuben","Noble, John-William","Taylor, Stuart (X)","Kilpatrick, Kayleigh","McKay, Hannah"),c("Losers in Love (2011)","Hislop, Tom","McKay, Reuben","Moir, Shaun","Noble, Graeme","Noble, John-William","Sandison, Martin","Taylor, Stuart (X)","Kilpatrick, Kayleigh","McKay, Hannah","Simpson, Julia (II)"),c("Love House (2011)","Joiner, Craig","McKay, Reuben","Moir, Shaun","Noble, John-William","Sandison, Martin","Kilpatrick, Kayleigh","Simpson, Julia (II)"),c("Street Fight (2012)","Hislop, Tom","Joiner, Craig","McKay, Reuben","Noble, Graeme","Noble, John-William","Sandison, Martin","Taylor, Stuart (X)","Kilpatrick, Kayleigh","McKay, Hannah","Simpson, Julia (II)"),c("The Book of Life","Dietz, David (I)","Joiner, Craig","McKay, Reuben","Noble, Graeme","Noble, John-William","Kilpatrick, Kayleigh","Simpson, Julia (II)"),c("The Hope Within (2009)",  "Joiner, Craig","McKay, Reuben","Noble, Graeme","Noble, John-William","Sandison, Martin"),c("The Shadow of Death (2011)","Hislop, Tom","Joiner, Craig","McKay, Reuben","Moir, Shaun","Noble, Graeme","Noble, John-William","Sandison, Martin","Taylor, Stuart (X)","Kilpatrick, Kayleigh","McKay, Hannah","Simpson, Julia (II)"),c("White Cobra (2014)","Joiner, Craig","McKay, Reuben","Noble, Graeme","Noble, John-William","Sandison, Martin","Kilpatrick, Kayleigh"),c("Unconditional Love (2010)","Dasz, Steven","Joiner, Craig","McKay, Reuben","Moir, Shaun","Noble, Graeme","Noble, John-William","Sandison, Martin","Taylor, Stuart (X)","Chan, Juju","Kilpatrick, Kayleigh","Marshall, Scarlett","McKay, Hannah"))


from_list <- c()
for (i in actor_list){
	for (a in seq(length(i)-1)){
		from_list <- c(from_list,i[1])
	}
	
}

to_list <- c()
for (i in actor_list){
	to_list <- c(to_list,i[seq(2, length(i))])
}
for (i in seq(length(from_list))) {

}

relations <- data.frame(from=from_list, to=to_list)
from_list <- unique(from_list)
to_list <- unique(to_list)

g <- graph.empty(directed = F)
g <- add.vertices(g,nv=length(from_list),attr=list(name=from_list),type=rep(FALSE,length(from_list)))
g <- add.vertices(g,nv=length(to_list),attr=list(name=to_list),type=rep(TRUE,length(to_list)))
edge.list.vec <- as.vector(t(as.matrix(data.frame(relations))))
g <- add.edges(g,edge.list.vec)
V(g)$color <- V(g)$type
V(g)$color=gsub("FALSE","red",V(g)$color)
V(g)$color=gsub("TRUE","blue",V(g)$color)
png("network.png", width = 4, height = 4, units = 'in',res=300)
plot(g, edge.color="gray30",layout=layout_as_bipartite(g,hgap = 0.3,vgap=2),vertex.size=5,vertex.label.dist=-1,vertex.label.cex=0.5,rescale=FALSE,xlim=c(0,7),ylim=c(0,2))
dev.off()

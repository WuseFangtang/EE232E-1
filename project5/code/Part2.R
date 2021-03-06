getDisplayName<-function(x){
	return(x[[1]])
}
getLocation<-function(x){
	return(x[[2]])
}

IsTriIneq<-function(x){
	vs<-tri[seq(3*x-2,3*x)]
	ei<-get.edge.ids(g,c(vs[1],vs[2],vs[1],vs[3],vs[2],vs[3]))
	dis<-edge_attr(g)$weight[ei]
	if(dis[1]+dis[2] < dis[3]){
		return(FALSE)
	}
	if(dis[2]+dis[3] < dis[1]){
		return(FALSE)
	}
	if(dis[1]+dis[3] < dis[2]){``
		return(FALSE)
	}
	return(TRUE)	
}

data <- fromJSON(file="vertice_attr.json")
data <- data[3][[1]]
DisplayName <- sapply(data,getDisplayName)
Location <- lapply(data,getLocation)
v_df <-data.frame(Movement_ID=1:length(DisplayName),DisplayName,I(Location))

sf_data<-read.csv("san_francisco-censustracts-2017-4-All-MonthlyAggregate.csv")
sf_data_12<-sf_data[sf_data$month==12,c(1:2,4)]
colnames(sf_data_12)[3] <- c("weight")
g<-graph_from_data_frame(sf_data_12,vertices=v_df,directed=FALSE)

isolated<-V(g)[degree(g)==0]
g2<-delete_vertices(g,isolated)
small_component<-V(g2)[components(g2)$membership!=1]
g3<-delete_vertices(g2,small_component)
g4<-simplify(g3, edge.attr.comb="mean")
g<-g4

g_mst<-mst(g)
tri<-triangles(g)$name
selected<-sample.int(length(tri)/3,1000,replace=FALSE)
sample_result<-sapply(selected,IsTriIneq)
mean(sample_result)

dfs_result<-dfs(g_mst,1)
order_name<-dfs_result$order$name
tsploop<-c(order_name,order_name[1])

tspcost<-0
for(i in 1:(length(tsploop)-1)){
	v<-tsploop[c(i,i+1)]
	if(are_adjacent(g,v[1],v[2]))
	{
	   ei<-get.edge.ids(g,c(v[1],v[2]))
	   tspcost<- tspcost+edge_attr(g)$weight[ei]
	}
	else
	{
	   tspcost<- tspcost+distances(g,v=v[1],to=v[2])[1]
	}
	#print(tspcost)#463480.1
}
tspcost
sum(E(g_mst)$weight)#279408.2

allcost<-rep(NA,25)
for(start in 1:25)
{
	dfs_result<-dfs(g_mst,start)
    order_name<-dfs_result$order$name
    tsploop<-c(order_name,order_name[1])
    tspcost<-0
    for(i in 1:(length(tsploop)-1))
    {
		v<-tsploop[c(i,i+1)]
		if(are_adjacent(g,v[1],v[2]))
		{
	   	ei<-get.edge.ids(g,c(v[1],v[2]))
	   	tspcost<- tspcost+edge_attr(g)$weight[ei]
		}
		else
		{
	   	tspcost<- tspcost+distances(g,v=v[1],to=v[2])[1]
		}
	#print(tspcost)#463480.1
     }
    print(paste(start,tspcost,sep=":"))
    allcost[start]=tspcost
}

order<-c(order,order[1])
Longitude<-sapply(order$Location,function(x) x[1])
Latitude<-sapply(order$Location,function(x) x[2])
tsp_points<-data.frame(Latitude,Longitude)
write.csv(tsp_points,file="points.csv",row.names=FALSE)

library(igraph)
library(ggplot2)
library(ggmap)
load('sf_graph.rda')

endpoints<-ends(g_mst,E(g_mst))
points<-as.vector(t(endpoints))
lo<-sapply(points,function(x) vertex_attr(g_mst,"Location",index=V(g_mst)[x])[[1]][1])
la<-sapply(points,function(x) vertex_attr(g_mst,"Location",index=V(g_mst)[x])[[1]][2])
mst_points<-data.frame(lo,la)
mst_points$Line<-rep(1:length(E(g_mst)),each=2)

baseMap = get_map(location="San Francisco", zoom=8,maptype = "roadmap")
map <- ggmap(baseMap) +
  geom_path(aes(x=lo, y=la, group=Line), data=mst_points, alpha=0.9)
map <-  map+ labs(x="Longitude",y="Latitude",title="MST Visualization on San Francisco Map")
map<- map + theme(plot.title = element_text(size=15,hjust=0.5))

ggsave("MST.png",device="png",path=".",width=10)

  
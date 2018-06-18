library(igraph)
library(data.table)
library(hash)
library(colorRamps)
library(ggplot2)
setwd("E:\\pj5\\finance_data\\finance_data")
rm(list = ls());
Sys.setlocale("LC_TIME", "English")

# read name_sector
name_sector = fread("Name_sector.csv", header = TRUE)

# read every file in the name list
file_name = name_sector$Symbol
returned <- function(cp){
  ret = c(0)
  for (i in 2:length(cp)){
    q = (cp[i] - cp[i-1])/cp[i-1]
    ret = c(ret,q)
  }
  return (ret)
}

#normalize the return
normalize_return <- function(ret){
  q = c()
  for (i in 1:length(ret)){
    q = c(q, log(1+ret[i]))
  }
  return (q)
}

#calculate the weights
calc_weight <- function (r1, r2){
  n = length(r1)
  mean1 = mean(r1 * r2)
  mean2 = mean(r1 * r1)
  mean3 = mean(r2 * r2)
  rou = (mean1 - mean(r1) * mean(r2)) / 
        sqrt((mean2 - (mean(r1)) ** 2) * (mean3 - (mean(r2) ** 2)))
  return (rou)
}

#data clean - drop stocks with fewer days record
delete_name = c()
for (name in file_name){
  name_temp <- paste(getwd(), "/data/",name, ".csv", sep ="")
  file_temp <- fread(name_temp, header = TRUE)
  cp_temp = file_temp$Close
  if (length(cp_temp) != 765){
    delete_name <- c(delete_name, name)
  }
}
stock_r_hash <- hash()
file_name = setdiff(file_name, delete_name)
for (name in file_name){
  name_temp <- paste(getwd(), "/data/",name, ".csv", sep ="")
  file_temp <- fread(name_temp, header = TRUE)
  cp_temp = file_temp$Close
  r = returned(cp_temp)
  .set(stock_r_hash, keys = name, values = r)
}
relation = data.frame(from = c(), to = c(), weights = c())
from = c()
to = c()
weights = c()
unnormal_weights = c()
for (i in 1:(length(file_name) - 1)){
  for (j in (i+1) : length(file_name)){
    name1 = file_name[i]
    name2 = file_name[j]
    q1 = normalize_return(stock_r_hash[[name1]])
    q2 = normalize_return(stock_r_hash[[name2]])
    rou = calc_weight(q1,q2)
    weight = sqrt(2*(1-rou))
    from = c(from, name1)
    to = c(to, name2)
    weights = c(weights, weight)
    # unnoramlized weights
    q3 = stock_r_hash[[name1]]
    q4 = stock_r_hash[[name2]]
    rou1 = calc_weight(q3,q4)
    weight2 = sqrt(2*(1-rou1))
    unnormal_weights = c(unnormal_weights, weight2)
  }
}
relation = data.frame(from = from, to = to, weights = weights, un_weights = unnormal_weights)

# Question 1, lower and upper bound of rou
min = min(relation$weights)
max = max(relation$weights)
print( 1 - (min ** 2) / 2)
print( 1 - (max ** 2) / 2)

# Question 2 - unnormalized weights
relation_weight = data.frame("weight" = relation$un_weights);
p <-ggplot(relation_weight, aes(x=weight));
p <-p + geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth=0.03,
                 colour="black", fill="white");
p <- p+ geom_density(alpha=.2, fill="#FF6666")  # Overlay with transparent density plot
p <- p + ggtitle("Distribution of unnormalized weights")
p

# Question 2 - distribution of degree
g = graph_from_data_frame(d = relation, directed = FALSE, vertices = file_name)
deg_df= data.frame("degree"=degree(g));
p <-  ggplot(deg_df, aes(x=degree)) + geom_histogram(binwidth=.5);
p <-  p + ggtitle("Distribution of Graph");
p

plot(degree.distribution(g), main = "Distribution of Graph", xlab = "degree")
hist(degree.distribution(g), breaks = 50, main = "Distribution of Graph")

# Question3 - MST
set_edge_attr(g,"weights", value = relation$weights)
g_mst = mst(g,weights = relation$weights)
sectors = unique(name_sector$Sector)
#set colors for different sectors
pal <- rainbow(n = length(sectors))
for (i in 1:length(V(g_mst))){
  sector_i = name_sector$Sector[which(name_sector$Symbol == V(g_mst)[i]$name)]
  print(sector_i)
  V(g_mst)[i]$sector <- sector_i
  V(g_mst)[i]$color <- pal[which(sectors == sector_i)]
}
plot(g_mst, 
     # layout = layout_as_tree,
     vertex.size = 5, 
     vertex.label.cex=0.3,vertex.label.dist=0, main = "MST for the graph"
     )

# Question4 - two metrics

#calculate the size of each sector
size_sec = c()
for (sec in sectors){
  size_sec = c(size_sec, length(V(g_mst)[which(V(g_mst)$sector == sec)]))
}

#calculate alpha
alpha1 = 0
alpha2 = 0
for (i in 1:length(V(g_mst))){
  neighbors = neighbors(g_mst, V(g_mst)[i])
  count = 0
  now_sector = V(g_mst)[i]$sector
  for (j in 1:length(neighbors)){
    if (neighbors[j]$sector == now_sector){
      count = count + 1
    } 
  }
  alpha1 = alpha1 + (count / length(neighbors))
  alpha2 = alpha2 + (size_sec[which(sectors == now_sector)] / length(V(g_mst)))
}
alpha1 = alpha1 / length(V(g_mst))
alpha2 = alpha2 / length(V(g_mst))
print(alpha1)
print(alpha2)

# Question 5
stock_week_hash = hash()
for (name in file_name){
  name_temp <- paste(getwd(), "/data/",name, ".csv", sep ="")
  file_temp <- fread(name_temp, header = TRUE)
  co_date = file_temp$Date
  mondays_idx = which(weekdays(as.Date(co_date,'%Y-%m-%d')) == "Monday")
  cp_temp = file_temp$Close[mondays_idx]
  r = returned(cp_temp)
  .set(stock_week_hash, keys = name, values = r)
}

from = c()
to = c()
weights = c()
unnormal_weights = c()
for (i in 1:(length(file_name) - 1)){
  for (j in (i+1) : length(file_name)){
    name1 = file_name[i]
    name2 = file_name[j]
    q1 = normalize_return(stock_week_hash[[name1]])
    q2 = normalize_return(stock_week_hash[[name2]])
    rou = calc_weight(q1,q2)
    weight = sqrt(2*(1-rou))
    from = c(from, name1)
    to = c(to, name2)
    weights = c(weights, weight)
    # unnoramlized weights
    q3 = stock_week_hash[[name1]]
    q4 = stock_week_hash[[name2]]
    rou1 = calc_weight(q3,q4)
    weight2 = sqrt(2*(1-rou1))
    unnormal_weights = c(unnormal_weights, weight2)
  }
}
relation2 = data.frame(from = from, to = to, weights = weights, un_weights = unnormal_weights)

g2 = graph_from_data_frame(d = relation, directed = FALSE, vertices = file_name)

set_edge_attr(g2,"weights", value = relation2$weights)
g_mst2 = mst(g2,weights = relation2$weights)
sectors = unique(name_sector$Sector)
pal <- rainbow(n = length(sectors))
for (i in 1:length(V(g_mst2))){
  sector_i = name_sector$Sector[which(name_sector$Symbol == V(g_mst2)[i]$name)]
  print(sector_i)
  V(g_mst2)[i]$sector <- sector_i
  V(g_mst2)[i]$color <- pal[which(sectors == sector_i)]
}
plot(g_mst2,  vertex.size = 5, 
     layout = layout_as_tree,
     vertex.label.cex=0.5,vertex.label.dist=0, main = "MST for using only Monday data"
)

# claculating alphas for clustering of weekly data
alpha1 = 0
alpha2 = 0
for (i in 1:length(V(g_mst2))){
  neighbors = neighbors(g_mst2, V(g_mst2)[i])
  count = 0
  now_sector = V(g_mst2)[i]$sector
  for (j in 1:length(neighbors)){
    if (neighbors[j]$sector == now_sector){
      count = count + 1
    } 
  }
  alpha1 = alpha1 + (count / length(neighbors))
  alpha2 = alpha2 + (size_sec[which(sectors == now_sector)] / length(V(g_mst2)))
}
alpha1 = alpha1 / length(V(g_mst2))
alpha2 = alpha2 / length(V(g_mst2))
print(alpha1)
print(alpha2)
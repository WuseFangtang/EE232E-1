library(ggplot2)
load('Q9Q10Data')

drawDensityPlot <- function(data,filename,titlename){
	df <- data.frame(data)
	p<-ggplot(df,aes(x=data))+geom_histogram(aes(y=..density..),bins=30,color="black",fill="white")+geom_density(alpha=.2,color="darkblue",fill="lightblue")
	p <- p + labs(x="Rate",y="Density",title=titlename)
	p <- p + theme(plot.title = element_text(size=10))	
	p <- p + geom_vline(aes(xintercept=mean(data)),
            color="red", linetype="dashed", size=0.5)        
     ggsave(paste(filename,".png",sep=""),device="png",path="./plots")
}

drawDensityPlot(batman,"batman","Distribution of Rates of Batman v Superman: Dawn of Justice (2016)'s Neighbors")
drawDensityPlot(mission,"mission","Distribution of Rates of Mission: Impossible - Rogue Nation (2015)'s Neighbors")
drawDensityPlot(minion,"minion","Distribution of Rates of Minions (2015)'s Neighbors")

drawDensityPlot(batman2,"batman2","Distribution of Rates of Batman v Superman: Dawn of Justice (2016)'s Neighbors in the same Community")
drawDensityPlot(mission2,"mission2","Distribution of Rates of Mission: Impossible - Rogue Nation (2015)'s Neighbors in the same Community")
drawDensityPlot(minion2,"minion2","Distribution of Rates of Minions (2015)'s Neighbors in the same Community")

batmandf<-data.frame("Batman v Superman: Dawn of Justice (2016)",batman)
colnames(batmandf)=c("Movie","Rate")
missiondf<-data.frame("Mission: Impossible - Rogue Nation (2015)",mission)
colnames(missiondf)=c("Movie","Rate")
miniondf<-data.frame("Minions (2015)",minion)
colnames(miniondf)=c("Movie","Rate")
data<-rbind(batmandf,missiondf,miniondf)

p <- ggplot(data, aes(x=Movie, y=Rate,fill=Movie)) + 
  geom_boxplot(notch=FALSE)
p <- p + labs(title="Boxplot of Rates of Three Movies' Neighbors")	
p <- p + stat_summary(fun.y=mean, geom="point", shape=22, size=1)
p <- p+scale_fill_brewer(palette="Accent")+theme_minimal()
p <- p + theme(plot.title = element_text(size=10,hjust=0.5),legend.position="none")
ggsave("boxplot1.png",device="png",path="./plots",width=10)


batmandf2<-data.frame("Batman v Superman: Dawn of Justice (2016)",batman2)
colnames(batmandf2)=c("Movie","Rate")
missiondf2<-data.frame("Mission: Impossible - Rogue Nation (2015)",mission2)
colnames(missiondf2)=c("Movie","Rate")
miniondf2<-data.frame("Minions (2015)",minion2)
colnames(miniondf2)=c("Movie","Rate")
data2<-rbind(batmandf2,missiondf2,miniondf2)

p <- ggplot(data2, aes(x=Movie, y=Rate,fill=Movie)) + 
  geom_boxplot(notch=FALSE)
p <- p + labs(title="Boxplot of Rates of Three Movies' Neighbors in the same Community")	
p <- p + stat_summary(fun.y=mean, geom="point", shape=22, size=1)
p <- p+scale_fill_brewer(palette="Accent")+theme_minimal()
p <- p + theme(plot.title = element_text(size=10,hjust=0.5),legend.position="none")
ggsave("boxplot2.png",device="png",path="./plots",width=10)


batmandf$Movie<-NULL
batmandf$Question<-"Q9"
batmandf2$Movie<-NULL
batmandf2$Question<-"Q10"
batman_comp<-rbind(batmandf,batmandf2)
batman_comp$Question <- factor(batman_comp$Question,
    levels = c('Q9','Q10'),ordered = TRUE)
p <- ggplot(batman_comp, aes(x=Question, y=Rate,fill=Question)) + 
  geom_boxplot(notch=FALSE)
p <- p + labs(title="Boxplot of Rates of 'Batman v Superman: Dawn of Justice (2016)'s Neighbors in Q9 and Q10")	
p <- p + stat_summary(fun.y=mean, geom="point", shape=22, size=1)
p <- p + scale_fill_brewer(palette="Accent")+theme_minimal()
p <- p + theme(plot.title = element_text(size=10,hjust=0.5),legend.position="none")
ggsave("batman_box.png",device="png",path="./plots",width=10)

missiondf$Movie<-NULL
missiondf$Question<-"Q9"
missiondf2$Movie<-NULL
missiondf2$Question<-"Q10"
mission_comp<-rbind(missiondf,missiondf2)
mission_comp$Question <- factor(mission_comp$Question,
    levels = c('Q9','Q10'),ordered = TRUE)
p <- ggplot(mission_comp, aes(x=Question, y=Rate,fill=Question)) + 
  geom_boxplot(notch=FALSE)
p <- p + labs(title="Boxplot of Rates of 'Mission: Impossible - Rogue Nation (2015)' s Neighbors in Q9 and Q10")	
p <- p + stat_summary(fun.y=mean, geom="point", shape=22, size=1)
p <- p + scale_fill_brewer(palette="Accent")+theme_minimal()
p <- p + theme(plot.title = element_text(size=10,hjust=0.5),legend.position="none")
ggsave("mission_box.png",device="png",path="./plots",width=10)

miniondf$Movie<-NULL
miniondf$Question<-"Q9"
miniondf2$Movie<-NULL
miniondf2$Question<-"Q10"
minion_comp<-rbind(miniondf,miniondf2)
minion_comp$Question <- factor(minion_comp$Question,
    levels = c('Q9','Q10'),ordered = TRUE)
p <- ggplot(minion_comp, aes(x=Question, y=Rate,fill=Question)) + 
  geom_boxplot(notch=FALSE)
p <- p + labs(title="Boxplot of Rates of 'Minions (2015)' s Neighbors in Q9 and Q10")	
p <- p + stat_summary(fun.y=mean, geom="point", shape=22, size=1)
p <- p + scale_fill_brewer(palette="Accent")+theme_minimal()
p <- p + theme(plot.title = element_text(size=10,hjust=0.5),legend.position="none")
ggsave("minion_box.png",device="png",path="./plots",width=10)
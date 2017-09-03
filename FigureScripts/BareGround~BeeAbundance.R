#clear environment and set working directory
rm(list=ls())
setwd("C:/Users/Robbie/Desktop/Learning R/rweber/Data/Working")

#load libraries
library(ggplot2)

#read in the data, alt + dash = <-
BeeStuff <- read.csv("ETrap_Bees.csv")

#changing year column to numeric from integer
pch.listAM <- as.numeric(BeeStuff$Year)
pch.listAM

#Making plot, label axes
plot(BeeStuff$BareGround,BeeStuff$Ind,
     xlab = "Percent Bare Ground", ylab = "Bee Abundance",
     pch = c(pch.listAM), col="red")

#Fitting linear model
model=lm(BeeStuff$Ind~BeeStuff$BareGround)
model
summary(model)
#trendline on model
abline(model)
legend("topright",bty="n",
       legend=paste("R2=",format(summary(model)$adj.r.squared,digits=4)))

#Better plot (Morgan's)
#Change year column to factor
BeeStuff$Year<-as.factor(BeeStuff$Year) 
BGonBAAM <- lm(Ind~BareGround, data=BeeStuff)
BGonBAAM
summary(BGonBAAM)

#Find intercept and slope before plotting Best fit line
coef(BGonBAAM)

#Creating plot, setting color and size of points, create trendline, label x and y, title figure and enter in title w/ \n
BGonBAAMplot <- ggplot(data=BeeStuff, aes(x=BareGround,y=Ind))  +
  geom_point(aes(shape=Year,color=Year), size=3)  +
  geom_abline(intercept= -0.7025809,  slope=0.3164081)  +
  #change background color to bw, axis labels
  theme_bw() +
  labs(x="Bare Ground Percentage", y="Number of Individuals") +
  ggtitle("Influence of Bare Ground \non Bee Abundance")   +
  #center title, change size and bold
  theme(plot.title = element_text(size = 15,face="bold",hjust=0.5))  +
        theme(legend.text = element_text(size=10))
BGonBAAMplot

#New dataset
BeeStuffBTA <- read.csv("ETrapBeesMMM.csv")
BeeStuffBTA$Year <- as.factor(BeeStuffBTA$Year)
Morgmodel <- lm(Ind~BareGround,data=BeeStuffBTA)
summary(Morgmodel)
coef(Morgmodel)
BeeStuffBTA


#Plot that shit
BGonBAMMMplot <- ggplot(data=BeeStuffBTA, aes(x=BareGround,y=Ind))  +
  geom_point(aes(shape=Year,color=Year), size=3)  +
  geom_abline(intercept= 3.2448811,  slope=0.2484154)  +
  #change background color to bw, axis labels
  theme_bw() +
  labs(x="Bare Ground Percentage", y="Number of Individuals") +
  ggtitle("Influence of Bare Ground \non Bee Abundance")   +
  #center title, change size and bold
  theme(plot.title = element_text(size = 15,face="bold",hjust=0.5))  +
  theme(legend.text = element_text(size=10))
BGonBAMMMplot

#Combined dataset from all three years
BeeStuffATY <- read.csv("ETrapBeesCombined.csv")
BeeStuffATY
BeeStuffATY$Year <- as.factor(BeeStuffATY$Year)
CombModel <- lm(Ind~BareGround,data=BeeStuffATY)
summary(CombModel)
coef(CombModel)

#copy paste from above and change names 
BGonBACombplot <- ggplot(data=BeeStuffATY, aes(x=BareGround,y=Ind))  +
  geom_point(aes(shape=Year,color=Year), size=3)  +
  geom_abline(intercept=  0.6387116, slope=0.2785092)  +
  #change background color to bw, axis labels
  theme_bw() +
  labs(x="Bare Ground Percentage", y="Number of Individuals") +
  ggtitle("Influence of Bare Ground \non Bee Abundance")   +
  #center title, change size and bold
  theme(plot.title = element_text(size = 15,face="bold",hjust=0.5))  +
  theme(legend.text = element_text(size=10))
BGonBACombplot

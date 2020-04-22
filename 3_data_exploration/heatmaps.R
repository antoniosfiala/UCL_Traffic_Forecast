# LOAD DATA AND PACKAGES --------------------------------------------------

library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(tseries)
library(naniar)
source("starima_package.R")

setwd("C:/Users/hanna/Desktop/Winter2020/spatio-temporal/assessment/analysis/UCL_Traffic_Forecast")

tspeeds<-read.csv("2_data_split/traffic_speeds.csv")


# DATA PROCESSING ---------------------------------------------------------

tspeeds$Date<-as.Date(tspeeds$Date)
tspeeds$day<-weekdays(tspeeds$Date)
tspeeds$week=1
tspeeds$week[1261:2520]=2
tspeeds$week[2521:3780]=3
tspeeds$week[3781:5040]=4
tspeeds$week[5041:nrow(tspeeds)]=5
tspeeds$wtspeeds$week<-factor(tspeeds$week,levels=c("1","2","3","4","5"),labels=c("week1",
                                                                                  "week2","week3","week4","week 5"))
heatdata <- gather(tspeeds,road,time,X1745:X453)
heatdata <- heatdata[c('Date', 'time', 'Time', 'week', 'day', 'road')] # get rid of the wtspeeds column 
heatdata$day <- factor(heatdata$day, levels = c('Saturday', 'Sunday', 'Monday', 'Tuesday','Wednesday', 'Thursday', 'Friday')) # get days column to be factor
heatdata <- heatdata[order(heatdata$day),] # order by days

# get rid of the 'x' in front of road links
for(i in 1:length(heatdata$road)){
  split_ <- strsplit(heatdata$road[i], 'X')
  heatdata$road[i]<- split_[[1]][[2]]
}

# remove speed values that are really high and mess up the visuals 
nosun <- heatdata[heatdata$time<1.25,]

hist(heatdata$time)

plot(heatdata$time, heatdata$Time)

# BASIC VIZ ---------------------------------------------------------------

ggplot(data=heatdata, aes(x=time))+
  geom_histogram(color="black", fill="lightblue")+
  #geom_density(color="darkblue", fill="lightblue")+
  theme_minimal()+
  ggtitle(label='Histogram of all speed measurements')

ggplot(data=heatdata, aes(x=Time, y=time))+
  #geom_histogram(color="black", fill="lightblue")+
  #geom_density(color="darkblue", fill="lightblue")+
  geom_point()+
  theme_minimal()+
  ggtitle(label='Histogram of all speed measurements')

# MAKE TOTAL HEATMAP ------------------------------------------------------

# make the map 
ggplot(data=heatdata,mapping=aes(x=Time,y=road,fill=time))+
  geom_tile()+
  facet_grid(week~day,switch="x",scales="free_x",space="free_x")+
  #scale_fill_gradient(name="time",low="#ff9c9c",high="#fc2121")+
  scale_fill_viridis(name="Speed (s/m)",option ="D", begin=0.1, end=0.95)+
  theme(strip.placement="outside",plot.title=element_text(hjust=0.5))+
  ggtitle(label="All data heatmap. No values removed")+
  theme_minimal()+
  theme(panel.spacing.x=unit(0.05, "lines"),panel.spacing.y=unit(0.15, "lines")) # adjust the spacing between panels


# MAKE WEEKLY HEATMAPS ----------------------------------------------------


makemap <- function(start, end, title, data){
  
  # get the appropriate weekly subset
  subset <- subset(data,data$Date>start&tspeeds$Date<=end)
  subset <- gather(subset, road, time, X1745:X453)
  
  # make the heatmap
  hmap<-ggplot(data=subset,mapping=aes(x=Time,y=road,fill=time))+
    geom_tile()+
    facet_grid(~day,scales="free_x",space="free_x")+
    scale_fill_gradient(name="Speed (s/m)",low="#FFFFFF",high="#012345")+
    theme(strip.placement="outside",plot.title=element_text(hjust=0.5))+
    ggtitle(label=title)+
    theme_bw()
  
  # return the output
  return(hmap)
  
}

w1 <- makemap('2010-12-31', '2011-01-07', "Week 1", tspeeds)
w2 <- makemap('2011-01-07', '2011-01-14', "Week 2", tspeeds)
w3 <- makemap('2011-01-14', '2011-01-21', "Week 3", tspeeds)
w4 <- makemap('2011-01-21', '2011-01-28', "Week 4", tspeeds)
w5 <- makemap('2011-01-28', '2011-01-30', "Week 5", tspeeds)

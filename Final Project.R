library(plyr)
library(dplyr)
library(tidyverse)#library for filter
library(ggplot2)
flood <- read.csv("C:/Users/sony/Desktop/STAT 605/Final Project/Project/flood.csv")
#filter(flood,flood$Country=="India")
#summary(flood)

#data preparation
#drop variables:Annual DFO # (discontinued),Glide,Nations,Validation (post event #3503),Damage (USD),"News" if validated
flood<-flood[-c(2,3,6,9,15,22)]
#drop the duplicated variable - began date
flood<-flood[-21]
#date transformation
flood$Began<-as.Date(as.character((flood$Began)), format = " %d-%h-%y")
flood$Ended<-as.Date(as.character((flood$Ended)), format = " %d-%h-%y")
flood$Duration.in.Days<-as.numeric(flood$Duration.in.Days)


for (i in 1:nrow(flood)){
  if(!is.na(flood$Began[i])){
    if(format.Date(flood$Began[i], "%m")=="12" || format.Date(flood$Began[i],"%m")=="01"|| format.Date(flood$Began[i],"%m")=="02"){
      flood$Season[i]<-"Winter"
    } else if(format.Date(flood$Began[i], "%m")=="03" || format.Date(flood$Began[i], "%m")=="04"|| format.Date(flood$Began[i], "%m")=="05"){
      flood$Season[i]<-"Spring"
    } else if (format.Date(flood$Began[i], "%m")=="06" || format.Date(flood$Began[i], "%m")=="07"|| format.Date(flood$Began[i], "%m")=="08"){
      flood$Season[i]<-"Summer"
    } else {
      flood$Season[i]<-"Autumn"
    }
  }
  if(flood$Duration.in.Days[i]<=7 && flood$Duration.in.Days[i]>=1){
    flood$Duration.length[i]<-"S" #stands for short
    
  } else if(flood$Duration.in.Days[i]>=7 && flood$Duration.in.Days[i]<=21){
    flood$Duration.length[i]<-"M" #stands for medium
    
    
  } else if(flood$Duration.in.Days[i]>=21 && flood$Duration.in.Days[i]<=35 ){
    flood$Duration.length[i]<-"L" #stands for long
    
  } else{
    flood$Duration.length[i]<-"VL" #stands for very long
    
  }
}


###Q1:
#flood seasonality and duration
ggplot(flood,aes(x=Season,fill=Duration.length))+ geom_bar() +coord_flip() + labs(title="flood seasonality and duration")+ theme_bw()

  library(dplyr)
library(ggplot2)
library(ggmap)
library(maps)
library(stringr)
library(data.table)
knitr::opts_chunk$set(echo = TRUE)

#import data
#setwd("/Users/sadierichardson/Desktop")
read.csv("C:/Users/sony/Desktop/STAT 605/Final Project/Project/flood.csv") -> flood
flood1 <- flood

#Separate data into groups by region (using latitude and longitude)
#rename centroidx and centroidy as latitude and longitude
colnames(flood1)[colnames(flood1) == "Centroid.Y"] <- "latitude"
colnames(flood1)[colnames(flood1) == "Centroid.X"] <- "longitude"
#lat and long in numeric format
flood1$latitude <- as.numeric(as.character(flood1$latitude))
flood1$longitude <- as.numeric(as.character(flood1$longitude))
  
#make a world map
map.world <- map_data(map="world")
map <- ggplot()
map <- map + geom_map(data=map.world, map=map.world, aes(map_id=region, x=long, y=lat), fill = "white", col = "gray45")
 
 #format begin as date
flood1$Began<-as.Date(as.character((flood1$Began)), format = " %d-%h-%y")
#format month as character
flood1$month <- as.character(format(flood1$Began, format = '%h'))
#add season column
flood1$season <-  ifelse(flood1$month %in% c("Jun", "Jul", "Aug"),"Summer", 
                         ifelse(flood1$month %in% c("Dec", "Jan", "Feb"), "Winter", 
                                ifelse(flood1$month %in% c("Sep", "Oct", "Nov"), "Fall", 
                                       ifelse(flood1$month %in% c("Mar", "Apr", "May"), "Spring", NA))))
flood_season <- flood1 %>%
  filter(!is.na(season))
  
 season_map <- map +
  geom_point(data = subset(flood1, season %in% c("Fall", "Spring", "Summer", "Winter")), aes(x=longitude, y=latitude, color = season, size = quartile), size = .5, alpha = .2) + labs(title = "Floods Around the World by Season", x = "Longitude", y = "Latitude")  +  facet_wrap(~ season, nrow = 2) + theme(legend.position="none")
  
season_map
  
  flood1$Year<-substring(flood1$Began,1,4) 
occ<-count(flood1,'Year')
ggplot(flood1, aes(Began, fill = Severity..)) + geom_histogram() +  scale_x_date()+geom_bar(width=1)+  theme_bw()+labs(title="Severity Across Years")+xlab("flood Began Date")+ theme_bw()
  
  flood$Began<-as.Date(as.character((flood$Began)), format = " %d-%h-%y")
flood$Ended<-as.Date(as.character((flood$Ended)), format = " %d-%h-%y")
flood$Duration.in.Days<-as.numeric(flood$Duration.in.Days)


for (i in 1:nrow(flood)){
  if(!is.na(flood$Began[i])){
    if(format.Date(flood$Began[i], "%m")=="12" || format.Date(flood$Began[i],"%m")=="01"|| format.Date(flood$Began[i],"%m")=="02"){
      flood$Season[i]<-"Winter"
    } else if(format.Date(flood$Began[i], "%m")=="03" || format.Date(flood$Began[i], "%m")=="04"|| format.Date(flood$Began[i], "%m")=="05"){
      flood$Season[i]<-"Spring"
    } else if (format.Date(flood$Began[i], "%m")=="06" || format.Date(flood$Began[i], "%m")=="07"|| format.Date(flood$Began[i], "%m")=="08"){
      flood$Season[i]<-"Summer"
    } else {
      flood$Season[i]<-"Autumn"
    }
  }
  if(flood$Duration.in.Days[i]<=7 && flood$Duration.in.Days[i]>=1){
    flood$Duration.length[i]<-"S" #stands for short
    
  } else if(flood$Duration.in.Days[i]>=7 && flood$Duration.in.Days[i]<=21){
    flood$Duration.length[i]<-"M" #stands for medium
    
    
  } else if(flood$Duration.in.Days[i]>=21 && flood$Duration.in.Days[i]<=35 ){
    flood$Duration.length[i]<-"L" #stands for long
    
  } else{
    flood$Duration.length[i]<-"VL" #stands for very long
    
  }
    
    #the pie chart with "other" included
bp2<- ggplot(flood_SVL, aes(x=factor(TopCountry,levels=names(sort(table(TopCountry),increasing=TRUE))), y="", fill=TopCountry))+geom_bar(width = 1,stat = "identity")
pie2 <- bp2 + coord_polar("y") +ylab("")+xlab("") + theme_minimal()+theme(axis.title.x=element_blank(),axis.title.y=element_blank(), legend.position="none",axis.text = element_blank())+labs(title="with others included")

grid.newpage()
vp4<-viewport(x=0.35,y=0.5,height=1,width=1)
vp5<-viewport(x=0.85,y=0.25,height=0.5,width=0.5)
print(pie,vp=vp4)
print(pie2,vp=vp5)
  
  #create a df or something else with the summary output.

temp <- row.names(as.array(summary(flood$Main.cause, max=11))) 
flood$Main.cause<-tolower(flood$Main.cause)
flood$Main.cause<- as.character(flood$Main.cause) 
temp<-tolower(temp)


#pull out top 10 most frequence causes, and list other not that frequent causes as "other"

flood$TopCause <- ifelse(
  #condition: match flood$Main.cause with row.names in summary data frame
  flood$Main.cause %in% temp, 
  # if satisfies the condition, then it should be named as flood$Main.cause
  flood$Main.cause, 
  ## else it should be named "Other"
  "Other" 
)
#factorize the output
flood$TopCause<- as.factor(flood$TopCause)
Top_Causes<- factor(flood$TopCause,
                    levels=names(sort(table(flood$TopCause),increasing=TRUE)))

ggplot(flood,aes(x=Top_Causes))+ geom_bar(fill=rainbow(8)) + coord_flip() + labs(title="Top 8 Main Causes of Floods") + theme_bw()+xlab("Top Causes")
}
flood_noNA<- flood[!(flood$Severity..=="#N/A"),]
ggplot(flood_noNA) + aes(x = Severity.., y = Duration.length) + geom_count() + labs(title="Flood Severity v.s Duration Length")+ theme_bw()
  
  flood_SVL<- subset(flood, flood$Severity.. ==2 & flood$Duration.length=="VL")

temp2 <- row.names(as.data.frame(summary(flood_SVL$Country, max=8))) 
temp3 <- row.names(as.data.frame(summary(flood$Country, max=8))) 
flood_SVL$Country<-as.character(flood_SVL$Country)

flood_SVL$TopCountry <- ifelse(
  #condition: match flood$Main.cause with row.names in summary data frame
  flood_SVL$Country %in% temp2, 
  # if satisfies the condition, then it should be named as flood$Main.cause
  flood_SVL$Country, 
  ## else it should be named "Other"
  "Other" 
)
#factorize the output
flood_SVL$TopCountry<- as.factor(flood_SVL$TopCountry)
flood_SVL_temp<-subset(flood_SVL,!TopCountry=="Other")

#the pie chart with "other" excluded(theme different!!)
library(grid)
library(gridBase)

bp<- ggplot(flood_SVL_temp, aes(x=factor(TopCountry,levels=names(sort(table(TopCountry),increasing=TRUE))), y="", fill=TopCountry))+geom_bar(width = 1,stat = "identity")
pie <- bp + coord_polar("y") +ylab("")+xlab("")+labs(title="Long & Severe floods Among Countries")+ theme_minimal()+theme(axis.text = element_blank(), legend.position="left")

  topcause <- flood1 %>% 
  select(latitude, longitude, Main.cause, Dead, Country, Displaced)

topcause$Main.cause <- str_to_lower(topcause$Main.cause)

topcause$Main.cause[grepl("tropical storm", topcause$Main.cause)]<-"Tropical Storm"
topcause$Main.cause[grepl("typhoon", topcause$Main.cause)]<-"Typhoon"
topcause$Main.cause[grepl("tropical cyclone", topcause$Main.cause)]<-"Tropical Cyclone"
topcause$Main.cause[grepl("snow", topcause$Main.cause)]<-"Ice/Snow"
topcause$Main.cause[grepl("ice", topcause$Main.cause)]<-"Ice/Snow"
topcause$Main.cause[grepl("monsoon", topcause$Main.cause)]<-"Monsoonal Rain"
topcause$Main.cause[grepl("heavy rain", topcause$Main.cause)]<-"Heavy Rain"

topcause <- topcause %>%
  filter(Main.cause %in% c("Tropical Storm","Ice/Snow","Monsoonal Rain", "Heavy Rain", "Brief Torrential Rain", "Torrential Rain", "Typhoon", "Tropical Cyclone"))
  map+geom_point(data=subset(topcause, Main.cause %in% c("Tropical Storm","Ice/Snow","Monsoonal Rain", "Brief Torrential Rain", "Torrential Rain", "Typhoon", "Tropical Cyclone")), aes(x=longitude, y=latitude, color = Main.cause), size = 1, alpha = .7) + labs(title = "Floods Around the World by Cause", x = "Longitude", y = "Latitude", color = "Main Cause") + theme(legend.position="bottom")
  
  topcause$Dead <- as.numeric(as.character(topcause$Dead))
dead_stats <- topcause %>% 
  group_by(Main.cause) %>% 
  summarise(mean_deaths = mean(Dead),
            max_deaths = max(Dead),
            sum_deaths = sum(Dead),
            count_occurrences = n(),
            death_per_occurrence = sum_deaths/count_occurrences)
ggplot(dead_stats, aes(Main.cause)) + geom_bar(aes(weight = dead_stats$death_per_occurrence, fill = dead_stats$Main.cause)) + 
  labs(title = "People Killed per Flood by Cause of Flood", x = "Cause of Flood", y = "Deaths per Occurrence") + theme_bw() + theme(legend.position="none", text = element_text(size=10))
  
  topcause$Displaced <- as.numeric(as.character(topcause$Displaced))
displaced_stats <- topcause %>% 
  group_by(Main.cause) %>% 
  summarise(mean_disp = mean(Displaced),
            max_disp = max(Displaced),
            sum_disp = sum(Displaced),
            count_occurrences = n(),
            disp_per_occurrence = sum_disp/count_occurrences)
ggplot(displaced_stats, aes(Main.cause)) + geom_bar(aes(weight = displaced_stats$disp_per_occurrence, fill = displaced_stats$Main.cause)) +
  labs(title = "People Displaced per Flood by Cause of Flood", x = "Cause of Flood", y = "Displaced per Occurrence") + theme_bw() + theme(legend.position="none", text = element_text(size=10))

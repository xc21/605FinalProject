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
ggplot(flood,aes(x=Season,fill=Duration.length))+ geom_bar() + coord_flip() + labs(title="Flood seasonality and their duration") 

###Q2:
###What are the main causes of these floods?

#create a df or something else with the summary output.
temp <- row.names(as.data.frame(summary(flood$Main.cause, max=10))) 
flood$Main.cause<- as.character(flood$Main.cause) 


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

ggplot(flood,aes(x=Top_Causes))+ geom_bar() + coord_flip() + labs(title="What are the main causes of these floods?") 

#
subset(flood, format.Date(flood$Began, "%m")=="05" & format.Date(date, "%y")=="07")

levels(flood$country)
unique(flood$Country) #346 countries, top countries: USA, China, India, Indonesia,Philippines,
summary(flood)

#Q2:seasonality analysis
ggplot(flood, aes(x=Season, y=Duration.in.Days)) + 
  geom_violin()


#flood in usa
flood_USA<-subset(flood,Country=="USA")



rm(list=ls())

# Library
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(gganimate)
library(gifski)
library(av)


#working directory
setwd("C:/Users/Gemstone/Documents/Covid analytics")

#datascot <- read.csv("https://raw.githubusercontent.com/tomwhite/covid-19-uk-data/master/data/covid-19-totals-scotland.csv", na.strings = "", fileEncoding = "UTF-8-BOM")
#head(datascot)
#write.csv(datascot,"datascot.csv")

# Data
scotdata <- read.csv(file.choose(), header = T)
head(scotdata)



#dates?
scotdata1<- scotdata %>% 
  mutate(DateScot  = dmy(DateScot))
head(scotdata1)

plotSCOT<-ggplot(data=scotdata1) +
  geom_line(aes(x = DateScot, y = Deaths),color = 'red')+
  geom_line(aes(x = DateScot, y = ConfirmedCases),color = 'blue')+
  geom_line(aes(x = DateScot, y = Tests),color = 'black')+
  geom_point(aes(x = DateScot, y = ConfirmedCases),color = 'blue',size = 1.5) +
  geom_point(aes(x = DateScot, y = Deaths),color = 'red',size = 1.5) +
  geom_point(aes(x = DateScot, y = Tests),color = 'black',size = 1.5) +
  ylab("Number")+
  ggtitle("Scotland plot")+
  xlab("Date")+ 
  theme_bw()+
transition_reveal(DateScot)
 
anim_save()


CovidCases<- ggplot(scotdata1, aes(DateScot, ConfirmedCases)) + 
  geom_bar(stat = 'identity', width = 1, fill="blue", colour="black")+
  geom_smooth(aes(x = DateScot, y = ConfirmedCases),color = 'red',
              method = "loess", se = FALSE, fullrange=FALSE) +
  geom_point(aes(x = DateScot, y = ConfirmedCases),color = 'blue',size = 1.5) +
  #geom_area(fill = 'pink')+ 
  ylim(0, 9000)+
  ylab("Covid Cases")+
  xlab("Date")+ 
  ggtitle("Daily Cases and curve (Scot)")+
  theme_bw()


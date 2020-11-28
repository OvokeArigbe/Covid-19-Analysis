rm(list=ls())

#libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(gganimate)
library(sensitivity)
library(ggrepel)
library(ggQC)
library(reshape2)
library(ggpubr)
library(ggthemes)

#working directory
setwd("C:/Users/Gemstone/Documents/Covid analytics")

# Data
Worlddata <- read.csv(file.choose(), header = T)
head(Worlddata)

#calculating death rates for selected countries
Worlddatanew <- Worlddata %>% filter(deaths > 0)%>% filter(countriesAndTerritories == 'United_States_of_America' |
                                       countriesAndTerritories == 'France' |
                                       countriesAndTerritories == 'United_Kingdom' |
                                       countriesAndTerritories == 'Spain' |
                                       countriesAndTerritories == 'Italy' |
                                       countriesAndTerritories == 'Germany') %>% 
  group_by(countriesAndTerritories) %>% 
  mutate(deathRate = sum(deaths)/sum(cases))%>% mutate(deathpop=(deaths/popData2018)*1000000)


#Bar plot of death per million for selected countries
worldbar <-Worlddatanew%>% ggplot(aes(reorder(x=countriesAndTerritories,deathpop), 
           y = deathpop,
           fill = countriesAndTerritories)) +
  geom_bar(stat = 'identity') +
  xlab("Countries")+
  ylab("Covid-19 Death rate")+
  #legend(title = "Countries")+
  theme_bw()+
  ggtitle("Death-rate 4 sm countries")+
  guides(fill = F)+
  coord_flip(clip ="off",expand=FALSE)



 

#Death and cases per continent

worlddata1<- Worlddata%>% group_by(continentExp) %>% filter(deaths >0)%>%
  summarise(TotalDeaths = sum(deaths), TotalCases=sum(cases))

  worlddatam <- melt(worlddata1[,c('continentExp','TotalDeaths','TotalCases')],id.vars = 1)
  
  continentplot <-ggplot(worlddatam, aes(x = continentExp,y = value)) + 
    geom_bar(aes(fill = variable), position = "dodge", stat="identity", width=0.5) +
    xlab("Countries")+
    ylab("Covid-19 (Cases, Death")+
      ggtitle("Cases & Death per Continent")+
      yscale("log10", .format = TRUE)+
      theme_classic()
    
  

#Ploting cases and death for selected countries
selcon <-Worlddata %>% filter(deaths > 0)%>% filter (countriesAndTerritories == 'France' |
                                                 countriesAndTerritories == 'Italy' |
                                                 countriesAndTerritories == 'Germany'|
                                                 countriesAndTerritories == 'United_Kingdom' |
                                                 countriesAndTerritories == 'China' |
                                                 countriesAndTerritories == 'Nigeria'|
                                                 countriesAndTerritories == 'United_States_of_America') %>%
  group_by(countriesAndTerritories) %>%
  summarise(TotalCases = sum(cases), TotalDeaths = sum(deaths)) %>%
  ggplot(aes(countriesAndTerritories)) +
  geom_point(aes(y = TotalCases, color = "Cases")) +
  geom_point(aes(y = TotalDeaths, color= "Deaths")) +
  ylab("Total")+
  xlab("Countries")+ 
  ggtitle("Cases & Death for select countries")+
  theme_bw()+
  coord_flip()

# Formating dates?
Worlddatanew1 <- Worlddatanew %>% 
  mutate(dateRep  = dmy(dateRep))

# Animated world cummulative death time series plot
worlddeath<- Worlddatanew1 %>% filter(deaths > 0)%>% 
  group_by(dateRep) %>% 
  summarise(TotalCases=sum(cases),
            TotalDeath=sum(deaths)) %>% 
  mutate(cumlc = cumsum(TotalCases), cumld=cumsum(TotalDeath)) %>% 
  ggplot() +
  geom_line(aes(x = dateRep, y = cumlc),color = 'blue') +
  geom_line(aes(x = dateRep, y = cumld),color = 'red')+
  geom_point(aes(x = dateRep, y = cumlc),color = 'blue',size = 1.5) +
  geom_point(aes(x = dateRep, y = cumld),color = 'red',size = 1.5) +
  #geom_area(fill = 'pink')+
  ylab("Cumulative (Deaths, cases)")+
  xlab("Date")+ 
  yscale("log10",.format=TRUE)+
  ggtitle("World cum. Deaths and Cases")+
  theme_bw()
  #transition_reveal(dateRep)

  #UK daily cases and death
worlddatauk<- Worlddatanew1 %>% filter(deaths > 0)%>% 
  filter(countriesAndTerritories == 'United_Kingdom')%>%
  group_by(dateRep)
  
plotUK<-ggplot(data=worlddatauk) +
  geom_line(aes(x = dateRep, y = deaths),color = 'red')+
  geom_line(aes(x = dateRep, y = deaths),color = 'blue')+
  geom_point(aes(x = dateRep, y = cases),color = 'blue',size = 1.5) +
  geom_point(aes(x = dateRep, y = deaths),color = 'red',size = 1.5) +
  #geom_area(fill = 'pink')+
  ylab("number")+
  xlab("Date")+ 
  ggtitle("Daily death & Cases(UK)")+
  theme_bw()
  #transition_reveal(dateRep)


#UK daily cases and death
CovidCases<- ggplot(worlddatauk, aes(dateRep, cases)) + 
  geom_bar(stat = 'identity', width = 1, fill="blue", colour="black")+
  geom_smooth(aes(x = dateRep, y = cases),color = 'red',
              method = "loess", se = FALSE, fullrange=FALSE) +
  geom_point(aes(x = dateRep, y = deaths),color = 'blue',size = 1.5) +
  #geom_area(fill = 'pink')+ 
  ylim(0, 9000)+
  ylab("Covid Cases")+
  xlab("Date")+ 
  ggtitle("Daily Cases and curve (UK)")+
  theme_bw()

ggarrange(CovidCases,plotUK,worlddeath,selcon,continentplot,worldbar)
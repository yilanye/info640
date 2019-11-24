library(tidyverse) 
library(rpart) 
library(rpart.plot) 
library(rattle) 
library(RColorBrewer) 
library(class) 
library(factoextra)

library(tidyverse)
library(lubridate)
library(leaflet)
library(sp)
library(ggthemes)
library(maps)
library(mapdata)
library(sf)
library(topicmodels)

Athl <- read_csv(file ="Desktop/120-years-of-olympic-history-athletes-and-results/athlete_events.csv")
head(Athl)
summary(Athl)
glimpse(Athl)

athldata<-select(Athl,Name,Sex, Age,Height,Weight,Team,Year,Season, City,Sport)
athldata
summary(athldata)
unique(athldata$Sport)

athl_2000<-athldata %>% filter(Year>2000)
ggplot(athl_2000,aes(x=athl_2000$Year,y=athl_2000$Age,color=athl_2000$Sex))+geom_jitter(alpha = 0.6)+
  labs(title = "Athletes' age an sex visualization ")
summary(athl_2000$Sex)

athldata$Sex <- as.factor(gsub("M", "1",Athl$Sex)) 
athldata$Sex <- as.factor(gsub("F", "0", Athl$Sex))


athl_f<-athldata %>% filter(Sex=="F")
athl_f_shoot<-athldata %>% filter(Sex=="F") %>% filter(Sport=="Shooting")
ggplot(athl_f, aes(x=athl_f$Weight,y=athl_f$Height, color=athl_f$Season))+geom_point()
ggplot(athl_f_shoot, aes(x=athl_f_shoot$Weight,y=athl_f_shoot$Height, color=athl_f_shoot$Year))+geom_point()

sum_metal<- read.csv(file = "Desktop/olympic-games/summer.csv")
head(sum_metal)  
summary(sum_metal)
glimpse(sum_metal)
unique(sum_metal$Sport)


sum_london<-sum_metal %>% filter(City=="London") %>% filter(Year==2012)
summary(sum_london)
sum_london_GBR<-sum_metal %>% filter(City=="London") %>% filter(Country=="GBR") %>% filter(Year==2012)
summary(sum_london_GBR)

ggplot(sum_london, aes(x=sum_london$Country,y=sum_london$Event,color=sum_london$Metal))+geom_point()
sum_london<-sum_metal %>% filter(City=="London") %>% filter(Gender=="Women")

summary(sum_metal$Gender)

nation<- read_csv(file = "Desktop/olympic-games/dictionary.csv")
head(nation)  
summary(nation)  
gdp<-nation %>% arrange(desc(GDPperCapita))
topgdp<-head(gdp,n=10)
topgdp
ggplot(topgdp,aes(x=topgdp$Country,y=topgdp$GDPperCapita))+geom_point() + labs(title = "Top Ten GDP per Capita ")

pop<-nation %>% arrange(desc(Population))
toppop<-head(pop,n=10)
toppop

ggplot(toppop,aes(x=toppop$Country,y=toppop$Population))+geom_point()  + labs(title = "Top Ten Populations ")


  

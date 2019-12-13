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

#Population

london<-read.csv(file = "Desktop/2012 Population.csv")
head(london)
summary(london)
unique(london$Country)
medaldata<-select(london,Country,Weighted_Medals, Population)
london_medal<-medaldata %>% arrange(desc(Weighted_Medals)) 
head(london_medal)
london_medal$Population<-as.integer(london_medal$Population)
ggplot(london_medal,aes(x=Population,y=Weighted_Medals))+geom_jitter(alpha = 0.6)+ stat_smooth(method = "lm", se=FALSE)+
  labs(title = "Population Vs Weighted Medals in 2012 London Olympics") + scale_y_continuous("Weighted Medals")+
  theme(panel.background = element_blank(),
        legend.background = element_blank(),
        legend.key = element_blank(),
        strip.background = element_blank(),
        axis.text = element_text(colour = "black"),
        axis.ticks = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        strip.text = element_blank(),
        panel.spacing = unit(1, "lines"))


lm_medalPOP<-lm(Weighted_Medals~Population,data=london_medal)
lm_medalPOP

summary(lm_medalPOP)
coef(lm_medalPOP)

#GDP

londonGDP<-read.csv(file = "Desktop/2012 GDP.csv")
head(londonGDP)
summary(londonGDP)

medaldata<-select(londonGDP,Country,Weighted_Medals,GDP...US.billion.)
london_medal<-medaldata %>% arrange(desc(Weighted_Medals))
head(london_medal)

ggplot(london_medal,aes(x=GDP...US.billion.,y=Weighted_Medals))+geom_jitter(alpha = 0.6)+stat_smooth(method = "lm", se=FALSE)+
  labs(title = "GDP Vs Weighted Medals in 2012 London Olympics") + scale_x_continuous("GDP ($US Dollar)")+scale_y_continuous("Weighted Medals")+
  theme(panel.background = element_blank(),
        legend.background = element_blank(),
        legend.key = element_blank(),
        strip.background = element_blank(),
        axis.text = element_text(colour = "black"),
        axis.ticks = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        strip.text = element_blank(),
        panel.spacing = unit(1, "lines"))
lm_medalGDP<-lm(Weighted_Medals~GDP...US.billion.,data=london_medal)
lm_medalGDP
  
summary(lm_medalGDP)
coef(lm_medalGDP)


#Host City Advantage

GB<-read.csv(file = "Desktop/Great Britain.csv")
head(GB)
summary(GB)

GB1996<-GB%>%filter(Year>="1996")
ggplot(GB1996,aes(x=Year,y=GB.Total))+geom_point()+ stat_smooth(method = "lm", se=FALSE)+
  labs(title = "Olympics Performance of Great Britain Between 1996 and 2016") + scale_y_continuous("Weighted Medals of GB/Total Weighted Medals")
lm_GB1996<-lm(GB.Total~Year,data=GB1996)
lm_GB1996
new_GB2012 <- data.frame("Year" = 2012)
predict(lm_GB1996, newdata=new_GB2012)
summary(lm_GB1996)
coef(lm_GB1996)


GB1948<-GB%>%filter(Year>="1948")%>%filter(Year<="1992")
ggplot(GB1948,aes(x=Year,y=GB.Total))+geom_point()+ stat_smooth(method = "lm", se=FALSE)+
  labs(title = "Olympics Performance of Great Britain Between 1948 and 1992") + scale_y_continuous("Weighted Medals of GB/Total Weighted Medals")
lm_GB1948<-lm(GB.Total~Year,data=GB1948)
lm_GB1948
new_GB1948 <- data.frame("Year" = 1948)
predict(lm_GB1948, newdata=new_GB1948)
summary(lm_GB1948)
coef(lm_GB1948)



GB1896<-GB%>%filter(Year>="1896")%>%filter(Year<="1936")
ggplot(GB1896,aes(x=Year,y=GB.Total))+geom_point()+ stat_smooth(method = "lm", se=FALSE)+
  labs(title = "Olympics Performance of Great Britain Between 1896 and 1936") + scale_y_continuous("Weighted Medals of GB/Total Weighted Medals")
lm_GB1896<-lm(GB.Total~Year,data=GB1896)
lm_GB1896
new_GB1908 <- data.frame("Year" = 1908)
predict(lm_GB1896, newdata=new_GB1908)
summary(lm_GB1896)
coef(lm_GB1896)


GB<-read.csv(file = "Desktop/Great_Britain.csv")
head(GB)
summary(GB)

ggplot(GB,aes(x=Year,y=Athletes))+geom_point()+ geom_line() +
  labs(title = "Athletes Number of Great Britain in Olympics") 

ggplot(GB,aes(x=Year,y=GB_Weighted_Medals..Total_Weighted_Medals ))+geom_point()+ geom_line() +
  labs(title = "Athletes Performance of Great Britain in Olympics") 


ggplot(GB,aes(x=Athletes,y=GB_Weighted_Medals..Total_Weighted_Medals ))+geom_point()+ stat_smooth(method="lm") +
  labs(title = "Athletes Performance Vs. Athletes Number of Great Britain in Olympics") +scale_y_continuous("Weighted Medals of GB/Total Weighted Medals")



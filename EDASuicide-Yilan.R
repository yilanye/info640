install.packages("lubridate")
install.packages("dplyr")
install.packages("ggthemes")

library(lubridate)
library(dplyr)
library(tidyverse)

scd<-read.csv("Desktop/suicide.csv",header=TRUE,check.names =FALSE)
class(scd)
names(scd)
str(scd)
glimpse(scd)
summary(scd) 
head(scd)

#SCD and POP in USA
scd_USA <-scd %>% filter(country=="United States of America")
head(scd_USA)
summary(scd_USA) 
ggplot(scd_USA,aes(x=year,y=suicides_no,color=age))+geom_jitter(alpha=.6)+expand_limits(y=0)+facet_grid(.~sex)+labs(title="Suicide Number in USA")
ggplot(scd_USA,aes(x=year,y=population,color=age))+geom_jitter(alpha=.6)+expand_limits(y=0)+facet_grid(.~sex)+labs(title="Population in USA")
sum(is.na(scd_USA))

#Suicide Number in USA by Age
ggplot(scd_USA,aes(x=age,y=suicides_no))+geom_jitter(alpha=.6)+expand_limits(y=0)+labs(title="Suicide Number in USA by Age")

#SCD/POP in USA
scd_USA_SpP<-scd_USA%>%mutate(ScdoverPop=suicides_no/population)
head(scd_USA_SpP)
ggplot(scd_USA_SpP,aes(x=age,y=ScdoverPop))+geom_jitter(alpha=.6)+expand_limits(y=0)+labs(title="Suicide Number/Population in USA by Age")


#Mid-Age in USA
scd_USA_mid<-scd %>% filter(country=="United States of America") %>% filter(age=="35-54 years")
head(scd_USA_mid)
sum(is.na(scd_USA_mid))
glimpse(scd_USA_mid)
tail(scd_USA_mid)
scd_USA_mid_f<-scd %>% filter(country=="United States of America") %>% filter(age=="35-54 years") %>% filter(sex=="female")
summary(scd_USA_mid_f)
scd_USA_mid_m<-scd %>% filter(country=="United States of America") %>% filter(age=="35-54 years") %>% filter(sex=="male")
summary(scd_USA_mid_m)


ggplot(scd_USA_mid,aes(x=year,y=suicides_no,color=sex))+geom_jitter(alpha=.6)+expand_limits(y=0)+labs(title="People Age of 35-54 Suicide Number in USA")
ggplot(scd_USA_mid,aes(x=year,y=population,color=sex))+geom_jitter(alpha=.6)+expand_limits(y=0)+labs(title="Population in USA")

#SCD and POP in Japan
scd_JP <- scd %>% filter(country=="Japan") 
summary(scd_JP) 
head(scd_JP)
head(scd_JP)%>%arrange(desc(suicides_no))

                       
ggplot(scd_JP,aes(x=year,y=suicides_no,color=age))+geom_jitter(alpha=.6)+expand_limits(y=0)+facet_grid(.~sex)+labs(title="Suicide Number in Japan")
ggplot(scd_JP,aes(x=year,y=population,color=age))+geom_jitter(alpha=.6)+expand_limits(y=0)+facet_grid(.~sex)+labs(title="Population in Japan")

#Suicide Number in USA by Age
head(scd_JP)
ggplot(scd_JP,aes(x=age,y=suicides_no))+geom_jitter(alpha=.6)+expand_limits(y=0)+labs(title="Suicide Number in Japan by Age")

#SCD/POP in Japan
scd_JP_SpP<-scd_JP%>%mutate(ScdoverPop=suicides_no/population)
head(scd_JP_SpP)
ggplot(scd_JP_SpP,aes(x=age,y=ScdoverPop))+geom_jitter(alpha=.6)+expand_limits(y=0)+labs(title="Suicide Number/Population in Japan by Age")


#Mid-Age in Japan
scd_JP_mid<-scd %>% filter(country=="Japan") %>% filter(age=="35-54 years")
head(scd_JP_mid)
ggplot(scd_JP_mid,aes(x=year,y=suicides_no,color=sex))+geom_jitter(alpha=.6)+expand_limits(y=0)+labs(title="People Age of 35-54 Suicide Number in Japan")
ggplot(scd_JP_mid,aes(x=year,y=population,color=sex))+geom_jitter(alpha=.6)+expand_limits(y=0)+labs(title="Population in Japan")
scd_JP_mid_f<-scd %>% filter(country=="Japan") %>% filter(age=="35-54 years") %>% filter(sex=="female")
summary(scd_JP_mid_f)
scd_JP_mid_m<-scd %>% filter(country=="Japan") %>% filter(age=="35-54 years") %>% filter(sex=="male")
summary(scd_JP_mid_m)



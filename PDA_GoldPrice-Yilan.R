library(lubridate)
library(dplyr)
library(tidyverse)
library(broom)

gld<-read.csv("../../2 Info 640 Data Analysis/PDA-Yilan/gld_price_data.csv",header=TRUE,check.names =FALSE)
head(gld)
tail(gld)

#Gold Price VS. USO
ggplot(gld,aes(x=USO, y=GLD))+
  geom_jitter(alpha=.6)+
  expand_limits(y=0)+
  stat_smooth(method = "lm",se=FALSE)+
  labs(title = "Gold Price VS. US Oil Price")

lm_gld<-lm(GLD~USO,data=gld)
summary(lm_gld)
new_gld <- data.frame("USO" = 60)
predict(lm_gld, newdata=new_gld)

mygld<- broom::augment(lm_gld, newdata=new_gld)
mygld


#Gold Price VS. S&P 500 Index 
ggplot(gld,aes(x=SPX, y=GLD))+
  geom_jitter(alpha=.6)+
  expand_limits(y=0)+
  stat_smooth(method = "lm",se=FALSE)+
  labs(title = "Gold Price VS. S&P 500 Index")


lm_gld<-lm(GLD~SPX,data=gld)
summary(lm_gld)
new_gld <- data.frame("SPX" = 2500)
predict(lm_gld, newdata=new_gld)

mygld<- broom::augment(lm_gld, newdata=new_gld)
mygld



#Gold Price VS. Silver Price
ggplot(gld,aes(x=SLV, y=GLD))+
  geom_jitter(alpha=.6)+
  expand_limits(y=0)+
  stat_smooth(method = "lm",se=FALSE)+
  labs(title = "Gold Price VS. Silver Price")

lm_gld<-lm(GLD~SLV,data=gld)
summary(lm_gld)
new_gld <- data.frame("SLV" = 40)
predict(lm_gld, newdata=new_gld)

mygld<- broom::augment(lm_gld, newdata=new_gld)
mygld

#Additional Code
#S&P 500 Index VS. EUR/USD
ggplot(gld,aes(x=EURperUSD, y=SPX))+geom_jitter(alpha=.6)+
  expand_limits(y=0)+
  stat_smooth(method = "lm",se=FALSE)+
  labs(title = "S&P 500 Index VS. EUR/USD")

ggplot(gld,aes(x=SPX, y=EURperUSD))+
  geom_jitter(alpha=.6)+
  expand_limits(y=0)+
  stat_smooth(method = "lm",se=FALSE)+
  labs(title = "EUR/USD VS. S&P 500 Index ")

#US Oil VS. EUR/USD
ggplot(gld,aes(x=EURperUSD, y=USO))+
  geom_jitter(alpha=.6)+
  expand_limits(y=0)+
  stat_smooth(method = "lm",se=FALSE)+
  labs(title = "US Oil VS. EUR/USD")

ggplot(gld,aes(x=USO, y=EURperUSD))+
  geom_jitter(alpha=.6)+
  expand_limits(y=0)+
  stat_smooth(method = "lm",se=FALSE)+
  labs(title = "EUR/USD VS. US Oil ")

#SLiver Price VS. EUR/USD
ggplot(gld,aes(x=EURperUSD, y=SLV))+
  geom_jitter(alpha=.6)+
  expand_limits(y=0)+
  stat_smooth(method = "lm",se=FALSE)+
  labs(title = "Silver Price VS. EUR/USD")

ggplot(gld,aes(x=SLV, y=EURperUSD))+
  geom_jitter(alpha=.6)+
  expand_limits(y=0)+
  stat_smooth(method = "lm",se=FALSE)+
  labs(title = "EUR/USD VS. Sliver Price ")


#Gold Price VS. EUR/USD
ggplot(gld,aes(x=EURperUSD, y=GLD))+
  geom_jitter(alpha=.6)+
  expand_limits(y=0)+
  stat_smooth(method = "lm",se=FALSE)+
  labs(title = "Gold Price VS. EUR/USD")

ggplot(gld,aes(x=GLD, y=EURperUSD))+
  geom_jitter(alpha=.6)+
  expand_limits(y=0)+
  stat_smooth(method = "lm",se=FALSE)+
  labs(title = "EUR/USD VS. Gold Price ")





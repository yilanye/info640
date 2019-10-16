install.packages("gmodels")
library(gmodels)  #gmodels has the ci function to calculate confidence interval of a normall
library(tidyverse)

#read.csv("../yilanye/Desktop/")
bodytemp <- rnorm(10000, mean=97.82, sd=.69) 
glimpse(bodytemp)
hist(bodytemp)

set.seed(1234) #setting the seed makes the "random" samples repeatable ... and the same ever
bodysample <- sample(bodytemp, 10) 
mean(bodysample)

bodysample <- sample(bodytemp, 100) 
mean(bodysample)


bodysample <- sample(bodytemp, 1000) 
mean(bodysample)

our_sample <- numeric(10000) 

 for(i in 1:10000)
  a_sample <- sample(bodytemp, 50) 
  our_sample[i] <- mean(a_sample)


hist(our_sample, breaks = 50)

temp_mean <- mean(bodytemp)
temp_stdev <- sd(bodytemp, na.rm = TRUE) 
sample_size = length(bodytemp)
temp_mean

error_n <- qnorm(0.975)*temp_stdev/sqrt(sample_size) 
left_n <- temp_mean - error_n
right_n <- temp_mean + error_n
error_t <- qt(0.975, df=sample_size-1) 
left_t <- temp_mean - error_t
right_t <- temp_mean + error_t

print(left_n) 
print(right_n)

print(left_t) 
print(right_t)

ci(bodytemp, confidence=0.95)


t.test(bodytemp, mu=temp_mean, conf.level = .95)

realtemps <- read.csv("Desktop/../Dropbox/2 Info 640 Data Analysis/info640Michelle/DataSets//Normtemp.csv", header = TRUE) 
glimpse(realtemps)

realtemps$Gender <- as.factor(realtemps$Gender)
summary(realtemps)
hist(realtemps$Body.Temp)

body_mean = mean(realtemps$Body.Temp) 
t.test(realtemps$Body.Temp, mu= body_mean, conf.level = .95)

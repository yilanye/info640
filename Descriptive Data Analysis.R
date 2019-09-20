install.packages("gapminder")

library(tidyverse)
library(dplyr)
library(gapminder)

summary(gapminder)
glimpse(gapminder)
head(gapminder)
tail(gapminder)

sum(is.na(gapminder))

gp_cnt_life <- select(gapminder, country, lifeExp)
head(gp_cnt_life)

gp_no_pop <- select(gapminder, -pop)
head(gp_no_pop)

gp_1957 <- gapminder %>% filter(year == 1957)
glimpse(gp_1957)
head(gp_1957, n=10)

gp_us <- gapminder %>% filter(country == "United States")
head(gp_us, n=15)

gp_1957_Asia <- gapminder %>% filter(year==1957, continent=="Asia")
head(gp_1957_Asia, 8)

write.csv(gp_1957_Asia, 'gapminder1957Asia.csv')

gapminder %>% arrange(pop)
gapminder %>% arrange(desc(pop))

gapminder %>% filter(year == 1957) %>% arrange(desc(pop))

gapminder %>% mutate(popMil = pop/1000000)

gapminder %>% mutate(gdp = gdpPercap* pop)

gapminder %>%
  mutate(gdp = gdpPercap * pop) %>%
  filter(year == 1957) %>%
  arrange(desc(gdp))

head(gapminder)

gap_gdp_1957 <- gapminder %>%
  mutate(gdp = gdpPercap * pop) %>%
  filter(year == 1957) %>%
  arrange(desc(gdp))

gapminder %>%
  summarize(meanLifeExp = mean(lifeExp))

gapminder %>%
  filter(year==1957) %>%
  summarize(meanLifeExp = mean(lifeExp))

gapminder %>%
  filter(year==2007) %>%
  summarize(meanLifeExp = mean(lifeExp))


gapminder %>%
  filter(year==1957) %>%
  summarize(meanLifeExp = mean(lifeExp),
            totalPop = sum(as.numeric(pop)))

gapminder %>%
  filter(year==2007) %>%
  summarize(meanLifeExp = mean(lifeExp),
            totalPop = sum(as.numeric(pop)))

gapminder %>%
  group_by(year) %>%
  summarize(meanLifeExp = mean(lifeExp),
            totalPop = sum(as.numeric(pop)))
gapminder %>%
  filter(year==2002) %>%
  filter(country=="United States") %>%
  summarize(meanLifeExp = mean(lifeExp),
            totalGdp = sum(as.numeric(gdpPercap)))

gapminder %>%
  group_by(continent, year) %>%
  summarize(meanLifeExp = mean(lifeExp),
            totalPop = sum(as.numeric(pop)))

gapminder %>%
  group_by(country, year) %>%
  summarize(meanGdp= mean(gdpPercap))

by_year <- gapminder %>%
  group_by(year) %>%
  summarize(totalPop = sum(as.numeric(pop)),
            meanLifeExp = mean(lifeExp))

head(by_year)

ggplot(by_year, aes(x = year, y = totalPop)) +
  geom_point()

ggplot(by_year, aes(x = year, y = totalPop)) +
  geom_point()+
  expand_limits(y=0)


by_year_continent <- gapminder %>%
  group_by(year, continent) %>%
  summarize(totalPop = sum(as.numeric(pop)),
            meanLifeExp = mean(lifeExp))

head(by_year_continent)

ggplot(by_year_continent, aes(x = year, y = totalPop, color = continent)) +
  geom_point() +
  expand_limits(y = 0)

by_year_continent<- gapminder %>%
  group_by(year, continent) %>%
  summarize(meanGdpPerCap = mean((gdpPercap)))
            

head(by_year_continent)

ggplot(by_year_continent, aes(x = year, y = meanGdpPerCap, color = continent)) +
  geom_point() +
  expand_limits(y = 0)

#1
summary(gapminder)

#2
gap2007 <- gapminder %>% filter(year=="2007")
gap2007[which.min(gap2007$lifeExp),]
gap2007[which.min(gap2007$pop),]
gap2007[which.min(gap2007$gdpPercap),]
gap2007[which.max(gap2007$lifeExp),]
gap2007[which.max(gap2007$pop),]
gap2007[which.max(gap2007$gdpPercap),]

#3
start_year <- min(gapminder['year'])
end_year <- max(gapminder['year'])
start_pop <- min(gapminder['pop'])
end_pop <- max(gapminder['pop'])
pop_growth_rate <- (end_pop-start_pop)/(end_year-start_year)
pop_growth_rate


#4
gap_grouped <- gapminder %>%
  group_by(continent, year) %>%
  summarize(meanLifeExp = mean(lifeExp),
            totalPop = sum(as.numeric(pop)),
            meanGdpPercap = mean(gdpPercap))
summary(gap_grouped)

#4
ggplot(gap_grouped, aes(x = year, y = meanLifeExp, color=continent)) +
  geom_line()
ggplot(gap_grouped, aes(x = year, y = totalPop, color=continent)) +
  geom_line()
ggplot(gap_grouped, aes(x = year, y = meanGdpPercap, color=continent)) +
  geom_line()






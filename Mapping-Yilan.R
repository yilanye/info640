update.packages()
install.packages("tidyverse")
install.packages("ggmap") 
install.packages("maps") 
install.packages("mapdata") 
install.packages("sf") 
install.packages("sp") 
install.packages("ggthemes") 
install.packages("rgdal")
install.packages("jpeg")
install.packages("tigris") 
install.packages("tmap") 
install.packages("leaflet") 
install.packages("textdata")
install.packages("topicmodels")
install.packages("NLP")

library(tidyverse)
library(lubridate)
library(leaflet)
library(sp)
library(ggthemes)
library(maps)
library(mapdata)
library(sf)
library(topicmodels)

library(rgdal)
library(ggmap)
library(tmap)
library(tigris)
library(stringr)
library(tm)

#Ggmaps
register_google(key ="AIzaSyAl9iTzyLBV-koT1-NAB9TOFsCSMym_bCU",write=TRUE)

nyc <- c(lon = -73.950, lat = 40.6971)
nyc_map <- get_map(location = nyc, zoom = 10, scale=1) 
ggmap(nyc_map)

nyc_map <- get_map(location = nyc, zoom =12, scale=3) 
ggmap(nyc_map)


noise_311 <- read.csv("Dropbox/2 Info 640 Data Analysis/info640Michelle/DataSets/311_Service_Requests_from_2010_to_present.csv" )
glimpse(noise_311)
dim(noise_311)
not_all_na <- function(x) any(!is.na(x))
noise_311_clean <- noise_311 %>% select_if(not_all_na)
glimpse(noise_311_clean)

noise_311_clean$Created.Date <- mdy_hms(noise_311_clean$Created.Date) 
noise_311_clean$Closed.Date <- mdy_hms(noise_311_clean$Closed.Date) 
glimpse(noise_311_clean)
ggmap(nyc_map) + geom_point(aes(Longitude, Latitude), data = noise_311_clean)

unique(noise_311_clean$Descriptor)
as.character(noise_311_clean$Descriptor)

icecream_311 <- noise_311_clean %>% filter(str_detect(Descriptor, "Ice Cream"))
dim(icecream_311) 
unique(icecream_311$Descriptor) 
glimpse(icecream_311)


ggmap(nyc_map) + geom_point(aes(Longitude, Latitude), data = icecream_311)

nyc_map <- get_map(location = nyc, zoom = 11, scale=2) 
ggmap(nyc_map) + geom_point(aes(Longitude, Latitude), data = icecream_311)


ggmap(nyc_map) + geom_point(aes(Longitude, Latitude, color=Address.Type), data = icecream_311)

ggmap(nyc_map, base_layer = ggplot(icecream_311, aes(Longitude, Latitude))) + 
  geom_point(aes( color=Address.Type), data = icecream_311)

ggmap(nyc_map, base_layer = ggplot(icecream_311, aes(Longitude, Latitude))) + 
  geom_point(aes( color=Address.Type), data = icecream_311) +
  facet_wrap(~Address.Type)


ggmap(nyc_map, base_layer = ggplot(icecream_311, aes(Longitude, Latitude))) + 
  geom_point(aes( color= Borough), data = icecream_311) +
  facet_wrap(~Address.Type) +
  theme_void()+
  theme(legend.position = "bottom", plot.title = element_text(hjust = .5))+
  labs(title = "Ice Cream Noise complaints 2017 by address type", caption = "Source: NYC Open Data")

leaflet()%>%
  addTiles()%>%
  setView (-74.00,40.71,zoom = 12)

#Thematic Maps
nyc_counties<- c("New York", "Kings","Queens","Richmond","Bronx")
nyc_tract <-tracts(state = "NY", nyc_counties, cb=TRUE)

summary(nyc_tract)

plot(nyc_tract)

head(nyc_tract,n=1)

proj4string(nyc_tract)

income <- read.csv("Dropbox/2 Info 640 Data Analysis/info640Michelle/DataSets/medianhouseholdincomecensustract.csv", header = TRUE)

glimpse(income) 
glimpse(nyc_tract)

any(duplicated(income$TRACTCE10))
any(duplicated(income$GEOID10))

nyc_tracts_merge <- sp::merge(nyc_tract, income, by.x = "GEOID", by.y = "GEOID10")
glimpse(nyc_tracts_merge)

tm_shape(nyc_tracts_merge) + tm_fill(col = "MHI")

nyc_water <- area_water("NY", nyc_counties)

tm_shape(nyc_tracts_merge) +
  tm_fill(col = "MHI", title="Median Household Income") + 
  tm_shape(nyc_water)+
  tm_fill(col = "grey90")

nta <- readOGR("Dropbox/2 Info 640 Data Analysis/info640Michelle/DataSets/Neighborhood Tabulation Areas-20191108/geo_export_482dad1a-b1a1-4d72-89d2-86f4a6b590e5.shp")

proj4string(nta)
nta <- spTransform(nta,proj4string(nyc_tract))

tm_shape(nyc_tracts_merge) + tm_fill(col = "MHI") + tm_shape(nyc_water)+ tm_fill(col = "grey90") + tm_shape(nta) + tm_borders()

tm_shape(nyc_tracts_merge) +
  tm_fill(col = "MHI", title="Median Income NYC", palette = "Reds")+ tm_shape(nyc_water)+
  tm_fill(col = "grey90") +
  tm_shape(nta) +
  tm_borders(col= "grey30", lwd=2) +
  tm_credits("Source: 2010 Census, 10 year Estimates")

save_tmap(width=6, heigth=10)

#Exercise
tm_shape(nyc_tracts_merge) + tm_fill(col = "energy_cos")

nyc_water <- area_water("NY", nyc_counties)

tm_shape(nyc_tracts_merge) +
  tm_fill(col = "energy_cos", title="Energy Cost") + 
  tm_shape(nyc_water)+
  tm_fill(col = "grey90")

tm_shape(nyc_tracts_merge) + tm_fill(col = "energy_cos") + tm_shape(nyc_water)+ tm_fill(col = "grey90") + tm_shape(nta) + tm_borders()

tm_shape(nyc_tracts_merge) +
  tm_fill(col = "energy_cos", title="Energy Cost NYC", palette = "Reds")+ tm_shape(nyc_water)+
  tm_fill(col = "grey90") +
  tm_shape(nta) +
  tm_borders(col= "grey30", lwd=2) +
  tm_credits("Source: 2010 Census, 10 year Estimates")

#Interactive Maps
leaflet(nta) %>%
  addTiles() %>%
  addPolygons(popup = ~ntaname) %>% addProviderTiles("CartoDB.Positron")

leaflet(nta) %>%
  addTiles() %>%
  addPolygons(popup = ~ntaname) %>%
  addMarkers(~Longitude, ~Latitude, data = icecream_311) %>% addProviderTiles("CartoDB.Positron") %>%
  setView(-73.98, 40.75, zoom = 13)






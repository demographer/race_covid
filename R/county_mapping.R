##Create maps of counties observed in mid-May and in mid-June
##The files "my_counties[date]" are from "my_county" values in the estimation & figures R scripts


library(tidyverse)
library(urbnmapr)

#load files to merge
my_counties_may9 <- read_excel("C:/Users/Serge/Google Drive/demog/pnas/source_data/observed_counties_may_9.xlsx", skip = 1)

my_counties_june13 <- read_excel("C:/Users/Serge/Google Drive/demog/pnas/source_data/observed_counties_jun_13.xlsx", skip = 1)


ggplot() + 
  geom_polygon(data = urbnmapr::states, mapping = aes(x = long, y = lat, group = group), fill = "grey", color = "white") +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45)
household_data <- left_join(countydata, counties, by = "county_fips") 


countymap1 <- semi_join(counties, my_counties_may9, by = "county_fips" )
countymap1$color <- 1
countymap1 %>%
  ggplot(aes(long, lat, group = group, fill = color)) +
  geom_polygon(color = NA) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  labs(fill = " ")


countymap2 <- semi_join(counties, my_counties_june13, by = "county_fips" )
countymap2$color <- 1
countymap2 %>%
  ggplot(aes(long, lat, group = group, fill = color)) +
  geom_polygon(color = NA) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  labs(fill = " ")

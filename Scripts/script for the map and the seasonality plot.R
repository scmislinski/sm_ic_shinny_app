
library(tidyverse)
library(here)
library(tmap)
library(broom)
library(sf)
library(janitor)
library(ggplot2)
library(patchwork)
library(lubridate)
library(tsibble)
library(feasts)
library(fable)



fires_all_sf <- read_sf(here("data","2022_1999_Hawaii_large_Fire_Perimeters_UH_NREM", "fires_1999_2022.shp")) %>%
  clean_names()

fires_tmap_sf <- fires_all_sf %>%
  select(year, fire_month, day, area_ha, county, island, geometry)

#st_write(fires_tmap_sf, here('data/fires_tmap_sf.shp'))

##################Tmap Need to figure out how to select specific years
tmap_mode(mode = "view")
tm_shape(fires_tmap_sf) +
  tm_fill("area_ha", palette = "OrRd") +
  tm_layout(title = "Fires by Area for the Islands of Hawai'i", title.size = 1)




################# Seasonality plot by month
#having issues with the date
fires_df <- st_drop_geometry(fires_all_sf)


fires_ts <- fires_df %>%
  mutate(date = lubridate :: ymd(yyyymmdd)) %>%
  as_tsibble(key = NULL,
             index = date) %>%
  group_by(fire_month)


  gg_season(y = area, pal = hcl.colors(n = 9)) +
  theme_light()+
  labs(x= 'Year', y= 'area_ha') +
  ggtitle("Seasonal Patterns Fires in Hawai'i")










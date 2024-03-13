################# Code and Graphs for Tab 2 Fires Map

#libaries

library(tidyverse)
library(here)
library(patchwork)

annual_fires <- read_csv(here("data", "Hawaii_Annual_Area_Burned_1904_2022_Trauernicht.csv"))


fires_max_area_gg <- ggplot(annual_fires, aes(x = year, y = max_area_acres)) +
  geom_col() +
  labs(title = "Fires in Hawai'i from 1904 to 2022",
       x = "Year",
       y = "Area Burned (Acres)") +
  theme_classic()
fires_max_area_gg

fires_percentage_gg <- ggplot(annual_fires, aes(x = year, y = percent_total_land_area)) +
  geom_col(fill = "darkred") +
  labs(title = "Precent of Land Burned in Hawai'i from 1904 to 2022",
       x = "Year",
       y = "Percent of Land Burned") +
  theme_classic()
fires_percentage_gg


ggsave(here("Plots", "Precent_land.png"), fires_percentage_gg, width=6,
       height=4, units="in", dpi=800)


combined_graphs <- (fires_max_area_gg + fires_percentage_gg)
combined_graphs


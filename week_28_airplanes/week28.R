#---------------------------------------------------------------------------#
# Nom : week28_airplanes.r                                                  #
# Description : Week 28 of Tidy Tuesday challenge                           #
# Auteur : Pietro Violo                                                     #
#---------------------------------------------------------------------------#

rm(list=ls(all=TRUE))
options(scipen=999)

# Library
library(tidyverse)
library(data.table)
library(tidytuesdayR)
library(ggforce)
require(hrbrthemes)
library(ggshadow)
library(viridis)



this_week <- tt_load(2022, week = 28)

df <- this_week$flights

df <- as.data.table(df)

airports <- fread("GlobalAirportDatabase.txt", sep = ":")

colnames(airports) <- c("APT_ICAO",
                        "IATA Code",
                        "APT_NAME",
                        "City/Town",
                        "STATE_NAME",
                        "Lat Deg",
                        "Lat Min",
                        "Lat Sec",
                        "Lat Dir",
                        "Long Deg",
                        "Long Min",
                        "Long Sec",
                        "Long Dir",
                        "Altitude",
                        "lat",
                        "lon"
                        )

# Number of flights in 2022, FLT_TOT_1 is the number of total flights departures is FLT_DEP_1

euro_2021 <- df[YEAR==2021,
       .(nflights = sum(FLT_DEP_1)),
         by = .(APT_ICAO)]

euro_2021 %>% pull(lat) %>% min()

# Left join with coordinates

euro_2021 <- left_join(euro_2021, airports, by = "APT_ICAO") %>% 
  filter(APT_NAME != "N/A")


# Graph
quantile(euro_2021$nflights)

euro_2021 <- euro_2021 %>% mutate(quantile = case_when(nflights < 2000 ~ "a Bottom 25%",
                                          nflights > 2000 & nflights < 5700 ~ "b 25-50%",
                                          nflights > 5700 & nflights < 19400 ~ "c 50-75%",
                                          nflights > 19400 ~ "d Top 25%"))

png("europe_airports.png", res = 300, width = 5000, height = 5000)

  ggplot() +
  borders("world", xlim = c(-25, 50), 
          ylim = c(25, 70), size = 0.05, 
          fill = "black") +
  geom_point(data = euro_2021, aes(lon,lat, color = quantile, size = quantile)) +
  geom_voronoi_segment(data = euro_2021, 
                       aes(lon, lat),
                       color = "white",
                       size = 0.1) +
  labs(
    title = "Airports in Europe"
  ) +
  theme_ipsum(base_family = "LM Roman 10") +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank()
  ) +
  coord_map(
    projection = "gilbert", 
    xlim = c(-25, 50), 
    ylim = c(25, 70)
  ) +
    scale_color_viridis_d(option = "magma")
  
dev.off()


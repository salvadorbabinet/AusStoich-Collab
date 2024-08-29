library(tidyverse)
library(dplyr)
library(ggplot2)

aus_data

#------------------------Environmental Data Variation---------------------------
summary(aus_data) # look at quartiles, where most data lies


australia_map <- map_data("world", region = "Australia")
australia_map <- geom_polygon(data = australia_map, aes(x = long, y = lat, group = group),
                              fill = "lightgray", color = "black")


env_data <- aus_data %>% select(
  lat_deg, long_deg,
  SN_total_0_30, SP_total_0_30, SOC_total_0_30,
  CEC_total_0_30, AP_total_0_30,
  NPP, MAT, PPT, AET,
  precipitation_seasonality, temp_seasonality) %>%
  #long format easier for facet wrapping
  pivot_longer(
    cols = -c(lat_deg, long_deg), names_to = "variable", values_to = "value")


#total observations on map
ggplot() + 
  australia_map +
  geom_point(data = aus_data, aes(x = long_deg, y = lat_deg)) +
  theme_minimal() +
  labs(title = "Row Observations")


#env variables on map
ggplot() + australia_map +
  geom_point(data = env_data,
             aes(x = long_deg, y = lat_deg,
                 size = value,
                 color = variable), alpha = 0.7) +
  facet_wrap(~variable, scales = "free") +
  theme_minimal() +
  theme(legend.position = "right")
#not really comparable


#individual env variables mapped + visualized

#Options:
#SN_total_0_30, SP_total_0_30, SOC_total_0_30,
#CEC_total_0_30, AP_total_0_30,
#NPP, MAT, PPT, AET,
#precipitation_seasonality, temp_seasonality

ggplot() + australia_map +
  geom_point(data = env_data %>% filter(variable == "SN_total_0_30"),
             aes(x = long_deg, y = lat_deg,
                 size = value,
                 color = variable), alpha = 0.3) +
  theme_minimal() +
  theme(legend.position = "right")

#how are our values spread?
ggplot(data = aus_data, mapping = aes(x = temp_seasonality)) +
  geom_bar(fill = "salmon", width = 0.5) + 
  theme_minimal()

ggplot(data = aus_data, mapping = aes(x = temp_seasonality)) +
  geom_histogram(fill = "salmon") + 
  theme_minimal()

#--------------------------------Species Frequency-----------------------------------
species <- as.data.frame(table(aus_data$species_binom))

#basic
ggplot(data = aus_data, mapping = aes(x = species_binom)) +
  geom_bar() +
  theme_minimal()

#how many of each species
ggplot(data = species, mapping = aes(x = reorder(Var1, -Freq),
                                     y = Freq)) +
  geom_col() +
  labs(x = "Species") +
  coord_flip() +
  theme_minimal()
  
ggplot(data = subset(species, Freq > 30),
       mapping = aes(x = reorder(Var1, -Freq), y = Freq,
                     fill = Var1)) +
  geom_col() +
  scale_fill_discrete() +
  labs(x = "Species") +
  coord_flip() +
  theme_minimal() 

species_geo <- tibble(
  species_binom = aus_data$species_binom,
  lat = aus_data$lat_deg,
  long = aus_data$long_deg) %>%
  group_by(species_binom) %>%
  mutate(frequency = n()) %>%
  ungroup() %>%
  select(species_binom, lat, long, frequency)

ggplot() + australia_map +
  geom_point(data = subset(species_geo, frequency > 50), mapping = aes(x = long, y = lat,
             color = species_binom), size = 2) +
  scale_color_discrete() +
  theme_minimal() 

#--------------------------------Missing Data-----------------------------------






            
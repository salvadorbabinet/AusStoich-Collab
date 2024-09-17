library(tidyverse)
library(tidyr)
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
species <- as.data.frame(table(aus_data$species_binom))  %>%
  arrange(desc(Freq))

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


#---------genus
genus <- as.data.frame(table(aus_data$genus)) %>%
  arrange(desc(Freq))

ggplot(data = genus, mapping = aes(x = reorder(Var1, -Freq),
                                     y = Freq)) +
  geom_col() +
  labs(x = "Genus") +
  coord_flip() +
  theme_minimal()

ggplot(data = subset(genus, Freq > 60),
       mapping = aes(x = reorder(Var1, -Freq), y = Freq,
                     fill = Var1)) +
  geom_col() +
  scale_fill_discrete() +
  labs(x = "Genus") +
  coord_flip() +
  theme_minimal() 


#--------family
family <- as.data.frame(table(aus_data$family)) %>%
  arrange(desc(Freq))


ggplot(data = family, mapping = aes(x = reorder(Var1, -Freq),
                                   y = Freq)) +
  geom_col() +
  labs(x = "Family") +
  coord_flip() +
  theme_minimal()


#--------------------------------Missing Data-----------------------------------

#NAs stored as string for visualization purposes
na_data <- aus_data %>%
  mutate(across(c(leaf_P_per_dry_mass,leaf_C_per_dry_mass,
                  NP_ratio, CN_ratio, CP_ratio), as.character)) %>%
  
  replace_na(list(leaf_N_per_dry_mass= "NA", leaf_P_per_dry_mass = "NA",
                  leaf_C_per_dry_mass = "NA",NP_ratio = "NA",
                  CN_ratio = "NA", CP_ratio = "NA"))
#entire columns stored as characters

na_data <- na_data %>%
  filter(leaf_P_per_dry_mass == "NA" | leaf_C_per_dry_mass == "NA" |
           NP_ratio == "NA" | CN_ratio == "NA" | CP_ratio == "NA")
#6508 entries
#meaning that 83% of rows have at least one leaf nutrient NA


ggplot() +
  australia_map +
  geom_point(data = na_data, aes(x = long_deg, y = lat_deg)) +
  theme_minimal()+
  labs(title = "Missing Data")
#missing data is spread evenly across Australia


#---------families and genera with missing data
ggplot(data = na_data, mapping = aes(x = family)) +
  geom_bar(fill = "darkgreen") + 
  theme_minimal() +
  coord_flip() + 
  theme(axis.text.y = element_text(size = 6)) +
  labs(title = "Missing Data")

ggplot(data = na_data, mapping = aes(x = reorder(family, -after_stat(count)))) +
  geom_bar(fill = "darkgreen") + 
  theme_minimal() +
  coord_flip() + 
  theme(axis.text.y = element_text(size = 6)) +
  labs(title = "Missing Data")

ggplot(data = na_data, mapping = aes(x = reorder(family, -after_stat(count)))) +
  geom_bar(fill = "darkgreen") + 
  theme_minimal() +
  coord_flip() + 
  theme(axis.text.y = element_text(size = 6)) +
  labs(title = "Missing Data")

ggplot(data = na_data, mapping = aes(x = genus)) +
  geom_bar(fill = "darkgreen") + 
  theme_minimal() +
  coord_flip() + 
  theme(axis.text.y = element_text(size = 2)) +
  labs(title = "Missing Data")


#do species frequency stuff again here
length(unique(na_data$family)) #120
length(unique(na_data$genus)) #395
length(unique(na_data$species_binom)) #1337

#--------counts of missing data
na_species <- as.data.frame(table(na_data$species_binom))
na_species <- na_species %>%
  arrange(desc(Freq))

na_genus <- as.data.frame(table(na_data$genus))
na_genus <- na_genus %>%
  arrange(desc(Freq))

na_family <- as.data.frame(table(na_data$family))%>%
  arrange(desc(Freq))


ggplot(data = subset(species, Freq > 30),
       mapping = aes(x = reorder(Var1, -Freq), y = Freq,
                     fill = Var1)) +
  geom_col() +
  scale_fill_discrete() +
  labs(x = "Species") +
  coord_flip() +
  theme_minimal() 


ggplot(data = subset(na_genus, Freq > 50), mapping = aes(x = reorder(Var1, -Freq),
                                   y = Freq, fill = Var1)) +
  geom_col() +
  scale_fill_discrete() +
  labs(x = "Genera with NAs") +
  coord_flip() +
  theme_minimal()

ggplot(data = subset(na_genus, Freq < 100), mapping = aes(x = reorder(Var1, -Freq),
                                                         y = Freq)) +
  geom_col() +
  labs(x = "Genera with NAs < 100") +
  coord_flip() +
  theme_minimal()

#everything before is just the presence of ONE NA in any leaf nutrient column
#from excel, 2083 rows have BOTH no C and no P values

two_na_data <- na_data %>%
  filter(leaf_C_per_dry_mass == "NA" & leaf_P_per_dry_mass == "NA")

#for either one or the other:
one_na_data <- na_data %>%
  filter(!(leaf_C_per_dry_mass == "NA" & leaf_P_per_dry_mass == "NA"))

ggplot() +
  australia_map +
  geom_point(data = two_na_data, aes(x = long_deg, y = lat_deg)) +
  theme_minimal()+
  labs(title = "Missing C and P values")

#------------Missing data densities

#base of all leaf N observations
ggplot(data = aus_data, mapping = aes(x = leaf_N_per_dry_mass)) +
      geom_density() +
      theme_minimal()

       
p + ggplot(data = na_data, mapping = aes(x = leaf_N_per_dry_mass)) +
  geom_density() #two seperate panels nvm

#try katies way, thank you katie :"))))))

#specify colors
color_palette <- c("NA rows" = "lightgray", "All data" = "lightgreen")

#density plot of missing leaf concentration data
#using (complete) leaf N to compare rows with NAs for P and C
ggplot() +
  geom_density(data = aus_data,
               mapping = aes(x = leaf_N_per_dry_mass,
                             fill = "All data", alpha = 0.5)) +
  geom_density(data = na_data,
               mapping = aes(x = leaf_N_per_dry_mass,
                             fill = "NA rows", alpha = 0.5)) +
  labs(title = "Leaf N observations with Missing Leaf P and/or Leaf C",
       x = "leaf N", y = "Density") +
  theme_minimal() +
  scale_fill_manual(values = color_palette, name = " ") +
  theme(legend.position = c(0.83, 0.86))
  

#Density plot of missing leaf data across environmental variables
#do for all environmental variables

#density of observations for environmental variable 

#base plots
ggplot() +
  geom_density(data = aus_data, 
               mapping = aes(x = SP_total_0_30)) +
  theme_minimal()



#Options:
#SN_total_0_30, SP_total_0_30, SOC_total_0_30,
#CEC_total_0_30, AP_total_0_30,
#NPP, MAT, PPT, AET,
#precipitation_seasonality, temp_seasonality

ggplot() +
  geom_density(data = aus_data,
               mapping = aes(x = SN_total_0_30,
                             fill = "All data", alpha = 0.5)) +
  geom_density(data = na_data,
               mapping = aes(x = SN_total_0_30,
                             fill = "NA rows", alpha = 0.5)) +
  labs(title = "Observations with Missing Leaf P and/or Leaf C",
       y = "Density") +
  theme_minimal() +
  scale_fill_manual(values = color_palette, name = " ") +
  theme(legend.position = c(0.83, 0.86))
library(rtry)
library(dplyr)
library(tidyr)
library(jsonlite)
library(curl)
library(data.table)

setwd("/Users/sofiaquijada/Desktop/")
getwd()

try_data <- rtry_import(
                    input ="TRY data AusStoich/leaf_data.txt",
                    separator = "\t",
                    encoding = "Latin-1",
                    quote = "",
                    showOverview = TRUE)

#gives nice summary, observation counts per species
trydata_summary <- rtry_explore(try_data, AccSpeciesName, TraitName, TraitID, sortBy = AccSpeciesID)

#include exclusively mature individuals, leaves, and non-experimental

# 327 - experimental. Remove all experimental entries 
try_data <- try_data %>% filter(DataID != 327)
View(filter(try_data, DataID == 327))

# 413 - maturity information 
age <- filter(try_data, DataID == 413)
#need to prune to exclusively mature individuals, by StdValueStr
#so include StdValueStr = mature, Adult
try_data <- try_data %>%
  filter(!(DataID == 413 & !StdValueStr %in% c("Adult", "mature")))

# 1444 - leaf development
View(filter(try_data, DataID == 1444))
#only include mature leaves, by OrigValueStr
try_data <- try_data %>%
  filter(!(DataID == 1444 & !StdValueStr == "mature"))

# 1961 - leaf health
View(filter(try_data, DataID == 1961))
#they all seem healthy

trydata_pruned <- rtry_select_col(input = try_data, AccSpeciesName, ObservationID, ObsDataID,
               OrigUnitStr, OrigValueStr, TraitName, DataName, StdValue, UnitName, StdValueStr)

#only want lat longs + continent + region, and trait data in mg/g + ratios
#so keep only trait data in TraitName and columns in "DataName" that are latitude or long

keep <- c("Latitude", "Longitude", "Location Continent", "Location Region", 
          "Leaf nitrogen content per dry mass (Nmass)",
          "Leaf phosphorus content per dry mass (Pmass)",
          "Leaf carbon content per dry mass",
          "Leaf nitrogen/phosphorus (N/P) ratio", 
          "Leaf carbon/nitrogen (C/N) ratio",
          "Leaf carbon/phosphorus (C/P) ratio",
          "Location Country",
          "AccSpeciesName")


trydata_pruned <- filter(trydata_pruned, DataName%in%keep)
#ensure all plain trait data is in mg/g

#for dataname in leaf N, P or C, get rid of all entries whose units aren't in mg/g
trydata_pruned <- trydata_pruned %>% 
    filter(!(DataName %in% c("Leaf nitrogen content per dry mass (Nmass)",
                           "Leaf phosphorus content per dry mass (Pmass)",
                           "Leaf carbon content per dry mass") & UnitName != "mg/g"))


#for summary of data coverage prior to pivoting:
View(rtry_explore(trydata_pruned, AccSpeciesName, DataName, sortBy = DataName))

#only numerical values
pivoted <- pivot_wider(trydata_pruned, id_cols = ObservationID,
                      names_from = DataName, values_from = c(StdValue))

pivoted <- pivoted%>% select(!c("Location Country", "Location Region",
                                "Location Continent"))

#location info
metadata <- pivot_wider(trydata_pruned, id_cols = ObservationID,
    names_from = DataName, values_from = OrigValueStr)

metadata <- metadata %>% select("ObservationID", "Location Country", "Location Region",
                                "Location Continent")

#join both
trait_data <- left_join(pivoted, metadata, by = "ObservationID")


#rename trait columns
trait_data <- rename(.data = trait_data,
       leaf_N = `Leaf nitrogen content per dry mass (Nmass)`,
       leaf_P = `Leaf phosphorus content per dry mass (Pmass)`,
       leaf_C = `Leaf carbon content per dry mass`,
       NP_ratio = `Leaf nitrogen/phosphorus (N/P) ratio`, 
       CN_ratio = `Leaf carbon/nitrogen (C/N) ratio`,
       CP_ratio = `Leaf carbon/phosphorus (C/P) ratio`, 
       continent = `Location Continent`,
       region = `Location Region`,
       country = `Location Country`)

#remove blankspaces
trait_data[trait_data == ""] <- NA 

#rename continents properly
View(as.data.frame(unique(trait_data$continent)))

trait_data <- trait_data %>% 
  mutate(continent = recode(continent,
                            "Aus"= "Australia",
                            "E" = "Europe",
                            "NAm" = "North America",
                            "SAm" = "South America",
                            "Asi" = "Asia",
                            "Af" = "Africa",
                            "Pac" = "Pacific",
                            "Afr" = "Africa",
                            "Eur" = "Europe",
                            "Austrtalia" = "Australia"))


#calculate stoich ratios whenever possible!!
#have to be careful when values are smaller than 1
#due to division

#i wonder how they calculated it if they're smaller than one
#we didnt do this... to do.... 

#----- plots!!!

#do a boxplot by continent
length(unique(trait_data$continent))
length(unique(trait_data$country))

ggplot(trait_data, aes(x = continent, y = CP_ratio)) +
  geom_boxplot() +
  theme_classic()


#ok now map
library(maps)

world_map <- map_data("world")
ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group),
               fill = "white", color = "black") + theme_minimal() +
  geom_point(data = pivoted, aes(x = Longitude, y = Latitude), color = "darkgreen", size = 0.2) +
  labs(title = "trait obs", x = "Longitude", y = "Latitude")
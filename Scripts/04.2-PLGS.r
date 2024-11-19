library(ape)
library(caper)
library(dplyr)
library(tibble)
library(here)
library(ggtree)

# PLGS regressions
# Need tree and associated subset of aus_data, follow steps.
# Following code is for entire aus_data

#1. Read in tree + remove internal node labels.

phylo_tree <- read.tree(here("Inputs/Trees/ausdata.tre"))

# To inspect internal nodes:
View(as.data.frame(phylo_tree$node.label))

#Note: w/o removal of internal node labels, "tips and labels" won't
#match when creating comparative data object
phylo_tree$node.label <- NULL
print(phylo_tree$Nnode) 
View(as.data.frame(phylo_tree$node.label))
View(phylo_tree) #Node num should be > 0


# 2. Get average trait data with associated variation metrics matched with tree information.

#Only interested in nutrient columns
aus_data <- select_relevant_columns(aus_data)

#Add variation metrics, then get average per species.
#Force to dataframe as rownames can't be set to tibbles
avg_var_ausdata <- row.names(as.data.frame(average_nutrient_data(add_variation(aus_data))))

#Match order between tree tip labels and nutrient data.
ausdata_match <- avg_var_ausdata[match(phylo_tree$tip.label, row.names(avg_var_ausdata)), ]

# 3. Create a comparative data object.

comp_data <- comparative.data(phy = phylo_tree, data = ausdata_match, names.col = "species_binom")


# 4. Perform PGLS

#Simple linear regression 
P_pgls_model <- pgls(avg_leaf_P ~ SP_total_0_30, data = comp_data, lambda = "ML")
summary(P_pgls_model)

#Multiple linear regression
pgls_model <- pgls(avg_leaf_N ~ SN_total_0_30 + SP_total_0_30 + SOC_total_0_30 +
                CEC_total_0_30 + AP_total_0_30 + NPP + MAT + PPT + AET +
                precipitation_seasonality + temp_seasonality,
                   data = comp_data,
                   lambda = "ML")


#how to summarize outputs?
#how to interpret PLGS??? - what are the estimation parameters, does it matter much

#automate plotting to look at these all at once
#need to do diagnostic plots: 4 of them...
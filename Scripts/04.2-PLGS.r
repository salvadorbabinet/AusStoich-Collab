library(ape)
library(caper)
library(dplyr)
library(tibble)
library(here)
library(ggtree)
library(broom)
library(tidyr)
library(httpgd)
library(nlme)

hgd()
hgd_browse()


# PLGS regressions
# Need tree and associated subset of aus_data, follow steps.
# Following code is for entire aus_data

#1. Read in tree + remove internal node labels.

phylo_tree <- read.tree(here("Inputs/Trees/ausdata_all_pos_sp.tre"))


# To inspect internal nodes:
View(as.data.frame(phylo_tree$node.label))

#Note: w/o removal of internal node labels, "tips and labels" won't
#match when creating comparative data object
phylo_tree$node.label <- NULL
print(phylo_tree$Nnode) 
View(as.data.frame(phylo_tree$node.label))
View(phylo_tree) #Node num should be > 0


# 2. Get average trait data with associated variation metrics matched with tree information.

#Prune to cleaner selection, only include nutrient and env.
# TO-DO!!!

#Add variation metrics, then get average per species.
#Force to dataframe as rownames can't be set to tibbles
avg_var_ausdata <- average_nutrient_data(add_variation(aus_data))
avg_var_ausdata <- as.data.frame(avg_var_ausdata)
row.names(avg_var_ausdata) <- avg_var_ausdata$species_binom

#Match order between tree tip labels and nutrient data.
ausdata_match <- avg_var_ausdata[match(phylo_tree$tip.label, row.names(avg_var_ausdata)), ]

#Want comparable estimates for env/climate predictors
#Z-score them using base R scale()

# List of predictor columns to be z-scored
predictor_columns <- c("SN_total_0_30", "SP_total_0_30", "SOC_total_0_30",
                       "CEC_total_0_30", "AP_total_0_30", "NPP", "MAT", "PPT", "AET",
                       "precipitation_seasonality", "temp_seasonality")

# Z-score the predictor columns
ausdata_match[predictor_columns] <- scale(ausdata_match[predictor_columns])


# 3. Create a comparative data object.

comp_data <- comparative.data(phy = phylo_tree, data = ausdata_match, names.col = "species_binom")


# 4. Perform PGLS

#Simple phylo linear regression
pgls_model <- pgls(avg_leaf_N ~ SN_total_0_30, data = comp_data, lambda = "ML")
summary(pgls_model)


#Multiple linear regression
N_pgls_model <- pgls(log(avg_leaf_N) ~ SN_total_0_30 + SP_total_0_30 + SOC_total_0_30 +
                CEC_total_0_30 + AP_total_0_30 + NPP + MAT + PPT + AET +
                precipitation_seasonality + temp_seasonality,
                   data = comp_data,
                   lambda = "ML")
summary(N_pgls_model)
plot(N_pgls_model)
abline(a = 0, b = 1, col = "red", lty = 2)

P_pgls_model <- pgls(log(avg_leaf_P) ~ SN_total_0_30 + SP_total_0_30 + SOC_total_0_30 +
                CEC_total_0_30 + AP_total_0_30 + NPP + MAT + PPT + AET +
                precipitation_seasonality + temp_seasonality,
                   data = comp_data,
                   lambda = "ML")
summary(P_pgls_model)
plot(P_pgls_model)
abline(a = 0, b = 1, col = "red", lty = 2)


C_pgls_model <- pgls(log(avg_leaf_C) ~ SN_total_0_30 + SP_total_0_30 +   SOC_total_0_30 +
                CEC_total_0_30 + AP_total_0_30 + NPP + MAT + PPT + AET +
                precipitation_seasonality + temp_seasonality,
                   data = comp_data,
                   lambda = "ML")
summary(C_pgls_model)
plot(C_pgls_model)
abline(a = 0, b = 1, col = "red", lty = 2)


#Attempting to plot estimates

# Function to extract coefficients and statistics from a pgls model
extract_pgls <- function(model, model_name) {
  summary_model <- summary(model)
  coefs <- summary_model$coefficients
  data.frame(
    term = rownames(coefs),
    estimate = coefs[, "Estimate"],
    std.error = coefs[, "Std. Error"],
    statistic = coefs[, "t value"],
    p.value = coefs[, "Pr(>|t|)"],
    model = model_name,
    type = "PGLS"
  )
}

# Extract coefficients and statistics from each model
N_tidy <- extract_pgls(N_pgls_model, "N")
P_tidy <- extract_pgls(P_pgls_model, "P")
C_tidy <- extract_pgls(C_pgls_model, "C")

# Combine the extracted data into a single data frame
combined_tidy <- bind_rows(N_tidy, P_tidy, C_tidy)
#to parse through significant estimates or not
combined_tidy <- combined_tidy %>%
  mutate(significant = ifelse(p.value <= 0.05, "Significant", "Not Significant"))


# Plot all estimates
  ggplot(combined_tidy, aes(x = model, y = estimate, color = model)) +
  geom_point(size = 4, position = position_dodge(width = 0.5)) +
  facet_wrap(~ term, scales = "free_y") +
  theme_minimal() +
  labs(title = "Estimates of PGLS Models for log(N), log(P), and log(C)",
       x = "Model",
       y = "Estimate") +
  theme(legend.position = "right")


# Plot estimates by significance
  ggplot(combined_tidy, aes(x = model, y = estimate, color = model, shape = significant)) +
  geom_point(size = 4, position = position_dodge(width = 0.5)) +
  scale_shape_manual(values = c("Significant" = 16, "Not Significant" = 1)) +
  facet_wrap(~ term, scales = "free_y") +
  theme_minimal() +
  labs(title = "Estimates of PGLS Models for log(N), log(P), and log(C), p < 0.05",
       x = "Model",
       y = "Estimate",
       color = "Model",
       shape = "Significance") +
  theme(legend.position = "right")

#4 Diagnostic plots



# - Look at data, assess spread of variation metrics
#recall that goal was to set SE as variance for the model

avg_ausdata <- average_nutrient_data(add_variation(select_relevant_columns(aus_data)))
#may be too repetitive
#consider omitting (above)

ggplot(data = avg_ausdata) +
  geom_histogram(mapping = aes(x = sqrt(avg_leaf_C)), fill = "lightgreen") +
  theme_minimal()
#log transforming right skews leaf C

# Tree plots:
#horizontal base
all_pos_sp_plot <- ggtree(phylo_tree) + geom_tiplab(size = 0.5)

#most basic, no coloring, horizontal bar plot
all_pos_sp_plot + geom_facet(
  panel = 'Trait',
  data = avg_ausdata,
  geom = geom_col,
  mapping = aes(x = CV_C),
  orientation = "y") +
  ggtitle("") +
  theme(plot.title = element_text(size = 20))


# --------------- Now do same model without phylogeny: GLS

#GLS allows for errors that are correlated and not normally distributed
#following code is essentially the same as an OLS though

N_gls <- gls(log(avg_leaf_N) ~ SN_total_0_30 + SP_total_0_30 + SOC_total_0_30 +
                CEC_total_0_30 + AP_total_0_30 + NPP + MAT + PPT + AET +
                precipitation_seasonality + temp_seasonality, ausdata_match)
summary(N_gls)
plot(N_gls)

P_gls <- gls(log(avg_leaf_P) ~ SN_total_0_30 + SP_total_0_30 + SOC_total_0_30 +
                CEC_total_0_30 + AP_total_0_30 + NPP + MAT + PPT + AET +
                precipitation_seasonality + temp_seasonality, data = ausdata_match,
                na.action = na.omit)
summary(P_reg)
plot(P_gls)

C_gls <- gls(log(avg_leaf_C) ~ SN_total_0_30 + SP_total_0_30 + SOC_total_0_30 +
                CEC_total_0_30 + AP_total_0_30 + NPP + MAT + PPT + AET +
                precipitation_seasonality + temp_seasonality, data = ausdata_match,
                na.action = na.omit)
summary(C_reg)
plot(C_gls)

#tidy() can't get info from gls data
extract_gls <- function(model, model_name) {
  summary_model <- summary(model)
  coefs <- summary_model$tTable
  data.frame(
    term = rownames(coefs),
    estimate = coefs[, "Value"],
    std.error = coefs[, "Std.Error"],
    statistic = coefs[, "t-value"],
    p.value = coefs[, "p-value"],
    model = model_name,
    type = "GLS"
  )
}

#extract model outputs
N_tidy_gls <- extract_gls(N_gls, "N")
P_tidy_gls <- extract_gls(P_gls, "P")
C_tidy_gls <- extract_gls(C_gls, "C")

combined_gls <- bind_rows(N_tidy_gls, P_tidy_gls, C_tidy_gls)

combined_gls <- combined_gls %>%
  mutate(significant = ifelse(p.value <= 0.05, "Significant", "Not Significant"))

ggplot(combined_gls, aes(x = model, y = estimate, color = model, shape = significant)) +
  geom_point(size = 4, position = position_dodge(width = 0.5)) +
  scale_shape_manual(values = c("Significant" = 16, "Not Significant" = 1)) + 
  facet_wrap(~ term, scales = "free_y") +
  theme_minimal() +
  labs(title = "Estimates of GLS Models for log(N), log(P), and log(C), p < 0.05",
       x = "Model",
       y = "Estimate",
       color = "Model",
       shape = "Significance") +
  theme(legend.position = "right")


#check AIC of models
AIC_gls <- AIC(N_gls)
AIC_pgls <- AIC(N_pgls_model)


AIC_gls #N: 861
AIC_pgls #57.7

N_pgls_model$lambda







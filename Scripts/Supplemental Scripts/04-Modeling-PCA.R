# AusStoich Modeling
# Libraries & functions ----
library(here)
library(broom)
library(tidyverse)
theme_set(theme_bw())

library(httpgd)
hgd()
hgd_browse()

library(corrplot)
library(patchwork)

library(tidymodels)
tidymodels_prefer()
library(bestNormalize)
library(ggforce)

# Functions for simple regressions (on leaf N for now)
# tidymodels will streamline this eventually

# Create subset with predictor of interest & log-transformed outcome
# Only looking at leaf N so far, generalize to different outcomes?
prep_simple_data <- function(predictor, outcome, data = aus_data) {
  data |> select({{outcome}}, {{predictor}}) |>
  mutate(log_outcome = log({{outcome}})) |>
  filter(log_outcome > 0) # For now w/ weird low N, revisit this
}

# Plot raw vs log-transformed outcome to check linearity condition
check_linearity_condition <- function(predictor, outcome, data = simple_reg_data) {
  ggplot(data, aes(x = {{predictor}}, y = {{outcome}})) +
    geom_jitter(alpha = 0.5) +
    geom_smooth(se = F, color = "dodgerblue1") +
    geom_smooth(
      method = lm,
      color = "firebrick2",
      se = F,
      linetype = "dashed",
      linewidth = 1.5
    )
}

# Outputs linearity & residual normality / homoscedasticity visuals
# Outputs regression summary (coefficients, p, R2)
simple_regression_outputs <- function(predictor, outcome, data = simple_reg_data) {
  label <- rlang::englue("Predictor = {{predictor}}, outcome = log transformed {{outcome}}")

  h1 <- ggplot(data, aes(x = {{predictor}})) + geom_histogram()
  plot(h1)

  p1 <- check_linearity_condition({{predictor}}, {{outcome}})
  p2 <- check_linearity_condition({{predictor}}, log_outcome)
  plot(p1 + p2)

  r1 <- simple_reg |> augment() |> ggplot(aes(x = .resid)) + geom_histogram()
  r2 <- simple_reg |> augment() |>
    ggplot(aes(x = log_outcome, y = .resid)) +
    geom_point(alpha = 0.5)
  plot(r1 + r2 + plot_annotation(title = label))

  summary(simple_reg)
}

# PCA functions
plot_loadings <- function(component, data = pca_loadings) {
  label <- rlang::englue("Loadings plot for PC{{component}}")
  data |> filter(PC == component) |> 
    ggplot(aes(y = column, x = value)) +
    geom_col() +
    labs(title = label)
}


aus_data


# Simple regression: N ~ SN (sample process) ----
# Log transform outcome to meet regression conditions
simple_reg_data <- prep_simple_data(SN_total_0_30, leaf_N_per_dry_mass)
simple_reg_data

# Look at data
check_linearity_condition(SN_total_0_30, leaf_N_per_dry_mass)
ggplot(simple_reg_data, aes(x = SN_total_0_30)) + geom_histogram()
ggplot(simple_reg_data, aes(x = leaf_N_per_dry_mass)) + geom_histogram()

check_linearity_condition(SN_total_0_30, log_outcome)
ggplot(simple_reg_data, aes(x = log_outcome)) + geom_histogram()

# Residuals...
# With raw data (for reference)
simple_reg_raw <- lm(leaf_N_per_dry_mass ~ SN_total_0_30, simple_reg_data)
simple_reg_raw |> augment() |> ggplot(aes(x = .resid)) + geom_histogram()
simple_reg_raw |> augment() |>
  ggplot(aes(x = leaf_N_per_dry_mass, y = .resid)) +
  geom_point(alpha = 0.5)

summary(simple_reg_raw)

# With log-transformed data
simple_reg <- lm(log_outcome ~ SN_total_0_30, simple_reg_data)
simple_reg |> augment() |> ggplot(aes(x = .resid)) + geom_histogram()
simple_reg |> augment() |>
  ggplot(aes(x = log_outcome, y = .resid)) +
  geom_point(alpha = 0.5)

par(mfrow = c(2,2))
plot(simple_reg)

summary(simple_reg)


# Simple regressions w/ N outcome ----
# N ~ SN (soil nitrogen)
simple_reg_data <- prep_simple_data(SN_total_0_30, leaf_N_per_dry_mass)
simple_reg <- lm(log_outcome ~ SN_total_0_30, simple_reg_data)
simple_regression_outputs(SN_total_0_30, leaf_N_per_dry_mass)
par(mfrow = c(1,1))
plot(simple_reg)

# N ~ SOC (soil carbon)
simple_reg_data <- prep_simple_data(SOC_total_0_30, leaf_N_per_dry_mass)
simple_reg <- lm(log_outcome ~ SOC_total_0_30, simple_reg_data)
simple_regression_outputs(SOC_total_0_30, leaf_N_per_dry_mass)

# N ~ CEC (cation exchange)
simple_reg_data <- prep_simple_data(CEC_total_0_30, leaf_N_per_dry_mass)
simple_reg <- lm(log_outcome ~ CEC_total_0_30, simple_reg_data)
simple_regression_outputs(CEC_total_0_30, leaf_N_per_dry_mass)

# N ~ PPT (precipitation)
simple_reg_data <- prep_simple_data(PPT, leaf_N_per_dry_mass)
simple_reg <- lm(log_outcome ~ PPT, simple_reg_data)
simple_regression_outputs(PPT, leaf_N_per_dry_mass)

# N ~ MAT
simple_reg_data <- prep_simple_data(MAT, leaf_N_per_dry_mass)
simple_reg <- lm(log_outcome ~ MAT, simple_reg_data)
simple_regression_outputs(MAT, leaf_N_per_dry_mass)

# N ~ precipitation_seasonality
simple_reg_data <- prep_simple_data(precipitation_seasonality, leaf_N_per_dry_mass)
simple_reg <- lm(log_outcome ~ precipitation_seasonality, simple_reg_data)
simple_regression_outputs(precipitation_seasonality, leaf_N_per_dry_mass)


# Does removing single-sp. observations change things? ----
multiple_observed_species <- aus_data |> count(species_binom) |> filter(n > 2)
pruned_data <- aus_data |> filter(species_binom %in% multiple_observed_species$species_binom)

compare_pruning <- function(var1, var2, data1 = aus_data, data2 = pruned_data) {
  p1 <- ggplot(data1, aes(x = {{var1}})) + geom_histogram() + labs(title = "Full data")
  p2 <- ggplot(data2, aes(x = {{var1}})) + geom_histogram() + labs(title = "Pruned data")
  plot(p1 + p2)

  q1 <- ggplot(data1, aes(x = {{var2}})) + geom_histogram() + labs(title = "Full data")
  q2 <- ggplot(data2, aes(x = {{var2}})) + geom_histogram() + labs(title = "Pruned data")
  plot(q1 + q2)

  s1 <- ggplot(data1, aes(x = {{var2}}, y = {{var1}})) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = lm, se = FALSE) +
    labs(title = "Full data")
  s2 <- ggplot(data2, aes(x = {{var2}}, y = {{var1}})) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = lm, se = FALSE) +
    labs(title = "Pruned data")
  s1 + s2
}

compare_pruning(leaf_N_per_dry_mass, SN_total_0_30)

simple_reg_aus_data <- prep_simple_data(SN_total_0_30, leaf_N_per_dry_mass, aus_data)
simple_reg <- lm(log_outcome ~ SN_total_0_30, simple_reg_aus_data)
simple_regression_outputs(SN_total_0_30, leaf_N_per_dry_mass, simple_reg_aus_data)
plot(simple_reg)

simple_reg_pruned_data <- prep_simple_data(SN_total_0_30, leaf_N_per_dry_mass, pruned_data)
simple_reg <- lm(log_outcome ~ SN_total_0_30, simple_reg_pruned_data)
simple_regression_outputs(SN_total_0_30, leaf_N_per_dry_mass, simple_reg_pruned_data)
plot(simple_reg)


# PCA ----
# Center / scale only numeric variables, then run PCA
ggplot(aus_data, aes(x = SN_total_0_30)) + geom_histogram()

pca_data <- aus_data |> 
  select(SN_total_0_30:temp_seasonality) |>
  mutate(across(everything(), log)) |>
  scale()

ggplot(pca_data, aes(x = SN_total_0_30)) + geom_histogram()

aus_pca <- prcomp(pca_data)

# Look at outputs
print(aus_pca) # Loadings and eigens

plot(aus_pca) # Scree plot (variance captured per component)
pca_eigens <- tidy(aus_pca, "pcs") # Probably want up to PC5
pca_eigens

pca_loadings <- tidy(aus_pca, "loadings")
p1 <- plot_loadings(1)
p2 <- plot_loadings(2)
p3 <- plot_loadings(3)
p4 <- plot_loadings(4)
p1 + p2 + p3 + p4

pca_scores <- tidy(aus_pca, "scores") # Then can plot components to each other
pca_scores |> pivot_wider(names_from = PC, values_from = value) |>
  ggplot(aes(x = `1`, y = `2`)) + # Choose PCs to plot here
  geom_point(alpha = 0.5)


# base R glm (multiple regression)
glm_data <- aus_data |>
  mutate(log_leaf_N = log(leaf_N_per_dry_mass)) |>
  filter(log_leaf_N > 0)
aus_glm <- lm(log_leaf_N ~ SN_total_0_30 + PPT + MAT + precipitation_seasonality, glm_data)
r1 <- aus_glm |> augment() |> ggplot(aes(x = .resid)) + geom_histogram()
r2 <- aus_glm |> augment() |>
  ggplot(aes(x = log_leaf_N, y = .resid)) +
  geom_point(alpha = 0.5)
r1 + r2
summary(aus_glm)

# generalized equivalent: type of data not compatible
# need binary / proportion / count data
# aus_GLM <- glm(
#   leaf_N_per_dry_mass ~ SN_total_0_30 + PPT + MAT + precipitation_seasonality,
#   family = poisson(link = "log"),
#   data = aus_data
#)


# tidymodels attempt ----
# Starting with leaf N outcome
# Correlation plot 
aus_data |> select(!Unique_ID:myc_type) |> 
  select(!leaf_P_per_dry_mass:CP_ratio) |> 
  cor(use = 'pairwise.complete.obs') |> 
  corrplot(method = 'ellipse', tl.col = 'black')

ggplot(aus_data, aes(x = MAT, y = SN_total_0_30)) + geom_point()

# Data split 
aus_split <- aus_data |> 
  select(!c(Unique_ID, leaf_P_per_dry_mass:CP_ratio)) |> 
  initial_split(prop = 0.8, strata = leaf_N_per_dry_mass)
aus_train <- training(aus_split)
aus_test <- testing(aus_split)

n1 <- ggplot(aus_data, aes(x = leaf_N_per_dry_mass)) + 
  geom_histogram() + 
  ggtitle('All data')
n2 <- ggplot(aus_train, aes(x = leaf_N_per_dry_mass)) + 
  geom_histogram() + 
  ggtitle('Training data')
n3 <- ggplot(aus_test, aes(x = leaf_N_per_dry_mass)) + 
  geom_histogram() + 
  ggtitle('Testing data')

# Check leaf N stratification 
n1 + n2 + n3
rm(n1, n2, n3)

# Initial recipe with PCA pre-requirements 
aus_rec <- recipe(leaf_N_per_dry_mass ~ ., data = aus_train) |> 
  update_role(dataset_id:ln_CP_ratio, new_role = 'tag') |> 
  step_zv(all_numeric_predictors()) |> 
  step_orderNorm(all_numeric_predictors()) |> 
  step_normalize(all_numeric_predictors()) |> 
  prep()
aus_rec

# Check scaling and normalizing 
aus_rec_processed <- bake(aus_rec, new_data = aus_test)
p1 <- aus_test |> 
  ggplot(aes(x = NPP)) + 
  geom_histogram(bins = 50, color = 'white') + 
  ggtitle('Original test set') + 
  theme_bw()

p2 <- aus_rec_processed |> 
  ggplot(aes(x = NPP)) +
  geom_histogram(bins = 50, color = 'white') +
  ggtitle('Processed test set') +
  theme_bw()

p1 + p2

# Add PCA step 
aus_pca <- aus_rec |> 
  step_pca(all_numeric_predictors(), num_comp = 4) |> 
  prep()
aus_pca

# Variance coverage and loadings 
pca_extract <- aus_pca$steps[[4]]
glimpse(pca_extract)

pca_variance <- pca_extract |> tidy(type = 'variance')
pca_variance |> 
  filter(terms %in% c('percent variance', 'cumulative percent variance')) |> 
  pivot_wider(names_from = terms, values_from = value) |> 
  select(!id)

pca_loadings <- pca_extract |> 
  tidy(type = 'coef') |> 
  mutate(component = parse_number(component))
pca_loadings |> # Top contributors to top components 
  group_by(component) |> 
  slice_max(abs(value), n = 3) |> 
  print(n = 15)

# Loadings plots for...
# Individual component 
pca_loadings |> filter(component == 1) |> 
  ggplot(aes(y = terms, x = value)) +
  geom_col() + 
  labs(
    title = 'PC1 Loadings Plot', 
    x = 'Contribution', 
    y = 'Term')

# Top components 
pca_loadings |> filter(component <= 6) |> 
ggplot(aes(x = value, y = terms)) + 
  geom_col() + 
  facet_wrap(~component) +
  labs(
    title = 'Loadings plot of top 6 components (95.2% variance explained)',
    x = 'Contribution',
    y = 'Term'
  )

# Then bake and plot components to view scores
aus_pca_processed <- aus_pca |> bake(all_predictors(), new_data = aus_test)
aus_pca_processed # Includes numeric tag variables

# Plot w/o additional visualization 
aus_pca_processed |> ggplot(aes(x = .panel_x, y = .panel_y)) +
  geom_point(alpha = 0.5, size = 0.5) +
  geom_autodensity(alpha = 0.3) +
  facet_matrix(vars(everything()), layer.diag = 2)

# Bin and visualize outcome ? 
aus_pca_processed |>  
  ggplot(aes(x = .panel_x, y = .panel_y, 
      color = cut_number(leaf_N_per_dry_mass, 4), # Cut by interval or number ? 
      fill = cut_number(leaf_N_per_dry_mass, 4)
      )) +
  geom_point(alpha = 0.5, size = 0.5) +
  geom_autodensity(alpha = 0.3) +
  facet_matrix(vars(everything()), layer.diag = 2) + 
  labs(color = 'Foliar N Range', fill = 'Foliar N Range')

# Or do major families ? 

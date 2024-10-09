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

# library(tidymodels)
# tidymodels_prefer()
# library(bestNormalize)
# library(ggforce)

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
  h1 <- ggplot(data, aes(x = {{predictor}})) + geom_histogram()
  plot(h1)
  
  p1 <- check_linearity_condition({{predictor}}, {{outcome}})
  p2 <- check_linearity_condition({{predictor}}, log_outcome)
  plot(p1 + p2)

  r1 <- simple_reg |> augment() |> ggplot(aes(x = .resid)) + geom_histogram()
  r2 <- simple_reg |> augment() |>
    ggplot(aes(x = log_outcome, y = .resid)) +
    geom_point(alpha = 0.5)
  plot(r1 + r2)

  summary(simple_reg)
}


aus_data


# Simple regression: N ~ SN ----
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
simple_reg_log <- lm(log_outcome ~ SN_total_0_30, simple_reg_data)
simple_reg_log |> augment() |> ggplot(aes(x = .resid)) + geom_histogram()
simple_reg_log |> augment() |>
  ggplot(aes(x = log_outcome, y = .resid)) +
  geom_point(alpha = 0.5)

summary(simple_reg_log)


# Simple regression: N ~ CEC ----
simple_reg_data <- prep_simple_data(CEC_total_0_30, leaf_N_per_dry_mass)
simple_reg <- lm(log_outcome ~ CEC_total_0_30, simple_reg_data)

simple_reg_data
simple_regression_outputs(CEC_total_0_30, leaf_N_per_dry_mass)


# Simple regression: N ~ ----


# base R glm


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
  update_role(dataset_id:myc_type, new_role = 'factor') |> 
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
    title = 'Loadings plot of top 6 components (93.5% variance explained)',
    x = 'Contribution',
    y = 'Term'
  )

# Then bake and plot components to view scores
aus_pca_processed <- aus_pca |> bake(where(is.numeric), new_data = aus_test)
aus_pca_processed

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

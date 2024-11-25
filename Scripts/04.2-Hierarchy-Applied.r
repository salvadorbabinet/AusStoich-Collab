# Hierarchical-Modeling
# Libraries and functions ----
library(tidyverse)
theme_set(theme_bw())
library(broom)
library(patchwork)

# Intercept and slope plot for a nest object and definined nesting factor
intercept_slope <- function(nest, nest_def) {
    nest |> select({{nest_def}}, n, results, GOF) |>
        unnest(cols = c(results, GOF)) |>
        ggplot(mapping = aes(
            x = fct_reorder({{nest_def}}, estimate),
            y = estimate,
            ymin = estimate - std.error,
            ymax = estimate + std.error,
            color = GOF
            )
        ) +
        geom_pointrange() +
        facet_wrap(~term, scales = "free") +
        theme(axis.text.x = element_text(angle = 90))
}

intercept_slope_bins <- function(nest, nest_def) {
    nest |> select({{nest_def}}, n, results, GOF) |>
        unnest(cols = c(results, GOF)) |>
        ggplot(mapping = aes(
            x = {{nest_def}},
            y = estimate,
            ymin = estimate - std.error,
            ymax = estimate + std.error,
            color = GOF
            )
        ) +
        geom_pointrange() +
        facet_wrap(~term, scales = "free") +
        theme(axis.text.x = element_text(angle = 90))
}

intercept_slope_bins_no_x <- function(nest, nest_def) {
    nest |> select({{nest_def}}, n, results, GOF) |>
        unnest(cols = c(results, GOF)) |>
        ggplot(mapping = aes(
            x = {{nest_def}},
            y = estimate,
            ymin = estimate - std.error,
            ymax = estimate + std.error,
            color = GOF
            )
        ) +
        geom_pointrange() +
        facet_wrap(~term, scales = "free") +
        theme(axis.text.x = element_blank())
}

intercept_slope_no_x <- function(nest, nest_def) {
    nest |> select({{nest_def}}, n, results, GOF) |>
        unnest(cols = c(results, GOF)) |>
        ggplot(mapping = aes(
            x = fct_reorder({{nest_def}}, estimate),
            y = estimate,
            ymin = estimate - std.error,
            ymax = estimate + std.error,
            color = GOF
            )
        ) +
        geom_pointrange() +
        facet_wrap(~term, scales = "free") +
        theme(axis.text.x = element_blank())
}

scatter_intercept_slope <- function(nest, nest_def) {
    nest |> select({{nest_def}}, results) |>
        unnest(cols = results) |>
        select({{nest_def}}, term, estimate) |>
        pivot_wider(names_from = term, values_from = estimate) |>
        ggplot(aes(x = `(Intercept)`, y = SN_total_0_30)) +
            geom_point() +
            geom_smooth(method = "lm", se = FALSE) +
            labs(x = "Intercept Estimate", y = "Coefficient Estimate")
}


# Taxonomic nesting? ----
str(aus_data)
aus_data |> count(family) |> arrange(desc(n)) |> filter(n > 30)
aus_data |> count(genus) |> arrange(desc(n))

# Log leaf N
family_nest <- aus_data |> nest_by(family) |>
    mutate(
        n = nrow(data),
        model_data = list(
            mutate(
                data,
                leaf_N_per_dry_mass = log(leaf_N_per_dry_mass),
                SN_total_0_30 = log(SN_total_0_30),
                .keep = "used"
            )
        )
    ) |>
    filter(n > 7) |>
    mutate(
        model = list(lm(leaf_N_per_dry_mass ~ SN_total_0_30, model_data)),
        results = list(tidy(model)),
        residuals = list(resid(model)),
        GOF = list(summary(model)$r.squared)
    )

# Object with appended residuals (for Sofia)
# ONLY families above set observation threshold
data_plus_resid <- family_nest |>
    select(family, data, residuals) |>
    unnest(cols = c(data, residuals)) |>
    relocate(species_binom, leaf_N_per_dry_mass, SN_total_0_30, residuals, .after = family)

ggplot(data_plus_resid, aes(x = residuals)) + geom_histogram()


# Plotting nested results ----
family_nest
family_nest |> select(family, n, results, GOF) |>
    unnest(cols = c(results, GOF)) |>
    arrange(desc(GOF)) |>
    print(n = Inf) 
# 1 family with R2 > 0.7, 5 families with R2 > 0.5

# Plot intercepts and slopes by family
# For family_nest with a few families, label the x axis
p1 <- intercept_slope_x(family_nest, family) +
    labs(
        title = "Intercepts and slopes for leaf N ~ soil N across families",
        subtitle = "Observations: n > 30",
        x = "Family")
p1

# Same plot with x axis label removed (for all families in family_nest)
p1 <- intercept_slope_no_x(family_nest, family) +
    labs(
        title = "Intercepts and slopes for leaf N ~ soil N across families",
        subtitle = "Observations: n > 7",
        x = "Family")
p1

# Plot how term estimates change with intercept estimates
scatter_intercept_slope(family_nest, family)

# Intercept distribution
p1 <- family_nest |> select(family, results) |>
    unnest(cols = results) |>
    select(family, term, estimate) |>
    pivot_wider(names_from = term, values_from = estimate) |>
    ggplot(aes(x = `(Intercept)`)) + geom_histogram() +
    labs(x = "Intercept Estimate", y = "Frequency")

# Coefficient distribution
p2 <- family_nest |> select(family, results) |>
    unnest(cols = results) |>
    select(family, term, estimate) |>
    pivot_wider(names_from = term, values_from = estimate) |>
    ggplot(aes(x = SN_total_0_30)) + geom_histogram() +
    labs(x = "Coefficient Estimate", y = "Frequency")

p1 + p2



# Plots to compare centering / scaling ----
p1 <- ggplot(aus_data, aes(x = SN_total_0_30, y = leaf_N_per_dry_mass)) + geom_point()
p2 <- ggplot(unnest(family_nest, centered_data), aes(x = SN_total_0_30, y = leaf_N_per_dry_mass)) + geom_point()
p1 + p2

h1 <- ggplot(aus_data, aes(x = SN_total_0_30)) + geom_histogram()
h2 <- ggplot(unnest(family_nest, centered_data), aes(x = SN_total_0_30)) + geom_histogram()
h1 + h2

h1 <- ggplot(aus_data, aes(x = leaf_N_per_dry_mass)) + geom_histogram()
h2 <- ggplot(unnest(family_nest, centered_data), aes(x = leaf_N_per_dry_mass)) + geom_histogram()
h1 + h2


# Sample model ----
lm_data <- unnest(family_nest, model_data) |>
    filter(family == "Fabaceae") |>
    select(leaf_N_per_dry_mass, SN_total_0_30)

lm_fab <- lm(leaf_N_per_dry_mass ~ SN_total_0_30, lm_data)
plot(lm_fab)
test <- summary(lm_fab)

comparison_data <- aus_data |>
    filter(family == "Amaranthaceae") |>
    select(leaf_N_per_dry_mass, SN_total_0_30)

lm_compare <- lm(leaf_N_per_dry_mass ~ SN_total_0_30, comparison_data)
plot(lm_compare) 
summary(lm_compare) # Centering predictor does not change model
ggplot(data = lm_amar, mapping = aes(x = lm_amar$residuals)) + geom_histogram()


# Latitude nesting?
latitude_nest <- aus_data |>
    mutate(lat_deg = cut(lat_deg, 150)) |>
    nest_by(lat_deg) |>
    mutate(
        n = nrow(data),
        model_data = list(
            mutate(
                data,
                leaf_N_per_dry_mass = log(leaf_N_per_dry_mass),
                SN_total_0_30 = log(SN_total_0_30),
                .keep = "used"
            )
        )
    ) |>
    mutate(
        model = list(lm(leaf_N_per_dry_mass ~ SN_total_0_30, model_data)),
        results = list(tidy(model)),
        residuals = list(resid(model)),
        GOF = list(summary(model)$r.squared)
    )
latitude_nest |> arrange(desc(n))
latitude_nest |> arrange(n)
latitude_nest <- latitude_nest |> filter(n>6)

p1 <- intercept_slope_bins_no_x(latitude_nest, lat_deg)
p2 <- intercept_slope_no_x(latitude_nest, lat_deg)
p1 + p2

scatter_intercept_slope(latitude_nest, lat_deg)

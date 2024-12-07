# Hierarchical-Modeling
# Libraries and functions
library(tidyverse)
theme_set(theme_bw())
library(broom)
library(patchwork)

library(httpgd)
hgd()
hgd_browse()

# Scatter plot simple relationship
scatter_plot <- function(xvar, yvar, data = aus_data) {
    ggplot(data, aes(x = {{xvar}}, y = {{yvar}})) +
        geom_point(alpha = 0.5) +
        geom_smooth() + geom_smooth(method = "lm", linetype = "dashed")
}

# Nesting functions
# Nest object makers ----
# Make nest object by pre-determined categorical variable
# I use this to nest by family, but other factors might work
categorical_nest <- function(outcome, predictor, obs_min, tb = aus_data) {
    tb |> nest_by(family) |>
    mutate(
        n = nrow(data),
        model_data = list(
            mutate(
                data,
                outcome = ({{outcome}}),
                predictor = ({{predictor}}),
                .keep = "used"
            )
        )
    ) |>
    filter(n > obs_min) |>
    mutate(
        model = list(lm(outcome ~ predictor, model_data)),
        results = list(tidy(model)),
        fitted = list(fitted(model)),
        residuals = list(resid(model)),
        GOF = list(summary(model)$r.squared)
    )
}


# Nest visualizations ----
# Intercept and slope plots for a definined nesting factor
intercept_slope <- function(nest, nest_def, yvar, xvar, obs_min) {
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
        theme(axis.text.x = element_text(angle = 90)) +
        labs(
            title = rlang::englue("Intercepts and slopes for {{yvar}} ~ {{xvar}} across families"),
            subtitle = rlang::englue("Observations: n > {{obs_min}}"),
            x = rlang::englue("{{nest_def}}")
        )
}

# Intercept and slope plots without x-axis labels
# Useful for looking at entire nested object instead of a subset
intercept_slope_no_x <- function(nest, nest_def, yvar, xvar, obs_min) {
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
        theme(axis.text.x = element_blank()) +
        labs(
            title = rlang::englue("Intercepts and slopes for {{yvar}} ~ {{xvar}} across families"),
            subtitle = rlang::englue("Observations: n > {{obs_min}}"),
            x = rlang::englue("{{nest_def}}"))
}

# Intercept and slope plots for a binned continuous nesting variable
# Does not re-order bins by term estimate
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

# Same as above, but without x-axis labels
# Does not re-order bins by term estimate
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

# Plot intercept and slope estimates against each other
# Instead of separate plots
scatter_intercept_slope <- function(nest, nest_def, yvar) {
    nest |> select({{nest_def}}, results) |>
        unnest(cols = results) |>
        select({{nest_def}}, term, estimate) |>
        pivot_wider(names_from = term, values_from = estimate) |>
        ggplot(aes(x = `(Intercept)`, y = {{yvar}})) +
            geom_point() +
            geom_smooth(method = "lm", se = FALSE) +
            labs(x = "Intercept Estimate", y = "Coefficient Estimate")
}

# Plot distribution of estimates as a histogram
estimate_histogram <- function(nest, nest_def, xvar) {
    plot_data <- nest |> select({{nest_def}}, results) |>
        unnest(cols = results) |>
        select({{nest_def}}, term, estimate) |>
        pivot_wider(names_from = term, values_from = estimate)
    
    p1 <- ggplot(plot_data, aes(x = `(Intercept)`)) + geom_histogram() +
        labs(x = "Intercept Estimate", y = "Frequency")
    p2 <- ggplot(plot_data, aes(x = {{xvar}})) + geom_histogram() +
        labs(x = "Coefficient Estimate", y = "Frequency")

    plot(p1 + p2)
}


# Taxonomic nesting?
str(aus_data)
aus_data |> count(family) |> arrange(desc(n)) |> filter(n > 30)
aus_data |> count(genus) |> arrange(desc(n))

# Log leaf N ~ log soil N ----
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
p1 <- intercept_slope(family_nest, family) +
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

scatter_intercept_slope(family_nest, family, SN_total_0_30) # Term- to intercept-estimates
estimate_histogram(family_nest, family, SN_total_0_30) # Estimate distributions


# Sample model ----
ggplot(aus_data, aes(x = SN_total_0_30, y = log(leaf_N_per_dry_mass))) +
  geom_point() + geom_smooth(method = "lm", se = FALSE)

summary(lm(log(leaf_N_per_dry_mass) ~ log(SN_total_0_30), data = aus_data))
summary(lm(log(leaf_N_per_dry_mass) ~ SN_total_0_30, data = aus_data))

# Ecological reason for investigating Fabaceae 
lm_data <- unnest(family_nest, model_data) |>
    filter(family == "Winteraceae") |>
    select(leaf_N_per_dry_mass, SN_total_0_30)

ggplot(lm_data, aes(x = exp(SN_total_0_30), y = exp(leaf_N_per_dry_mass))) + geom_point()

lm_fab <- lm(leaf_N_per_dry_mass ~ SN_total_0_30, lm_data)
plot(lm_fab)

# High n and GOF for log(leaf N) to log(soil N)
lm_data <- unnest(family_nest, model_data) |>
    filter(family == "Rhamnaceae") |>
    select(leaf_N_per_dry_mass, SN_total_0_30)

lm_rham <- lm(leaf_N_per_dry_mass ~ SN_total_0_30, lm_data)
plot(lm_rham)
summary(lm_rham)

ggplot(lm_data, aes(x = exp(SN_total_0_30), y = exp(leaf_N_per_dry_mass))) + geom_point() +
    stat_function(fun = \(x) exp(3.34)* x ^ 0.34)

# How does model change without logging soil N?
lm_rham <- lm(
    log(leaf_N_per_dry_mass) ~ log(SN_total_0_30),
    filter(aus_data, family == "Rhamnaceae")
    )

summary(lm_rham) # Similar R2, different estimates
plot(lm_rham)

ggplot(
    filter(aus_data, family == "Rhamnaceae"),
    aes(x = SN_total_0_30, y = log(leaf_N_per_dry_mass))
    ) + geom_point() + geom_smooth()


# Log leaf N ~ untransformed soil N ---- 
# Changes intercept-coefficient relationship
family_nest <- aus_data |> nest_by(family) |>
    mutate(
        n = nrow(data),
        model_data = list(
            mutate(
                data,
                leaf_N_per_dry_mass = log(leaf_N_per_dry_mass),
                SN_total_0_30 = SN_total_0_30,
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

intercept_slope_no_x(family_nest, family)
scatter_intercept_slope(family_nest, family, SN_total_0_30)
estimate_histogram(family_nest, family, SN_total_0_30)


# Log leaf N ~ other variables, nested by family
# Using nesting functions to optimize process
# MAT ----
scatter_plot(log(MAT), log(leaf_N_per_dry_mass))
summary(lm(log(leaf_N_per_dry_mass) ~ log(MAT), aus_data)) # Very poor R2
family_nest <- categorical_nest(leaf_N_per_dry_mass, MAT, 30)

intercept_slope(family_nest, family, `foliar N`, `MAT`, 30)
intercept_slope_no_x(family_nest, family, `foliar N`, `MAT`, 7)

# Investigate model for families w/ high n and GOF
family_nest |> unnest(GOF) |>
    arrange(desc(GOF)) |>
    filter(n > 20) # Potentially good fit for Picrodendraceae, Malvaceae

# Can plot raw and log-transformed values against each other
# By changing aes()
family_nest |> filter(family %in% c("Picrodendraceae", "Malvaceae")) |>
    unnest(model_data) |> filter(family == "Malvaceae") |>
    ggplot(aes(x = MAT, y = leaf_N_per_dry_mass)) +
    geom_point() + geom_smooth(method = lm)

family_nest |> filter(family %in% c("Picrodendraceae", "Malvaceae")) |>
    unnest(results) |> select(family, term:p.value)

family_nest |> filter( family == "Malvaceae") |>
    select(family, fitted, residuals) |> 
    unnest(everything()) |>
    ggplot(aes(x = fitted, y = residuals)) + geom_point()

family_nest |> filter( family == "Malvaceae") |>
    select(family, fitted, residuals) |> 
    unnest(everything()) |>
    ggplot(aes(x = residuals)) + geom_histogram()

# Validate against model w/o transformed MAT
log_raw <- family_nest |> filter( family == "Malvaceae") |>
    select(family, model_data) |> unnest(model_data)
log_raw_model <- lm(outcome ~ MAT, log_raw)
summary(log_raw_model) # Better R2 with log-log compared to log-raw
ggplot(log_raw_model, aes(x = .fitted, y = .resid)) + geom_point()
ggplot(log_raw_model, aes(x = .resid)) + geom_histogram()
# Residual plots and distributions similar


# PPT ----
# No compelling estimate magnitudes
scatter_plot(PPT, log(leaf_N_per_dry_mass))
summary(lm(log(leaf_N_per_dry_mass) ~ PPT, aus_data)) # Not logarithmic

family_nest <- categorical_nest(log(leaf_N_per_dry_mass), log(PPT), 7)
intercept_slope(family_nest, family, `log leaf N`, `log PPT`, 30)
intercept_slope_no_x(family_nest, family, `log leaf N`, `log PPT`, 7)

family_nest <- categorical_nest(log(leaf_N_per_dry_mass), PPT, 7)
intercept_slope(family_nest, family, `log leaf N`, `PPT`, 30)
intercept_slope_no_x(family_nest, family, `log leaf N`, `PPT`, 7)


# Latitude nesting? ----
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

scatter_intercept_slope(latitude_nest, lat_deg, SN_total_0_30)
estimate_histogram(latitude_nest, lat_deg, SN_total_0_30)


# Stoichiometric hierarchy
# Libraries and functions
library(httpgd)
hgd()
hgd_browse()

# Scatter plot simple relationship
scatter_plot <- function(xvar, yvar, data = aus_data) {
    ggplot(data, aes(x = {{xvar}}, y = {{yvar}})) +
        geom_point(alpha = 0.5) +
        geom_smooth(se = FALSE) + geom_smooth(method = "lm", linetype = "dashed")
}


# To make nest objects ----
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

# Nesting with more complex formulae (multiple regression)
categorical_nest_multi <- function(outcome, pred1, pred2, obs_min, tb = aus_data) {
    tb |> nest_by(family) |>
    mutate(
        n = nrow(data),
        model_data = list(
            mutate(
                data,
                outcome = {{outcome}},
                pred1 = {{pred1}},
                pred2 = {{pred2}},
                .keep = "used"
            )
        )
    ) |>
    filter(n > obs_min) |>
    mutate(
        model = list(lm(outcome ~ pred1 + pred2, model_data)),
        results = list(tidy(model)),
        fitted = list(fitted(model)),
        residuals = list(resid(model)),
        GOF = list(summary(model)$r.squared)
    )
}

# Make nest object by cut continuous variable
# To investigate how climate, etc. mediate interactions
continuous_nest <- function(nest_def, cut_number, outcome, predictor, obs_min, tb = aus_data) {
    tb |> mutate(nest_def = cut({{nest_def}}, cut_number)) |>
    nest_by(nest_def) |>
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


# To visualize nests ----
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
            title = rlang::englue("Intercepts and slopes for {{yvar}} ~ {{xvar}} by {{nest_def}}"),
            subtitle = rlang::englue("Observations: n > {{obs_min}}"),
            x = rlang::englue("{{nest_def}}"))
}

# Intercept and slope plots for a binned continuous nesting variable
# Does not re-order bins by term estimate
intercept_slope_bins_labeled <- function(nest, nest_def = nest_def) {
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
intercept_slope_bins <- function(nest, nest_def, yvar, xvar, obs_min, cut_number) {
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
        theme(axis.text.x = element_blank()) +
        labs(
            title = rlang::englue("Intercepts and slopes for {{yvar}} ~ {{xvar}} across binned {{nest_def}}"),
            subtitle = rlang::englue("Observations: n > {{obs_min}}, No. of bins: n = {{cut_number}}"),
            x = rlang::englue("{{nest_def}}"))
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


aus_NP <- aus_data |> filter(!is.na(leaf_P_per_dry_mass))
ggplot(aus_NP, aes(x = log(NP_ratio))) + geom_histogram()

aus_NP |> count(species_binom) |> arrange(desc(n)) # Eucalypts and acacias...
aus_NP |> filter(species_binom == "Acacia_rostellifera")

scatter_plot(
    AP_total_0_30,
    ln_NP_ratio,
    data = filter(aus_NP, species_binom == "Melaleuca_systena"))

# Acacia_rostellifera, Flindersia_bourjotiana


# Categorical cut (taxonomy)
# Simple regression ----
ggplot(aus_NP, aes(x = log(SN_total_0_30), y = log(NP_ratio))) +
    geom_point(alpha = 0.5) + geom_smooth(method = "lm")
 
summary(lm(log(NP_ratio) ~ log(SP_total_0_30), aus_NP))

family_nest <- categorical_nest(log(NP_ratio), log(SN_total_0_30), 9, aus_NP)
family_nest |> unnest(GOF) |> arrange(desc(GOF)) |>
    select(!c(data, model_data, model))

intercept_slope_no_x(family_nest, family, `log leaf NP`, `log SN`, 9)

family_nest <- categorical_nest(log(leaf_P_per_dry_mass), log(AP_total_0_30), 9, aus_NP)
family_nest |> unnest(GOF) |> arrange(desc(GOF)) |>
    select(!c(data, model_data, model)) |>
    print(n = 20)

family_nest |> filter(family == "Cyperaceae") |>
    unnest(results) |> select(family, term:p.value)

intercept_slope_no_x(family_nest, family, `log leaf P`, `log AP`, 9)

family_nest |> filter(family %in% c("Cyperaceae", "Sapindaceae", "Rutaceae")) |>
    unnest(model_data) |> filter(family == "Cyperaceae") |>
    ggplot(aes(x = outcome, y = predictor)) +
    geom_point() + geom_smooth(method = lm) +
    labs(
        title = "Family = Cyperaceae",
        y = "Log N:P", x = "Log Available P")

family_nest |> filter(family == "Cyperaceae") |>
    unnest(results) |>
    ggplot(aes(x = outcome, y = predictor)) +
    geom_point() + geom_smooth(method = lm) +
    labs(
        title = "Family = Cyperaceae",
        y = "Log N:P", x = "Log Available P")


# Multiple regression ----
family_nest <- categorical_nest_multi(ln_NP_ratio, log(SN_total_0_30), log(AET), 9, aus_NP)
family_nest |> select(family, n, results, GOF) |>
    unnest(c(results, GOF))

intercept_slope_no_x(family_nest, family, `ln leaf N:P`, `ln SN + ln SP`, 10)


# Continuous cuts ----
# MAT
MAT_nest <- continuous_nest(MAT, 200, log(NP_ratio), log(AP_total_0_30), 9, aus_NP)
MAT_nest <- MAT_nest |> rename(MAT = nest_def)
MAT_nest |> unnest(GOF) |> arrange(desc(GOF)) |>
    select(!c(data, model_data, model))
    print(n = 20)

intercept_slope_bins(MAT_nest, MAT, NP_ratio, AP_total_0_30, 9, 180)
intercept_slope_no_x(MAT_nest, MAT, NP_ratio, AP_total_0_30, 9)

# PPT
PPT_nest <- continuous_nest(PPT, 200, log(NP_ratio), log(AP_total_0_30), 9, aus_NP)
PPT_nest <- PPT_nest |> rename(PPT = nest_def)
PPT_nest |> unnest(GOF) |> arrange(desc(GOF)) |>
    select(!c(data, model_data, model))
    print(n = 20)

intercept_slope_bins(PPT_nest, PPT, NP_ratio, AP_total_0_30, 9, 180)
intercept_slope_no_x(PPT_nest, PPT, NP_ratio, AP_total_0_30, 9)

seasonality_nest <- continuous_nest(MAT, 200, log(NP_ratio), precipitation_seasonality, 9, aus_NP)
seasonality_nest <- seasonality_nest |> rename(precip_seasonality = nest_def)
seasonality_nest |> unnest(GOF) |> arrange(desc(GOF)) |>
    select(!c(data, model_data, model))
    print(n = 20)

intercept_slope_bins(seasonality_nest, precip_seasonality, NP_ratio, AP_total_0_30, 9, 180)
intercept_slope_no_x(MAT_nest, MAT, NP_ratio, AP_total_0_30, 9)
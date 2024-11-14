# Hierarchical-Modeling
# Libraries and functions
library(tidyverse)
library(broom)
library(patchwork)


# Taxonomic nesting?
str(aus_data)
aus_data |> count(family) |> arrange(desc(n)) |> filter(n > 7)
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
        GOF = list(summary(model)$r.squared) # Just changed this, troubleshoot before push
    )

# Object with apprended residuals
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

family_nest |> select(family, n, results, GOF) |>
    unnest(cols = c(results, GOF)) |>
    ggplot(mapping = aes(
        x = fct_reorder(family, estimate),
        y = estimate,
        ymin = estimate - std.error,
        ymax = estimate + std.error,
        color = r.squared)
    ) +
    geom_pointrange() +
    facet_wrap(~term, scales = "free") +
    theme(axis.text.x = element_text(angle = 90))

family_nest |> select(family, results) |>
    unnest(cols = results) |>
    select(family, term, estimate) |>
    pivot_wider(names_from = term, values_from = estimate) |>
    ggplot(aes(x = `(Intercept)`, y = SN_total_0_30)) +
    geom_point()

family_nest |> select(family, results) |>
    unnest(cols = results) |>
    select(family, term, estimate) |>
    pivot_wider(names_from = term, values_from = estimate) |>
    ggplot(aes(x = `(Intercept)`)) + geom_histogram()

family_nest |> select(family, results) |>
    unnest(cols = results) |>
    select(family, term, estimate) |>
    pivot_wider(names_from = term, values_from = estimate) |>
    ggplot(aes(x = SN_total_0_30)) + geom_histogram()



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
summary(lm_fab)

comparison_data <- aus_data |>
    filter(family == "Amaranthaceae") |>
    select(leaf_N_per_dry_mass, SN_total_0_30)

lm_compare <- lm(leaf_N_per_dry_mass ~ SN_total_0_30, comparison_data)
plot(lm_compare) 
summary(lm_compare) # Centering predictor does not change model
ggplot(data = lm_amar, mapping = aes(x = lm_amar$residuals)) + geom_histogram()



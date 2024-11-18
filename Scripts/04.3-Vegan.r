# Investigating vegan package (RDA, varpart)
library(corrplot)
library(patchwork)

library(vegan)
library(httpgd)
hgd()
hgd_browse()


# Data objects
str(aus_data)

aus_NP <- aus_data |>
    filter(!is.na(leaf_P_per_dry_mass)) |>
    select(leaf_N_per_dry_mass:leaf_P_per_dry_mass)
ggplot(aus_NP, aes(x = leaf_P_per_dry_mass)) + geom_histogram()

aus_explain <- aus_data |> select(where(is.numeric)) |>
    select(!Unique_ID:long_deg) |>
    filter(!is.na(leaf_P_per_dry_mass)) |>
    select(!leaf_N_per_dry_mass:ln_CP_ratio)

str(aus_explain)
ggplot(aus_explain, aes(x = AET)) + geom_histogram()


# Check colinearity
aus_explain |> cor() |> corrplot(
    method = "pie", #Color, ellipse, pie are useful
    type = "upper",
    tl.col = "black",
    title = "Predictors not transformed or standardized",
    mar = c(0, 0, 2, 0)
    )


# Does transformation change this? A little...
aus_explain_transform <- aus_explain |> mutate(across(everything(), log)) |>
    decostand(method = "standardize") |>
    tibble()
aus_explain_transform

ggplot(aus_explain_transform, aes(x = SP_total_0_30)) + geom_histogram()
aus_explain_transform |> cor() |> corrplot(
    method = "pie",
    type = "upper",
    tl.col = "black",
    title = "Log-transformed and standardized predictors",
    mar = c(0, 0, 2, 0)
    )


# So, lots of colinearity within soil / climate
# Variance partition?
aus_soil <- aus_explain |> select(!MAT:temp_seasonality)
aus_climate <- aus_explain |> select(!SN_total_0_30:NPP)

aus_soil_transform <- aus_explain_transform |> select(!MAT:temp_seasonality)
aus_climate_transform <- aus_explain_transform |> select(!SN_total_0_30:NPP)

# Leaf N only
aus_variance <- varpart(
    aus_data$leaf_N_per_dry_mass,
    select(aus_data, SN_total_0_30:NPP),
    select(aus_data, MAT:temp_seasonality)
)
aus_variance
plot(aus_variance)

aus_variance_transform <- varpart(aus_foliage$leaf_N_per_dry_mass, aus_soil_transform, aus_climate_transform)
# Transformation doesn't affect variance calculation much
# But large residuals, tiny fractions

# N and P (~1/2 the data)
aus_variance <- varpart(aus_NP, aus_soil, aus_climate)
aus_variance
plot(aus_variance)
# Same issues


# What about plant traits?
aus_data |> select(woodiness:myc_type) |>
    count(across(everything(), is.na))

aus_data |> filter(is.na(woodiness))
aus_data |> filter(is.na(myc_type))

aus_soil <- aus_data |>
    filter(!is.na(woodiness)) |>
    select(SN_total_0_30:NPP)

aus_climate <- aus_data |>
    filter(!is.na(woodiness)) |>
    select(MAT:temp_seasonality)

aus_traits <- aus_data |>
    filter(!is.na(woodiness)) |>
    select(woodiness:reclass_life_history)

aus_N <- aus_data |>
    filter(!is.na(woodiness)) |>
    select(leaf_N_per_dry_mass)

aus_variance <- varpart(
    aus_N,
    aus_soil,
    aus_climate,
    aus_traits
)
aus_variance
plot(aus_variance)
# Not looking much better...
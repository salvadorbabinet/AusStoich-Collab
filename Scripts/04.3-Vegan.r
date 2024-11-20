# Investigating vegan package (RDA, varpart)
# Libraries and functions ----
library(corrplot)
library(patchwork)

library(vegan)
library(httpgd)
hgd()
hgd_browse()


# Co-linearity between continuous variables ----
# Separate response and numeric explanatory data
str(aus_data)

aus_NP <- aus_data |>
    filter(!is.na(leaf_P_per_dry_mass)) |>
    select(leaf_N_per_dry_mass:leaf_P_per_dry_mass) |>
    mutate(across(everything(), log))
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
# Good to standardize and transform for later RDAs


# So, lots of colinearity within soil / climate
# Variance partition? ----
aus_soil <- aus_explain_transform |> select(!MAT:temp_seasonality)
aus_climate <- aus_explain_transform |> select(!SN_total_0_30:NPP)

# Leaf N only (non-transformed for sanity check)
aus_variance <- varpart(
    aus_data$leaf_N_per_dry_mass,
    select(aus_data, SN_total_0_30:NPP),
    select(aus_data, MAT:temp_seasonality)
)
aus_variance
plot(aus_variance)

# And transformed...
aus_variance <- varpart(
    log(aus_data$leaf_N_per_dry_mass),
    aus_soil,
    aus_climate
)
# Transformation doesn't affect variance calculation much
# But large residuals, tiny fractions

# N and P (~1/2 the data)
aus_variance <- varpart(aus_NP, aus_soil, aus_climate)
aus_variance
plot(aus_variance)
# Same issues


# What about plant traits? Need to remove missing woodiness entry
# Major categories together: ----
aus_data |> select(woodiness:myc_type) |> count(across(everything(), is.na))

aus_data |> filter(is.na(woodiness))
aus_data |> filter(is.na(myc_type))

aus_narm_woodiness <- aus_data |> filter(!is.na(woodiness))
aus_soil <- aus_narm_woodiness |> select(SN_total_0_30:NPP) |> # Is NPP a soil var?
    mutate(across(everything(), log)) |>
    decostand(method = "standardize") |>
    tibble()
aus_climate <- aus_narm_woodiness |> select(MAT:temp_seasonality) |>
    mutate(across(everything(), log)) |>
    decostand(method = "standardize") |>
    tibble()
aus_traits <- aus_narm_woodiness |> select(woodiness:putative_BNF)
aus_N <- aus_narm_woodiness |> select(leaf_N_per_dry_mass) |>
    mutate(across(everything(), log)) |>
    decostand(method = "standardize") |>
    tibble()
# Transforming and standardizing doesn't affect single N partition
# NP partition: ...

aus_variance <- varpart(
    aus_N,
    aus_soil,
    aus_climate,
    aus_traits
)
aus_variance
plot(aus_variance)
# Including plant traits (especially N fixation) bumps up R2.


# What about family? (genus redundant) ----
# This may eventually lead to a partial RDA with conditioning matrix
# as either family taxonomy or phylogenetic distances?
aus_family <- aus_narm_woodiness |> select(family)
aus_variance <- varpart(
    aus_N,
    aus_soil,
    aus_climate,
    aus_traits,
    aus_family
)
aus_variance # Family explains a lot!
plot(
    aus_variance,
    main = "Transformed and standardized leaf N",
    Xnames = c("Soil", "Climate", "Traits", "Family"),
    bg = c("azure2", "azure2", "lightskyblue1", "indianred")
)


# Run RDAs to test significance of each fraction.
anova.cca(rda(aus_N, aus_family))
anova.cca(rda(aus_N, aus_traits))
anova.cca(rda(aus_N, aus_climate))
anova.cca(rda(aus_N, aus_soil))
# Each explanatory matrix (without controlling across matrices)
# is statistically significant.

# Partial RDAs
# Variance in N explained by family, controlling for trait
# I.e., family alone
anova.cca(rda(aus_N, aus_family, aus_traits)) # Significant

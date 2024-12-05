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

aus_explain <- aus_data |> select(where(is.numeric)) |>
    select(!Unique_ID:long_deg) |>
    select(!leaf_N_per_dry_mass:ln_CP_ratio)

aus_explain_P <- aus_data |> select(where(is.numeric)) |>
    select(!Unique_ID:long_deg) |>
    filter(!is.na(leaf_P_per_dry_mass)) |>
    select(!leaf_N_per_dry_mass:ln_CP_ratio)

str(aus_explain)
ggplot(aus_explain, aes(x = AET)) + geom_histogram()

# Check colinearity
aus_explain_P |> cor() |> corrplot(
    method = "pie", #Color, ellipse, pie are useful
    type = "upper",
    tl.col = "black",
    title = "P-complete data",
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


# Correlations across levels of dataset completion
# All numeric data
aus_data |> select(where(is.numeric)) |>
    select(!Unique_ID:long_deg) |>
    select(!ln_NP_ratio:ln_CP_ratio) |>
    cor(use = "pairwise.complete.obs") |> corrplot(
        method = "color", #Color, ellipse, pie are useful
        type = "upper",
        tl.col = "black",
        title = "Pairwise complete",
        mar = c(0, 0, 2, 0)
    )

aus_data |> select(where(is.numeric)) |>
    select(!Unique_ID:long_deg) |>
    select(!ln_NP_ratio:ln_CP_ratio) |>
    mutate(across(everything(), log)) |>
    cor(use = "pairwise.complete.obs") |> corrplot(
        method = "color", #Color, ellipse, pie are useful
        type = "full",
        tl.col = "black",
        title = "Pairwise complete, log-transformed",
        mar = c(0, 0, 2, 0)
    )

# All numeric data, N- and P-complete
aus_data |> select(where(is.numeric)) |>
    select(!Unique_ID:long_deg) |>
    select(!leaf_C_per_dry_mass) |>
    select(!CN_ratio:ln_CP_ratio) |>
    filter(!is.na(leaf_P_per_dry_mass)) |>
    cor() |> corrplot(
        method = "ellipse", #Color, ellipse, pie are useful
        type = "full",
        tl.col = "black",
        title = "P-complete data",
        mar = c(0, 0, 2, 0)
    )

# All numeric data, N-complete
aus_data |> select(where(is.numeric)) |>
    select(!Unique_ID:long_deg) |>
    select(!leaf_P_per_dry_mass:ln_CP_ratio) |>
    cor() |> corrplot(
        method = "color", #Color, ellipse, pie are useful
        type = "full",
        tl.col = "black",
        title = "N-complete data",
        mar = c(0, 0, 2, 0)
    )

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

# NPP grouped with soil
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
# NP partition: 

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

# To-do: check results with co-linear variables removed
# Run RDAs to test significance of each fraction. ----
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


# Run with N and P ----
aus_narm_P <- aus_data |> filter(!is.na(leaf_P_per_dry_mass))

filter(aus_data, is.na(woodiness)) |> select(woodiness, leaf_P_per_dry_mass)
ggplot(aus_narm_P, aes(x = leaf_P_per_dry_mass)) + geom_histogram()

aus_soil <- aus_narm_P |> select(SN_total_0_30:NPP) |> # Is NPP a soil var?
    mutate(across(everything(), log)) |>
    decostand(method = "standardize") |>
    tibble()
aus_climate <- aus_narm_P |> select(MAT:temp_seasonality) |>
    mutate(across(everything(), log)) |>
    decostand(method = "standardize") |>
    tibble()
aus_traits <- aus_narm_P |> select(woodiness:putative_BNF)
aus_family <- aus_narm_P |> select(family)
aus_NP <- aus_narm_P |> select(leaf_N_per_dry_mass:leaf_P_per_dry_mass) |>
    mutate(across(everything(), log)) |>
    decostand(method = "standardize") |>
    tibble()

aus_variance <- varpart(
    aus_NP,
    aus_soil,
    aus_climate,
    aus_traits,
    aus_family
)
aus_variance 
plot(
    aus_variance,
    Xnames = c("Soil", "Climate", "Traits", "Family"),
    bg = c("azure2", "azure2", "lightskyblue1", "indianred")
)

# Significance check
anova.cca(rda(aus_NP, aus_family))
anova.cca(rda(aus_NP, aus_traits))
anova.cca(rda(aus_NP, aus_climate))
anova.cca(rda(aus_NP, aus_soil))
# These are all significant
anova.cca(rda(aus_NP, aus_family, aus_traits))
anova.cca(rda(aus_NP, aus_family, aus_soil))


# P only
aus_P <- aus_narm_P |> select(leaf_P_per_dry_mass) |>
    mutate(across(everything(), log)) |>
    decostand(method = "standardize") |>
    tibble()

aus_variance <- varpart(
    aus_P,
    aus_soil,
    aus_climate,
    aus_traits,
    aus_family
)
aus_variance 
plot(
    aus_variance,
    Xnames = c("Soil", "Climate", "Traits", "Family"),
    bg = c("azure2", "azure2", "lightskyblue1", "indianred")
)


# Variance partition with restructured categorization----
aus_subset <- aus_data |> filter(!is.na(woodiness))
aus_env <- aus_subset |> select(
    SN_total_0_30:AP_total_0_30,
    MAT:PPT,
    precipitation_seasonality:temp_seasonality
    ) |>
    mutate(across(everything(), log)) |>
    decostand(method = "standardize") |>
    tibble()
aus_traits <- aus_subset |> select(woodiness:putative_BNF)
aus_family <- aus_subset |> select(family)
aus_planteffects <- aus_subset |> select(NPP, AET) |>
    mutate(across(everything(), log)) |>
    decostand(method = "standardize") |>
    tibble()
aus_outcome <- aus_subset |> select(leaf_N_per_dry_mass) |>
    mutate(across(everything(), log)) |>
    decostand(method = "standardize") |>
    tibble()

aus_variance <- varpart(
    aus_outcome,
    aus_env,
    aus_planteffects,
    aus_traits,
    aus_family
)

aus_variance
plot(
    aus_variance,
    Xnames = c("Environment", "Plant Effects", "Plant Traits", "Family"),
    bg = c("azure2", "azure2", "lightskyblue1", "indianred")
    )

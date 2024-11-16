# Investigating vegan package (RDA, varpart)
library(vegan)
library(httpgd)
hgd()
hgd_browse()

# Data objects
str(aus_data)
aus_foliage <- aus_data |> select(leaf_N_per_dry_mass:leaf_C_per_dry_mass)
ggplot(aus_foliage, aes(x = leaf_C_per_dry_mass)) + geom_histogram()

aus_predictors <- aus_data |> select(where(is.numeric)) |>
    select(!Unique_ID:long_deg) |>
    select(!leaf_N_per_dry_mass:ln_CP_ratio) |>
    decostand(method = "standardize")
str(aus_predictors)
ggplot(aus_predictors, aes(x = SN_total_0_30)) + geom_histogram()

rda()
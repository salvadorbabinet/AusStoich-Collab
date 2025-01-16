# Intro hierarchical modeling from meeting with Andrew
library(tidyverse)
library(tidymodels)
library(httpgd)
hgd()
hgd_browse()

nested_aus_data <- aus_data |> nest_by(family) |>
  mutate(
    row_count = nrow(data), 
    mean_sn = mean(data$SN_total_0_30), 
    data_c = list(
      mutate(data, SN_total_0_30 = SN_total_0_30 - mean_sn)
    )) |> 
  filter(row_count > 2) |> 
  mutate(
    models = list(lm(formula = leaf_N_per_dry_mass ~ SN_total_0_30, data = data_c)),
    tidymodels = list(broom::tidy(models))
  )

nested_aus_data |> select(-c(data, models)) |> tidyr::unnest(tidymodels) |> 
  mutate(family = forcats::fct_reorder(family, estimate, .fun = min)) |>
  ggplot(mapping = aes(x = family, y = estimate, ymin = estimate - std.error, ymax = estimate + std.error)) + 
  geom_pointrange() + 
  facet_wrap(~term, scales = "free")

nested_aus_data |> select(-c(data, models)) |> tidyr::unnest(tidymodels) |> 
  select(family, term, estimate) |> 
  tidyr::pivot_wider(names_from = term, values_from = estimate) |>
  ggplot(mapping = aes(x = `(Intercept)`, y = SN_total_0_30)) + 
  geom_point()

nested_aus_data |> select(-c(data, models)) |> tidyr::unnest(tidymodels) |> 
  select(family, term, estimate) |> 
  tidyr::pivot_wider(names_from = term, values_from = estimate) |>
  ggplot(mapping = aes(x = `(Intercept)`)) + 
  geom_histogram()

nested_aus_data$tidymodels[[2]]
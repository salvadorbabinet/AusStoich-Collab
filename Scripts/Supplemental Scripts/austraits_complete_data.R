library(readr)
library(tidyr)
library(dplyr)
library(stringr)
library(here)

remotes::install_github("traitecoevo/austraits", dependencies = TRUE, upgrade = "ask")

austraits::get_versions()

most_recent_doi <- austraits::get_versions()|> dplyr::pull("doi") |> dplyr::first()


austraits <- austraits::load_austraits(doi = most_recent_doi) # you can load from the Zenodo doi

growth_form_woodiness <-
  
  austraits$traits %>%
  
  dplyr::filter(dataset_id %in% c("Wenk_2022")) %>%
  
  dplyr::rename(references = measurement_remarks) %>%
  
  dplyr::select(taxon_name, trait_name, value, references) %>%
  
  dplyr::distinct(taxon_name, trait_name,.keep_all = TRUE) %>%
  
  tidyr::pivot_wider(names_from = trait_name, values_from = value)

complete_traits <-
  
  austraits$traits %>%
  
  dplyr::filter(dataset_id %in% c("Wenk_2023")) %>%
  
  dplyr::rename(life_history = value) %>%
  
  dplyr::select(taxon_name, life_history) %>%
  
  dplyr::distinct(taxon_name,.keep_all = TRUE) %>%
  
  dplyr::left_join(growth_form_woodiness) %>%
  
  dplyr::select(taxon_name, plant_growth_form, woodiness_detailed, life_history, references)

rm(austraits, growth_form_woodiness, most_recent, most_recent_doi)

#extract complete_traits as csv, match in v3 spreadsheet
write_csv(complete_traits, file = here('Inputs', 'Supplemental Inputs - Sofia',
                                       'austraits_complete_categorical.csv'))

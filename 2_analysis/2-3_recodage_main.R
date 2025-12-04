library(stringr)
library(dplyr)

## Liens 
liens <- liens %>%
  mutate(
    sexe = case_when(
      sexe == "2" ~ "F", 
      sexe == "1" ~ "M"
  ), 
  age = 2025-anai, 
  prenom = str_to_title(prenom))
liens

saveRDS(liens, file = "1_data/processed/liens.RDS")

## Coordonnées
coordonnees <- coordonnees %>%
  mutate(MAIL = case_when(
    POSTENQ_MAIL == EAR_MAIL  ~ POSTENQ_MAIL, 
    is.na(EAR_MAIL) ~ POSTENQ_MAIL, 
    is.na(POSTENQ_MAIL) ~ paste0(EAR_MAIL, " (EAR)"),
    TRUE ~ paste0(POSTENQ_MAIL, " / ", EAR_MAIL, " (EAR)")
  ))

saveRDS(coordonnees, file = "1_data/processed/coordonnees.RDS")

codecommunes <- read_csv("https://www.data.gouv.fr/api/1/datasets/r/7acc46ad-1c79-43d9-8f2d-d0a8ec78c068")
codecommunes$Commune <- paste0(codecommunes$nomCommune, " (", codecommunes$codeDepartement, ")")
codecommunes$codeCommune
BIEF <- BIEF %>% 
  mutate(across(contains("COM"), ~ str_remove(.x, "2024"))) %>%
  mutate(across(contains("COM"),
                ~ codecommunes$Commune[match(.x, codecommunes$codeCommune)]))



saveRDS(BIEF, file = "1_data/processed/BIEF.Rds")


library(stringr)
library(dplyr)

liens <- readRDS(file = "1_data/processed/liens.Rds")
## Liens
BIEF$anai
egos <- BIEF %>%
  mutate(age_x = 2025-anai, 
         relation_ego = "EGO",
         sexe_x = str_sub(sexe, 1, 1)) %>%
  select(identifiant, sexe_x, age_x, relation_ego)
  
liens <- liens%>%
  mutate(
    sexe_x = case_when(
      sexe_x == "2" ~ "F", 
      sexe_x == "1" ~ "M"
      ), 
  age_x = 2025-anai_x)

menages <- bind_rows(egos, liens) %>% 
  arrange(identifiant) %>%
  mutate(libelle_ego = fct_recode(as.character(lien_r),
    "Conjoint-e" = "1", 
    "Enfant" = "2", 
    "Beau fils ou belle fille (enfant du conjoint-e)" = "5", 
    "Frère ou soeur (demi et quasi compris)" = "8", 
    "Neveu ou nièce" = "11", 
    "Belle-soeur ou beau-frère" = "14", 
  )) %>%
  mutate(relation_ego = case_when(
    relation_ego == "EGO" ~ relation_ego, 
    TRUE ~libelle_ego
  )) %>%
  select(identifiant, sexe_x, age_x, relation_ego)

saveRDS(liens, file = "1_data/processed/liens.rds")
saveRDS(menages, file = "1_data/processed/menages.rds")

## Coordonnées

saveRDS(coordonnees, file = "1_data/processed/coordonnees.RDS")

codecommunes <- read_csv("https://www.data.gouv.fr/api/1/datasets/r/7acc46ad-1c79-43d9-8f2d-d0a8ec78c068")
codecommunes$Commune <- paste0(codecommunes$nomCommune, " (", codecommunes$codeDepartement, ")")
codecommunes$codeCommune

BIEF <- BIEF %>% 
  mutate(across(contains("COM"), ~ str_remove(.x, "2024"))) %>%
  mutate(across(contains("COM"),
                ~ codecommunes$Commune[match(.x, codecommunes$codeCommune)]))



saveRDS(BIEF, file = "1_data/processed/BIEF.Rds")


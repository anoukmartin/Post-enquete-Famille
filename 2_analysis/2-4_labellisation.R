# =======================================================================
# 🧩 Script de recodage des variables du questionnaire EF 2025 et ERA (BI) 2025
# =======================================================================

# Chargement des librairies
library(tidyverse)
library(readxl)
library(glue)
library(dplyr)
library(purrr)
library(stringr)
library(questionr)
source("fonctions/match_vars.R")


# -----------------------------------------------------------------------
# 1. Charger les données
# -----------------------------------------------------------------------
# Données du questionnaire (déjà présentes dans RStudio)

EF <- readRDS(file = "1_data/cleaned/EF.rds")
BI <- readRDS(file = "1_data/cleaned/BI.rds") 
liens <- readRDS(file = "1_data/processed/liens.rds")
coordonnees <- readRDS(file = "1_data/processed/coordonnees.rds")


liens <- liens  %>%
  mutate(interrogationId = str_sub(identifiant, 1, 9),
         identifiant = str_sub(identifiant, 10, -1)) 

# Anomalies importantes 

# les deux alexandra
BI[BI$identifiant == "ALEXANDRA 1973-05-09", "identifiant"] <- paste0("ALEXANDRA 1973-05-09", c(" N°1", " N°2"))
EF[EF$identifiant == "ALEXANDRA 1973-05-09", "identifiant"] <- paste0("ALEXANDRA 1973-05-09", c(" N°1", " N°2"))
coordonnees[coordonnees$identifiant == "ALEXANDRA 1973-05-09", "identifiant"] <- paste0("ALEXANDRA 1973-05-09", c(" N°1", " N°2"))
liens[liens$identifiant == "ALEXANDRA 1973-05-09", "identifiant"][1:3] <- "ALEXANDRA 1973-05-09 N°1"
liens[liens$identifiant == "ALEXANDRA 1973-05-09", "identifiant"] <- "ALEXANDRA 1973-05-09 N°2"




# -----------------------------------------------------------------------
# 1. Charger les metadonnées des variables
# -----------------------------------------------------------------------

vars_BI <- readRDS(file = "1_data/cleaned/vars_BI.rds")
vars_EF <- readRDS(file = "1_data/cleaned/vars_EF.rds")

# Vérification
glimpse(vars_EF)
glimpse(vars_BI)



# On fusionne les deux tableau de données (EAR) et (EF)
vars_BI$Source <- "EAR"
vars_EF$Source <- "EF"

vars_all <- bind_rows(vars_BI, vars_EF)
glimpse(vars_all)

temp <- data.frame(Variable = namesBI_tech, 
                   Question = NA, 
                   Modalites = NA, 
                   Valeur = NA,
                   Type = NA, 
                   Filtre = NA, 
                   Qui = NA, 
                   Theme = "Technique", 
                   Source = "EAR")

vars_all <- bind_rows(vars_all, temp)


temp <- data.frame(Variable = namesEF_RP, 
                   Question = NA, 
                   Modalites = NA, 
                   Valeur = NA,
                   Type = NA, 
                   Filtre = NA, 
                   Qui = NA, 
                   Theme = "from EAR", 
                   Source = "EF")
vars_all <- bind_rows(vars_all, temp)

temp <- data.frame(Variable = namesEF_postenq , 
                   Question = c("Post-enquête ?", "Souhaite etre contacté par mail ?", "Souhaite etre contacté par téléphone ?", "Mail", "Telephone"), 
                   Modalites = c("1 - Oui | 2 - Non", rep("1 - Oui | 2 - Non", 2), rep(NA, 2)), 
                   Type = c(rep("Qualitative", 3), rep("Texte libre", 2)),
                   Valeur = c(rep("Boléenne", 3), rep("En clair", 2)),
                   Filtre = NA, 
                   Qui = NA, 
                   Theme = "Post-enquete", 
                   Source = "EF")

vars_all <- bind_rows(vars_all, temp)

temp <- data.frame(Variable = namesEF_tech, 
                   Question = NA, 
                   Modalites = NA, 
                   Valeur = NA,
                   Type = NA, 
                   Filtre = NA, 
                   Qui = NA, 
                   Theme = "Technique", 
                   Source = "EF")
vars_all <- bind_rows(vars_all, temp)

freq(vars_all$Modalites)
# vars_all <- vars_all %>% 
#   mutate(Valeur = if_else(Modalites == "1 - Oui | 2 - Non", "Boléenne", Valeur)) %>%
#   mutate(Type = if_else(Modalites == "1 - Oui | 2 - Non", "Qualitative", Type)) 

BIEF <- left_join(BI, EF, by = "identifiant")
BIEF <- left_join(coordonnees, BIEF)

# On match les noms de variables
anomalie1 <- names(BIEF)[!(names(BIEF) %in% vars_all$Variable)]
anomalie2 <- vars_all$Variable[!(vars_all$Variable %in% names(BIEF))]
 

# Trouver les correspondances
correspondances <- trouver_correspondances(names(BIEF), vars_all$Variable, seuil_similarite = -10) 
# test 
names(BIEF)[!(names(BIEF) %in% correspondances$Variable.x)]

# Afficher les résultats
print(correspondances)

glimpse(correspondances)

correspondances <- correspondances %>%
  group_by(Variable.x) %>%
  slice_max(lev_pond, n = 1, with_ties = FALSE) %>%
  ungroup()
  
correspondances <- correspondances %>%
  mutate(Variable.y = case_when(
    lev_pond <= 0.77 ~ NA, 
    TRUE ~ Variable.y
  )) %>%
  select(Variable.x, Variable.y) %>%
  rename(Variable = Variable.x, Var = Variable.y)
vars_all$i <- row.names(vars_all)

# Tableau de TOUTES LES VARIABLES 
vars_all2 <- left_join(correspondances, vars_all %>% filter(!is.na(Variable)), by = c("Var" = "Variable")) %>%
  mutate(i = as.integer(i)) %>%
  arrange(i) 
orph <- vars_all2 %>%
  filter(is.na(Var)) %>%
  mutate(Source = case_when(
    str_detect(Variable, "[[:lower:]]") ~ "EAR", 
    TRUE ~ "EF"
  )) %>%
  mutate(Question = case_when(
    str_detect(Variable, regex('sexe|sex', ignore_case = T)) ~ "Sexe",
    str_detect(Variable, regex('age', ignore_case = T)) ~ "Age",
    str_detect(Variable, regex('MINEUR', ignore_case = T)) ~ "Est mineur",
    str_detect(Variable, regex('anais|anai|annai', ignore_case = T)) ~ "Année de naissance",
    str_detect(Variable, regex('mnais|mnai', ignore_case = T)) ~ "Mois de naissance",
    str_detect(Variable, regex('jnais|jnai', ignore_case = T)) ~ "Jour de naissance",
    str_detect(Variable, regex('datenai', ignore_case = T)) ~ "Date de naissance",
    str_detect(Variable, regex('lieunai', ignore_case = T)) ~ "Lieu de naissance",
    str_detect(Variable, regex('nai|nais', ignore_case = T)) ~ "Naissance",
    str_detect(Variable, regex('prenom', ignore_case = T)) ~ "Prénom", 
    str_detect(Variable, regex('nom', ignore_case = T)) ~ "Nom de famille", 
    str_detect(Variable, regex('trav', ignore_case = T)) ~ "Travail", 
    str_detect(Variable, regex('sal_ind', ignore_case = T)) ~ "Salarié? indépendant?",
    str_detect(Variable, regex('sal_fp', ignore_case = T)) ~ "Salarié de la fonction publique",
    str_detect(Variable, regex('sal_ent', ignore_case = T)) ~ "Salarié d'une entreprise",
    str_detect(Variable, regex('cont', ignore_case = T)) ~ "Contact", 
    str_detect(Variable, regex('mail', ignore_case = T)) ~ "Mail", 
    str_detect(Variable, regex('tel', ignore_case = T)) ~ "Tel",
    str_detect(Variable, regex('prox_ego', ignore_case = T)) ~ "Habite a proximité d'égo",
    str_detect(Variable, regex('prox_fra', ignore_case = T)) ~ "Habite a proximité des frères/soeurs",
    str_detect(Variable, regex('codegeo', ignore_case = T)) ~ "Code géographique",
    str_detect(Variable, regex('iris', ignore_case = T)) ~ "Code Iris",
    str_detect(Variable, regex('codedepartement', ignore_case = T)) ~ "Code Département",
    str_detect(Variable, regex('rga', ignore_case = T)) ~ "Code RGA",
    str_detect(Variable, regex('rgl', ignore_case = T)) ~ "Code RGL",
  ))

orph
vars_all2 <- vars_all2 %>%
  filter(!is.na(Var)) %>%
  bind_rows(orph)

vars_all2 <- vars_all2 %>%
  mutate(
  Qui = case_when(
    !is.na(Qui) ~ Qui,
    
    
    # ENFLOG + chiffre final → enfantlogementX
    str_detect(Variable, "ENFLOG[0-9]") ~ 
      glue("Enfant logement n°{str_extract(Variable, '[0-9]$')}"),
    
    # ENFAIL + chiffre final → enfantailleurX
    str_detect(Variable, "ENFAIL[0-9]") ~ 
      glue("Enfant ailleur n°{str_extract(Variable, '[0-9]$')}"),
    
    # ENF21 + chiffre final → enfantduconjoint
    str_detect(Variable, "ENF21[0-9]") ~ 
      "Enfant(s) du conjoint",
    
    # PAR + chiffre final → parentX
    str_detect(Variable, "PAR1") ~ 
      glue("Parent n°1"),
    str_detect(Variable, "PAR2") ~ 
      glue("Parent n°2"),
    
    str_ends(Variable, "_C|_C[0-9]|_U|CJT|CONJ|CJ|MARI|PACS|SEPAR|ANNEE_S") ~ 
      glue("Conjoint-e actuel/Dernier-e conjoint-e"), 
    
    str_ends(Variable, "_U1") ~ 
      glue("Premier-e conjoint-e"),
    
    str_ends(Variable, "_U2") ~ 
      glue("Autre conjoint-e"),
    
    str_detect(Variable, "PETIT_ENF") ~ 
      glue("Petit(s) enfant(s)"),
    
    str_detect(Variable, "FRERE|SOEUR") ~ 
      glue("Frère(s) et soeur(s)"),
    
    TRUE ~ "ego"
  )
) 


vars_all2 <- vars_all2 %>%
  mutate(
    Theme = case_when(
      !is.na(Theme) ~ Theme,
      Qui == "ego" & str_detect(Variable, "LANGUE") ~ "Langues",
      Qui == "ego" & str_detect(Variable, "AID_|AIDE_") ~ "Aides",
      Qui == "ego" & str_detect(Variable, "VECU|DEP_PARENT|HEBERG") ~ "Jeunesse", 
      Qui == "ego" & str_detect(Variable, "ENF") ~ "Parentalité",
      Qui == "ego" & str_detect(Variable, "COUPLE|AUT_UN") ~ "Conjugalité",
      Qui == "ego" & str_detect(Variable, "TRAV|EMP") ~ "Travail", 
      Qui == "ego" & str_detect(Variable, "ETU") ~ "Etudes",
      Qui == "ego" & str_detect(Variable, "POSTENQ") ~ "Post-enquête",
      Qui == "ego" & str_detect(Variable, regex("iris|rga|rgl|codegeo|codedepartement", ignore_case = T)) ~ "Localisation",
      Qui == "ego" & str_detect(Variable, "id|ID|Id|IND|QUEST|VAL") ~ "Technique",
    Qui == "ego" ~ "id"
  ))

vars_all2


vars_all2$Source[is.na(vars_all2$Source)]
vars_all2$Source <- vars_all2$Source %>% fct_relevel("EAR", "EF")
vars_all2$Qui[is.na(vars_all2$Qui)]
vars_all2$Qui <- vars_all2$Qui %>% fct_relevel(
    "ego", 
    "Conjoint-e actuel/Dernier-e conjoint-e", "Premier-e conjoint-e",
    "Autre conjoint-e", 
    "Enfant logement n°1", "Enfant logement n°2","Enfant logement n°3",
    "Enfant logement n°4", "Enfant logement n°5", "Enfant logement n°6",
    "Enfant logement n°7", "Enfant logement n°8", 
    "Enfant ailleur n°1", "Enfant ailleur n°2", "Enfant ailleur n°3",
    "Enfant ailleur n°4", "Enfant ailleur n°5", "Enfant ailleur n°6",
    "Enfant ailleur n°7", "Enfant ailleur n°8", 
    "parent1", "parent2", "Parent n°1", "Parent n°2", 
    "Frère(s) et soeur(s)", "Petit(s) enfant(s)"
  )
vars_all2$Theme[is.na(vars_all2$Theme)] <- "Autre"
vars_all2$Theme <- vars_all2$Theme %>% fct_relevel(
    "id", "naissance", "nationalité", "résidence 1er janvier",
    "residence études", "residence alternée", "emploi", "lieu travail",
    "profession", "Travail", "santé",  "Conjugalité", "Parentalité", "Etudes", "études", "couple",
    "Jeunesse", "Aides", "Langues", "Autre"
  )


vars_all2 <- vars_all2 %>%
  arrange(Source, Qui, Theme)
vars_all2$k <- row.names(vars_all2)
vars_all2
  
# -----------------------------------------------------------------------
# 2. Nettoyer les Modalites pour préparation du recodage
# -----------------------------------------------------------------------
# Exemple de format attendu dans ton fichier :
# "1 - Oui | 2 - Non | 3 - NSP"

# On va transformer cela en liste utilisable par R
vars_clean <- vars_all2 %>%
  filter(Variable %in% names(BIEF)) %>%
  #filter(!is.na(Modalites)) %>%
  mutate(
    Modalites = str_split(Modalites, "\\|"),
    Modalites = map(Modalites, ~str_trim(.x)), 
    Modalites = if_else(Modalites == "NA", NA, Modalites)
  ) 
head(vars_clean)
glimpse(vars_clean)
# glimpse(BIEF)


# ---------------------------------------------------------------------
# T3. Recodage auto labelled ------------------------------------------
# ---------------------------------------------------------------------

library(labelled)
build_labels <- function(modalites) {
  tibble(raw = modalites) %>%
    mutate(
      code = str_extract(raw, "^[0-9]+") |> as.integer(),
      label = str_remove(raw, "^[0-9]+\\s*[–-]\\s*")
    ) %>%
    select(code, label) %>%
    { setNames(.$code, .$label) }
}

build_labels(modalites = c("1 – Masculin", "2 – Féminin"))


apply_dictionary <- function(data, dico, varname) {
  
  if (!varname %in% names(data)) return(data)
  
  meta <- dico %>%
    filter(Variable == varname) %>%
    slice(1)
  
  if (nrow(meta) == 0) return(data)
  
  # 1. label de variable et attributs
  var_label(data[[varname]]) <- as.character(meta$Question)
  attr(data[[varname]], "Type") <- as.character(meta$Type)
  attr(data[[varname]], "Valeur") <- as.character(meta$Valeur)
  cat("# [", varname, "] ", var_label(data[[varname]]), " : ", 
      attr(data[[varname]], "Type"), ", ", attr(data[[varname]], "Valeur"), "\n")
  
  # 2. value labels si qualitatives
  if (!is.null(meta$Modalites[[1]])) {
    
    x <- data[[varname]]
    
    if (all(is.na(x)) || is.logical(x)) return(data)
    
    if (!is.numeric(x)) data[[varname]] <- as.numeric(x)
    
    mods <- meta$Modalites[[1]]
    if (!is.null(mods) && length(mods) > 0) {
      labels <- build_labels(mods)
      if (length(labels) > 0) {
        val_labels(data[[varname]]) <- labels
        }
    }
    
    print(freq(data[[varname]]))
    
  } else {
    print(summary(data[varname]))
  }
  
  return(data)
}



vars_to_process <- intersect(vars_clean$Variable, names(BIEF))
length(vars_to_process)

BIEF_lab <- BIEF
BIEF_lab[BIEF_lab == "" & !is.na(BIEF_lab)] <- NA
for (v in vars_to_process) {
  BIEF_lab <- apply_dictionary(BIEF_lab, vars_clean, v)
}




dir.create("1_data/labelled")
saveRDS(BIEF_lab, "1_data/labelled/BIEF_lab.rds")

## Recodage auto terminé, donc on peut aussi faire un export en recodages en claire

BIEF_rec <- to_factor(BIEF_lab, levels = "p", sort_levels = "v")
freq(BIEF_rec$dipl)

# res <- sapply(names(BIEF_rec), function(v) {
#   lab <- var_label(BIEF_rec[[v]])
#   if (is.null(lab) || is.na(lab)) {lab <- "" } else {
#     lab <- paste0(" ", lab)}
#   paste0("[", v, "]", lab)
# }, USE.NAMES = FALSE)
# names(BIEF_rec) <- res
# var_label(BIEF_rec) <- NULL


saveRDS(BIEF_rec, "1_data/labelled/BIEF_rec.rds")

saveRDS(vars_all2, "1_data/cleaned/vars_BIEF.rds")

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

EF <- readRDS(file = "1_data/processed/EF.rds")
BI <- readRDS(file = "1_data/processed/BI.rds") 
liens <- readRDS(file = "1_data/processed/liens.rds")
coordonnees <- readRDS(file = "1_data/processed/coordonnees.rds")


# Uniformisation des noms de variables
names(EF)


# Unifromisation des identifiant invifiduels 
BI <- BI  %>%
  mutate(interrogationId = str_sub(identifiant, 1, 9),
         identifiant = str_sub(identifiant, 10, -1))

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

vars_BI <- readRDS(file = "1_data/processed/vars_EAR.rds")
vars_EF <- readRDS(file = "1_data/processed/vars_EF.rds")

# Vérification
glimpse(vars_EF)
glimpse(vars_BI)


# 2. quelques améliorations à la main pour améliorer l'appariment 

# Certaines variables de sexe sont codées à l'envers pour les femmes et les hommes dans l'EF, on doit corriger cela
gender_var <- vars_EF %>%
  filter(str_ends(Variable, "H|F")) %>%
  filter(str_detect(Variable, "SEXE")) 
# On va changer les valeurs pour ces variables puis on corrigera le dictionnaire 
gender_var$Variable
names(BI)[names(BI) %in% names(EF)]
EF <- left_join(EF, BI[, c("identifiant", "sexe")], by = "identifiant")
EF$SEXE_C
EF <- EF %>%
  mutate(SEXE_C = case_when(
    sexe == "1" & SEXE_C == "1" ~  2, 
    sexe == "1" & SEXE_C == "2" ~ 1, 
    TRUE ~ SEXE_C)) %>%
  mutate(SEXE_U1 = case_when(
    sexe == "1" & SEXE_U1 == "1" ~  2, 
    sexe == "1" & SEXE_U1 == "2" ~ 1, 
    TRUE ~ SEXE_U1)) %>%
  mutate(SEXE_U2 = case_when(
    sexe == "1" & SEXE_U2 == "1" ~  2, 
    sexe == "1" & SEXE_U2 == "2" ~ 1, 
    TRUE ~ SEXE_U2))

gender_var$Modalites

vars_EF <- vars_EF %>%
  mutate(Modalites = case_when(
    Variable %in% gender_var$Variable ~ "1 - Un homme | 2 - Une femme", 
    TRUE ~ Modalites
  ))
EF$sexe <- NULL
rm(gender_var)
EF$ADOPT_ENFLOG1





#
names(EF)[str_starts(names(EF), "RP")] <- str_remove(names(EF)[str_starts(names(EF), "RP")], "RP")




# On fusionne les deux tableau de données (EAR) et (EF)
vars_BI$Source <- "EAR"
vars_EF$Source <- "EF"

vars_all <- bind_rows(vars_BI, vars_EF)

vars_all <- vars_all %>%
  mutate(Valeur = if_else(Modalites == "1 - Oui | 2 - Non", "Boléenne", Valeur)) %>%
  mutate(Type = if_else(Modalites == "1 - Oui | 2 - Non", "Qualitative", Valeur)) 
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
  slice_max(composite_score, n = 1, with_ties = FALSE) %>%
  ungroup()
  
correspondances <- correspondances %>%
  mutate(Variable.y = case_when(
    composite_score <= 0.02 ~ NA, 
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
    str_detect(Variable, "[lower]") ~ "EAR", 
    TRUE ~ "EF"
  )) %>%
  mutate(Question = case_when(
    str_detect(Variable, regex('sexe|sex', ignore_case = T)) ~ "Sexe",
    str_detect(Variable, regex('age', ignore_case = T)) ~ "Age",
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
    
    str_ends(Variable, "_C|_C[0-9]|_U|CJT|CONJ|CJ|MARI|PACS") ~ 
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
  
  # 1. label de variable (toujours)
  var_label(data[[varname]]) <- as.character(meta$Question)
  attr(data[[varname]], "Type") <- as.character(meta$Type)
  attr(data[[varname]], "Valeur") <- as.character(meta$Valeur)
  print(paste0("# [", varname, "] ", var_label(data[[varname]]), " : ", 
               attr(data[[varname]], "Type"), ", ", attr(data[[varname]], "Valeur")))
  
  # 2. value labels uniquement si applicable
  if (!is.na(meta$Type) & meta$Type == "Qualitative") {
    
    x <- data[[varname]]
    
    # cas problématique : tout NA ou logique
    if (all(is.na(x)) || is.logical(x)) {
      return(data)
    }
    
    # modalités présentes
    mods <- meta$Modalites[[1]]
    if (is.null(mods) || length(mods) == 0) {
      return(data)
    }
    
    labels <- build_labels(mods)
    
    if (length(labels) > 0) {
      data[[varname]] <- labelled(
        x,
        labels = labels,
        label = var_label(x)
      )
    }
    print(freq(data[[varname]]))
  } else {
  print(summary(data[varname]))}
  return(data)
}



vars_to_process <- intersect(vars_clean$Variable, names(BIEF))
length(vars_to_process)

BIEF_rec <- BIEF
for (v in vars_to_process) {
  BIEF_rec <- apply_dictionary(BIEF_rec, vars_clean, v)
}


## Recodage auto terminé : 



# ------------------------------------------------------------------------------
# 4. Codes géographiques -------------------------------------------------------
# ------------------------------------------------------------------------------

## On ajoute des labels pour les variables géo

# URL du shapefile ZIP officiel
url <- "https://data.iledefrance.fr/api/explore/v2.1/catalog/datasets/communes-et-arrondissements-municipaux-annee-en-cours/exports/shp?lang=fr&timezone=Europe%2FBerlin&use_labels=true"

# Chemin temporaire pour télécharger le zip
temp <- tempfile(fileext = ".zip")
download.file(url, temp)

# Décompresser le zip dans un dossier temporaire
unzip_dir <- tempdir()
unzip(temp, exdir = unzip_dir)

# Trouver le fichier .shp dans le dossier décompressé
shp_file <- list.files(unzip_dir, pattern = "\\.shp$", full.names = TRUE)

# Lire le shapefile
idf_sf <- st_read(shp_file[1])

# Visualiser
carte_idf <- ggplot(idf_sf) +
  geom_sf(data = idf_sf, fill = "grey95", color = "grey60") +
  coord_sf(expand = FALSE) +
  theme_void() 
carte_idf






## Liens

egos <- BIEF_rec %>%
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

saveRDS(coordonnees, file = "1_data/processed/coordonnees.rds")

codecommunes <- read_csv("https://www.data.gouv.fr/api/1/datasets/r/7acc46ad-1c79-43d9-8f2d-d0a8ec78c068")
codecommunes$Commune <- paste0(codecommunes$nomCommune, " (", codecommunes$codeDepartement, ")")
codecommunes$codeCommune

BIEF <- BIEF %>% 
  mutate(across(contains("COM"), ~ str_remove(.x, "2024"))) %>%
  mutate(across(contains("COM"),
                ~ codecommunes$Commune[match(.x, codecommunes$codeCommune)]))


# -----------------------------------------------------------------------
# 3. Créer dynamiquement les instructions de recodage
# -----------------------------------------------------------------------
# Exemple de fonction qui génère du code R pour recoder chaque variable

generate_recode <- function(var_name, modalities) {
  if (length(modalities) == 0) return(NULL)
  
  # Extraire les codes et libellés
  codes <- str_extract(modalities, "^[0-9]+")
  labels <- str_remove(modalities, "^[0-9]+\\s*[-–]\\s*")
  
  # Construire la syntaxe recode()
  recode_pairs <- paste0('"', codes, '" = "', labels, '"' ,collapse = ", ")
  
  glue::glue('## {var_name}
            freq(BIEF${var_name})
            BIEF <- BIEF %>% mutate({var_name} = recode(as.character({var_name}), {recode_pairs}))
            freq(BIEF${var_name})
            
             ')
}

# -----------------------------------------------------------------------
# 4. Générer tout le script de recodage automatiquement
# -----------------------------------------------------------------------
library(glue)

recode_code <- map2_chr(vars_clean$Variable, vars_clean$Modalites, generate_recode)

# Afficher les premières lignes du script généré
cat(recode_code[1:20], sep = "\n\n\n")

# -----------------------------------------------------------------------
# 5. (Optionnel) Écrire le script dans un fichier .R
# -----------------------------------------------------------------------
writeLines(recode_code, "2_analysis/2-2_recode_BIEF_2025.R")

# Tu pourras ensuite exécuter ce fichier dans RStudio :
source("2_analysis/2-2_recode_BIEF_2025.R", verbose = T)
# -----------------------------------------------------------------------

# Après exécution, toutes les variables codées numériquement auront des libellés texte

saveRDS(BIEF, file = "1_data/processed/BIEF.rds")
saveRDS(BIEF_rec, file = "1_data/processed/BIEF_recodé.rds")
saveRDS(vars_all2, file = "1_data/processed/vars_all.rds")
saveRDS(liens, file = "1_data/processed/liens.rds")
saveRDS(coordonnees, file = "1_data/processed/coordonnees.rds")

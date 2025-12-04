# =======================================================================
# 🧩 Script de recodage des variables du questionnaire EF 2025
# =======================================================================

# Chargement des librairies
library(tidyverse)
library(readxl)

# -----------------------------------------------------------------------
# 1. Charger les données
# -----------------------------------------------------------------------
# Données du questionnaire (déjà présentes dans RStudio)

EF <- readRDS(file = "1_data/processed/EF.rds")
BI <- readRDS(file = "1_data/processed/BI.rds")
names(EF)
names(BI)

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

EF <- left_join(EF, BI[, c("identifiant", "sexe")])
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

# On ajoute des variables qui manque 
head(vars_EF)
vars_EF <- bind_rows(vars_EF, 
                 data.frame(Variable = c("AG_ENFLOG1", "AG_ENFAIL1"), 
                            Question = c("Age de l'enfant vivant dans le logement",
                                         "Age de l'enfant vivant dans le logement"),
                            Modalites = c(NA, NA),
                            Type = c("Quantitative", "Quantitative")))


head(vars_BI)
temp <- rbind(vars_BI[1, ], vars_BI[1, ], vars_BI[1, ])
temp$Variable <- c("jnai", "mnai", "anai")
temp$Question <- c("jour de naissance", "mois de naissance", "année de naissance")

vars_BI <- rbind(vars_BI, temp)
rm(temp)


names(EF)[str_starts(names(EF), "RP")] <- str_remove(names(EF)[str_starts(names(EF), "RP")], "RP")

# On fusionne les deux tableau de données (EAR) et (EF)
vars_BI$Source <- "EAR"
vars_EF$Source <- "EF"

vars_all <- bind_rows(vars_BI, vars_EF)
BIEF <- left_join(BI, EF, by = "identifiant")

# On match les noms de variables
anomalie1 <- names(BIEF)[!(names(BIEF) %in% vars_all$Variable)]
anomalie2 <- vars_all$Variable[!(vars_all$Variable %in% names(BIEF))]
 


# Trouver les correspondances
correspondances <- trouver_correspondances(names(BIEF), vars_all$Variable, seuil_similarite = 0.6)

# Afficher les résultats
print(correspondances)
correspondances <- correspondances %>%
  filter(lv_similarity <=1)%>%
  select(Variable.x, Variable.y) %>%
  rename(Variable = Variable.x, Var = Variable.y)
vars_all$i <- row.names(vars_all)

# Tableau de TOUTES LES VARIABLES 
vars_all2 <- left_join(correspondances, vars_all, by = c("Var" = "Variable")) %>%
  mutate(i = as.integer(i)) %>%
  arrange(i)

anomalie1 <- names(BIEF)[!(names(BIEF) %in% vars_all2$Variable)]

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
    str_detect(Variable, "PAR[0-9]") ~ 
      glue("Parent n°{str_extract(Variable, '[0-9]$')}"),
    
    str_ends(Variable, "_C|_C[0-9]|_U") ~ 
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
      Qui == "ego" & str_detect(Variable, "AIDE_") ~ "Aides",
      Qui == "ego" & str_detect(Variable, "VECU|DEP_PARENT|HBERG") ~ "Jeunesse", 
      Qui == "ego" & str_detect(Variable, "ENF") ~ "Parentalité",
      Qui == "ego" & str_detect(Variable, "COUPLE|MARI|PACS|AUT_UN") ~ "Conjugalité",
      Qui == "ego" & str_detect(Variable, "TRAV|EMP") ~ "Travail", 
      Qui == "ego" & str_detect(Variable, "ETU") ~ "Etudes",
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
    "parent1", "parent2", "Parent n°1", "Parent n°2", "Parent n°3", "Parent n°NA",
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
  
# -----------------------------------------------------------------------
# 2. Nettoyer les Modalites pour préparation du recodage
# -----------------------------------------------------------------------
# Exemple de format attendu dans ton fichier :
# "1 - Oui | 2 - Non | 3 - NSP"

# On va transformer cela en liste utilisable par R
vars_clean <- vars_all2 %>%
  filter(Variable %in% names(BIEF)) %>%
  filter(!is.na(Modalites)) %>%
  mutate(
    Modalites = str_split(Modalites, "\\|"),
    Modalites = map(Modalites, ~str_trim(.x))
  )
head(vars_clean)

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
  
  glue::glue('BIEF <- BIEF %>% mutate({var_name} = recode(as.character({var_name}), {recode_pairs}))')
}

# -----------------------------------------------------------------------
# 4. Générer tout le script de recodage automatiquement
# -----------------------------------------------------------------------
library(glue)

recode_code <- map2_chr(vars_clean$Variable, vars_clean$Modalites, generate_recode)

# Afficher les premières lignes du script généré
cat(recode_code[1:20], sep = "\n\n")

# -----------------------------------------------------------------------
# 5. (Optionnel) Écrire le script dans un fichier .R
# -----------------------------------------------------------------------
writeLines(recode_code, "2_analysis/2-2_recode_BIEF_2025.R")

# Tu pourras ensuite exécuter ce fichier dans RStudio :
source("2_analysis/2-2_recode_BIEF_2025.R")
# -----------------------------------------------------------------------

# Après exécution, toutes les variables codées numériquement auront des libellés texte

saveRDS(BIEF, file = "1_data/processed/BIEF.Rds")
saveRDS(vars_all2, file = "1_data/processed/vars_all.Rds")

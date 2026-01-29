# =======================================================================
# 🧩 Script de recodage des variables du questionnaire EF 2025
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

names(EF)
names(BI)
names(BI) <- str_remove(names(BI), "_x")
names(BI) <- str_remove(names(BI), "_merge")
BI <- BI  %>%
  mutate(interrogationId = str_sub(identifiant, 1, 9),
         identifiant = str_sub(identifiant, 10, -1))

liens <- liens  %>%
  mutate(interrogationId = str_sub(identifiant, 1, 9),
         identifiant = str_sub(identifiant, 10, -1)) 

# les deux alexandra
BI[BI$identifiant == "ALEXANDRA 1973-05-09", "identifiant"] <- paste0("ALEXANDRA 1973-05-09", c(" N°1", " N°2"))
EF[EF$identifiant == "ALEXANDRA 1973-05-09", "identifiant"] <- paste0("ALEXANDRA 1973-05-09", c(" N°1", " N°2"))
coordonnees[coordonnees$identifiant == "ALEXANDRA 1973-05-09", "identifiant"] <- paste0("ALEXANDRA 1973-05-09", c(" N°1", " N°2"))
liens[liens$identifiant == "ALEXANDRA 1973-05-09", "identifiant"][1:3] <- "ALEXANDRA 1973-05-09 N°1"
liens[liens$identifiant == "ALEXANDRA 1973-05-09", "identifiant"] <- "ALEXANDRA 1973-05-09 N°2"

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
EF$ADOPT_ENFLOG1
# On ajoute des variables qui manque 
head(vars_EF)


temp <- vars_EF[vars_EF$Variable == "AIDE_APPORT", ]
temp$Question <- temp$Question %>%
  str_replace("\\(", "?") %>%
  str_sub(1, 173) 
temp <- temp[rep(1, 5), ]
temp$Variable <- paste0(temp$Variable, 1:5)
temp$Question <- paste(temp$Question, c("Oui, une aide pour les tâches quotidiennes", 
                                         "Oui, une aide financière ou matérielle", 
                                         "Oui, un soutien moral",
                                         "Oui, vous êtes tuteur / tutrice ou curateur/curatrice",
                                         "Non, aucune aide de ce type"))
temp$Modalites <- "1 - Oui | 2 - Non"

vars_EF <- bind_rows(vars_EF %>%
                       filter(Variable != "AIDE_APPORT"), 
                     temp)

temp$Variable <- paste0("QUI_AID_APP_", 1:5)
temp <- temp[-5, ]
temp$Question <- temp$Question %>%
  str_sub( 175, -1)
temp$Question <- paste0("A qui ? ", temp$Question)
temp <- temp[rep(1:4, 5+4+4+3), ]
temp <- temp %>% arrange(Variable)
temp$Variable <- paste0(temp$Variable, rep(c(rep("A", 5), rep("B", 4), rep("C", 4), rep("D", 3)), 4))
temp$Variable <- paste0(temp$Variable, rep(c(1:4, "D", 1:3, "D", 1:3, "D", 1:2, "D"), 4))
temp <- temp %>%
  mutate(Question = case_when(
    str_ends(Variable, "1") ~ str_glue("{Question} Vos parents/votre parent"), 
    str_ends(Variable, "D") ~ str_glue("{Question} Autre : en clair"), 
    str_ends(Variable, "A2|B2") ~ str_glue("{Question} Votre conjoint-e"), 
    str_ends(Variable, "A3|C2") ~ str_glue("{Question} Votre/vos enfant(s)"), 
    str_ends(Variable, "A4|B3|C3|D2") ~ str_glue("{Question} Un autre membre de votre famille")
  ))

temp <- temp %>%
  mutate(Modalites = if_else(
    str_ends(Variable, "D"), NA, Modalites))

vars_EF <- bind_rows(vars_EF, temp)


## AIde recue 

temp <- vars_EF[vars_EF$Variable == "AIDE_RECUE", ]
temp$Question <- temp$Question %>%
  str_replace("\\(", "?") %>%
  str_sub(1, 204) 
EF$AIDE_RE
temp <- temp[rep(1, 4), ]
temp$Variable <- paste0(temp$Variable, 1:4)

temp$Question <- paste(temp$Question, c("Oui, une aide pour les tâches quotidiennes", 
                                        "Oui, une aide financière ou matérielle", 
                                        "Oui, un soutien moral",
                                        "Non, aucune aide de ce type"))
temp$Modalites <- "1 - Oui | 2 - Non"

vars_EF <- bind_rows(vars_EF %>%
                       filter(Variable != "AIDE_RECUE"), 
                     temp)

temp$Variable <- paste0("QUI_AID_REC_", 1:4)
temp <- temp[-4, ]
temp$Question <- temp$Question %>%
  str_sub( 206, -1)
temp$Question <- paste0("De qui ? ", temp$Question)
temp <- temp[rep(1:3, 5+4+4+3), ]
temp <- temp %>% arrange(Variable)
temp$Variable <- paste0(temp$Variable, rep(c(rep("A", 5), rep("B", 4), rep("C", 4), rep("D", 3)), 3))
temp$Variable <- paste0(temp$Variable, rep(c(1:4, "D", 1:3, "D", 1:3, "D", 1:2, "D"), 3))
temp <- temp %>%
  mutate(Question = case_when(
    str_ends(Variable, "1") ~ str_glue("{Question} Vos parents/votre parent"), 
    str_ends(Variable, "D") ~ str_glue("{Question} Autre : en clair"), 
    str_ends(Variable, "A2|B2") ~ str_glue("{Question} Votre conjoint-e"), 
    str_ends(Variable, "A3|C2") ~ str_glue("{Question} Votre/vos enfant(s)"), 
    str_ends(Variable, "A4|B3|C3|D2") ~ str_glue("{Question} Un autre membre de votre famille")
  ))

temp <- temp %>%
  mutate(Modalites = if_else(
    str_ends(Variable, "D"), NA, Modalites))

vars_EF <- bind_rows(vars_EF, temp)


vars_EF <- bind_rows(vars_EF, 
     data.frame(Variable = c("AG_ENFLOG1", 
                                         "AG_ENFAIL1", 
                                         "ADOPT_ENFLOG1", 
                                         "ADOPT_ENFAIL1", 
                                         "ANADOPT_ENFLOG1", 
                                         "ANADOPT_ENFAIL1", 
                                         "PNAI_PAR1",
                                         "TYP_PLACE1"), 
                            Question = c("Age de l'enfant vivant dans le logement",
                                         "Age de l'enfant vivant ailleur", 
                                         "Adoption de l'enfant vivant dans le logement",
                                         "Adoption de l'enfant vivant ailleur", 
                                         "Année de l'adoption de l'enfant vivant dans le logement", 
                                         "Année de l'adoption de l'enfant vivant ailleur", 
                                         "Pays de naissance du parent", 
                                         "Type de placement"),
                            Modalites = c(NA, NA, rep("1 - Oui | 2 - Non", 2), NA, NA, NA, NA),
                            Type = c("Quantitative", "Quantitative", "Qualitative", "Qualitative", 
                                     "Quantitative", "Quantitative", "Qualitative",  "Qualitative" 
                                     )))


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
      Qui == "ego" & str_detect(Variable, "id|ID|Id") ~ "Technique",
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
saveRDS(vars_all2, file = "1_data/processed/vars_all.rds")
saveRDS(liens, file = "1_data/processed/liens.rds")
saveRDS(coordonnees, file = "1_data/processed/coordonnees.rds")

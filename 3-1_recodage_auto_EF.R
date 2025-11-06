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
names(EF)
names(BI)

# Vérification
glimpse(vars_EF)
glimpse(vars_BI)

# Certaines variables de sexe sont codées à l'envers pour les femmes et les hommes. 
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

gender_var$Modalités

vars_EF <- vars_EF %>%
  mutate(Modalités = case_when(
    Variable %in% gender_var$Variable ~ "1 - Un homme | 2 - Une femme", 
    TRUE ~ Modalités
  ))

rm(gender_var)
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
vars_all2 <- left_join(correspondances, vars_all, by = c("Var" = "Variable")) %>%
  mutate(i = as.integer(i)) %>%
  arrange(i)

anomalie1 <- names(BIEF)[!(names(BIEF) %in% vars_all$Variable)]


# -----------------------------------------------------------------------
# 2. Nettoyer les modalités pour préparation du recodage
# -----------------------------------------------------------------------
# Exemple de format attendu dans ton fichier :
# "1 - Oui | 2 - Non | 3 - NSP"

# On va transformer cela en liste utilisable par R
vars_clean <- vars_all %>%
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
writeLines(recode_code, "recode_BIEF_2025.R")

# Tu pourras ensuite exécuter ce fichier dans RStudio :
source("recode_BIEF_2025.R")
# -----------------------------------------------------------------------

# Après exécution, toutes les variables codées numériquement auront des libellés texte

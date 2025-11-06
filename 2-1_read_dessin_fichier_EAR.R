library(readODS)

## Import doc 
# Exemple : chemin vers ton fichier CSV (à adapter)
file_path <- "Thèse/Post-enquête/doc_donnees"


## questionnaire EF
doc <- "dessin_fichier_EAR.ods"
# Construction de l'URL complète
url <- paste0(base_url, "/", file_path, "/", doc)
# Requête WebDAV avec authentification
res <- GET(url, authenticate(user, app_pwd))
# Vérifier que la requête a réussi
stop_for_status(res)

# créer un fichier temporaire
tmpfile <- tempfile(fileext = ".ods")

# écrire le contenu de res dans ce fichier
writeBin(res$content, tmpfile)

vars_EAR <- read_ods(tmpfile, sheet = 1)
names(vars_EAR)
vars_BI <- vars_EAR %>%
  filter(`Présence dans la table... \n(BDD Omer)` == "bulletinindividuels")

vars_BI <- vars_BI  %>%
  rename(
    Variable = `Nom de la variable \nen BDD`,
    Question = `Intitulé de la question`,
    Modalites = `Modalités de réponse`,
    Remarques = `Remarques, filtres, redirections, etc.`
  ) %>%
  mutate(

    # Extraire les modalités sous forme de liste séparée par "|"
    Modalites = ifelse(Modalites == "" | is.na(Modalites) | str_detect(Modalites, "suggester"), NA, 
                       str_replace_all(Modalites, "\\n", " | ")),
    Modalites = str_replace_all(Modalites, "True", "1"), 
    Modalites = str_replace_all(Modalites, "False", "2"),
    # Supprimer les retours à la ligne dans Modalites et Question
    Question = str_squish(Question),
    Modalites = str_squish(Modalites),
    
    # Déterminer Type (Quantitative / Qualitative) automatiquement
    Type = case_when(
      str_detect(tolower(Variable), "nb|age|annee|quant|cbenf|surf") ~ "Quantitative",
      TRUE ~ "Qualitative"
    ),
    
    # Déterminer Valeur selon le type
    Valeur = case_when(
      str_detect(tolower(Variable), "annee") ~ "Année",
      str_detect(tolower(Variable), "age") ~ "Âge (entier)",
      str_detect(tolower(Variable), "nb|cbenf|surf") ~ "Nombre entier",
      Type == "Qualitative" ~ "Catégorielle",
      TRUE ~ ""
    ),
    
    # Filtre (on met les remarques si elles contiennent "Posée si")
    Filtre = ifelse(str_detect(Remarques, "Posée si"), Remarques, "")
  ) %>%
  select(Variable, Question, Modalites, Type, Valeur, Filtre)

rm(res, tmpfile, doc)

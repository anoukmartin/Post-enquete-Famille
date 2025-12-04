# --------------------------------------------------------------
# 0‑2_data_import_ods.R
# Importation du questionnaire EF depuis le serveur WebDAV
# (fichier ODS) et création d’une table de métadonnées nettoyée
# --------------------------------------------------------------

# ------------------------------------------------------------------
# 1. Packages -------------------------------------------------------
# ------------------------------------------------------------------
suppressPackageStartupMessages({
  library(httr)          # requêtes HTTP / WebDAV
  library(readODS)      # lecture de fichiers .ods
  library(tidyverse)    # dplyr, stringr, etc.
})

# ------------------------------------------------------------------
# 2. Fonction de logging --------------------------------------------
# ------------------------------------------------------------------
log_message <- function(msg, level = "INFO") {
  stamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  cat(sprintf("[%s] [%s] %s\n", stamp, level, msg))
}

# ------------------------------------------------------------------
# 3. Configuration --------------------------------------------------
# ------------------------------------------------------------------
config <- list(
  base_url   = "https://sdrive.cnrs.fr/remote.php/dav/files/1426185",
  file_path  = "Thèse/Post-enquête/doc_donnees",
  ods_file   = "dessin_fichier_EAR.ods",
  # noms sous lesquels on sauvegardera le résultat
  out_name   = "vars_EAR",
  # variables d’environnement contenant les identifiants WebDAV
  user_env   = "USER_SSPCLOUD",
  pwd_env    = "MDP_SSPCLOUD"
)

# ------------------------------------------------------------------
# 4. Fonction d’import ODS sécurisée --------------------------------
# ------------------------------------------------------------------
import_ods_webdav <- function(url, user, pwd, ods_name) {
  tryCatch({
    
    log_message(paste0("Début de l’import du fichier ODS : ", ods_name))
    
    # 4.1 Requête GET avec authentification
    resp <- GET(url, authenticate(user, pwd))
    stop_for_status(resp)               # lève une erreur si le code HTTP >=400
    
    # 4.2 Sauvegarde temporaire du contenu
    tmp <- tempfile(fileext = ".ods")
    writeBin(content(resp, "raw"), tmp)
    
    # 4.3 Lecture du premier sheet
    df <- read_ods(tmp, sheet = 1)
    
    log_message(paste0("Lecture du fichier ODS terminée (", nrow(df), " lignes)"))
    return(df)
    
  }, error = function(e) {
    log_message(
      paste0("Erreur lors de l’import du fichier ODS '", ods_name,
             "' : ", e$message),
      level = "ERROR"
    )
    return(NULL)
  })
}

# ------------------------------------------------------------------
# 5. Lancement de l’import -------------------------------------------
# ------------------------------------------------------------------
ods_url <- file.path(
  config$base_url,
  config$file_path,
  config$ods_file
)

ods_raw <- import_ods_webdav(
  url   = ods_url,
  user  = Sys.getenv(config$user_env),
  pwd   = Sys.getenv(config$pwd_env),
  ods_name = config$ods_file
)

if (is.null(ods_raw)) {
  log_message("Importation du fichier ODS impossible → arrêt du script", "ERROR")
  stop("Importation ODS échouée")
}

# ------------------------------------------------------------------
# 6. Nettoyage et transformation du tableau ------------------------
# ------------------------------------------------------------------
names(ods_raw)
vars_EAR <- ods_raw %>% 
  # 6.1 Sélection des variables qui figurent dans la table “bulletinindividuels”
  filter(`Présence dans la table...\n(BDD Omer)` == "bulletinindividuels") %>%
  # 6.2 Renommage des colonnes utiles
  rename(
    Variable   = `Nom de la variable\nen BDD`,
    Question   = `Intitulé de la question`,
    Modalites  = `Modalités de réponse`,
    Remarques  = `Remarques, filtres, redirections, etc.`,
    Qui = `qui`,
    Theme = `theme`
  ) %>%
  # 6.3 Nettoyage et enrichissement
  mutate(
    # 6.3.1 Nettoyage des modalités
    Modalites = case_when(
      Modalites == "" | is.na(Modalites) | str_detect(Modalites, "suggester") ~ NA_character_,
      TRUE ~ str_replace_all(Modalites, "\\n", " | ")
    ),
    Modalites = str_replace_all(Modalites, "True",  "1"),
    Modalites = str_replace_all(Modalites, "False", "2"),
    Modalites = str_squish(Modalites),
    
    # 6.3.2 Nettoyage de la question
    Question = str_squish(Question),
    
    
    
    # 6.3.3 Détermination automatique du type
    Type = case_when(
      str_detect(tolower(Variable), "nb|age|annee|quant|cbenf|surf") ~ "Quantitative",
      TRUE                                                   ~ "Qualitative"
    ),
    
    # 6.3.4 Attribution d’une description de valeur selon le type
    Valeur = case_when(
      str_detect(tolower(Variable), "annee")          ~ "Année",
      str_detect(tolower(Variable), "age")           ~ "Âge (entier)",
      str_detect(tolower(Variable), "nb|cbenf|surf") ~ "Nombre entier",
      Type == "Qualitative"                          ~ "Catégorielle",
      TRUE                                            ~ NA_character_
    ),
    
    # 6.3.5 Extraction du filtre (si présent dans les remarques)
    Filtre = if_else(str_detect(Remarques, "Posée si"), Remarques, NA_character_)
  ) %>%
  select(Variable, Question, Modalites, Type, Valeur, Filtre, Qui, Theme)

log_message(paste0("Métadonnées EAR prêtes (", nrow(vars_EAR), " variables)"))

# ------------------------------------------------------------------
# 7. Sauvegarde ------------------------------------------------------
# ------------------------------------------------------------------
out_path <- file.path("1_data/processed", paste0(config$out_name, ".rds"))
saveRDS(vars_EAR, file = out_path)
log_message(paste0("Objet sauvegardé dans ", out_path))

# ------------------------------------------------------------------
# 8. Nettoyage de l’environnement ------------------------------------
# ------------------------------------------------------------------
rm(list = c("ods_raw", "ods_url"))
log_message("Script d’import ODS terminé avec succès")

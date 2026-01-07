# 0_setup/0-1_data_import.R
# Script d'importation des données depuis WebDAV
# Version améliorée avec gestion des erreurs et documentation

# Packages nécessaires
library(httr)
library(tidyverse)

# Fonction de logging simple
log_message <- function(message, level = "INFO") {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  cat(sprintf("[%s] [%s] %s\n", timestamp, level, message))
}

# Configuration
config <- list(
  base_url = "https://sdrive.cnrs.fr/remote.php/dav/files/1426185",
  file_path = "Thèse/Post-enquête/listes_donnees",
  files = c(
    "paquet1_coordonnees.csv",
    "paquet2_reponsesEF.csv",
    "paquet3_bi.csv",
    "paquet4_liens.csv"
  ),
  names = c(
    "coordonnees",
    "EF",
    "BI",
    "liens"
  )
)

# Fonction d'importation sécurisée
import_webdav_data <- function(url, user, password, file_name) {
  tryCatch({
    log_message(paste("Importation de", file_name))
    
    # Requête WebDAV avec authentification
    res <- GET(url, authenticate(user, password))
    
    # Vérification du statut
    stop_for_status(res)
    
    # Chargement du CSV
    data <- read.csv(text = content(res, "text"), header = TRUE)
    
    log_message(paste("Importation réussie pour", file_name))
    return(data)
  }, error = function(e) {
    log_message(paste("Erreur lors de l'importation de", file_name, ":", e$message), "ERROR")
    return(NULL)
  })
}

# Importation des données
data_frames <- list()

for (i in seq_along(config$files)) {
  # Construction de l'URL complète
  url <- paste0(config$base_url, "/", config$file_path, "/", config$files[i])
  
  # Importation du fichier
  data_frames[[config$names[i]]] <- import_webdav_data(
    url = url,
    user = Sys.getenv("USER_SSPCLOUD"),
    password = Sys.getenv("MDP_SSPCLOUD"),
    file_name = config$files[i]
  )
  
  # Vérification que l'importation a réussi
  if (is.null(data_frames[[config$names[i]]])) {
    log_message(paste("Impossible d'importer le fichier", config$files[i]), "ERROR")
    stop("Arrêt du script en raison d'une erreur d'importation")
  }
}

# Attribution des noms aux data.frames
coordonnees <- data_frames$coordonnees
EF <- data_frames$EF
BI <- data_frames$BI
liens <- data_frames$liens

# Nettoyage
rm(data_frames, url, i)

dir.create("1_data")
dir.create("1_data/processed")
# Sauvegarde des données importées
saveRDS(coordonnees, file = "1_data/processed/coordonnees.rds")
saveRDS(EF, file = "1_data/processed/EF.rds")
saveRDS(BI, file = "1_data/processed/BI.rds")
saveRDS(liens, file = "1_data/processed/liens.rds")

log_message("Importation des données terminée avec succès")

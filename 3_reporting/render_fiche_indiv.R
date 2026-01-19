BIEF <- readRDS(file = "1_data/processed/BIEF.rds")
list_id_individus <- as.character(BIEF$identifiant)

library(quarto)
library(progress)

# Dossier de sortie
output_dir <- "3_reporting/individus/html"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# Suivi des rendus
produit <- setNames(
  logical(length(list_id_individus)),
  list_id_individus
)

# Barre de progression
pb <- progress_bar$new(
  total = length(list_id_individus),
  format = "render pour :id [:bar] :current/:total (:percent)  "
)

# Boucle de rendu
for (id in list_id_individus) {
  
  pb$tick(tokens = list(id = id))
  
  output_file <- paste0(id, "_fiche_infos.html")
  
  tryCatch({
    
    quarto_render(
      input = "3_reporting/fiche_indiv.qmd",
      execute_params = list(
        id_individu = id,
        missing_values = FALSE
      ),
      output_file = output_file,
      quiet = TRUE   # 🔇 mute
    )
    
    file.rename(
      from = file.path("3_reporting", output_file),
      to   = file.path(output_dir, output_file)
    )
    
    produit[id] <- file.exists(
      file.path(output_dir, output_file)
    )
    
  }, error = function(e) {
    produit[id] <- FALSE
  })
}

# 📋 Bilan final
cat("\n===== BILAN DES RENDUS =====\n")

if (all(produit)) {
  cat("✔ Tous les rapports ont été générés avec succès (",
      sum(produit), "/", length(produit), ")\n", sep = "")
} else {
  
  cat("⚠ Rapports manquants pour les individus suivants :\n")
  print(names(produit)[!produit])
  
  cat("\n✔ Rapports générés : ",
      sum(produit), "/", length(produit), "\n", sep = "")
}


# Script d'exportation de fichiers vers WebDAV

library(httr)
library(tools)

## Encoding 
encode_path <- function(path) {
  gsub("%2F", "/", URLencode(path, reserved = TRUE))
}

# Fonction de logging
log_message <- function(message, level = "INFO") {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  cat(sprintf("[%s] [%s] %s\n", timestamp, level, message))
}

# Configuration WebDAV
config_export <- list(
  base_url = "https://sdrive.cnrs.fr/remote.php/dav/files/1426185",
  remote_path = "Thèse/Post-enquête/Enquétés/fiches_infos_synthetiques",
  local_path = output_dir
)

# Fonction d’upload sécurisé
upload_webdav_file <- function(local_file, remote_url, user, password) {
  tryCatch({
    log_message(paste("Export de", basename(local_file)))
    
    res <- PUT(
      url = remote_url,
      authenticate(user, password),
      body = upload_file(local_file)
    )
    
    stop_for_status(res)
    
    log_message(paste("Export réussi pour", basename(local_file)))
    TRUE
  }, error = function(e) {
    log_message(
      paste("Erreur lors de l'export de", basename(local_file), ":", e$message),
      "ERROR"
    )
    FALSE
  })
}

# Vérification du dossier local
if (!dir.exists(config_export$local_path)) {
  stop("Le dossier local n'existe pas :", config_export$local_path)
}


# Liste des fichiers à exporter
files_to_upload <- list.files(
  config_export$local_path,
  full.names = TRUE,
  recursive = FALSE
)
files_to_upload

if (length(files_to_upload) == 0) {
  stop("Aucun fichier à exporter dans le dossier local")
}

# Export des fichiers
for (file in files_to_upload) {
  remote_url <- paste0(
    config_export$base_url, "/",
    encode_path(
      paste0(config_export$remote_path, "/", basename(file))
    )
  )
  
  success <- upload_webdav_file(
    local_file = file,
    remote_url = remote_url,
    user = Sys.getenv("USER_SSPCLOUD"),
    password = Sys.getenv("MDP_SSPCLOUD")
  )
  
  if (!success) {
    stop("Arrêt du script suite à une erreur d'export")
  }
}

log_message("Export des fichiers terminé avec succès")


library(pdftools)

pdf_files <- file.path(
  "3_reporting/individus/synthetiques",
  paste0(sort(list_id_individus), "_fiche_infos.pdf")
)

pdf_files <- pdf_files[file.exists(pdf_files)]

pdf_combine(
  input  = pdf_files,
  output = "3_reporting/fiches_infos_synthetiques.pdf"
)

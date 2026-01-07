# --------------------------------------------------------------
# 0-3_extraction_odt.R
#   • Téléchargement d’un fichier .odt via WebDAV
#   • Extraction du XML interne (content.xml)
#   • Parsing des variables, questions, modalités, type, valeur, filtre
#   • Sauvegarde du tableau de métadonnées au format RDS
# --------------------------------------------------------------

# --------------------------------------------------------------
# 1. Packages --------------------------------------------------
# --------------------------------------------------------------

suppressPackageStartupMessages({
  library(httr)        # requêtes HTTP / WebDAV
  library(xml2)        # lecture du XML interne d’un .odt
  library(stringr)     # manipulation de chaînes
  library(tidyverse)   # dplyr, tibble, etc.
})

# --------------------------------------------------------------
# 2. Fonction de logging ---------------------------------------
# --------------------------------------------------------------
log_message <- function(msg, level = "INFO") {
  stamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  cat(sprintf("[%s] [%s] %s\n", stamp, level, msg))
}

# --------------------------------------------------------------
# 3. Configuration --------------------------------------------
# --------------------------------------------------------------
config <- list(
  # ----- serveur WebDAV -----
  base_url   = "https://sdrive.cnrs.fr/remote.php/dav/files/1426185",
  file_path  = "Thèse/Post-enquête/doc_donnees",
  odt_file   = "Questionnaire_EF_2025_simplifié.odt",
  
  # ----- sortie -----
  out_name   = "vars_EF",
  out_dir    = "1_data/processed",
  
  # ----- identifiants -----
  user_env   = "USER_SSPCLOUD",
  pwd_env    = "MDP_SSPCLOUD"
)

# --------------------------------------------------------------
# 4. Fonction d’import ODT sécurisé ----------------------------
# --------------------------------------------------------------
import_odt_webdav <- function(url, user, pwd, odt_name) {
  tryCatch({
    log_message(paste0("Début du téléchargement ODT : ", odt_name))
    
    # 4.1 Requête GET avec authentification
    resp <- GET(url, authenticate(user, pwd))
    stop_for_status(resp)               # arrête le script si le code HTTP >= 400
    
    # 4.2 Enregistrement temporaire
    tmp_odt <- file.path(tempdir(), odt_name)
    writeBin(content(resp, "raw"), tmp_odt)
    
    log_message(paste0("Fichier ODT téléchargé → ", tmp_odt))
    return(tmp_odt)
    
  }, error = function(e) {
    log_message(
      paste0("Erreur de téléchargement ODT '", odt_name, "' : ", e$message),
      level = "ERROR"
    )
    return(NULL)
  })
}

# --------------------------------------------------------------
# 5. Fonction d’extraction du XML -----------------------------
# --------------------------------------------------------------
extract_xml_content <- function(odt_path) {
  tryCatch({
    log_message("Décompression du fichier ODT (extraction de content.xml)")
    
    # Un .odt est en réalité un zip ; on ne garde que content.xml
    unzip(
      odt_path,
      files = "content.xml",
      exdir = tempdir(),
      overwrite = TRUE
    )
    xml_path <- file.path(tempdir(), "content.xml")
    xml_doc  <- read_xml(xml_path)
    
    log_message("XML chargé avec succès")
    return(xml_doc)
    
  }, error = function(e) {
    log_message(
      paste0("Erreur lors de l’extraction du XML : ", e$message),
      level = "ERROR"
    )
    return(NULL)
  })
}

# --------------------------------------------------------------
# 6. Fonction de parsing du texte ODT -------------------------
# --------------------------------------------------------------
parse_odt_metadata <- function(xml_doc) {
  tryCatch({
    
    # 6.1 Récupération de tout le texte brut
    paragraphs <- xml_find_all(xml_doc, ".//text:p", ns = xml_ns(xml_doc))
    full_text  <- paste(
      xml_text(paragraphs) %>%
        str_remove_all("Fin du filtre") %>%
        str_remove_all("🜄 Filtre"), 
      collapse = "\n"
    )
    
    # 6.2 Extraction des paires [VARIABLE] + ➡ question
    matches <- str_match_all(
      full_text,
      "\\[([A-Z0-9_]+)\\]\\s*\\n\\s*➡\\s*(.*)"
    )[[1]]
    
    rows <- vector("list", nrow(matches))
    
    for (i in seq_len(nrow(matches))) {
      var_name <- matches[i, 2]
      question <- matches[i, 3]
      
      ## ---- 6.2.1 Extraction ROBUSTE des modalités -----------------
      
      # 1. Extraire le segment du bloc de la variable
      segment_pat <- sprintf(
        "(?s)\\[%s\\][^\\n]*\\n(.*?)(?=\\n\\[[A-Z0-9_]+\\]|$)",
        var_name
      )
      segment <- str_match(full_text, segment_pat)[, 2]
      
      modalities <- NA_character_
      
      if (!is.na(segment)) {
        
        # Découper en lignes et nettoyer
        mod_lines <- str_split(segment, "\\n")[[1]] |> str_trim()
        
        # Retirer lignes explicatives du type "(Ne pas lire)"
        mod_lines <- mod_lines[!str_detect(mod_lines, "^\\(.*\\)$")]
        
        # Motif unique qui gère TOUS les formats
        modal_pattern <- paste0(
          "^\\s*(\\d+)\\s*[-–]\\s*(.*)$",       # 1 - Texte
          "|^(.*)\\s*\\[[A-Z0-9_]+?(\\d)\\]$",  # Texte [VAR3]
          "|^(Oui|Non)$",                       # Oui / Non seuls
          "|^•\\s*(.*)$"                        # Puces : • Texte
        )
        
        mods <- list()
        
        for (line in mod_lines) {
          
          m <- str_match(line, modal_pattern)
          if (!is.na(m[1])) {
            
            # Cas 1 : format classique  "1 - Texte"
            if (!is.na(m[2])) {
              code <- m[2]
              texte <- m[3]
            }
            
            # Cas 2 : "Texte [VAR3]"
            else if (!is.na(m[5])) {
              code <- m[5]
              texte <- str_trim(m[4])
            }
            
            # Cas 3 : Oui / Non simples
            else if (!is.na(m[6])) {
              texte <- m[6]
              code <- texte
            }
            
            # Cas 4 : Puce "• texte"
            else if (!is.na(m[7])) {
              texte <- m[7]
              code <- texte
            }
            
            mods[[length(mods) + 1]] <- sprintf("%s - %s", code, texte)
          }
        }
        
        # Format final : "1 - X | 2 - Y | 3 - Z"
        if (length(mods) > 0) {
          modalities <- paste(unlist(mods), collapse = " | ")
        }
      }
      
      ## ---- 6.2.2 Détermination du type ---------------------------
      var_type <- ifelse(
        str_detect(tolower(var_name),
                   "nb|age|annee|anai|quant|cbenf|ag_"
        ),
        "Quantitative",
        "Qualitative"
      )
      
      val_type <- case_when(
        str_detect(tolower(var_name), "annee|anai") ~ "Année",
        str_detect(tolower(var_name), "age")        ~ "Âge (entier)",
        str_detect(tolower(var_name), "nb|cbenf")   ~ "Nombre entier",
        var_type == "Qualitative"                   ~ "Catégorielle",
        TRUE                                        ~ NA_character_
      )
      
      ## ---- 6.2.3 Extraction d’un filtre éventuel ------------------
      filtre <- NA_character_
      segment_start <- str_locate(full_text, sprintf("\\[%s\\]", var_name))[1]
      if (!is.na(segment_start)) {
        sub_txt <- substr(full_text, segment_start, nchar(full_text))
        filt_match <- str_match(
          sub_txt,
          "Condition du filtre\\s*:\\s*(.*?)(?=\\n\\[|$)"
        )[, 2]
        
        if (!is.na(filt_match)) filtre <- str_trim(filt_match)
      }
      
      ## ---- 6.2.4 Construction de la ligne -------------------------
      rows[[i]] <- tibble(
        Variable   = var_name,
        Question   = str_trim(question),
        Modalites  = modalities,
        Type       = var_type,
        Valeur     = val_type,
        Filtre     = filtre
      )
    }
    
    # Assemblage final
    df <- bind_rows(rows)
    
    log_message(paste0("Extraction terminée : ", nrow(df), " variables détectées"))
    return(df)
    
  }, error = function(e) {
    log_message(
      paste0("Erreur pendant le parsing du texte ODT : ", e$message),
      level = "ERROR"
    )
    return(NULL)
  })
}


# --------------------------------------------------------------
# 7. Exécution du pipeline ------------------------------------
# --------------------------------------------------------------

# 7.1 Construction de l’URL complète
odt_url <- file.path(
  config$base_url,
  config$file_path,
  config$odt_file
)

# 7.2 Téléchargement
odt_path <- import_odt_webdav(
  url      = odt_url,
  user     = Sys.getenv(config$user_env),
  pwd      = Sys.getenv(config$pwd_env),
  odt_name = config$odt_file
)

if (is.null(odt_path)) {
  log_message("Téléchargement impossible → arrêt du script", "ERROR")
  stop("Import ODT échoué")
}

# 7.3 Extraction du XML
xml_doc <- extract_xml_content(odt_path)
if (is.null(xml_doc)) {
  log_message("Extraction XML impossible → arrêt du script", "ERROR")
  stop("Extraction XML échouée")
}

# 7.4 Parsing des métadonnées
vars_EF <- parse_odt_metadata(xml_doc)
if (is.null(vars_EF) || nrow(vars_EF) == 0) {
  log_message("Aucune métadonnée extraite → arrêt du script", "ERROR")
  stop("Parsing ODT échoué")
}

# --------------------------------------------------------------
# 8. Sauvegarde ------------------------------------------------
# --------------------------------------------------------------
# Crée le répertoire de sortie s’il n’existe pas
if (!dir.exists(config$out_dir)) {
  dir.create(config$out_dir, recursive = TRUE)
  log_message(paste0("Création du répertoire de sortie : ", config$out_dir))
}

out_path <- file.path(config$out_dir, paste0(config$out_name, ".rds"))
saveRDS(vars_EF, file = out_path)
log_message(paste0("Objet sauvegardé → ", out_path))

# --------------------------------------------------------------
# 9. Nettoyage -------------------------------------------------
# --------------------------------------------------------------
rm(list = c(
  "odt_path", "odt_url", "xml_doc", "metadata_EF_odt",
  "full_text", "paragraphs", "matches", "rows"
))
log_message("Script d’extraction ODT terminé avec succès")

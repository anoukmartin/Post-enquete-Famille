# ============================================================
# Extraction des variables et questions depuis un fichier .ODT
# ============================================================

## Import doc 
# Exemple : chemin vers ton fichier CSV (à adapter)
file_path <- "Thèse/Post-enquête/doc_donnees"


## questionnaire EF
doc <- "Questionnaire_EF_2025_simplifié.odt"
# Construction de l'URL complète
url <- paste0(base_url, "/", file_path, "/", doc)
# Requête WebDAV avec authentification
res <- GET(url, authenticate(user, app_pwd))
# Vérifier que la requête a réussi
stop_for_status(res)



# --- Sauvegarde temporaire du fichier ODT téléchargé ---
odt_file <- file.path(tempdir(), doc)
writeBin(content(res, "raw"), odt_file)

cat("✅ Fichier téléchargé via WebDAV :", odt_file, "\n")

# Extraire le XML principal
tmpdir <- tempdir()
unzip(odt_file, files = "content.xml", exdir = tmpdir, overwrite = TRUE)
xml_doc <- read_xml(file.path(tmpdir, "content.xml"))

# Extraire tout le texte des paragraphes
paragraphs <- xml_find_all(xml_doc, ".//text:p", ns = xml_ns(xml_doc))
text <- xml_text(paragraphs)
full_text <- paste(text, collapse = "\n")

# --- Trouver les variables et questions ---
matches <- str_match_all(full_text, "\\[([A-Z0-9_]+)\\][^\\n]*\\n?(?:➡\\s*)?([^\\n]*)")[[1]]

rows <- list()

for (i in seq_len(nrow(matches))) {
  var_name <- matches[i, 2]
  question <- matches[i, 3]
  
  # --- Extraire le bloc qui suit cette variable jusqu'à la suivante ---
  pattern_segment <- sprintf("(?s)\\[%s\\][^\\n]*\\n(.*?)(?=\\n\\[[A-Z0-9_]+\\]|$)", var_name)
  segment <- str_match(full_text, pattern_segment)[, 2]
  
  modalities <- ""
  if (!is.na(segment)) {
    mod_lines <- str_split(segment, "\\n")[[1]]
    mod_lines <- str_trim(mod_lines)
    
    # Filtrer les lignes pertinentes (Modalites uniquement)
    mod_lines <- mod_lines[
      str_detect(mod_lines, "^\\s*\\d+\\s*[-–]") |   # 1 - ...
        str_detect(mod_lines, "^•") |                  # puce
        str_detect(mod_lines, "^Oui$") |               # Oui
        str_detect(mod_lines, "^Non$")                 # Non
    ]
    
    # Supprimer les lignes explicatives entre parenthèses
    mod_lines <- mod_lines[!str_detect(mod_lines, "^\\(.*\\)$")]
    
    if (length(mod_lines) > 0) {
      modalities <- paste(mod_lines, collapse = " | ")
    }
  }
  
  # --- Déterminer le type de variable ---
  if (str_detect(tolower(var_name), "nb|age|annee|anai|quant|cbenf|ag_")) {
    var_type <- "Quantitative"
  } else {
    var_type <- "Qualitative"
  }
  
  # --- Type de valeur ---
  if (str_detect(tolower(var_name), "annee|anai")) {
    val_type <- "Année"
  } else if (str_detect(tolower(var_name), "age")) {
    val_type <- "Âge (entier)"
  } else if (str_detect(tolower(var_name), "nb|cbenf")) {
    val_type <- "Nombre entier"
  } else if (var_type == "Qualitative") {
    val_type <- "Catégorielle"
  } else {
    val_type <- ""
  }
  
  # --- Condition de filtre éventuelle ---
  segment_start <- str_locate(full_text, sprintf("\\[%s\\]", var_name))[1]
  filtre <- ""
  if (!is.na(segment_start)) {
    sub_text <- substr(full_text, segment_start, nchar(full_text))
    filt_match <- str_match(sub_text, "Condition du filtre\\s*:\\s*(.*?)(?=\\n\\[|$)")[, 2]
    if (!is.na(filt_match)) {
      filtre <- str_trim(filt_match)
    }
  }
  
  # --- Ajouter la ligne au tableau ---
  rows[[i]] <- data.frame(
    "Variable" = var_name,
    "Question" = str_trim(question),
    "Modalites" = modalities,
    "Type" = var_type,
    "Valeur" = val_type,
    "Filtre" = filtre,
    stringsAsFactors = FALSE
  )
}

# --- Assembler et exporter ---
vars_EF <- bind_rows(rows) 
vars_EF[vars_EF$Modalites == "", ]$Modalites <- NA

rm(xml_doc, filt_match, filtre, full_text, i, mod_lines, modalities, odt_file, pattern_segment, question, segment, segment_start, sub_text, text,tmpdir, val_type, var_name, var_type, rows, matches, paragraphs, res)


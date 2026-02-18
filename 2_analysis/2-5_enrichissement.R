# Erichissement des tables de données 

BIEF_lab <- readRDS("1_data/labelled/BIEF_lab.rds")

# ------------------------------------------------------------------------------
# 4. Codes géographiques -------------------------------------------------------
# ------------------------------------------------------------------------------

# on ajoute les communes des parents, enfants, ex_conjoints ...


codecommunes <- read_csv("https://www.data.gouv.fr/api/1/datasets/r/7acc46ad-1c79-43d9-8f2d-d0a8ec78c068")
codecommunes$Commune <- paste0(codecommunes$nomCommune, " (", codecommunes$codeDepartement, ")")
codecommunes$codeCommune
codecommunes$Departement <- paste0(codecommunes$nomDepartement, " (", codecommunes$codeDepartement, ")")

# 1️⃣ créer un vecteur code → labels concaténés
labs_communes <- codecommunes %>%
  group_by(codeCommune) %>%
  summarise(Commune_label = paste(unique(Commune), collapse = " / "), .groups = "drop") %>%
  { setNames(.$Commune_label, .$codeCommune) }

# 2️⃣ appliquer aux colonnes COM
BIEF_lab <- BIEF_lab %>%
  mutate(across(contains("COM"), ~ {
    
    x <- str_remove(.x, "2024")
    x <- as.character(x)
    
    # labels correspondant aux codes présents
    labs_to_apply <- labs_communes[names(labs_communes) %in% unique(x)]
    
    val_labels(x) <- labs_to_apply
    
    x
  }))

freq(BIEF_lab$PARENT_COM_ENFLOG1)
# meme chose version déparyement 

# 1️⃣ créer un vecteur code → labels concaténés
labs_departements <- codecommunes %>%
  group_by(codeDepartement) %>%
  summarise(Departement_label = paste(unique(Departement), collapse = " / "), .groups = "drop") %>%
  { setNames(.$Departement_label, .$codeDepartement) }

# 2️⃣ appliquer aux colonnes COM
BIEF_lab <- BIEF_lab %>%
  mutate(across(matches("dp|dlt|dep|DEP"), ~ {
    
    x <- as.character(.x)
    
    # labels correspondant aux codes présents
    labs_to_apply <- labs_departements[names(labs_departements) %in% unique(x)]
    
    val_labels(x) <- labs_to_apply
    
    x
  }))

# vérifier
freq(BIEF_lab$dpnai)
#############################################################################
### Consolidation des adresses postales : #####################################


# Construction de l’adresse complète pour le géocodage
BIEF_lab <- BIEF_lab %>%
  separate(Commune,
           into = c("Commune", "CodePostal"),
           sep = " \\(",
           remove = TRUE) %>%
  mutate(CodePostal = CodePostal %>%
           str_remove_all("\\)") %>%
           str_trim())

BIEF_lab <- BIEF_lab %>%
  mutate(
    adresse_complete = paste(
      AdressePostale,
      #ComplementAdresse,
      CodePostal,
      Commune,
      sep = ", "
    )
  )
BIEF_lab$adresse_complete

### Consolidations des adresses et géodoage 
library(banR)
coordonnees_geo <- geocode_tbl(BIEF_lab %>%
                                 select(identifiant, PRENOM, NOMMARITAL, NOMNAISSANCE, AdressePostale, CodePostal, Commune, ComplementAdresse, adresse_complete), 
                               adresse = adresse_complete)
glimpse(coordonnees_geo)

# test manquant 
coordonnees_geo[coordonnees_geo$result_status != "ok", ]

# numéro de rue manquant
coordonnees_geo <- coordonnees_geo %>%
  mutate(AdresseConsolidee = case_when(
    result_type != "housenumber" & str_starts(AdressePostale, "[:digit:]") ~ "WARN", 
    TRUE ~ result_label))
coordonnees_geo[coordonnees_geo$AdresseConsolidee == "WARN" , ]
coordonnees_geo <- coordonnees_geo %>%
  mutate(AdresseConsolidee = case_when(
    AdresseConsolidee == "WARN" & str_starts(AdressePostale, "59 ") ~ str_glue("59 {result_label}"), 
    AdresseConsolidee == "WARN" & str_starts(AdressePostale, "5  ") ~ str_glue("5 {result_label}"), 
    AdresseConsolidee == "WARN" & str_starts(AdressePostale, "37 B ") ~ str_glue("37bis {result_label}"), 
    AdresseConsolidee == "WARN" & str_starts(AdressePostale, "3 ") ~ str_glue("3 {result_label}"),
    TRUE ~ AdresseConsolidee))
coordonnees_geo[coordonnees_geo$AdresseConsolidee == "WARN" , ]

coordonnees_geo <- coordonnees_geo %>%
  mutate(
    ComplementAdresse = str_trim(ComplementAdresse) %>% str_to_title(),
    ComplementAdresse = if_else(ComplementAdresse == "", "", str_glue(", {ComplementAdresse}"))) 
coordonnees_geo <- coordonnees_geo %>%
  mutate(
    AdresseConsolidee = str_replace(
      AdresseConsolidee,
      "(\\b\\d{5}\\b)",
      str_glue("{ComplementAdresse} \\1")
    )) 
coordonnees_geo <- coordonnees_geo %>%
  mutate(
    AdresseSimple = str_sub(
      AdresseConsolidee,
      1, str_locate(AdresseConsolidee, "(\\b\\d{5}\\b)")[, 1] - 1) %>% str_trim,
    CPCommune = str_sub (
      AdresseConsolidee,
      str_locate(AdresseConsolidee, "(\\b\\d{5}\\b)")[, 1], -1))

str_locate(coordonnees_geo$AdresseConsolidee[1], "(\\b\\d{5}\\b)")
coordonnees_geo$ComplementAdresse
coordonnees_geo$AdresseSimple

## Informations supplémentaiers sur les communes de résidences
library(sf)
## On ajoute des labels pour les variables géo

# URL du shapefile ZIP officiel
url <- "https://data.iledefrance.fr/api/explore/v2.1/catalog/datasets/communes-et-arrondissements-municipaux-annee-en-cours/exports/shp?lang=fr&timezone=Europe%2FBerlin&use_labels=true"


# Chemin temporaire pour télécharger le zip
temp <- tempfile(fileext = ".zip")
download.file(url, temp)
rm(unzip_dir)
# Décompresser le zip dans un dossier temporaire
unzip_dir <- tempdir()
unzip(temp, exdir = unzip_dir)

# Trouver le fichier .shp dans le dossier décompressé
shp_file <- list.files(unzip_dir, pattern = "\\.shp$", full.names = TRUE)


# Lire le shapefile
idf_sf <- st_read(shp_file[2]) # ou 1 je sais pas pourquoi 



coordonnees_geo <- left_join(coordonnees_geo, idf_sf, by = c("result_citycode" = "Code_commun.1"))

coordonnees_geo <- coordonnees_geo %>%
  select(identifiant, AdresseConsolidee, AdresseSimple, CodePostal, Commune.x, result_citycode, Commune__Ar, 
         latitude, longitude,
         Libelle_CAT.1, Statut_UU20.1) %>%
  rename(
    Adresse_complete_consolidee = AdresseConsolidee, 
    Adresse_simple_consolidee = AdresseSimple, 
    Code_postal_consolide = CodePostal, 
    Commune_consolidee = Commune.x, 
    Commune_arrondissement = Commune__Ar,
    Code_commune = result_citycode,
    Aire_urbaine = Libelle_CAT.1, 
         Ruralite = Statut_UU20.1)



BIEF_lab <- left_join(BIEF_lab, coordonnees_geo, by = "identifiant")

BIEF_newvars <- names(coordonnees_geo)[-1]

############################################################
### 5. Pays 
#############################################################


isopays <- read_csv("https://sql.sh/ressources/sql-pays/sql-pays.csv", col_names = F)
isopays <- isopays[, -c(1,2, 6)]
names(isopays) <- c("iso2", "iso3", "pays")
#PNAI PAY
iso3_labels <- setNames(isopays$pays, isopays$iso3)

BIEF_lab <- BIEF_lab %>%
  mutate(across(matches("PNAI|PAY"), ~ {
    # convertir en character pour éviter les erreurs
    x_char <- as.character(.x)
    # garder uniquement les codes présents dans iso3_labels
    labels_x <- iso3_labels[names(iso3_labels) %in% x_char]
    labelled(x_char, labels = labels_x)
  }))
# Afficher le résultat
freq(BIEF_lab$PNAI_PAR1)
freq(BIEF_lab$PNAI_C)


### JSP######################

infos <- BIEF_lab %>%
  # Nom de famille : nom marital + nom de naissance si nécessaire
  mutate(
    NOMFAMILLE = case_when(
      is.na(NOMMARITAL) | NOMMARITAL == "" ~ NOMNAISSANCE,
      TRUE ~ str_glue("{NOMMARITAL} ({NOMNAISSANCE})")
    )
  ) %>%
  # Civilité
  mutate(
    civilite = case_when(
      sexe == 2  ~ "Madame",
      sexe == 1 ~ "Monsieur",
    ),
    parentMF = case_when(
      sexe == 2  ~ "mère",
      sexe == 1 ~ "père", 
    )
  )

tab <- infos[is.na(infos$civilite), ] # Brigitte et Sandra, on peut supposer que ce sont des femmes 
#ok
rm(tab)






############################################################
### 4. Indicateurs de séparation et résidence des enfants ###
############################################################
freq(infos$SEP_AUT_PAR_ENFLOG1)
freq(infos$PARENT_VIT_ENFLOG1)

infos <- infos %>%
  mutate(
    # Séparation avec autorité parentale partagée
    SEP_AUT_PAR_ENF = case_when(
      if_any(starts_with("SEP_AUT_PAR_ENF"), ~ . == 1, na.rm = TRUE) ~ "Oui",
      if_all(starts_with("SEP_AUT_PAR_ENF"), ~ is.na(.)) ~ NA_character_,
      TRUE ~ "Non"
    ),
    # Enfant vivant ailleurs
    AILL_AUT_PAR_ENF = case_when(
      if_any(
        starts_with("PARENT_VIT"),
        ~ . %in% c(2, 3),
        na.rm = TRUE
      ) ~ "Oui",
      if_all(starts_with("PARENT_VIT"), ~ is.na(.)) ~ NA_character_,
      TRUE ~ "Non"
    )
  )
freq(infos$SEP_AUT_PAR_ENF)
freq(infos$AILL_AUT_PAR_ENF)

############################################################
### 5. Typologie familiale (sous-populations)            ###
############################################################
freq(infos$COUPLE)
freq(infos$ENFAV_C)
infos <- infos %>%
  mutate(
    souspop = case_when(
      COUPLE %in% c(1, 2) & 
        (SEP_AUT_PAR_ENF == "Oui" | ENFAV_C == 1) ~ "Recomposée",
      COUPLE %in% c(1, 2) & 
        (SEP_AUT_PAR_ENF == "Non" | is.na(SEP_AUT_PAR_ENF)) ~ "Couple parental",
      COUPLE %in% c(3,4)  ~ "Monoparentale",
      TRUE ~ "Autre situation"
    )
  )
freq(infos$souspop)
tab <- infos[infos$souspop == "Autre situation", ] 
#ok
rm(tab)

############################################################
### 6. Comptage des enfants selon l’âge et la résidence  ###
############################################################
freq(infos$DC_ENFAIL1)

infos <- infos %>%
  rowwise() %>%
  mutate(
    nb_enfantsDCD = sum(c_across(starts_with("DC_ENF")) == 2, na.rm = TRUE),
    nb_enfantsVivants = NBENF - nb_enfantsDCD,
    nb_jeunesEnfants7 = sum(c_across(starts_with("ANAI_ENF")) > 2018, na.rm = TRUE),
    nb_enfantsMajeurs = sum(c_across(starts_with("ANAI_ENF")) <= 2006, na.rm = TRUE)
  ) %>%
  ungroup()


############################################################
### 7. Description familiale personnalisée               ###
############################################################
freq(infos$COUPLE)
infos <- infos %>%
  mutate(
    couple_parental_precis = case_when(
      COUPLE == 2 ~ "couple qui ne cohabite pas",
      nb_enfantsMajeurs == 1 & nb_enfantsDCD == 0 ~ "d'un enfant majeur",
      nb_enfantsMajeurs > 1 & nb_enfantsDCD == 0  ~ "d'enfants majeurs",
      CBENFAIL == 1 & nb_enfantsDCD == 0 ~ "d'un enfant vivant ailleurs",
      CBENFAIL > 1 & nb_enfantsDCD == 0 ~ "d'enfants vivant ailleurs",
      NBENF > 3 ~ "de famille nombreuse",
      nb_jeunesEnfants7 == 1 & nb_enfantsDCD == 0 ~ "d'un jeune enfant",
      nb_jeunesEnfants7 > 1 & nb_enfantsDCD == 0 ~ "de jeunes enfants",
      NBENF == 1 ~ "d'un enfant unique",
      NBENF == 2 ~ "de deux enfants",
      NBENF == 3 ~ "de trois enfants"
    ),
    description_personalisee = case_when(
      souspop == "Recomposée"     ~ str_glue("{parentMF} de famille recomposée"),
      souspop == "Monoparentale"  ~ str_glue("{parentMF} célibataire"),
      TRUE                       ~ str_glue("{parentMF} {couple_parental_precis}")
    )
  )
freq(infos$couple_parental_precis)
freq(infos$description_personalisee)

############################################################
### 8. Nettoyage et consolidation des professions        ###
############################################################

infos <- infos %>%
  mutate(
    profession = coalesce(
      na_if(prof, ""),
      na_if(prof_flou, "") %>% str_to_sentence()
    )
  )


############################################################
### 9. Mise en forme des prénoms et calcul de l’âge       ###
############################################################

annee_courante <- as.integer(format(Sys.Date(), "%Y"))

infos <- infos %>%
  mutate(
    across(
      contains("PRENOM"),
      ~ str_to_title(str_trim(.x))
    )
  ) %>%
  rowwise() %>%
  mutate(
    enfants_logement = {
      prenoms <- c_across(starts_with("PRENOM_ENFLOG"))
      annees  <- c_across(starts_with("ANAI_ENFLOG"))
      sexe  <- c_across(starts_with("SEXE_ENFLOG")) %>%
        str_sub(1, 1)
      paste(
        na.omit(
          ifelse(
            !is.na(prenoms) & prenoms != "" & !is.na(annees),
            paste0(prenoms, " (",sexe, "-", annee_courante - annees, ")"),
            NA_character_
          )
        ),
        collapse = ", "
      )
    },
    enfants_ailleurs = {
      prenoms <- c_across(starts_with("PRENOM_ENFAIL"))
      annees  <- c_across(starts_with("ANAI_ENFAIL"))
      sexe  <- c_across(starts_with("SEXE_ENFAIL")) %>%
        str_sub(1, 1)
      paste(
        na.omit(
          ifelse(
            !is.na(prenoms) & prenoms != "" & !is.na(annees),
            paste0(prenoms, " (",sexe, "-", annee_courante - annees, ")"),
            NA_character_
          )
        ),
        collapse = ", "
      )
    }
  ) %>%
  ungroup()

# enregistrement !!

saveRDS(BIEF_newvars, "1_data/enriched/list_newvars.rds")

new_infos <- names(infos)[!(names(infos) %in% names(BIEF_lab))]

infos <- infos %>%
  select(identifiant, any_of(new_infos))
BIEF_newvars <- c(BIEF_newvars, new_infos)
BIEF_lab <- left_join(BIEF_lab, infos, by = "identifiant")

###
# mise en forme html
####


BIEF_lab <- BIEF_lab %>%
  mutate(coordonnes_html = paste0( "<p>",
                                   "📞 <b>Téléphone :</b> ", POSTENQ_TEL, "<br>",
                                   "📧 <b>Email :</b> ", POSTENQ_MAIL, "<br>",
                                   "📍 <b>Adresse :</b> ", Adresse_complete_consolidee,
                                   "</p>")) %>%
  mutate(resume_sociodemo_html = paste0("<b>🧍 Individu</b>",
                                        "<ul>",
                                        "<li><b>Prénom :</b> ", PRENOM, "</li>",
                                        "<li><b>Nom :</b> ", NOMFAMILLE, "</li>",
                                        "<li><b>Âge :</b> ", 2026 - anai, "</li>",
                                        "<li><b>Sexe :</b> ", sexe, "</li>",
                                        "<li><b>En emploi :</b> ", situat, "</li>",
                                        "<li><b>Niveau de diplôme :</b> ", dipl, "</li>",
                                        "<li><b>Statut d'emploi :</b> ", empl, "</li>",
                                        "<li><b>Temps de travail :</b> ", tp, "</li>",
                                        "<li><b>Profession :</b> ", str_to_sentence(profession), "</li>",
                                        "<li><b>Entreprise :</b> ", rs, "</li>",
                                        "</ul>",
                                        
                                        "<b>💍 Situation conjugale</b>",
                                        "<ul>",
                                        "<li><b>En couple :</b> ", COUPLE, "</li>",
                                        "<li><b>Statut matrimonial :</b> ", matr, "</li>",
                                        "</ul>",
                                        
                                        "<b>🧑‍🤝‍🧑 Dernier conjoint</b>",
                                        "<ul>",
                                        "<li><b>Prénom :</b> ", PRENOM_C, "</li>",
                                        "<li><b>Âge :</b> ", 2026 - ANAI_C, "</li>",
                                        "<li><b>Sexe :</b> ", SEXE_C, "</li>",
                                        "<li><b>Emploi :</b> ", STATUT_C, "</li>",
                                        "</ul>",
                                        
                                        "<b>👶 Enfants</b>",
                                        "<ul>",
                                        "<li><b>Dans le logement :</b> ", enfants_logement, "</li>",
                                        "<li><b>Résidant ailleurs :</b> ", enfants_ailleurs, "</li>",
                                        "<li><b>Enfants du conjoint :</b> ", ENFAV_C, "</li>",
                                        "<li><b>Enfants décédés :</b> ", nb_enfantsDCD, "</li>",
                                        "</ul>",
                                        
                                        "<b>🔁 Synthétique </b>",
                                        "<ul>",
                                        "<li><b>Sous-population :</b> ", souspop, "</li>",
                                        "<li><b>Situation familiale :</b> ", description_personalisee, "</li>",
                                        "</ul>"))












saveRDS(BIEF_lab, "1_data/enriched/BIEF_lab.rds")



BIEF_rec <- to_factor(BIEF_lab, levels = "p", sort_levels = "v")
freq(BIEF_rec$dipl)

res <- sapply(names(BIEF_rec), function(v) {
  lab <- var_label(BIEF_rec[[v]])
  if (is.null(lab) || is.na(lab)) {lab <- "" } else {
    lab <- paste0(" ", lab)}
  paste0("[", v, "]", lab)
}, USE.NAMES = FALSE)
names(BIEF_rec) <- res
var_label(BIEF_rec) <- NULL


saveRDS(BIEF_rec, "1_data/enriched/BIEF_rec.rds")





# Trash #################################################



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

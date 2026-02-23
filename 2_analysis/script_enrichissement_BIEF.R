
# ==============================================================================
# Script unique — Enrichissement de la base BIEF
# ==============================================================================
# Objectif :
# - Ajouter labels géographiques (communes, départements, pays)
# - Consolider et géocoder les adresses
# - Produire indicateurs familiaux
# - Générer variables enrichies + sorties HTML
# - Exporter bases finales
# ==============================================================================


# =============================================================================
# 0. Librairies & Chargement ---------------------------------------------------
# =============================================================================

library(tidyverse)
library(labelled)
library(stringr)
library(banR)
library(sf)

BIEF_lab <- readRDS("1_data/labelled/BIEF_lab.rds")


# =============================================================================
# 1. Codes communes ------------------------------------------------------------
# =============================================================================

codecommunes <- read_csv(
  "https://www.data.gouv.fr/api/1/datasets/r/7acc46ad-1c79-43d9-8f2d-d0a8ec78c068"
)

codecommunes <- codecommunes %>%
  mutate(
    Commune = paste0(nomCommune, " (", codeDepartement, ")"),
    Departement = paste0(nomDepartement, " (", codeDepartement, ")")
  )

labs_communes <- codecommunes %>%
  group_by(codeCommune) %>%
  summarise(
    Commune_label = paste(unique(Commune), collapse = " / "),
    .groups = "drop"
  ) %>%
  { setNames(.$Commune_label, .$codeCommune) }

BIEF_lab <- BIEF_lab %>%
  mutate(across(contains("COM"), ~ {

    x <- str_remove(.x, "2024")
    x <- as.character(x)

    labs_to_apply <- labs_communes[
      names(labs_communes) %in% unique(x)
    ]

    val_labels(x) <- labs_to_apply
    x
  }))


# =============================================================================
# 2. Codes départements --------------------------------------------------------
# =============================================================================

labs_departements <- codecommunes %>%
  group_by(codeDepartement) %>%
  summarise(
    Departement_label = paste(unique(Departement), collapse = " / "),
    .groups = "drop"
  ) %>%
  { setNames(.$Departement_label, .$codeDepartement) }

BIEF_lab <- BIEF_lab %>%
  mutate(across(matches("dp|dlt|dep|DEP"), ~ {

    x <- as.character(.x)

    labs_to_apply <- labs_departements[
      names(labs_departements) %in% unique(x)
    ]

    val_labels(x) <- labs_to_apply
    x
  }))


# =============================================================================
# 3. Consolidation adresses ----------------------------------------------------
# =============================================================================
freq(BIEF_lab$Commune)
BIEF_lab <- BIEF_lab %>%
  separate(
    Commune,
    into = c("Commune", "CodePostal"),
    sep = " \\(",
    remove = TRUE
  ) %>%
  mutate(
    CodePostal = CodePostal %>%
      str_remove_all("\\)") %>%
      str_trim()
  )

BIEF_lab <- BIEF_lab %>%
  mutate(
    adresse_complete = paste(
      AdressePostale,
      CodePostal,
      Commune,
      sep = ", "
    )
  )


# =============================================================================
# 4. Géocodage BAN -------------------------------------------------------------
# =============================================================================

coordonnees_geo <- geocode_tbl(
  BIEF_lab %>%
    select(
      identifiant, PRENOM, NOMMARITAL, NOMNAISSANCE,
      AdressePostale, CodePostal, Commune,
      ComplementAdresse, adresse_complete
    ),
  adresse = adresse_complete
)

glimpse(coordonnees_geo)

# Diagnostic des échecs
coordonnees_geo %>%
  filter(result_status != "ok")
#ok

# =============================================================================
# 5. Nettoyage géocodage -------------------------------------------------------
# =============================================================================

# Détection : adresse sans housenumber
coordonnees_geo <- coordonnees_geo %>%
  mutate(
    AdresseConsolidee = case_when(
      result_type != "housenumber" &
        str_starts(AdressePostale, "[:digit:]") ~ "WARN",
      TRUE ~ result_label
    )
  )
coordonnees_geo %>%
  filter(AdresseConsolidee == "WARN")

# Corrections manuelles ciblées
coordonnees_geo <- coordonnees_geo %>%
  mutate(
    AdresseConsolidee = case_when(
      AdresseConsolidee == "WARN" &
        str_starts(AdressePostale, "59 ") ~ str_glue("59 {result_label}"),
      AdresseConsolidee == "WARN" &
        str_starts(AdressePostale, "5  ") ~ str_glue("5 {result_label}"),
      AdresseConsolidee == "WARN" &
        str_starts(AdressePostale, "37 B ") ~ str_glue("37bis {result_label}"),
      TRUE ~ AdresseConsolidee
    )
  )
coordonnees_geo$ComplementAdresse
coordonnees_geo <- coordonnees_geo %>%
  mutate(
    ComplementAdresse = str_trim(ComplementAdresse) %>%
      str_to_title(),
    ComplementAdresse = if_else(
      is.na(ComplementAdresse),
      "",
      str_glue(", {ComplementAdresse}")
    ),
    AdresseConsolidee = str_replace(
      AdresseConsolidee,
      "(\\b\\d{5}\\b)",
      str_glue("{ComplementAdresse} \\1")
    )
  )

coordonnees_geo <- coordonnees_geo %>%
  mutate(
    AdresseSimple = str_sub(
      AdresseConsolidee,
      1,
      str_locate(AdresseConsolidee, "(\\b\\d{5}\\b)")[,1] - 1
    ) %>% str_trim(),
    CPCommune = str_sub(
      AdresseConsolidee,
      str_locate(AdresseConsolidee, "(\\b\\d{5}\\b)")[,1],
      -1
    )
  )


# =============================================================================
# 6. Ajout infos territoriales (SIG) -------------------------------------------
# =============================================================================

url <- "https://data.iledefrance.fr/api/explore/v2.1/catalog/datasets/communes-et-arrondissements-municipaux-annee-en-cours/exports/shp"

temp <- tempfile(fileext = ".zip")
download.file(url, temp)

unzip_dir <- tempdir()
unzip(temp, exdir = unzip_dir)

shp_file <- list.files(
  unzip_dir,
  pattern = "\\.shp$",
  full.names = TRUE
)

idf_sf <- st_read(shp_file[1])
glimpse(idf_sf)
idf_sf$com_arm_cod

coordonnees_geo$result_citycode
all(coordonnees_geo$result_citycode %in% idf_sf$com_arm_cod)

coordonnees_geo <- left_join(
  coordonnees_geo,
  idf_sf,
  by = c("result_citycode" = "com_arm_cod")
)


# =============================================================================
# 7. Jointure base principale --------------------------------------------------
# =============================================================================
glimpse(coordonnees_geo)

coordonnees_geo <- coordonnees_geo %>%
  select(
    identifiant,
    AdresseConsolidee,
    AdresseSimple,
    CodePostal,
    Commune,
    result_citycode,
    com_arm_nam,
    latitude,
    longitude,
    com_arm_cat.3,
    com_arm_uu2.3
  ) %>%
  rename(
    Adresse_complete_consolidee = AdresseConsolidee,
    Adresse_simple_consolidee  = AdresseSimple,
    Code_postal_consolide      = CodePostal,
    Commune_consolidee         = Commune,
    Commune_arrondissement     = com_arm_nam,
    Code_commune               = result_citycode,
    Aire_urbaine               = com_arm_cat.3,
    Ruralite                   = com_arm_uu2.3
  )

glimpse(coordonnees_geo)
# Jointure à la base principale
BIEF_lab <- left_join(
  BIEF_lab,
  coordonnees_geo,
  by = "identifiant"
)

BIEF_newvars <- names(coordonnees_geo)[-1]





# =============================================================================
# 8. Pays ISO ------------------------------------------------------------------
# =============================================================================

isopays <- read_csv(
  "https://sql.sh/ressources/sql-pays/sql-pays.csv",
  col_names = FALSE
) %>%
  .[, -c(1,2,6)]

names(isopays) <- c("iso2","iso3","pays")

iso3_labels <- setNames(
  isopays$pays,
  isopays$iso3
)

BIEF_lab <- BIEF_lab %>%
  mutate(across(matches("PNAI|PAY"), ~ {

    x_char <- as.character(.x)

    labels_x <- iso3_labels[
      names(iso3_labels) %in% x_char
    ]

    labelled(x_char, labels = labels_x)
  }))


# =============================================================================
# 9. Variables familiales ------------------------------------------------------
# =============================================================================


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









# =============================================================================
# 10. Export final -------------------------------------------------------------
# =============================================================================



saveRDS(BIEF_lab, "1_data/enriched/BIEF_lab.rds")



BIEF_rec <- to_factor(BIEF_lab, levels = "p", sort_levels = "v")
freq(BIEF_rec$dipl)

# res <- sapply(names(BIEF_rec), function(v) {
#   lab <- var_label(BIEF_rec[[v]])
#   if (is.null(lab) || is.na(lab)) {lab <- "" } else {
#     lab <- paste0(" ", lab)}
#   paste0("[", v, "]", lab)
# }, USE.NAMES = FALSE)
# names(BIEF_rec) <- res
# var_label(BIEF_rec) <- NULL


saveRDS(BIEF_rec, "1_data/enriched/BIEF_rec.rds")











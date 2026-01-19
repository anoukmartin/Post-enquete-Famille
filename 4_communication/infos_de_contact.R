############################################################
### 1. Chargement des librairies et des données sources  ###
############################################################

library(dplyr)
library(stringr)
library(tidygeocoder)

# Données de coordonnées postales
coordonnees <- readRDS(file = "1_data/processed/coordonnees.rds")
glimpse(coordonnees)

# Données principales individus / familles
infos <- readRDS(file = "1_data/processed/BIEF.rds")


############################################################
### 2. Préparation et géocodage des adresses              ###
############################################################

# Construction de l’adresse complète pour le géocodage
coordonnees <- coordonnees %>%
  separate(Commune,
           into = c("Commune", "CodePostal"),
           sep = " \\(",
           remove = TRUE) %>%
  mutate(CodePostal = CodePostal %>%
           str_remove_all("\\)") %>%
           str_trim())
  

coordonnees <- coordonnees %>%
  mutate(
    adresse_complete = paste(
      AdressePostale,
      #ComplementAdresse,
      CodePostal,
      Commune,
      sep = ", "
    )
  )



### Consolidations des adresses et géodoage 
library(banR)
coordonnees_geo <- geocode_tbl(coordonnees, adresse = adresse_complete)
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




############################################################
### 3. Harmonisation identité, civilité et rôles parentaux ###
############################################################

freq(infos$sexe)
tab <- infos[is.na(infos$sexe), ] # Brigitte et Sandra, on peut supposer que ce sont des femmes 


## Sandra et Brigitte
infos[infos$identifiant == "BRIGITTE 1959-06-15", c("sexe", "PRENOM.x", "NOMNAISSANCE")] <-c("Féminin", "Brigitte", "HANSER")
infos[infos$identifiant == "SANDRA 1972-03-26", c("sexe", "PRENOM.x", "NOMNAISSANCE")] <-c("Féminin", "Sandra", "CREBOIS")

infos <- infos %>%
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
      sexe == "Féminin"  ~ "Madame",
      sexe == "Masculin" ~ "Monsieur",
    ),
    parentMF = case_when(
      sexe == "Féminin"  ~ "mère",
      sexe == "Masculin" ~ "père", 
    )
  )

tab <- infos[is.na(infos$civilite), ] # Brigitte et Sandra, on peut supposer que ce sont des femmes 
#ok
rm(tab)


############################################################
### 4. Indicateurs de séparation et résidence des enfants ###
############################################################

infos <- infos %>%
  mutate(
    # Séparation avec autorité parentale partagée
    SEP_AUT_PAR_ENF = case_when(
      if_any(starts_with("SEP_AUT_PAR_ENF"), ~ . == "Oui", na.rm = TRUE) ~ "Oui",
      if_all(starts_with("SEP_AUT_PAR_ENF"), ~ is.na(.)) ~ NA_character_,
      TRUE ~ "Non"
    ),
    # Enfant vivant ailleurs
    AILL_AUT_PAR_ENF = case_when(
      if_any(
        starts_with("PARENT_VIT"),
        ~ . %in% c("Non, il/elle vit ailleurs", "Non, il/elle est décédé(e)"),
        na.rm = TRUE
      ) ~ "Oui",
      if_all(starts_with("PARENT_VIT"), ~ is.na(.)) ~ NA_character_,
      TRUE ~ "Non"
    )
  )



############################################################
### 5. Typologie familiale (sous-populations)            ###
############################################################

infos <- infos %>%
  mutate(
    souspop = case_when(
      str_starts(COUPLE, "Oui") & 
        (SEP_AUT_PAR_ENF == "Oui" | ENFAV_C == "Oui") ~ "Recomposée",
      str_starts(COUPLE, "Oui") & 
        (SEP_AUT_PAR_ENF == "Non" | is.na(SEP_AUT_PAR_ENF)) ~ "Couple parental",
      str_starts(COUPLE, "Non") ~ "Monoparentale",
      TRUE ~ "Autre situation"
    )
  )

tab <- infos[infos$souspop == "Autre situation", ] 
#ok
rm(tab)

############################################################
### 6. Comptage des enfants selon l’âge et la résidence  ###
############################################################
infos
infos <- infos %>%
  rowwise() %>%
  mutate(
    nb_enfantsDCD = sum(c_across(starts_with("DC_ENF")) == "Non", na.rm = TRUE),
    nb_enfantsVivants = NBENF - nb_enfantsDCD,
    nb_jeunesEnfants7 = sum(c_across(starts_with("ANAI_ENF")) > 2018, na.rm = TRUE),
    nb_enfantsMajeurs = sum(c_across(starts_with("ANAI_ENF")) <= 2006, na.rm = TRUE),
    nb_enfantsAilleurs = sum(c_across(starts_with("ENFAIL")) <= 2006, na.rm = TRUE)
  ) %>%
  ungroup()


############################################################
### 7. Description familiale personnalisée               ###
############################################################

infos <- infos %>%
  mutate(
    couple_parental_precis = case_when(
      COUPLE == "Oui, avec quelqu'un qui vit dans un autre logement" ~ "couple qui ne cohabite pas",
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


############################################################
### 10. Sélection finale des variables utiles            ###
############################################################

infos <- infos %>%
  select(
    identifiant,
    PRENOM.x,
    NOMFAMILLE,
    anai, 
    civilite,
    sexe,
    tp, 
    situat, 
    rs,
    dipl, 
    matr, 
    stat, 
    empl,
    STATUT_C,
    ANAI_C,
    profession, 
    COUPLE,
    PRENOM_C,
    SEXE_C,
    ENFAV_C, 
    NBENFAV_C,
    NBENF, 
    nb_enfantsDCD,
    souspop,
    enfants_logement,
    enfants_ailleurs,
   description_personalisee, 
   profession
  )

head(infos, 15)


############################################################
### 11. Jointure finale avec les coordonnées géocodées   ###
############################################################


saveRDS(coordonnees_geo,"1_data/processed/coordonnees_geocodes.rds")
infos <- left_join(coordonnees_geo, infos)
saveRDS(infos, "1_data/processed/donnees_contact.rds")
glimpse(infos)

# Supposons que ton dataframe s'appelle coordonnees_geo
coordonnees_clean <- infos %>%
  mutate(
    identifiant, 
    civilite, 
    nom = NOMFAMILLE, 
    prenom = PRENOM.x, 
    adresse = AdresseConsolidee, 
    AdresseSimple, 
    CPCommune,
    email = POSTENQ_MAIL,
    telephone = POSTENQ_TEL) %>%
  select(identifiant, civilite, nom, prenom, adresse, AdresseSimple, CPCommune, email, telephone)

# Sauvegarde en CSV si besoin

write_csv(coordonnees_clean, "1_data/coordonnees_contact.csv")
saveRDS(coordonnees_clean, "1_data/coordonnees_contact.rds")
glimpse(coordonnees_clean)


## vCard pour gestion des contacts 
donnees_contact <- readRDS("1_data/processed/donnees_contact.rds") %>%
  select(-starts_with("result"))

glimpse(donnees_contact)

contacts_vcard <- donnees_contact %>%
  transmute(
    uid = identifiant,
    prenom = PRENOM.x,
    nom = as.character(NOMFAMILLE),
    civilite = civilite,
    email = na_if(POSTENQ_MAIL, ""),
    tel = na_if(POSTENQ_TEL, ""),
    rue = AdresseSimple,
    cp = CodePostal,
    ville = Commune,
    latitude = latitude,
    longitude = longitude,
    souspop = souspop,
    note = glue("{description_personalisee} / {str_to_lower(profession)} chez {rs} ({dipl})"))


write_vcard_zimbra <- function(df, file = "contacts_zimbra.vcf") {
  
  vcard <- apply(df, 1, function(row) {
    
    adr <- paste(
      "", "",
      paste0(row["rue"]),
      row["ville"],
      "",
      row["cp"],
      "France",
      sep = ";"
    )
    
    c(
      "BEGIN:VCARD",
      "VERSION:3.0",
      
      paste0("UID:", row["uid"]),
      paste0(
        "N:",
        row["nom"], ";",
        row["prenom"], ";;",
        row["civilite"], ";"
      ),
      paste0(
        "FN:",
        if (!is.na(row["civilite"])) paste0(row["civilite"], " "),
        row["prenom"], " ",
        row["nom"]
      ),
      
      if (!is.na(row["tel"]))
        paste0("TEL;TYPE=CELL:", row["tel"]),
      
      if (!is.na(row["email"]))
        paste0("EMAIL;TYPE=INTERNET:", row["email"]),
      
      if (!is.na(row["rue"]))
        paste0("ADR;TYPE=HOME:", adr),
      
      if (!is.na(row["souspop"]))
        paste0("CATEGORIES:", row["souspop"]),
      
      if (!is.na(row["note"]))
        paste0("NOTE:", row["note"]),
      
      if (!is.na(row["latitude"]) & !is.na(row["longitude"]))
        paste0("GEO:", row["latitude"], ";", row["longitude"]),
      
      "END:VCARD",
      ""
    )
  })
  
  writeLines(unlist(vcard), file, useBytes = TRUE)
}


write_vcard_zimbra(contacts_vcard, "4_communication/contacts_zimbra.vcf")

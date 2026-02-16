
source("fonctions/match_vars.R")
vars_EF <- readRDS("1_data/processed/vars_EF.rds")
EF <- readRDS("1_data/processed/EF.rds")

# Uniformisation des noms de variables
names(EF)
EF <- EF %>%
  select(ends_with("\\.y"))
names(EF) <- str_remove_all(names(EF), "_x|\\.x|\\.y|_merge")


# On ajoute des variables qui manque ###########################################
names(EF)[!(names(EF) %in% vars_EF$Variable)]
namesEF_RP <- names(EF)[str_starts(names(EF), "RP")]
namesEF_tech <- names(EF)[str_detect(names(EF), "[lower]") | str_detect(names(EF), "ID|id|CABFL|VAL|QUEST|REP|PRENOM_FIN")]

namesEF <- names(EF)[!(names(EF) %in% c(namesEF_tech, namesEF_RP))]

namesEF

correspondances <-  trouver_correspondances(namesEF, 
                                            vars_EF$Variable)

head(vars_EF)



# Variable "AIDE_APPORT" ####
temp <- vars_EF[vars_EF$Variable == "AIDE_APPORT", ]
temp$Question <- temp$Question %>%
  str_replace("\\(", "?") %>%
  str_sub(1, 173) 
temp <- temp[rep(1, 5), ]
temp$Variable <- paste0(temp$Variable, 1:5)
temp$Question <- paste(temp$Question, c("Oui, une aide pour les tâches quotidiennes", 
                                        "Oui, une aide financière ou matérielle", 
                                        "Oui, un soutien moral",
                                        "Oui, vous êtes tuteur / tutrice ou curateur/curatrice",
                                        "Non, aucune aide de ce type"))
temp$Modalites <- "1 - Oui | 2 - Non"

vars_EF <- bind_rows(vars_EF %>%
                       filter(Variable != "AIDE_APPORT"), 
                     temp)

temp$Variable <- paste0("QUI_AID_APP_", 1:5)
temp <- temp[-5, ]
temp$Question <- temp$Question %>%
  str_sub( 175, -1)
temp$Question <- paste0("A qui ? ", temp$Question)
temp <- temp[rep(1:4, 5+4+4+3), ]
temp <- temp %>% arrange(Variable)
temp$Variable <- paste0(temp$Variable, rep(c(rep("A", 5), rep("B", 4), rep("C", 4), rep("D", 3)), 4))
temp$Variable <- paste0(temp$Variable, rep(c(1:4, "D", 1:3, "D", 1:3, "D", 1:2, "D"), 4))
temp <- temp %>%
  mutate(Question = case_when(
    str_ends(Variable, "1") ~ str_glue("{Question} Vos parents/votre parent"), 
    str_ends(Variable, "D") ~ str_glue("{Question} Autre : en clair"), 
    str_ends(Variable, "A2|B2") ~ str_glue("{Question} Votre conjoint-e"), 
    str_ends(Variable, "A3|C2") ~ str_glue("{Question} Votre/vos enfant(s)"), 
    str_ends(Variable, "A4|B3|C3|D2") ~ str_glue("{Question} Un autre membre de votre famille")
  ))

temp <- temp %>%
  mutate(Modalites = if_else(
    str_ends(Variable, "D"), NA, Modalites))

vars_EF <- bind_rows(vars_EF, temp)


## AIde recue ####

temp <- vars_EF[vars_EF$Variable == "AIDE_RECUE", ]
temp$Question <- temp$Question %>%
  str_replace("\\(", "?") %>%
  str_sub(1, 204) 
EF$AIDE_RE
temp <- temp[rep(1, 4), ]
temp$Variable <- paste0(temp$Variable, 1:4)

temp$Question <- paste(temp$Question, c("Oui, une aide pour les tâches quotidiennes", 
                                        "Oui, une aide financière ou matérielle", 
                                        "Oui, un soutien moral",
                                        "Non, aucune aide de ce type"))
temp$Modalites <- "1 - Oui | 2 - Non"

vars_EF <- bind_rows(vars_EF %>%
                       filter(Variable != "AIDE_RECUE"), 
                     temp)

temp$Variable <- paste0("QUI_AID_REC_", 1:4)
temp <- temp[-4, ]
temp$Question <- temp$Question %>%
  str_sub( 206, -1)
temp$Question <- paste0("De qui ? ", temp$Question)
temp <- temp[rep(1:3, 5+4+4+3), ]
temp <- temp %>% arrange(Variable)
temp$Variable <- paste0(temp$Variable, rep(c(rep("A", 5), rep("B", 4), rep("C", 4), rep("D", 3)), 3))
temp$Variable <- paste0(temp$Variable, rep(c(1:4, "D", 1:3, "D", 1:3, "D", 1:2, "D"), 3))
temp <- temp %>%
  mutate(Question = case_when(
    str_ends(Variable, "1") ~ str_glue("{Question} Vos parents/votre parent"), 
    str_ends(Variable, "D") ~ str_glue("{Question} Autre : en clair"), 
    str_ends(Variable, "A2|B2") ~ str_glue("{Question} Votre conjoint-e"), 
    str_ends(Variable, "A3|C2") ~ str_glue("{Question} Votre/vos enfant(s)"), 
    str_ends(Variable, "A4|B3|C3|D2") ~ str_glue("{Question} Un autre membre de votre famille")
  ))

temp <- temp %>%
  mutate(Modalites = if_else(
    str_ends(Variable, "D"), NA, Modalites))

vars_EF <- bind_rows(vars_EF, temp)

names(EF)[!(names(EF) %in% vars_EF$Variable)]



# Fréquence contact parents ####

temp <- vars_EF %>%
  filter(Variable == "FREQ_VU_PAR1")
temp$Valeur <- "Ordinale"
temp <- temp[rep(1, 2), ]
temp$Variable[2] <- "FREQ_CONT_PAR1"
temp$Question[2] <- "A quelle fréquence communiquez-vous avec votre parent (téléphone, mail, SMS, appel vidéo, ...) ?"

vars_EF <- bind_rows(vars_EF %>%
                       filter(Variable != "FREQ_VU_PAR1"), temp)
# Meme chose petits enfants 

temp$Variable <- str_replace_all(temp$Variable, "PAR1", "PETIT_ENF1")
temp$Question <- str_replace_all(temp$Question, "votre parent", "votre (vos) petit-enfant(s)")

temp

vars_EF <- bind_rows(vars_EF, temp)

# Vie a proximité 
temp <- vars_EF %>%
  filter(str_starts(Variable, "PROX")) %>%
  add_row(Variable= "PROX_VIE_CJT", 
          Question = "Vivez-vous à moins d'une heure de votre conjoint(e) ?", 
          Modalites = "1 - Oui | 2 - Non", 
          Type = "Qualitative", 
          Valeur = "Boléenne") 
temp <- temp[rep(1, 3), ]
temp$Variable[2:3] <- c("PROX_EGO_PAR1", "PROX_FRAT_PAR1")
temp$Question[2:3] <- c("Vivez-vous à moins d'une heure de chez votre parent ?", 
                        "Votre parent a-t-il/elle d'autres enfants que vous qui vivent à moins d'une heure de chez lui/elle ?")
vars_EF <- bind_rows(vars_EF, temp)

# variables manquantes pour les enfants (logement et ailleurs)
vars_EF <- vars_EF %>%
  add_row(
    Variable = "AG_ENFLOG1",
    Question = "Age de l'enfant vivant dans le logement",
    Modalites = NA,
    Type = "Quantitative"
  ) %>%
  add_row(
    Variable = "AG_ENFAIL1",
    Question = "Age de l'enfant vivant ailleur",
    Modalites = NA,
    Type = "Quantitative"
  ) %>%
  add_row(
    Variable = "ADOPT_ENFLOG1",
    Question = "Adoption de l'enfant vivant dans le logement",
    Modalites = "1 - Oui | 2 - Non",
    Type = "Qualitative", 
    Valeur = "Boléenne",
  ) %>%
  add_row(
    Variable = "ADOPT_ENFAIL1",
    Question = "Adoption de l'enfant vivant ailleur",
    Modalites = "1 - Oui | 2 - Non",
    Type = "Qualitative", 
    Valeur = "Boléenne"
  ) %>%
  add_row(
    Variable = "ANADOPT_ENFLOG1",
    Question = "Année de l'adoption de l'enfant vivant dans le logement",
    Modalites = NA,
    Type = "Quantitative"
  ) %>%
  add_row(
    Variable = "ANADOPT_ENFAIL1",
    Question = "Année de l'adoption de l'enfant vivant ailleur",
    Modalites = NA,
    Type = "Quantitative"
  ) %>%
  add_row(
    Variable = "PNAIPAR1",
    Question = "Pays de naissance du parent",
    Modalites = NA,
    Type = "Qualitative",
    Valeur = "Catégorielle"
  ) %>%
  add_row(
    Variable = "TRAV_C_C", 
    Question = "Votre conjoint-e (ou dernièr-e conjoint-e) travaille t-il/elle ou a t il/elle déjà travailé ?",
    Modalites = "1 - Oui | 2 - Non", 
    Type = "Qualitative",
    Valeur = "Boléenne")


# Type de placement 
temp <- vars_EF %>%
  filter(str_starts(Variable, "TYP_PLAC"))
temp <- temp %>% 
  add_row(
    Variable = "TYP_PLACE1",
    Question = "Quel était le type de placement ?",
    Modalites = "1- Oui | 2 - Non",
    Type = "Qualitative",
    Valeur = "Boléenne"
  )

temp <- temp[rep(1, 4), ]
temp$Variable <- c(paste0(rep("TYP_PLACE", 4), 1:4))
temp$Question <- c(paste0(temp$Question, " ",
                          c("Placement en foyer", 
                            "Placement en famille d'accueil", 
                            "Placement chez une personne de la famille", 
                            "Autre type de placement")))
vars_EF <- bind_rows(vars_EF, temp)


# Précisions sur le statut d'emploi 

temp <- vars_EF %>%
  filter(str_starts(Variable, "SAL_"))

temp <- temp %>%
  add_row(Variable = "SAL_IND_C", 
          Question = "En le/la comptant, combien de personnes travaillent/travaillaient dans son entreprise ?", 
          Modalites = "1 - Une seule personne : il/elle travaille ou travaillait seul(e) | 2 - Entre 2 à 10 personnes | 3 - Entre 11 à 49 personnes | 4 - 50 personnes ou plus", 
          Type = "Qualitative", 
          Valeur = "Ordinale") %>%
  add_row(Variable = "SAL_FP_C", 
          Question = "Dans son emploi, il/elle est / était ...",
          Modalites = "1 - Manœuvre, ouvrier(ère) spécialisé(e) | 2 - Ouvrier(ère) qualifié(e) | 3 – Technicien(ne) | 4 - Agent(e) de catégorie C de la fonction publique | 5 – Agent(e) de catégorie B de la fonction publique | 6 – Agent(e) de catégorie A de la fonction publique | 7 - Dans une autre situation, je ne sais pas",
          Type = "Qualitative", 
          Valeur = "Ordinale") %>%
  add_row(Variable = "SAL_ENT_C", 
          Question = "Dans son emploi, il/elle est / était ...",
          Modalites = "1 - Manœuvre, ouvrier(ère) spécialisé(e) | 2 – Ouvrier(ère) qualifié(e), technicien(ne) d'atelier | 3 – Employé(e) de bureau, de commerce, de services | 4 - Agent de maîtrise (y compris administrative ou commerciale) | 5 - Technicien(ne) | 6 - Ingénieur(e), cadre d'entreprise | 7 - Dans une autre situation, je ne sais pas",
          Type = "Qualitative", 
          Valeur = "Ordinale") %>%
  add_row(Variable = "SAL_IND_PAR1", 
          Question = "En le/la comptant, combien de personnes travaillent/travaillaient dans son entreprise ?", 
          Modalites = "1 - Une seule personne : il/elle travaille ou travaillait seul(e) | 2 - Entre 2 à 10 personnes | 3 - Entre 11 à 49 personnes | 4 - 50 personnes ou plus", 
          Type = "Qualitative", 
          Valeur = "Ordinale") %>%
  add_row(Variable = "SAL_FP_PAR1", 
          Question = "Dans son emploi, il/elle est / était ...",
          Modalites = "1 - Manœuvre, ouvrier(ère) spécialisé(e) | 2 - Ouvrier(ère) qualifié(e) | 3 – Technicien(ne) | 4 - Agent(e) de catégorie C de la fonction publique | 5 – Agent(e) de catégorie B de la fonction publique | 6 – Agent(e) de catégorie A de la fonction publique | 7 - Dans une autre situation, je ne sais pas",
          Type = "Qualitative", 
          Valeur = "Ordinale") %>%
  add_row(Variable = "SAL_ENT_PAR1", 
          Question = "Dans son emploi, il/elle est / était ...",
          Modalites = "1 - Manœuvre, ouvrier(ère) spécialisé(e) | 2 – Ouvrier(ère) qualifié(e), technicien(ne) d'atelier | 3 – Employé(e) de bureau, de commerce, de services | 4 - Agent de maîtrise (y compris administrative ou commerciale) | 5 - Technicien(ne) | 6 - Ingénieur(e), cadre d'entreprise | 7 - Dans une autre situation, je ne sais pas",
          Type = "Qualitative", 
          Valeur = "Ordinale")

vars_EF <- bind_rows(vars_EF, temp)

# Interuptions de dtravail 

temp <- vars_EF %>%
  filter(str_starts(Variable, "INTER_TRAV")) 
temp$Modalites <- "1 - Oui | 2 - Non"
temp$Valeur <- "Boléenne"
temp <- temp[rep(1, 3), ]
temp$Variable <- paste0("INTER_TRAV", 1:3)
temp$Question <- paste0(str_remove(temp$Question, "...\\(\\*\\)"), 
                        c("toujours travaillé sans interruption", 
                          "eu une ou plusieurs périodes de chômage d'au moins six mois", 
                          "eu d'autres interruptions d'au moins six mois"))

vars_EF <- bind_rows(vars_EF, temp)

# date de naissance 

temp <- vars_EF %>%
  filter(str_starts(Variable, "DATNAI")) 
temp <- temp[rep(1, 4), ]
temp$Variable <- paste0(c("DAT", "J", "M", "A"), "NAIS_C")
temp$Question[2] <- str_replace(temp$Question[2], "la date", "le jour")
temp$Question[3] <- str_replace(temp$Question[3], "la date", "le mois")
temp$Question[4] <- str_replace(temp$Question[4], "la date", "l'année")


vars_EF <- bind_rows(vars_EF, temp[2:4, ])
temp
temp <- data.frame(Variable = "AGE", Question = "Age (calculé)", Modalites = NA, Type = "Qualitative", Valeur = "entiers naturels", Filtre = NA)

vars_EF <- bind_rows(vars_EF, temp)


# Certaines variables de sexe sont codées à l'envers pour les femmes et les hommes dans l'EF, on doit corriger cela
gender_var <- vars_EF %>%
  filter(str_ends(Variable, "H|F")) %>%
  filter(str_detect(Variable, "SEXE")) 
# On va changer les valeurs pour ces variables puis on corrigera le dictionnaire 
gender_var$Variable
names(BI)[names(BI) %in% names(EF)]

EF <- left_join(EF, BI[, c("identifiant", "sexe")], by = "identifiant")
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

gender_var$Modalites

vars_EF <- vars_EF %>%
  mutate(Modalites = case_when(
    Variable %in% gender_var$Variable ~ "1 - Un homme | 2 - Une femme", 
    TRUE ~ Modalites
  ))
EF$sexe <- NULL

correspondances <-  trouver_correspondances(namesEF, 
                                            vars_EF$Variable)
saveRDS(vars_EF, )



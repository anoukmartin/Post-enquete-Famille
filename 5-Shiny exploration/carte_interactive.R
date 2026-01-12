# Carte interactive ##############################################################################

install.packages(c("leaflet", "tidygeocoder", "dplyr"))
library(leaflet)


coordonnees <- readRDS("1_data/processed/donnees_contact.rds")


  
here()
# Fiches individuelles 
coordonnees <- coordonnees %>%
  mutate(fiche_info = str_glue("{here()}/3_reporting/individus_html/{id_individu}_fiche_infos.html")) %>%
  mutate(popup = paste0(
    "<div style='width: 340px;'>",
    
    "<h4>", identifiant, " (ID)</h4>",
    
    "<p>",
    "📞 <b>Téléphone :</b> ", POSTENQ_TEL, "<br>",
    "📧 <b>Email :</b> ", POSTENQ_MAIL, "<br>",
    "📍 <b>Adresse :</b> ", AdresseConsolidee,
    "</p>",
    
    "<hr>",
    
    "<b>🧍 Individu</b>",
    "<ul>",
    "<li><b>Prénom :</b> ", coordonnees$PRENOM.x, "</li>",
    "<li><b>Nom :</b> ", coordonnees$NOMFAMILLE, "</li>",
    "<li><b>Âge :</b> ", 2026 - coordonnees$anai, "</li>",
    "<li><b>Sexe :</b> ", coordonnees$sexe, "</li>",
    "<li><b>En emploi :</b> ", coordonnees$situat, "</li>",
    "<li><b>Niveau de diplôme :</b> ", coordonnees$dipl, "</li>",
    "<li><b>Statut d'emploi :</b> ", coordonnees$empl, "</li>",
    "<li><b>Temps de travail :</b> ", coordonnees$tp, "</li>",
    "<li><b>Profession :</b> ", str_to_sentence(coordonnees$profession), "</li>",
    "<li><b>Entreprise :</b> ", coordonnees$rs, "</li>",
    "</ul>",
    
    "<b>💍 Situation conjugale</b>",
    "<ul>",
    "<li><b>En couple :</b> ", coordonnees$COUPLE, "</li>",
    "<li><b>Statut matrimonial :</b> ", coordonnees$matr, "</li>",
    "</ul>",
    
    "<b>🧑‍🤝‍🧑 Dernier conjoint</b>",
    "<ul>",
    "<li><b>Prénom :</b> ", coordonnees$PRENOM_C, "</li>",
    "<li><b>Âge :</b> ", 2026 - coordonnees$ANAI_C, "</li>",
    "<li><b>Sexe :</b> ", coordonnees$SEXE_C, "</li>",
    "<li><b>Emploi :</b> ", coordonnees$STATUT_C, "</li>",
    "</ul>",
    
    "<b>👶 Enfants</b>",
    "<ul>",
    "<li><b>Dans le logement :</b> ", coordonnees$enfants_logement, "</li>",
    "<li><b>Résidant ailleurs :</b> ", coordonnees$enfants_ailleurs, "</li>",
    "<li><b>Enfants du conjoint :</b> ", coordonnees$ENFAV_C, "</li>",
    "<li><b>Enfants décédés :</b> ", coordonnees$nb_enfantsDCD, "</li>",
    "</ul>",
    
    "<b>🔁 Synthétique </b>",
    "<ul>",
    "<li><b>Sous-population :</b> ", coordonnees$souspop, "</li>",
    "<li><b>Situation familiale :</b> ", coordonnees$description_personalisee, "</li>",
    "</ul>",
    
    "<a href='", coordonnees$fiche_info, "'target='_blank'> 📄 Fiche individuelle </a>",
    
    
    "</div>"
  ))

# Leaflet carte
leaflet(coordonnees) %>%
  addTiles() %>%
  addMarkers(
    lng = ~longitude,
    lat = ~latitude,
    popup = ~popup, 
    label = ~id_individu,
    popupOptions = popupOptions(maxWidth = 700)
  )


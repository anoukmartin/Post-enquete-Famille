# app.R

library(shiny)
library(leaflet)
library(dplyr)
library(DT)
library(stringr)
library(shinythemes)

# -------------------------
# Chargement des données

color_subpop <- c(
  "Monoparentale" = "pink",  # rose pastel
  "Couple parental" = "lightblue", # bleu pastel
  "Recomposée" = "lightgreen"       # vert pastel
)

coordonnees <- readRDS("1_data/processed/donnees_contact.rds") %>%
  mutate(
    shape_icon = ifelse(sexe == "Féminin", "circle", "square"),
    subpop_color = case_when(
      souspop == "Monoparentale" ~ "pink",
      souspop== "Couple parental" ~ "lightblue",
      souspop == "Recomposée" ~ "lightgreen"
    )
  ) %>%
  mutate(
    fiche_html_path = paste0("3_reporting/individus_html/", identifiant, "_fiche_infos.html"),
    popup = paste0(
      "<div style='width: 340px;'>",
      
      "<h4>", identifiant, " (ID)</h4>",
      
      # 🔘 Bouton pour afficher la fiche dans l'onglet 3
      "<button onclick=\"Shiny.setInputValue('go_fiche', '", identifiant, "', {priority: 'event'})\">",
      "📄 Voir la fiche individuelle",
      "</button>",
      
      "<p>",
      "📞 <b>Téléphone :</b> ", POSTENQ_TEL, "<br>",
      "📧 <b>Email :</b> ", POSTENQ_MAIL, "<br>",
      "📍 <b>Adresse :</b> ", AdresseConsolidee,
      "</p>",
      
      "<hr>",
      
      "<b>🧍 Individu</b>",
      "<ul>",
      "<li><b>Prénom :</b> ", PRENOM.x, "</li>",
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
      "</ul>",
      
      
      "</div>"
    )
  )



icons_custom <- lapply(1:nrow(coordonnees), function(i) {
  awesomeIcons(
    icon = ifelse(coordonnees$sexe[i]=="Féminin","circle","square"),
    markerColor = case_when(
      coordonnees$souspop[i] == "Monoparentale" ~ "pink",
      coordonnees$souspop[i] == "Couple parental" ~ "lightblue",
      coordonnees$souspop[i] == "Recomposée" ~ "lightgreen",
      TRUE ~ "gray"
    ),
    library = "fa",
    iconColor = "white"
  )
})



pal <- colorFactor("viridis")

questionnaires <- readRDS("1_data/processed/BIEF.Rds")

# -------------------------
# Charger toutes les fiches HTML dans un data.frame
fiche_folder <- "3_reporting/individus_html/"
fiche_files <- list.files(fiche_folder, pattern = "_fiche_infos\\.html$", full.names = TRUE)

fiche_data <- lapply(fiche_files, function(f) {
  identifiant <- str_extract(basename(f), "^[^_]+")  # partie avant _fiche_infos.html
  content <- paste(readLines(f, warn = FALSE), collapse = "\n")
  data.frame(identifiant = identifiant, fiche_html = content, stringsAsFactors = FALSE)
}) %>% bind_rows()

# -------------------------
ui <- navbarPage(
  title = "Dashboard Enquête",
  id = "navbarPage",  # 🔑 nécessaire pour updateTabsetPanel
  theme = shinytheme("flatly"),
  
  # ----------------- Onglet 1 : Statistiques descriptives -----------------
  tabPanel(
    "📊 Statistiques descriptives",
    h3("Statistiques descriptives des individus"),
    p("À compléter...")
  ),
  
  # ----------------- Onglet 2 : Carte interactive -----------------
  tabPanel(
    "🗺️ Carte",
    leafletOutput("map", height = "80vh")
  ),
  
  # ----------------- Onglet 3 : Fiches individuelles -----------------
  tabPanel(
    "📄 Fiches individuelles",
    br(),
    textInput("search_text", "Chercher dans les fiches :", ""),
    uiOutput("fiche_search_results"),
    hr(),
    uiOutput("fiche_selected")
  ),
  
  # ----------------- Onglet 4 : Explorateur base de données -----------------
  tabPanel(
    "🗃️ Explorateur base",
    DTOutput("table_db")
  )
)

pal <- colorFactor(c("blue", "red", "green"), domain = c("Couple parental", "Monoparentale", "Recomposée"))
# -------------------------
server <- function(input, output, session) {
  
  # reactiveVal pour stocker la fiche sélectionnée
  selected_id <- reactiveVal(NULL)
  
  # ----------------- Onglet 2 : Carte Leaflet -----------------
  output$map <- renderLeaflet({

    
    leaflet(coordonnees) %>%
      addTiles() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addCircleMarkers(
        lng = ~longitude,
        lat = ~latitude,
        color = ~pal(souspop),
        stroke = FALSE, fillOpacity = 0.7,
        #icon = icons_custom,
        layerId = ~identifiant,
        label = ~identifiant,
        popup = ~popup,
        popupOptions = popupOptions(maxWidth = 400)
      ) %>%
      addLegend(data = coordonnees,
                position = "bottomleft",
                pal = pal, values = ~souspop,
                title = "Legend",
                opacity = 1) %>%
      addMiniMap(width = 150, height = 150)
  })
  
  # ----------------- Onglet 3 : Recherche fiches individuelles -----------------
  
  # Filtrer les fiches selon la recherche plein texte
  fiche_filtered <- reactive({
    req(input$search_text)
    txt <- str_to_lower(input$search_text)
    fiche_data %>% filter(str_detect(str_to_lower(fiche_html), txt))
  })
  
  # Afficher les boutons correspondant aux fiches trouvées
  output$fiche_search_results <- renderUI({
    fiches <- fiche_filtered()
    if (nrow(fiches) == 0) {
      HTML("<p style='color:red;'>Aucune fiche trouvée.</p>")
    } else {
      lapply(fiches$identifiant, function(id) {
        actionButton(inputId = paste0("btn_", id),
                     label = id,
                     style = "margin: 2px;")
      })
    }
  })
  
  # Observer les clics sur les boutons pour afficher la fiche HTML
  observe({
    fiches <- fiche_filtered()
    lapply(fiches$identifiant, function(id) {
      btn <- paste0("btn_", id)
      observeEvent(input[[btn]], {
        selected_id(id)
      })
    })
  })
  
  # Observer le clic sur le bouton du popup Leaflet
  observeEvent(input$go_fiche, {
    selected_id(input$go_fiche)
    updateTabsetPanel(session, inputId = "navbarPage",
                      selected = "📄 Fiches individuelles")
  })
  
  # Afficher la fiche correspondant à selected_id()
  output$fiche_selected <- renderUI({
    req(selected_id())
    fiche_html <- fiche_data$fiche_html[fiche_data$identifiant == selected_id()]
    tags$div(HTML(fiche_html))
  })
  
  # ----------------- Onglet 4 : Explorateur base de données -----------------
  output$table_db <- renderDT({
    questionnaires
  }, options = list(pageLength = 10, scrollX = TRUE))
}

# -------------------------
shinyApp(ui, server)


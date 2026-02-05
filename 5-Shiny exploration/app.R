# app.R

library(shiny)
library(leaflet)
library(dplyr)
library(tidyverse)
library(DT)
library(stringr)
library(shinythemes)
library(shinydashboard)
library(here)
library(webshot2)
library(shinyjs)


# -------------------------
# Chargement des données



coordonnees <- readRDS(here("1_data/processed/donnees_contact.rds")) %>%
  mutate(
    fiche_html_path = paste0("3_reporting/individus/html_complets/", identifiant, "_fiche_infos.html"),
    popup = paste0(
      "<div style='width: 600px;'>",
      
      "<h4>", identifiant, " (ID)</h4>",
      
      # 🔘 Bouton pour afficher la fiche dans l'onglet 3
      "<button onclick=\"Shiny.setInputValue('go_fiche', '", identifiant, "', {priority: 'event'})\">",
      "📄 Voir la fiche individuelle",
      "</button>",
      
      "<button onclick=\"Shiny.setInputValue('toggle_favori', '", identifiant, "', {priority: 'event'})\">",
      "⭐ Favori",
      "</button>",
      
      coordonnes_html, 
      
      "<hr>",
      
      resume_sociodemo_html,
      
      
      "</div>"
    )
  )




favoris_path <- here("1_data/processed/favoris.rds")

if (file.exists(favoris_path)) {
  favoris_init <- readRDS(favoris_path)
} else {
  favoris_init <- character(0)
  saveRDS(favoris_init, favoris_path)
}


tags_path <- here("1_data/processed/tags.rds")

if (file.exists(tags_path)) {
  tags_init <- readRDS(tags_path)
} else {
  tags_init <- list()
  saveRDS(tags_init, tags_path)
}

tags <- reactiveVal(tags_init)

questionnaires <- readRDS(here("1_data/processed/BIEF.rds")) %>%
  mutate(
    fiche = sprintf(
      '<button class="btn btn-sm btn-primary" onclick="Shiny.setInputValue(\'go_fiche\', \'%s\', {priority: \'event\'})">Voir fiche</button>',
      identifiant
    )
  )




# -------------------------
# Charger toutes les fiches HTML dans un data.frame
fiche_folder <- here("3_reporting/individus/html_complets/")
fiche_files <- list.files(fiche_folder, pattern = "_fiche_infos\\.html$", full.names = TRUE)

fiche_data <- lapply(fiche_files, function(f) {
  identifiant <- str_extract(basename(f), "^[^_]+")  # partie avant _fiche_infos.html
  content <- paste(readLines(f, warn = FALSE), collapse = "\n")
  data.frame(identifiant = identifiant, fiche_html = content, stringsAsFactors = FALSE)
}) %>% bind_rows()




# -------------------------
ui <- dashboardPage(
  dashboardHeader(title = "Dashboard Enquête"),
  
  dashboardSidebar(
    width = 300,
    
    sidebarMenu(
      id = "sidebarMenu",   # ← OBLIGATOIRE
      menuItem("Statistiques", tabName = "stats", icon = icon("chart-bar")),
      menuItem("Carte", tabName = "map", icon = icon("map")),
      menuItem("Fiches", tabName = "fiches", icon = icon("file-alt")),
      menuItem("Base", tabName = "base", icon = icon("database")),
      
      hr(),
      
      textInput("search_text", "Recherche globale"),
      
      checkboxInput("only_favoris", "Favoris uniquement", FALSE),
      
      selectizeInput(
        "filter_tags",
        "Filtrer par tags",
        choices = NULL,
        multiple = TRUE,
        options = list(placeholder = "Sélectionner un ou plusieurs tags")
      ),
      selectInput("facet_var", "Variable de facet", choices = c("souspop", "NULL")),
      
      selectInput(
        "filtre_souspop",
        "Sous-population",
        choices = c("Toutes", unique(coordonnees$souspop)),
        selected = "Toutes"
      )
    )
  ),
  
  dashboardBody(
    tabItems(
      
      tabItem(
        tabName = "stats",
        
        fluidRow(
          valueBoxOutput("nb_individus", width = 4),
          valueBoxOutput("age_moyen", width = 2), 
          valueBoxOutput("sex_ratio", width = 2), 
          valueBoxOutput("nenfants", width = 2), 
          valueBoxOutput("dipl_ratio", width = 2)
        ),
        plotOutput("plot_sexe"),
        plotOutput("plot_age"),
        h3("Conjugalité"),
        plotOutput("plot_couple"),
        plotOutput("plot_matr"),
        plotOutput("plot_ecartage"),
        h3("Enfants"),
        plotOutput("plot_enfants"),
        plotOutput("plot_enfants_aill"),
        plotOutput("plot_enfants_c"),
        h3("Positions sociales"),
        
        plotOutput("plot_diplome"),
        plotOutput("plot_emploi"),
        plotOutput("plot_statut"),
        plotOutput("plot_contrat"),
        plotOutput("plot_tp"),
        
      ),
      
      tabItem(
        tabName = "map",
        leafletOutput("map", height = "85vh")
      ),
      
      tabItem(
        tabName = "fiches",
        fluidRow(
          column(width = 3, uiOutput("fiche_search_results")),
          column(width = 9, uiOutput("fiche_selected"))
        )
      ),
      
      tabItem(
        tabName = "base",
        DTOutput("table_db")
      )
    )
  )
)

pal <- colorFactor(c("lightblue", "pink", "lightgreen"), domain = c("Couple parental", "Monoparentale", "Recomposée"))
# -------------------------
server <- function(input, output, session) {
  
  # Index
  outvars <- names(coordonnees)[names(coordonnees) %in% names(questionnaires)]
  outvars <- outvars[-1]
  
  index <-  left_join(coordonnees %>% select(-any_of(outvars)), questionnaires, by = "identifiant") %>%
    mutate(across(everything(), as.character)) %>%
    pivot_longer(-identifiant, names_to = "Variable", values_to = "valeur") %>%
    left_join(vars_all2 %>% select(Variable, Question), by = "Variable") %>%
    mutate(valeur = if_else(valeur == "", NA, valeur)) %>%
    mutate(Question = if_else((is.na(Question) | Question == ""), Variable, Question))
  
  # reactiveVal pour stocker la fiche sélectionnée
  selected_id <- reactiveVal(NULL)
  favoris <- reactiveVal(favoris_init)
  toggle_favori <- function(id) {
    f <- favoris()
    if (id %in% f) {
      f <- setdiff(f, id)
    } else {
      f <- c(f, id)
    }
    favoris(f)
    saveRDS(f, favoris_path)
  }
  
  ids_filtrés <- reactive({
    txt <- str_squish(str_to_lower(input$search_text))
    
    # --- filtre texte ---
    if (txt == "") {
      ids <- fiche_data$identifiant
    } else {
      conditions <- str_split(txt, "\\s+")[[1]]
      
      id_lists <- lapply(conditions, function(cond) {
        
        if (!str_detect(cond, ":")) {
          return(
            index %>%
              mutate(valeur_l = str_to_lower(str_squish(valeur))) %>%
              filter(!is.na(valeur_l),
                     str_detect(valeur_l, fixed(cond))) %>%
              pull(identifiant) %>%
              unique()
          )
        }
        
        parts <- str_split(cond, ":", n = 2, simplify = TRUE)
        
        index %>%
          mutate(
            Variable_l = str_to_lower(str_squish(Variable)),
            valeur_l = str_to_lower(str_squish(valeur))
          ) %>%
          filter(
            Variable_l == parts[1],
            !is.na(valeur_l),
            str_detect(valeur_l, fixed(parts[2]))
          ) %>%
          pull(identifiant) %>%
          unique()
      })
      
      ids <- Reduce(intersect, id_lists)
    }
    
    # --- filtre favoris ---
    if (isTRUE(input$only_favoris)) {
      ids <- intersect(ids, favoris())
    }
    
    # --- filtre tags ---
    if (!is.null(input$filter_tags) && length(input$filter_tags) > 0) {
      ids <- ids[
        purrr::map_lgl(
          ids,
          ~ all(input$filter_tags %in% (tags()[[.x]] %||% character(0)))
        )
      ]
    }
    
    # --- filtre sous-population ---
    if (input$filtre_souspop != "Toutes") {
      ids <- intersect(
        ids,
        coordonnees %>%
          filter(souspop == input$filtre_souspop) %>%
          pull(identifiant)
      )
    }
    
    ids
  })
  
  coordonnees_filtrées <- reactive({
    coordonnees %>% filter(identifiant %in% ids_filtrés())
  })
  
  questionnaires_filtrés <- reactive({
    questionnaires %>% filter(identifiant %in% ids_filtrés())
  })
  
  # ----------------- Onglet 1 : stats des ---------------------
  ## Quelques fonction utiles 

  
  plot_categorique <- function(data, fill_var, facet_var = NULL, titre = "") {
    # Transformer les chaînes en symboles pour ggplot
    fill_sym <- sym(fill_var)
    
    # Gérer le facet : NULL si aucun facet
    facet_sym <- if (!is.null(facet_var) && facet_var != "NULL") sym(facet_var) else NULL
    
    p <- data %>%
      ggplot(aes(x = sexe, fill = !!fill_sym)) +
      geom_bar(position = "fill") +
      geom_text(
        aes(label = after_stat(count)),
        stat = "count",
        position = position_fill(vjust = 0.5),
        color = "black"
      ) +
      scale_y_continuous(labels = scales::percent_format()) +
      labs(
        title = titre,
        x = NULL,
        y = "Pourcentage"
      ) +
      coord_flip() +
      scale_fill_brewer(palette = "Set3")+
      theme_minimal(base_size = 14) +
      theme(legend.position = "top", legend.box = "horizontal")
    
    if (!is.null(facet_sym)) {
      p <- p + facet_wrap(vars(!!facet_sym))
    }
    
    return(p)
  }
  
  
  plot_continu <- function(data, y_var, facet_var = NULL) {
    # Transformer les chaînes en symboles pour ggplot
    y_sym <- sym(y_var)
    
    # Gérer le facet : NULL si aucun facet
    facet_sym <- if (!is.null(facet_var) && facet_var != "NULL") sym(facet_var) else NULL
    
    p <- data %>%
      ggplot(aes(fill = sexe, x = !!y_sym)) +
      geom_histogram() +
      # labs( 
      #   title = "",
      #   x = NULL,
      #   y = "Pourcentage"
      # ) +
      #coord_flip() +
      scale_fill_brewer(palette = "Set3")+
      theme_minimal(base_size = 14) +
      theme(legend.position = "top", legend.box = "horizontal")
    
    if (!is.null(facet_sym)) {
      p <- p + facet_wrap(vars(!!facet_sym, sexe), scales = 'free')
    }
    
    return(p)
  }
  
  ## Stats des 
  output$nb_individus <- renderValueBox({
    valueBox(
      value = nrow(coordonnees_filtrées()),
      subtitle = "Individus",
      icon = icon("users"), 
      color = "olive"
    )
  })
  
  output$age_moyen <- renderValueBox({
    valueBox(
      value = round(mean(2025 - coordonnees_filtrées()$anai, na.rm = TRUE), 1),
      subtitle = "Âge moyen",
      icon = icon("birthday-cake"), 
      color = "olive"
    )
  })
  output$sex_ratio <- renderValueBox({
    valueBox(
      value = paste0(round(100*nrow(coordonnees_filtrées() %>% filter(sexe == "Féminin")) / nrow(coordonnees_filtrées()), 1), "%"),
      subtitle = "Sexe-ratio",
      icon = icon("person-dress"), 
      color = "olive"
    )
  })
  output$nenfants <- renderValueBox({
    valueBox(
      value = round(mean(coordonnees_filtrées()$NBENF, na.rm = TRUE), 1),
      subtitle = "Nombre d'enfants",
      icon = icon("baby"), 
      color = "olive"
    )
  })
  output$dipl_ratio <- renderValueBox({
    valueBox(
      value = paste0(round(100*nrow(coordonnees_filtrées() %>% 
                                      filter(dipl %in% c("Aucun diplôme",
                                                         "BEPC, brevet élémentaire, brevet des collèges, DNB",
                                                         "CAP, BEP ou diplôme de niveau équivalent"))) / nrow(coordonnees_filtrées()), 1), "%"),
      subtitle = "Faibles diplômes (>Bac)",
      icon = icon("graduation-cap"), 
      color = "olive"
    )
  })
  
  output$plot_sexe <- renderPlot({
    
    plot_categorique(
      data = coordonnees_filtrées(),
      fill_var = "souspop",
      facet_var = NULL, 
      titre = "Configuration familiale d'égo"
    )
  })
  
  output$plot_age <- renderPlot({
    
    
    # Gérer le facet : NULL si aucun facet
    facet_var <- input$facet_var
    facet_sym <- if (!is.null(facet_var) && facet_var != "NULL") sym(facet_var) else NULL
    
    p <- coordonnees_filtrées() %>%
      mutate(age = 2025 - anai) %>%
      ggplot(aes(x = sexe, y = age, fill = !!facet_sym)) +
      geom_boxplot(outlier.alpha = 0.4) +
      labs(
        title = "Age d'égo",
        x = NULL,
        y = "Âge",
      ) +
      coord_flip()+
      scale_fill_brewer(palette = "Set3")+
      theme_minimal(base_size = 14) +
      theme(legend.position = "top", legend.box = "horizontal")
    
    if (!is.null(facet_sym)) {
      p <- p + facet_wrap(vars(!!facet_sym))
    }
    
    p
    
  })
 
  output$plot_couple <- renderPlot({
    
    plot_categorique(
      data = coordonnees_filtrées(),
      fill_var = "COUPLE",
      facet_var = input$facet_var, 
      titre = "Situation conjugale d'égo"
    )
  })
  
  output$plot_matr <- renderPlot({
    
    plot_categorique(
      data = coordonnees_filtrées(),
      fill_var = "matr",
      facet_var = input$facet_var, 
      titre = "Statut matrimonial d'égo"
    )
  })
  
  output$plot_ecartage <- renderPlot({
    
    
    # Gérer le facet : NULL si aucun facet
    facet_var <- input$facet_var
    facet_sym <- if (!is.null(facet_var) && facet_var != "NULL") sym(facet_var) else NULL
    
    p <- coordonnees_filtrées() %>%
      mutate(ecart_age =  anai - ANAI_C) %>%
      ggplot(aes(x = sexe, y = ecart_age, fill = !!facet_sym)) +
      geom_boxplot(outlier.alpha = 0.4) +
      labs(
        title = "Ecart d'age entre ego et son/sa conjoint-e (ancien-ne conjoint-e si monoparentale)",
        x = NULL,
        y = "Année de naissance d'égo - année de naissance du/de la conjoint-e",
      ) +
      coord_flip()+
      scale_fill_brewer(palette = "Set3")+
      theme_minimal(base_size = 14) +
      theme(legend.position = "top", legend.box = "horizontal")
    
    if (!is.null(facet_sym)) {
      p <- p + facet_wrap(vars(!!facet_sym))
    }
    
    p
    
  })
  
  
  ####
  output$plot_enfants <- renderPlot({
    
    plot_categorique(data = coordonnees_filtrées() %>%
                       mutate(NBENF = factor(
                         NBENF,
                         levels = 0:14,
                         ordered = TRUE)),
                 fill_var = "NBENF",
                 facet_var = input$facet_var, 
                 titre = "Nombre d'enfants d'égo")
  })
  
  output$plot_enfants_aill <- renderPlot({
    coordonnees$CBENFAIL
    plot_categorique(data = coordonnees_filtrées() %>%
                       mutate(CBENFAIL = replace_na(CBENFAIL, 0), 
                              nb_enfantsDCD = replace_na(nb_enfantsDCD, 0)) %>%
                       mutate(CBENFAIL = (as.integer(CBENFAIL) - as.integer(nb_enfantsDCD)) %>%
                                factor(levels = 0:14,
                                       ordered = TRUE)),
                     fill_var = "CBENFAIL",
                     facet_var = input$facet_var, 
                     titre = "Nombre d'enfants d'égo résidant ailleurs")
  })
  
  
  output$plot_enfants_c <- renderPlot({
    
    plot_categorique(data = coordonnees_filtrées() %>%
                       #filter(str_starts(COUPLE, "Oui")) %>%
                       mutate(NBENFAV_C = NBENFAV_C %>%
                                replace_na(0) %>%
                                 factor(levels = 0:14,
                         ordered = TRUE)),
                     fill_var = "NBENFAV_C",
                     facet_var = input$facet_var, 
                     titre = "Nombre d'enfant du/de la conjoint-e d'égo (ancien-ne conjoint-e si monoparentale)")
  })
  
  


    output$plot_emploi <- renderPlot({
      plot_categorique(data = coordonnees_filtrées(),
                       fill_var = "situat",
                       facet_var = input$facet_var, 
                       titre = "Situation d'emploi")
    })
    
    output$plot_statut <- renderPlot({
      plot_categorique(data = coordonnees_filtrées(),
                       fill_var = "stat",
                       facet_var = input$facet_var, 
                       titre = "Statut dans l'emploi")
    })
    
    output$plot_contrat <- renderPlot({
      plot_categorique(data = coordonnees_filtrées(),
                       fill_var = "empl",
                       facet_var = input$facet_var, 
                       titre = "Type de contrat")
    })
    
    output$plot_tp <- renderPlot({
      plot_categorique(data = coordonnees_filtrées(),
                       fill_var = "tp",
                       facet_var = input$facet_var, 
                       titre = "Temps de travail")
    })
    
    output$plot_diplome <- renderPlot({
      plot_categorique(data = coordonnees_filtrées(),
                       fill_var = "dipl",
                       facet_var = input$facet_var, 
                       titre = "Niveau de diplome")
    })
 
 
  
  # ----------------- Onglet 2 : Carte Leaflet -----------------
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addProviderTiles(providers$CartoDB.Positron)
  })
  
  observe({
    leafletProxy("map", data = coordonnees_filtrées()) %>%
      clearMarkers() %>%
      clearControls() %>%
      addCircleMarkers(
        lng = ~longitude,
        lat = ~latitude,
        color = ~pal(souspop),
        stroke = FALSE,
        fillOpacity = 0.7,
        layerId = ~identifiant,
        popup = ~popup, 
        popupOptions = popupOptions(
          maxWidth = 800,   # largeur réelle
          minWidth = 600,
          maxHeight = 600,
          autoPan = TRUE
        )
      ) %>%
      addLegend(
        position = "bottomleft",
        pal = pal,
        values = ~souspop,
        title = "Légende"
      )
  })
  
  # ----------------- Onglet 3 : Recherche fiches individuelles -----------------
  all_tags <- reactive({
    unique(unlist(tags()))
  })
  # Filtrer les fiches selon la recherche plein texte
  fiche_filtered <- reactive({
    fiche_data %>%
      filter(identifiant %in% ids_filtrés()) %>%
      arrange(identifiant)
  })
  
  
  
  
  # Afficher les boutons correspondant aux fiches trouvées
  output$fiche_search_results <- renderUI({
    fiches <- fiche_filtered()
    
    if (nrow(fiches) == 0) {
      HTML("<p style='color:red;'>Aucune fiche trouvée.</p>")
    } else {
      tagList(
        lapply(fiches$identifiant, function(id) {
          div(
            style = "margin-bottom: 5px;",
            actionButton(
              inputId = paste0("btn_", id),
              label = id,
              width = "100%", 
              style = "text-align: left; padding-left: 10px;"
            )
          )
        })
      )}
  })
  observe({
    fiches <- fiche_filtered()
    lapply(fiches$identifiant, function(id) {
      btn <- paste0("btn_", id)
      observeEvent(input[[btn]], {
        selected_id(id)
      })
    })
  })
  
  observe({
    updateSelectizeInput(
      session,
      "filter_tags",
      choices = all_tags(),
      server = TRUE
    )
  })
  
  output$fiche_selected <- renderUI({
    req(selected_id())
    id <- selected_id()
    
    fiche_html <- fiche_data$fiche_html[fiche_data$identifiant == id]
    est_favori <- id %in% favoris()
    tags_id <- tags()[[id]]
    
    tagList(
      actionButton(
        "toggle_favori",
        label = if (est_favori) "★ Retirer des favoris" else "☆ Ajouter aux favoris"
      ),
      
      br(), br(),
      
      selectizeInput(
        "new_tag",
        "Ajouter un tag",
        choices = NULL,
        multiple = FALSE,
        options = list(
          create = TRUE,
          placeholder = "ex : prioritaire"
        )
      ),
      actionButton("add_tag", "Ajouter"),
      
      if (!is.null(tags_id) && length(tags_id) > 0) {
        div(
          strong("Tags :"),
          lapply(tags_id, function(tg) {
            actionLink(
              inputId = paste0("remove_tag_", tg),
              label = paste0("✕ ", tg),
              style = "margin-right: 8px;"
            )
          })
        )
      },
      
      hr(),
      htmltools::tags$div(HTML(fiche_html))
    )
  })
  
  
  add_tag <- function(id, tag) {
    t <- tags()
    t[[id]] <- unique(c(t[[id]], tag))
    tags(t)
    saveRDS(t, tags_path)
  }
  
  remove_tag <- function(id, tag) {
    t <- tags()
    t[[id]] <- setdiff(t[[id]], tag)
    tags(t)
    saveRDS(t, tags_path)
  }
  observeEvent(input$add_tag, {
    req(selected_id(), input$new_tag)
    tag <- str_squish(str_to_lower(input$new_tag))
    if (tag != "") {
      add_tag(selected_id(), tag)
      updateSelectizeInput(session, "new_tag", selected = "")
    }
  })
  observe({
    updateSelectizeInput(
      session,
      "new_tag",
      choices = all_tags(),
      server = TRUE
    )
  })
  observe({
    req(selected_id())
    id <- selected_id()
    tags_id <- tags()[[id]]
    
    if (!is.null(tags_id)) {
      lapply(tags_id, function(tg) {
        observeEvent(input[[paste0("remove_tag_", tg)]], {
          remove_tag(id, tg)
        }, ignoreInit = TRUE)
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
  
  observeEvent(input$toggle_favori, {
    req(selected_id())
    toggle_favori(selected_id())
  })
  
  # Observer le clic sur le bouton du popup Leaflet
  observeEvent(input$go_fiche, {
    selected_id(input$go_fiche)
    updateTabItems(session, "sidebarMenu", "fiches")
  })
  
  
  # ----------------- Onglet 4 : Explorateur base de données -----------------
  output$table_db <- renderDT({
    questionnaires_filtrés()
  }, options = list(pageLength = 10, scrollX = TRUE, searchHighlight = TRUE, escape = FALSE), filter = 'top')
  
}

# -------------------------
shinyApp(ui, server)


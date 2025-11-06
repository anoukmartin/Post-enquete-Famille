## Fiche indiv 
library(questionr)
head(liens, 10)
freq(liens$libelle_relation)




library(tidyverse)
library(knitr)
id_individu <- "JONATHAN 1985-05-04"
responses <- BIEF %>%
  filter(identifiant == id_individu) 
res <- data.frame(
  Variable = names(responses),
  Reponse = as.character(responses[1, ])
) 
res <- full_join(vars_all2 %>%, res, by = "Variable")

  pivot_longer(cols = names(BIEF)) %>%
  rowid_to_column(var = "Variable")
reponses <- vars_all2 %>%
  left


#nom_individu, reponses_individu, variables_df, dossier_sortie
EF$identifiant


# Fonction pour créer un fichier RMD pour un individu
creer_fichier_rmd <- function(id_individu) {
  # Chemin du fichier RMD
  chemin_fichier <- file.path("fiches_indiv/", paste0("rapport_", id_individu, ".Rmd"))
  reponses_individu <- EF %>%
    filter(identifiant == id_individu) 
  # Contenu du fichier RMD
  contenu <- c(
    "---",
    paste0("title: \"Rapport d'enquête pour ", id_individu, ")\""),
    "output: html_document",
    "---",
    "",
    paste0("# Rapport d'enquête pour ", id_individu, ")"),
    "",
    "Voici les questions et réponses pour cet individu :",
    "",
    "```{r setup, include=FALSE}",
    "knitr::opts_chunk$set(echo = TRUE)",
    "```",
    "",
    "```{r questions_reponses}",
    "questions_reponses <- list("
  )
  
  # Ajouter les questions et réponses
  for (row in 1:nrow(vars_EF2)) {
    nom_variable <- vars_EF2$Variable[row]
    question <- vars_EF2$Question[row]
    
    if (nom_variable %in% names(reponses_individu)) {
      reponse <- reponses_individu[[nom_variable]]
      
      
      contenu <- c(contenu,
                   paste0("  \"", nom_variable, "\" = list("),
                   paste0("    question = \"", question, "\","),
                   paste0("    reponse = \"", reponse, "\""), "  ), ")
    }
  }
  
  contenu <- c(contenu,
               ")",
               "",
               "for (var in names(questions_reponses)) {",
               "  cat('\\n**Question : **', questions_reponses[[var]]$question, '\\n')",
               "  cat('**Réponse : **', questions_reponses[[var]]$reponse, '\\n')",
               "  cat('\\n---\\n')",
               "}",
               "```"
  )
  
  # Écrire le fichier
  writeLines(contenu, chemin_fichier)
  message(paste("Fichier RMD créé pour (ID:", id_individu, "):", chemin_fichier))
}

creer_fichier_rmd("JONATHAN 1985-05-04")


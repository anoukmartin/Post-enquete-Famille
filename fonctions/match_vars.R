library(stringdist)
install.packages("fuzzyjoin")
library(fuzzyjoin)
library(dplyr)

# Fonction pour trouver la meilleure correspondance entre deux listes de variables
trouver_correspondances <- function(liste1, liste2, seuil_similarite = 0.7) {
  # Créer un dataframe pour chaque liste
  df1 <- data.frame(Variable = liste1)
  df2 <- data.frame(Variable = liste2)
  
  # Calculer la distance de similarité entre chaque paire de variables
  # Utilisation de plusieurs métriques de similarité
  df1 %>%
    fuzzy_left_join(df2, by = "Variable",
                    match_fun = function(x, y) {
                      # Combinaison de plusieurs métriques de similarité
                      stringdist::stringdist(x, y, method = "jw") < seuil_similarite |
                        stringdist::stringdist(x, y, method = "lv") < seuil_similarite |
                        stringdist::stringdist(x, y, method = "cosine") < seuil_similarite
                    }) %>%
    mutate(
      # Calculer plusieurs scores de similarité
      jw_similarity = stringdist::stringdist(Variable.x, Variable.y, method = "jw"),
      lv_similarity = stringdist::stringdist(Variable.x, Variable.y, method = "lv"),
      cosine_similarity = stringdist::stringdist(Variable.x, Variable.y, method = "cosine"),
      # Score composite pondéré
      composite_score = 0.4 * (1 - jw_similarity) + 0.3 * (1 - lv_similarity) + 0.3 * (1 - cosine_similarity)
    ) %>%
    arrange(desc(composite_score)) %>%
    group_by(Variable.x) %>%
    slice(1) %>%
    ungroup() %>%
    mutate(
      # Marquer les correspondances acceptables
      correspondance_acceptable = composite_score >= seuil_similarite
    )
}





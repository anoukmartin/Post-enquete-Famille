library(stringdist)
library(dplyr)
library(tidyr)


levenshtein_pondere <- function(a, b) {
  
  a <- tolower(a)
  b <- tolower(b)
  
  n <- nchar(a)
  m <- nchar(b)
  
  # matrice DP
  D <- matrix(0, nrow = n + 1, ncol = m + 1)
  
  # fonction de poids positionnel
  poids <- function(i) {
    1 / i
  }
  
  # initialisation
  for (i in 2:(n + 1)) {
    D[i, 1] <- D[i - 1, 1] + poids(i - 1)
  }
  
  for (j in 2:(m + 1)) {
    D[1, j] <- D[1, j - 1] + poids(j - 1)
  }
  
  # remplissage
  for (i in 2:(n + 1)) {
    for (j in 2:(m + 1)) {
      
      cost_sub <- ifelse(
        substr(a, i - 1, i - 1) ==
          substr(b, j - 1, j - 1),
        0,
        poids(min(i - 1, j - 1))
      )
      
      D[i, j] <- min(
        D[i - 1, j] + poids(i - 1),      # suppression
        D[i, j - 1] + poids(j - 1),      # insertion
        D[i - 1, j - 1] + cost_sub       # substitution
      )
    }
  }
  
  return(D[n + 1, m + 1])
}


similarite_lev_pond <- function(a, b) {
  
  dist <- levenshtein_pondere(a, b)
  
  max_len <- max(nchar(a), nchar(b))
  
  # distance max théorique pondérée
  max_dist <- sum(1 / (1:max_len))
  
  1 - (dist / max_dist)
}


library(stringdist)
library(dplyr)
library(tidyr)
liste1 <- namesEF
liste2 <- vars_EF$Variable
trouver_correspondances <- function(liste1, liste2, seuil_similarite = 0.7) {
  
  pairs <- expand_grid(
    Variable.x = liste1,
    Variable.y = liste2
  )
  pairs <- pairs %>%
    mutate(
      jw = 1 - stringdist(Variable.x, Variable.y, method = "jw"), 
      lv = stringdist(Variable.x, Variable.y, method = "lv"),
      cosine = stringdist(Variable.x, Variable.y, method = "cosine")
    ) 
  # on garde les 5 meilleurs candidats à l'appariement selon le jw score (pour privilégier les préfixes)
  pairs <- pairs %>%
    group_by(Variable.x) %>%
    slice_max(jw, n = 5) %>%
    ungroup()
  
  pairs$lev_pond <- mapply(
    similarite_lev_pond,
    pairs$Variable.x,
    pairs$Variable.y
  )
  pairs <- pairs %>%
    group_by(Variable.x) %>%
    slice_max(lev_pond, n = 1) %>%
    slice_max(jw, n = 1) %>%
    slice_max(lv, n = 1) %>%
    mutate(n = n()) %>%
    ungroup() 
  return(pairs)
}



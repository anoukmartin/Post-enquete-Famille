library(stringdist)
library(dplyr)
library(tidyr)

trouver_correspondances <- function(liste1, liste2, seuil_similarite = 0.7) {
  
  expand_grid(
    Variable.x = liste1,
    Variable.y = liste2
  ) %>%
    mutate(
      jw = stringdist(Variable.x, Variable.y, method = "jw"),
      lv = stringdist(Variable.x, Variable.y, method = "lv"),
      cosine = stringdist(Variable.x, Variable.y, method = "cosine"),
      composite_score =
        0.4 * (1 - jw) +
        0.3 * (1 - lv) +
        0.3 * (1 - cosine)
    ) %>%
    filter(composite_score >= seuil_similarite) %>%
    group_by(Variable.x) %>%
    slice_max(composite_score, n = 1) %>%
    ungroup()
}

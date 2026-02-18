# Export pour MaxQda 


BIEF_lab <- readRDS("1_data/labelled/BIEF_lab.rds")

BIEF_maxqda <- BIEF_lab %>%
  select(-any_of(c(namesBI_tech, namesEF_tech)))%>%
  select(where(~ !all(is.na(.))))
  

## Recodage auto terminé, donc on peut aussi faire un export en recodages en claire

BIEF_maxqda <- to_factor(BIEF_maxqda, levels = "p", sort_levels = "v")
freq(BIEF_maxqda$dipl)

res <- sapply(names(BIEF_maxqda), function(v) {
  lab <- var_label(BIEF_maxqda[[v]])
  if (is.null(lab) || is.na(lab)) {lab <- "" } else {
    lab <- paste0(" ", lab)}
  paste0("[", v, "]", lab)
}, USE.NAMES = FALSE)
names(BIEF_maxqda) <- res
var_label(BIEF_maxqda) <- NULL

write_excel_csv2(BIEF_maxqda, "1_data/export/BIEF_maxqda.csv")

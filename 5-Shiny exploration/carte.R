
library(dplyr)
library(stringr)
library(sf)
library(ggplot2)
library(geodata)
library(ggrepel)


library(sf)
library(ggplot2)
library(maps)



# URL du shapefile ZIP officiel
url <- "https://data.iledefrance.fr/api/explore/v2.1/catalog/datasets/communes-france/exports/shp?lang=fr&timezone=Europe%2FBerlin&use_labels=true"

# Chemin temporaire pour télécharger le zip
temp <- tempfile(fileext = ".zip")
download.file(url, temp)

# Décompresser le zip dans un dossier temporaire
unzip_dir <- tempdir()
unzip(temp, exdir = unzip_dir)

# Trouver le fichier .shp dans le dossier décompressé
shp_file <- list.files(unzip_dir, pattern = "\\.shp$", full.names = TRUE)

# Lire le shapefile
idf_sf <- st_read(shp_file[1])

# Visualiser
carte_idf <- ggplot(idf_sf) +
  geom_sf(data = idf_sf, fill = "grey95", color = "grey60") +
  coord_sf(expand = FALSE) +
  theme_void()

carte_idf
unique(coordonnees$Commune)
communes_counts <- coordonnees %>%
  mutate(Commune = if_else(str_starts(Commune, "PARIS"), "PARIS", Commune)) %>%
  group_by(Commune, sexe) %>%  # ou code_postal si tu utilises les codes postaux
  summarise(nb_individus = n())



idf_sf_counts <- idf_sf %>%
  left_join(communes_counts, by = c("Nom_Officie.8" = "Commune")) %>%
  filter(!is.na(nb_individus))

centres <- st_centroid(idf_sf_counts)
coords <- st_coordinates(centres)
centres <- centres %>%
  mutate(
    X = coords[, 1],
    Y = coords[, 2]
  )

ggplot() +
  geom_sf(data = idf_sf, fill = "grey95", color = "grey60") +  # fond des communes
  geom_sf(data = centres, aes(size = nb_individus, color = sexe ), alpha = 0.7) +
  geom_text_repel(
    data = centres,
    aes(x = X, y = Y, label = paste0(str_to_title(Nom_Officie.8), " [", nb_individus, "]")),
    size = 2.5,
    max.overlaps = Inf,
    box.padding = 0.3,
    point.padding = 0.2,
    min.segment.length = 0
  ) +
  scale_size_continuous(
    range = c(1, 15),
    name = "Nombre d'individus\n dans l'échantillon"
  ) +
  scale_color_discrete(
    name = "Sexe des individus\nenquêtés dans\nla commune"
  ) +
  coord_sf(expand = FALSE) +
  theme_void()


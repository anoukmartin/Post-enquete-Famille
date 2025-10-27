coordonnees$Commune

library(dplyr)
library(stringr)
library(sf)
library(ggplot2)
library(geodata)

# ---- 1. Extraction du code INSEE ----
data <- coordonnees %>%
  mutate(code_insee = str_extract(Commune, "\\d{5}"))

# ---- 2. Comptage par commune ----
ville_counts <- data %>%
  group_by(code_insee) %>%
  summarise(n = n())

# ---- 3. Charger les communes de France ----
# gadm level=3 = communes

# URL officielle des contours des communes (mise à jour 2025)
url <- "https://static.data.gouv.fr/resources/contours-des-communes/20250401-communes-france.geojson"

# Téléchargement dans un dossier temporaire
destfile <- tempfile(fileext = ".geojson")
download.file(url, destfile, mode = "wb")

head(coordonnees)

fr_com <- gadm(country = "FRA", level = 4, path = tempdir()) 
class(fr_com)
fr_com <- st_as_sf(fr_com) 
dep_com <- fr_com %>%
  filter(NAME_2 %in% c("Essonne", "Val-de-Marne"))
plot(dep_com)
unique(fr_com$NAME_2)
# Vérifie la structure pour trouver la colonne du code INSEE
# (selon la source, ça peut être "CC_1", "NL_NAME_3", etc.)
names(fr_com)
head(fr_com)

# ---- 4. Joindre tes données ----
# Adapte "INSEE_CODE" à la bonne colonne du shapefile
dep_data <- fr_com %>%
  left_join(ville_counts, by = c("INSEE_CODE" = "code_insee"))

# ---- 5. Filtrer sur Val-de-Marne (94) et Essonne (91) ----
dep_data <- dep_data %>%
  filter(substr(INSEE_CODE, 1, 2) %in% c("91", "94"))

# ---- 6. Carte ----
ggplot(dep_data) +
  geom_sf(aes(fill = n), color = "white") +
  scale_fill_viridis_c(option = "plasma", na.value = "grey90") +
  labs(title = "Nombre d'enquêtés par commune",
       fill = "Enquêtés") +
  theme_minimal()
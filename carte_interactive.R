# Carte interactive ##############################################################################

install.packages(c("leaflet", "tidygeocoder", "dplyr"))
library(leaflet)
library(tidygeocoder)
library(dplyr)

# Géocodage
coordonnees <- coordonnees %>%
  mutate(adresse_complete = paste(AdressePostale, ComplementAdresse, Commune, sep = ", "))

coordonnees_geo <- coordonnees %>%
  geocode(adresse_complete, method = "osm", lat = latitude, long = longitude)


# Fiches individuelles 
coordonnees_geo <- coordonnees_geo %>%
  mutate(popup = paste0(
    "<b> ", identifiant, "(ID) </b><br>",
    "📞 Téléphone : ", POSTENQ_TEL, "<br>",
    "📧 Email : ", POSTENQ_MAIL, " / ", EAR_MAIL, "<br>",
    "📍 Adresse : ", adresse_complete
  ))

# Leaflet carte
leaflet(coordonnees_geo) %>%
  addTiles() %>%
  addMarkers(
    lng = ~longitude,
    lat = ~latitude,
    popup = ~popup
  )


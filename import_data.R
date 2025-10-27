# Packages nécessaires
library(httr)
#library(askpass)

# MDP et username du sdrive

app_pwd <- Sys.getenv("MDP_SSPCLOUD")
user <- Sys.getenv("USER_SSPCLOUD")

# URL de base de ton espace WebDAV
base_url <- "https://sdrive.cnrs.fr/remote.php/dav/files/1426185"

# Exemple : chemin vers ton fichier CSV (à adapter)
file_path <- "Thèse/Post-enquête/listes_donnees"


# 4 tableaux de données 

## Tableau 1 : coordonnees
tableau <- "paquet1_coordonnees.csv"
# Construction de l'URL complète
url <- paste0(base_url, "/", file_path, "/", tableau)
# Requête WebDAV avec authentification
res <- GET(url, authenticate(user, app_pwd))
# Vérifier que la requête a réussi
stop_for_status(res)
# Charger le CSV directement en data.frame
coordonnees <- read.csv(text = content(res, "text"), header = TRUE)

## Tableau 2 : réponses enquête Familles
tableau <- "paquet2_reponsesEF.csv"
# Construction de l'URL complète
url <- paste0(base_url, "/", file_path, "/", tableau)
# Requête WebDAV avec authentification
res <- GET(url, authenticate(user, app_pwd))
# Vérifier que la requête a réussi
stop_for_status(res)
# Charger le CSV directement en data.frame
EF <- read.csv(text = content(res, "text"), header = TRUE)

## Tableau 3 : réponses recensement
tableau <- "paquet3_bi.csv"
# Construction de l'URL complète
url <- paste0(base_url, "/", file_path, "/", tableau)
# Requête WebDAV avec authentification
res <- GET(url, authenticate(user, app_pwd))
# Vérifier que la requête a réussi
stop_for_status(res)
# Charger le CSV directement en data.frame
BI <- read.csv(text = content(res, "text"), header = TRUE)

## Tableau 4 : tables des liens dans le ménage
tableau <- "paquet4_liens_new.csv"
# Construction de l'URL complète
url <- paste0(base_url, "/", file_path, "/", tableau)
# Requête WebDAV avec authentification
res <- GET(url, authenticate(user, app_pwd))
# Vérifier que la requête a réussi
stop_for_status(res)
# Charger le CSV directement en data.frame
liens <- read.csv(text = content(res, "text"), header = TRUE)


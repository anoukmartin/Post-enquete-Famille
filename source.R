library(here)
# Spécifiez le chemin du dossier contenant vos fichiers R
dossiers <- c("./0_setup/", "./fonctions", "./2_analysis")

# Liste tous les fichiers dans le dossier
for (dossier in dossiers) {
fichiers <- list.files(dossier, pattern = "\\.R$", full.names = TRUE)
print(paste0(dossier, " : ", fichiers))
# Source chaque fichier
for (fichier in fichiers) {
  source(fichier)
}
}

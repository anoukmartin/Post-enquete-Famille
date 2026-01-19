email_template_information <- "

{CIVILITE} {PRENOM} {NOM},

Lors de votre participation à l’enquête Familles 2025 de l’Insee, vous aviez accepté d' être {RECONTACTE} pour un entretien complémentaire. C’est dans ce cadre que je vous écris.
Vous trouverez en pièces jointes la lettre d’information présentant l’enquête par entretien et la notice RGPD précisant les garanties de confidentialité et le cadre légal de cette enquête par entretiens. Ces documents vous ont également adressés par courrier postal.

Je reviendrai vers vous prochainement afin de convenir d’un rendez-vous.
En attendant, je reste joignable par mail ou par téléphone (0671793218)

Cordialement,

Anouk MARTIN 
Responsable de l'enquête par entretiens
Doctorante au CRESPPA, UMR 7217, Université Paris 8 
anouk.martin35@univ-paris8.fr / 0671793218
59-61 rue Pouchet 75849 Paris Cedex 17 
"



library(glue)

dir.create("4_communication/emails_info", showWarnings = FALSE)




for (id in coordonnees_clean$identifiant) {
  if(!is.na(coordonnees_clean[coordonnees_clean$identifiant == id, ]$email) &
     coordonnees_clean[coordonnees_clean$identifiant == id, ]$email != ""){
  if(coordonnees_clean[coordonnees_clean$identifiant == id, ]$civilite == "Madame"){
    recontact <- "recontactée" } else {recontact <-  "recontacté"}
  
  corps <- glue(email_template_information,
                PRENOM = coordonnees_clean[coordonnees_clean$identifiant == id, ]$prenom,
                NOM = coordonnees_clean[coordonnees_clean$identifiant == id, ]$nom, 
                CIVILITE = coordonnees_clean[coordonnees_clean$identifiant == id, ]$civilite, 
                RECONTACTE = recontact)
  
  
    eml <- paste0(
    "From: Anouk MARTIN <anouk.martin35@univ-paris8.fr>\n",
    "To: ", coordonnees_clean[coordonnees_clean$identifiant == id, ]$email, "\n",
    "Subject: Information – Entretien complémentaire à l’enquête Familles 2025\n",
    "Content-Type: text/plain; charset=UTF-8\n",
    "\n",
    corps
  )
  
writeLines(eml,
           file.path("4_communication/emails_eml",
                     paste0("email_info_", id, ".eml")))
}
}


# Configuration WebDAV
config_export <- list(
  base_url = "https://sdrive.cnrs.fr/remote.php/dav/files/1426185",
  remote_path = "Thèse/Post-enquête/Materiel_de_communications/email_info",
  local_path = output_dir
)

# Fonction d’upload sécurisé
upload_webdav_file <- function(local_file, remote_url, user, password) {
  tryCatch({
    log_message(paste("Export de", basename(local_file)))
    
    res <- PUT(
      url = remote_url,
      authenticate(user, password),
      body = upload_file(local_file)
    )
    
    stop_for_status(res)
    
    log_message(paste("Export réussi pour", basename(local_file)))
    TRUE
  }, error = function(e) {
    log_message(
      paste("Erreur lors de l'export de", basename(local_file), ":", e$message),
      "ERROR"
    )
    FALSE
  })
}

# Vérification du dossier local
if (!dir.exists(config_export$local_path)) {
  stop("Le dossier local n'existe pas :", config_export$local_path)
}


# Liste des fichiers à exporter
files_to_upload <- list.files(
  config_export$local_path,
  full.names = TRUE,
  recursive = FALSE
)

if (length(files_to_upload) == 0) {
  stop("Aucun fichier à exporter dans le dossier local")
}

# Export des fichiers
for (file in files_to_upload) {
  remote_url <- paste0(
    config_export$base_url, "/",
    encode_path(
      paste0(config_export$remote_path, "/", basename(file))
    )
  )
  
  success <- upload_webdav_file(
    local_file = file,
    remote_url = remote_url,
    user = Sys.getenv("USER_SSPCLOUD"),
    password = Sys.getenv("MDP_SSPCLOUD")
  )
  
  if (!success) {
    stop("Arrêt du script suite à une erreur d'export")
  }
}

log_message("Export des fichiers terminé avec succès")

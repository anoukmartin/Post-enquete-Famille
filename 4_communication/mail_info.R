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

dir.create("4_communication/emails_eml", showWarnings = FALSE)




for (id in coordonnees_clean$identifiant) {
  
  if(coordonnees_clean[coordonnees_clean$identifiant == id, ]$civilite == "Madame"){
    recontact <- "recontactée" } else {recontact <-  "recontacté"}
  
  corps <- glue(email_template_information,
                PRENOM = coordonnees_clean[coordonnees_clean$identifiant == id, ]$prenom,
                NOM = coordonnees_clean[coordonnees_clean$identifiant == id, ]$nom, 
                CIVILITE = coordonnees_clean[coordonnees_clean$identifiant == id, ]$civilite, 
                RECONTACTE = recontact)
  
  
    eml <- paste0(
    "From: Anouk MARTIN <anouk.martin35@etud-univ.paris8.fr>\n",
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
corps

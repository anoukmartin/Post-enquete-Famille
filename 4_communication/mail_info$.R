email_template_information <- "

{CIVILITE} {NOM},
Lors de votre participation à l’enquête Familles 2025 de l’Insee, vous aviez accepté de pouvoir être {RECONTACTÉ/ÉE} pour un entretien complémentaire. C’est dans ce cadre que je vous écris.
Vous trouverez en pièces jointes la lettre d’information présentant l’enquête par entretien et la notice RGPD précisant les garanties de confidentialité et le cadre légal de cette enquête. Ces documents vous seront également adressés par courrier postal.
Je reviendrai vers vous prochainement afin de convenir d’un rendez-vous.
En attendant, je reste joignable par mail ou par téléphone (06 32 50 67 28) 
Cordialement,

Anouk Martin
Doctorante en sociologie – CRESPPA, Université Paris 8

"

library(glue)

dir.create("emails_eml", showWarnings = FALSE)

for (i in seq_len(nrow(contacts))) {
  
  corps <- glue(modele,
                prenom = contacts$prenom[i],
                entreprise = contacts$entreprise[i])
  
  eml <- paste0(
    "From: Anouk MARTIN <anouk.martin35@etud-univ.paris8.fr>\n",
    "To: ", contacts$email[i], "\n",
    "Subject: Information – Entretien complémentaire à l’enquête Familles 2025\n",
    "Content-Type: text/plain; charset=UTF-8\n",
    "\n",
    corps
  )
  
  writeLines(eml,
             file.path("emails_eml",
                       paste0("email_", i, ".eml")))
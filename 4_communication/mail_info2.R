
# chemin des PDF 
pdf_local <- function(id) {
  file.path("4_communication/lettres_info/", paste0( id, "_lettre_information.pdf"))
}

dir.create("4_communication/tmp")


library(emayili)
library(glue)
library(pdftools)

smtp <- emayili::server(
  host = "smtp.univ-paris8.fr",
  port = 587,
  username = Sys.getenv("SMTP_USER_UNIVP8"), 
  password = Sys.getenv("SMTP_PASS_UNIVP8"))

coordonnees_contact <- coordonnees_contact %>%
  arrange(identifiant)
for (id in coordonnees_contact$identifiant) {
  
  row <- coordonnees_contact[coordonnees_contact$identifiant == id, ]
  
  if (!is.na(row$email) && row$email != "") {
    
    recontact <- ifelse(row$civilite == "Madame", "recontactée", "recontacté")
    
    corps <- glue(
      "{row$civilite} {row$prenom} {row$nom},

Lors de votre participation à l’enquête Familles 2025 de l’Insee, vous aviez accepté d'être {recontact} pour un entretien complémentaire. C’est dans ce cadre que je vous écris.

Vous trouverez en pièces jointes la lettre d’information présentant l’enquête par entretien et la notice RGPD précisant les garanties de confidentialité et le cadre légal de cette enquête par entretiens. Ces documents vous ont également été adressés par courrier postal.

Je reviendrai vers vous prochainement afin de convenir d’un rendez-vous.

Cordialement,

Anouk MARTIN
Responsable du projet de recherche
Doctorante au CRESPPA, UMR 7217, CNRS - Université Paris 8
anouk.martin35@univ-paris8.fr / 0671793218
59-61 rue Pouchet 75849 Paris Cedex 17"
    )
    
    pdf_source <- pdf_local(id)
    
    pdf_lettre <- file.path(
      "4_communication/tmp",
      paste0("lettre_information.pdf")
    )
    
    pdf_rgpd <- file.path(
      "4_communication/tmp",
      paste0("notice_rgpd.pdf")
    )
    
    # 1re page : lettre d’information
    pdf_subset(
      input = pdf_source,
      pages = 1,
      output = pdf_lettre
    )
    
    # 3e page : notice RGPD
    pdf_subset(
      input = pdf_source,
      pages = 3,
      output = pdf_rgpd
    )
    
    email <- envelope() |>
      from("Anouk MARTIN <anouk.martin35@univ-paris8.fr>") |>
      #to(row$email) |>
      to("anouk.martin34@etud.univ-paris8.fr") |>
      subject("Information – Entretien complémentaire à l’enquête Familles 2025") |>
      text(corps) |>
      attachment(pdf_lettre) |>
      attachment(pdf_rgpd)
    print(email, details = TRUE)
    
    
    smtp(email, verbose = T)
    
    file.remove(pdf_lettre, pdf_rgpd)
  }
}

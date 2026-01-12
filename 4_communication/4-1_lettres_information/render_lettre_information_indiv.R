infos <- readRDS(file = "1_data/processed/donnees_contact.rds")

list_id_individus <- as.character(infos$identifiant)

library(quarto)
library(progress)

# Dossier de sortie
output_dir <- "4_communication/individus"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# Suivi des rendus
produit <- setNames(
  logical(length(list_id_individus)),
  list_id_individus
)

# Barre de progression
pb <- progress_bar$new(
  total = length(list_id_individus),
  format = "render pour :id [:bar] :current/:total (:percent)  "
)


# Boucle de rendu
for (id in list_id_individus) {
  
  pb$tick(tokens = list(id = id))
  
  output_file <- paste0(id, "_lettre_information.pdf")
  
  tryCatch({
    
    quarto_render(
      input = "4_communication/4-1_lettres_information/Lettre_information.qmd",
      execute_params = list(
        id_individu = id
      ),
      output_file = output_file,
      quiet = TRUE   # 🔇 mute
    )
    
    file.rename(
      from = file.path("4_communication/4-1_lettres_information", output_file),
      to   = file.path(output_dir, output_file)
    )
    
    produit[id] <- file.exists(
      file.path(output_dir, output_file)
    )
    
  }, error = function(e) {
    produit[id] <- FALSE
  })
}

# 📋 Bilan final
cat("\n===== BILAN DES RENDUS =====\n")

if (all(produit)) {
  cat("✔ Tous les rapports ont été générés avec succès (",
      sum(produit), "/", length(produit), ")\n", sep = "")
} else {
  
  cat("⚠ Rapports manquants pour les individus suivants :\n")
  print(names(produit)[!produit])
  
  cat("\n✔ Rapports générés : ",
      sum(produit), "/", length(produit), "\n", sep = "")
}

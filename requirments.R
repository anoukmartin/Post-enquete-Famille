

packages <- c("dplyr", "ggplot2", "tidyverse", "readr", "stringr", "data.table", 
              "readODS", "fuzzyjoin", "glue", "kableExtra", "questionr", "here", "banR", "tidygeocoder")


# Charger le package devtools
for(p in packages){
  if (!requireNamespace(p)) {
    install.packages(p)
  }
}



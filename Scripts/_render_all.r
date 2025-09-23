# 1. Charger les librairies nécessaires
# install.packages(c("rmarkdown", "purrr", "here"))
library(rmarkdown)
library(purrr)
library(here)

# 2. Lister les fichiers Rmd en utilisant here() pour un chemin robuste
# here("Scripts") construit le chemin vers le dossier "Scripts"
# à partir de la racine de ton projet RStudio. C'est la meilleure pratique !
rmd_files <- list.files(path = here("Scripts"), 
                        pattern = "\\.*Rmd$", full.names = TRUE)


# 3. Utiliser walk() pour appliquer la fonction render() sur chaque fichier
# walk() est fait pour les fonctions qui font une action (comme créer un fichier)
# plutôt que de retourner une valeur.
walk(rmd_files[-1], ~{
  message("Executing: ", .x)
  rmarkdown::render(.x)
  message("Done with: ", .x)
})

message("All R Markdown files have been executed!")
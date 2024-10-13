
# Installer et charger les packages nécessaires
#install.packages(c("httr", "jsonlite", "readr", "dplyr"))

library(dplyr)
library(httr)
library(jsonlite)
library(readr)

# Charger les adresses
adresses <- read_csv2("adresses-69.csv", show_col_types = FALSE)

# Extraire les codes postaux uniques du département
codes_postaux <- unique(adresses$code_postal)

# Fonction pour obtenir les logements existants
get_logements_existants <- function(code_postal) {
  base_url <- "https://data.ademe.fr/data-fair/api/v1/datasets/dpe-v2-logements-existants/lines"
  params <- list(
    page = 1,
    size = 10000,
    select = "_geopoint,Adresse_(BAN),N°DPE,Code_postal_(BAN),Etiquette_DPE,Date_réception_DPE,N°_département_(BAN),Année_construction,Type_bâtiment,Surface_habitable_logement,Type_énergie_principale_chauffage,Nom__commune_(BAN),Coût_total_5_usages,Type_énergie_n°1,Typologie_logement,N°_étage_appartement,Emission_GES_5_usages,Emission_GES_5_usages_par_m²,Etiquette_GES,Qualité_isolation_enveloppe",
    q = as.character(code_postal),
    q_fields = "Code_postal_(BAN)"
  )
  url_encoded <- modify_url(base_url, query = params)
  response <- GET(url_encoded)
  if (status_code(response) == 200) {
    content <- fromJSON(rawToChar(response$content), flatten = FALSE)
    if (!is.null(content$result)) {
      return(as.data.frame(content$result))
    }
  }
  return(data.frame())
}

# Fonction pour obtenir les logements neufs
get_logements_neufs <- function(code_postal) {
  base_url <- "https://data.ademe.fr/data-fair/api/v1/datasets/dpe-v2-logements-neufs/lines"
  params <- list(
    page = 1,
    size = 10000,
    select = "_geopoint,Adresse_(BAN),N°DPE,Code_postal_(BAN),Etiquette_DPE,Date_réception_DPE,N°_département_(BAN),Type_bâtiment,Surface_habitable_logement,Type_énergie_principale_chauffage,Nom__commune_(BAN),Coût_total_5_usages,Type_énergie_n°1,Typologie_logement,N°_étage_appartement,Emission_GES_5_usages,Emission_GES_5_usages_par_m²,Etiquette_GES,Qualité_isolation_enveloppe",
    q = as.character(code_postal),
    q_fields = "Code_postal_(BAN)"
  )
  url_encoded <- modify_url(base_url, query = params)
  response <- GET(url_encoded)
  if (status_code(response) == 200) {
    content <- fromJSON(rawToChar(response$content), flatten = FALSE)
    if (!is.null(content$result)) {
      return(as.data.frame(content$result))
    }
  }
  return(data.frame())
}

# DataFrames pour stocker les logements existants et neufs
existants_df <- data.frame()
neufs_df <- data.frame()

# Récupérer les logements existants et neufs pour chaque code postal
for (code_postal in codes_postaux) {
  # Logements existants
  logements_existants <- get_logements_existants(code_postal)
  existants_df <- bind_rows(existants_df, logements_existants)
  
  # Logements neufs
  logements_neufs <- get_logements_neufs(code_postal)
  neufs_df <- bind_rows(neufs_df, logements_neufs)
  
  Sys.sleep(1)  # Pause pour éviter de dépasser les limites de l'API
}

# Trouver les colonnes communes entre les deux DataFrames
colonnes_communes <- intersect(names(existants_df), names(neufs_df))

# Sélectionner uniquement les colonnes communes pour chaque DataFrame
existants_communes <- existants_df %>% select(all_of(colonnes_communes))
neufs_communes <- neufs_df %>% select(all_of(colonnes_communes))

# Fusionner les deux DataFrames
logements_df <- bind_rows(existants_communes, neufs_communes)

# Exporter le résultat dans un fichier CSV
write_csv(logements_df, "logements_69.csv")

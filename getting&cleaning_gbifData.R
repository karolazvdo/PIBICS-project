##################################################################
##################################################################
##  Getting and cleaning Gbif occurrence data                   ##
##  Karoline Azevedo, for PIBICS project 05 de Janeiro de 2025  ##
##################################################################
##################################################################
##
# Install and load packages
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
library(pacman)

p_load(rgbif, dplyr, purrr, readxl, sf, rnaturalearth, rnaturalearthdata)

# Import dataset
df <- read_excel("C:/Users/drive/Downloads/Dataset da Wikipedia.xlsx") 

# Get the shapefile from Brazil to filter points
brasil_shape <- ne_countries(country = "Brazil", scale = "medium", returnclass = "sf")

# --- 1. Function to getting records beyond the 5000 limit ---
buscar_gbif <- function(Nome_Cientifico) {
  start <- 0  # start point of pagination
  todos_registros <- tibble() # Initialize empty tibble
  
  repeat {
    tryCatch({
    # Searching GBIF (active pagination)
    res <- occ_search(scientificName = Nome_Cientifico,
                      limit = 5000,
                      start = start)
    
    # If there are no records or the data runs out, exit the loop
    if (is.null(res$data) || nrow(res$data) == 0) break
    
    # Combine results
    todos_registros <- bind_rows(todos_registros, res$data)
    
    # Updates the start point
    start <- start + 5000
    
    # Progress mensage
    cat("Registros coletados para:", Nome_Cientifico, "até agora:", nrow(todos_registros), "\n")
    
    # Small delay to avoid API throttling
    Sys.sleep(3)
    
  }, error = function(e) {
    cat("Erro ao coletar registros para:", Nome_Cientifico, "\n")
    return(NULL)  # If there is an error, return NULL and exit the loop.
  })
 }
  
  # If no records were found, return a tibble with NA values to avoid errors
  if (nrow(todos_registros) == 0) {
    cat("Nenhum registro encontrado para:", Nome_Cientifico, "\n")
    return(tibble(scientificName = Nome_Cientifico))  # Returns only the column with the species name
  }
  
  # Ensure all columns exist by filling missing ones with NA
  todas_colunas <- c("gbifID", "scientificName", "taxonRank", "taxonomicStatus", "speciesKey",
                     "acceptedTaxonKey", "class", "order", "family", "species", "genus",
                     "specificEpithet", "year", "decimalLongitude", "decimalLatitude",
                     "iucnRedListCategory", "level0Gid", "level0Name", "countryCode",
                     "institutionCode", "basisOfRecord", "coordinateUncertaintyInMeters")
  
  # Add columns that do not exist and fill with NA
  for (col in todas_colunas) {
    if (!col %in% names(todos_registros)) {
      todos_registros[[col]] <- NA
    }
  }
  
  # Returns records with standardized columns
  return(todos_registros[, todas_colunas])
}

# --- 2. Apply search for each species ---
cat("Buscando registros no GBIF para cada espécie...\n")

# Create an object to store partial records
gbif_registros_parcial <- tibble()
linha_erro <- NA  # Variable to store the line where the error occurred

# Loop to process each species and save the records in the environment
for (i in seq_along(df$Nome_Cientifico)) {
  especie <- df$Nome_Cientifico[i]
  
  tryCatch({
    registros <- buscar_gbif(especie)
    
    # If the search was successful, add to the partial dataset
    if (!is.null(registros)) {
      gbif_registros_parcial <- bind_rows(gbif_registros_parcial, registros)
    }
    
  }, error = function(e) {
    cat("Erro ao processar a espécie:", especie, "na linha", i, "\n")
    linha_erro <<- i  # Save the line where the error occurred
    return(NULL)
  })
}

# Finished running the function with the partial records---------------------
# Compare the two columns and find the values ​​that are in 'df' but not in 'gbif_registros_parcial'
# Run it again using the difference dataset.
differents <- df %>%
  filter(!(Nome_Cientifico %in% gbif_registros_parcial$species))

# Saving the records that have already been captured so as not to overlap the new dataset.
gbif_registros_round1 <- gbif_registros_parcial
rm(gbif_registros_parcial) # remove the previous one

# running the function again with the rest
# Create an object to store partial records
gbif_registros_parcial <- tibble()
linha_erro <- NA  # Variable to store the line where the error occurred

# Loop to process each species and save the records in the environment
for (i in seq_along(differents$Nome_Cientifico)) {
  especie <- differents$Nome_Cientifico[i]
  
  tryCatch({
    registros <- buscar_gbif(especie)
    
    # If the search was successful, add to the partial dataset
    if (!is.null(registros)) {
      gbif_registros_parcial <- bind_rows(gbif_registros_parcial, registros)
    }
    
  }, error = function(e) {
    cat("Erro ao processar a espécie:", especie, "na linha", i, "\n")
    linha_erro <<- i  # Save the line where the error occurred
    return(NULL)
  })
}

# join both dataset with records and remove NA values
gbif_registros <- gbif_registros_round1 %>%
  rbind(gbif_registros_parcial) %>%
  filter(., !is.na(species))

# Save collected records as far as possible----------------------------------
if (nrow(gbif_registros) > 0) {
  write.csv(gbif_registros, "gbif_registros.csv", row.names = FALSE)
}

# --- 3. Filter points within Brazil ---
if (nrow(gbif_registros) > 0) {
  # Convert GBIF data to an `sf` object
  gbif_sf <- gbif_registros %>%
    filter(!is.na(decimalLongitude) & !is.na(decimalLatitude)) %>%
    st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)
  
  # Filter only points within Brazil
  gbif_brasil <- st_join(gbif_sf, brasil_shape, join = st_within) %>%
    filter(!is.na(sovereignt))  # Only Brazil records
  
  # Saving
  write.csv(gbif_brasil, "gbif_registros_brasil.csv", row.names = FALSE)
  
  cat("Registros filtrados dentro do Brasil salvos em 'gbif_registros_brasil.csv'\n")
} else {
  cat("Nenhum registro encontrado para as espécies.\n")
}

###############################################################################
##  Cleaning Gbif data --------------------------------------------------------

# Loading required packages ---------------------------------------------------
p_load(dplyr, countrycode, readr, CoordinateCleaner, sf, tidyverse)


# Load dataset with GBIF records (filtered within Brazil)
data <- read.csv("gbif_registros_brasil.csv")

# Selecting relevant columns -------------------------------------------------
data <- as.data.frame(data[,c("gbifID", "scientificName",
                              "taxonomicStatus", "speciesKey",
                              "acceptedTaxonKey", "class", "order",
                              "family", "species", "year", "decimalLongitude",
                              "decimalLatitude",
                              "iucnRedListCategory", "basisOfRecord",
                              "coordinateUncertaintyInMeters")])

names(data)
dim(data)  
str(data)


# Replacing empty coordinate cells with NA -----------------------------------
data <- data %>%
  mutate(across(c("decimalLongitude", "decimalLatitude"), ~ifelse(.=="", NA, as.character(.))))

# Removing records without coordinates --------------------------------------
data <- data %>% 
  filter(!is.na(decimalLongitude)) %>%
  filter(!is.na(decimalLatitude))

# Filtering out other missing values -----------------------------------------
data <- data %>% filter(!is.na(species))  

# Flagging problematic records using `CoordinateCleaner` ---------------------
data$decimalLatitude <- as.numeric(data$decimalLatitude)
data$decimalLongitude <- as.numeric(data$decimalLongitude)

flags <- clean_coordinates(x = data,
                           lon = "decimalLongitude",
                           lat = "decimalLatitude",
                           species = "species",
                           tests = c("capitals", "centroids", "equal", "seas",
                                     "gbif", "institutions", "zeros"))

summary(flags)

# Removing dubious records ---------------------------------------------------
data_cl <- data[flags$.summary, ]  
data_fl <- data[!flags$.summary, ]
rm(flags, data_fl, data)

# Removing records with low coordinate precision ----------------------------
# Here we remove records with uncertainty > 100km (adjustable if needed)
hist(data_cl$coordinateUncertaintyInMeters / 1000, breaks = 20)

data_cl <- data_cl %>%
  filter(coordinateUncertaintyInMeters / 1000 <= 100 | is.na(coordinateUncertaintyInMeters))

# Removing fossils, dubious and unknown records -----------------------------
data_cl <- filter(data_cl, basisOfRecord == "HUMAN_OBSERVATION" | 
                    basisOfRecord == "OBSERVATION" |
                    basisOfRecord == "PRESERVED_SPECIMEN")

# Removing duplicate coordinates for the same species -----------------------
data_cl <- data_cl %>% distinct(species, decimalLatitude,
                                decimalLongitude, scientificName,
                                speciesKey, .keep_all = TRUE)  

# Temporal outlier detection ------------------------------------------------
flags <- cf_age(x = data_cl,
                lon = "decimalLongitude",
                lat = "decimalLatitude",
                taxon = "species",
                min_age = "year",
                max_age = "year",
                value = "flagged")

# Remove flagged temporal outliers
data_cl <- data_cl[flags, ]  

# Saving the cleaned dataset ------------------------------------------------
write.csv(data_cl, "anfibios_gbif_cleaned.csv", row.names = FALSE)
cat("Cleaned dataset saved as 'anfibios_gbif_cleaned.csv'\n")
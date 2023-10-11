##############################################################
# Herramienta digital Análisis de Riesgo SR - geodata_to_shapefiles.R
# Organización Panamericana de la Salud
# Autor: Luis Quezada
# Última fecha de modificación: 2023-10-02
# R 4.3.0
##############################################################

Sys.setlocale(locale = "es_ES.UTF-8")

# Working dir ----
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Libraries ----
library(geojsonio)
library(rmapshaper)
library(sf)
library(tidyverse)
library(leaflet)
library(sp)
library(readxl)
library(writexl)

# Settings ----
shapefile_settings <- read_xlsx("shapefile_settings.xlsx")

# Filename ----
file_type <- as.character(shapefile_settings[1,2])
if (file_type == "JSON") {
  file_name = paste0(as.character(shapefile_settings[2,2]),".json")
} else {
  file_name = as.character(shapefile_settings[2,2])
}


# Funcs ----

# Removes accents and uppercases ADMIN1 and ADMIN2 columns
admin_normalizer <- function(admin_df,transform=F) {
  # MAYUS
  admin_df$ADMIN1 <- toupper(admin_df$ADMIN1)
  admin_df$ADMIN2 <- toupper(admin_df$ADMIN2)
  
  # ACCENTS
  admin_df <- admin_df %>%
    mutate(
      ADMIN1 = gsub("Á","A", ADMIN1),
      ADMIN1 = gsub("É","E", ADMIN1),
      ADMIN1 = gsub("Í","I", ADMIN1),
      ADMIN1 = gsub("Ó","O", ADMIN1),
      ADMIN1 = gsub("Ú","U", ADMIN1),
      ADMIN1 = gsub("Ñ","N", ADMIN1),
      ADMIN1 = gsub("Ü","U", ADMIN1),
      ADMIN2 = gsub("Á","A", ADMIN2),
      ADMIN2 = gsub("É","E", ADMIN2),
      ADMIN2 = gsub("Í","I", ADMIN2),
      ADMIN2 = gsub("Ó","O", ADMIN2),
      ADMIN2 = gsub("Ú","U", ADMIN2),
      ADMIN2 = gsub("Ñ","N", ADMIN2),
      ADMIN2 = gsub("Ü","U", ADMIN2)
    )
  
  if (transform) {admin_df <- st_transform(admin_df, "+proj=longlat +datum=WGS84")}
  return(admin_df)
}

# Step 1 [raw shapefiles] ----
if (file_type == "JSON") {
  raw_shapefiles = read_sf(paste0("",file_name))      # Read geoJSON
} else {
  raw_shapefiles <- st_read(".",layer=file_name)       # Read .dbf .prj .shp .shx
}

# Step 2 [Standardize atriute names] ----
a1_id   <- as.character(shapefile_settings[3,2])
a1_name <- as.character(shapefile_settings[4,2])
a2_id   <- as.character(shapefile_settings[5,2])
a2_name <- as.character(shapefile_settings[6,2])
gmty  <- as.character(shapefile_settings[7,2])

country_shapefiles <- raw_shapefiles %>% rename(
  ADMIN1_GEO_ID:=!!a1_id,
  ADMIN1:=!!a1_name,
  GEO_ID:=!!a2_id,
  ADMIN2:=!!a2_name,
  geometry:=!!gmty
) %>% select(ADMIN1_GEO_ID,GEO_ID,ADMIN1,ADMIN2,geometry)
country_shapefiles <- admin_normalizer(country_shapefiles,transform=T)

# NA values
country_shapefiles <- country_shapefiles %>% 
  filter(!is.na(ADMIN1) | !is.na(ADMIN2) | !is.na(GEO_ID) | !is.na(ADMIN1_GEO_ID)) %>%
  filter(!st_is_empty(geometry))

# geometry combination
sf_use_s2(FALSE)
country_shapefiles = country_shapefiles %>%
  group_by(ADMIN1_GEO_ID,GEO_ID,ADMIN1,ADMIN2) %>% 
  summarize(geometry = st_union(geometry))


# Step 3 [QA] ----
# Remove missing values in ADMIN1 or ADMIN2 or GEO_ID

# Inspect for missing data and check counts match country's geography
cat("Resumen: \n");summary(country_shapefiles) # Check for NAs
cat("ADMIN1 únicos: ");length(unique(country_shapefiles$ADMIN1_GEO_ID)) # Check ADMIN1_GEO_ID Count
cat("GEO_ID únicos: ");length(unique(country_shapefiles$GEO_ID)) # Check GEO_ID Count
cat("Formas cargadas: ");nrow(country_shapefiles) # Check number of rows (compare againts GEO_ID count)


# Step 4 [Preview] ----
# Preview shapefiles in a map
map_preview <- leaflet(country_shapefiles) %>%
  addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
  addPolygons(fillColor = "orange",fillOpacity = .9,
              weight = 1.5,color = "black", opacity = .7)

# Step 5 [Save] ----
# Save shapefiles to use in the tool
dir.create("../Data/shapefiles")
st_write(country_shapefiles, "../Data/shapefiles/admin2.shp", append=FALSE)
write_xlsx(as.data.frame(country_shapefiles) %>% select(-geometry) %>% na.omit() %>% arrange(ADMIN1),"geocodigos_nombres.xlsx")

map_preview
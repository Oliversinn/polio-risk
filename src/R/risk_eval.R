# Organización Panamericana de la Salud
# Autor: Oliver Mazariegos
# Última fecha de modificación: 2023-10-09
# R 4.3.1
##############################################################

Sys.setlocale(locale = "es_ES.UTF-8")

# df = read_excel(PATH_country_data, sheet = 3, skip = 2, col_names = FALSE)
# colnames(df) = c('admin1', 'admin2', 'subnational', 'region', 'population', 'pfa','year1','year2','year3','year4','year5','ipv2','effective_inmunization_campaign')


library(readxl)
library(sf)
library(tidyverse)

rm(list = ls())

# PATHS ----
PATH_country_data   = "Data/country_data.xlsx"
PATH_risk_cut_offs  = "R/risk_cut_offs.xlsx"
PATH_shapefiles     = "Data/shapefiles/"

# VARS ----
LANG <- as.character(read_excel(PATH_country_data,sheet = 1)[8,2])

# LANG ----
LANG_TLS <- read_excel("R/translations.xlsx",sheet = "DASHBOARD") %>% select(LABEL,all_of(LANG))
colnames(LANG_TLS) <- c("LABEL","LANG")
lang_label <- function(label) {
  return(LANG_TLS$LANG[LANG_TLS$LABEL == label])
}

rep_label_admin1_name = lang_label("rep_label_admin1_name")
rep_label_admin1_name_plural = lang_label("rep_label_admin1_name_plural")
rep_label_admin2_name = lang_label("rep_label_admin2_name")
rep_label_admin2_name_plural = lang_label("rep_label_admin2_name_plural")

# UTILS ----

# Not in operator
`%!in%` <- Negate(`%in%`)

# Removes accents and uppercases ADMIN1 and ADMIN2 columns
admin_normalizer <- function(admin_df) {
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
}

var_norm <- function(x) {
  x = toupper(x)
  if (is.character(x)) {
    x = gsub("Á","A",x)
    x = gsub("É","E",x)
    x = gsub("Í","I",x)
    x = gsub("Ó","O",x)
    x = gsub("Ú","U",x)
    x = gsub("Ñ","N",x)
    x = gsub("Ü","U",x)
  }
  return(x)
}

# SCORING Functions ----

# Population and PFA
population_and_pfa <- function(population_inmunity_df) {
    score <- case_when(
      population_inmunity_df$population >= 100000 | population_inmunity_df$pfa == lang_label("yes") ~ TRUE,
      TRUE ~ FALSE
    )
    return(score)
}

# Score coverage Polio3
score_coverage_polio3 <- function(population_inmunity_df, year_column) {
  population_and_pfa <- population_and_pfa(population_inmunity_df)
  score = case_when(
    population_and_pfa == TRUE & round(population_inmunity_df[[year_column]],0)  < 80 ~ 8,
    population_and_pfa == TRUE & (round(population_inmunity_df[[year_column]],0) >= 80 & round(population_inmunity_df[[year_column]],0) < 90) ~ 5,
    population_and_pfa == TRUE & (round(population_inmunity_df[[year_column]],0) >= 90 & round(population_inmunity_df[[year_column]],0) < 95) ~ 2,
    population_and_pfa == TRUE & (round(population_inmunity_df[[year_column]],0) >= 95 & round(population_inmunity_df[[year_column]],0) <= 100) ~ 0,
    population_and_pfa == TRUE & round(population_inmunity_df[[year_column]],0)  > 100 ~ 2,
    population_and_pfa == FALSE & round(population_inmunity_df[[year_column]],0)  < 80 ~ 10,
    population_and_pfa == FALSE & (round(population_inmunity_df[[year_column]],0) >= 80 & round(population_inmunity_df[[year_column]],0) < 90) ~ 6,
    population_and_pfa == FALSE & (round(population_inmunity_df[[year_column]],0) >= 90 & round(population_inmunity_df[[year_column]],0) < 95) ~ 3,
    population_and_pfa == FALSE & (round(population_inmunity_df[[year_column]],0) >= 95 & round(population_inmunity_df[[year_column]],0) <= 100) ~ 0,
    population_and_pfa == FALSE & round(population_inmunity_df[[year_column]],0)  > 100 ~ 3
  )
  return(score)
}

# Score IPV2
score_ipv2 <- function(population_inmunity_df) {
  population_and_pfa <- population_and_pfa(population_inmunity_df)
  score = case_when(
    population_and_pfa == TRUE & round(population_inmunity_df[["ipv2"]],0)  < 80 ~ 8,
    population_and_pfa == TRUE & (round(population_inmunity_df[["ipv2"]],0) >= 80 & round(population_inmunity_df[["ipv2"]],0) < 90) ~ 5,
    population_and_pfa == TRUE & (round(population_inmunity_df[["ipv2"]],0) >= 90 & round(population_inmunity_df[["ipv2"]],0) < 95) ~ 2,
    population_and_pfa == TRUE & (round(population_inmunity_df[["ipv2"]],0) >= 95 & round(population_inmunity_df[["ipv2"]],0) <= 100) ~ 0,
    population_and_pfa == TRUE & round(population_inmunity_df[["ipv2"]],0)  > 100 ~ 2,
    population_and_pfa == FALSE & round(population_inmunity_df[["ipv2"]],0)  < 80 ~ 10,
    population_and_pfa == FALSE & (round(population_inmunity_df[["ipv2"]],0) >= 80 & round(population_inmunity_df[["ipv2"]],0) < 90) ~ 6,
    population_and_pfa == FALSE & (round(population_inmunity_df[["ipv2"]],0) >= 90 & round(population_inmunity_df[["ipv2"]],0) < 95) ~ 3,
    population_and_pfa == FALSE & (round(population_inmunity_df[["ipv2"]],0) >= 95 & round(population_inmunity_df[["ipv2"]],0) <= 100) ~ 0,
    population_and_pfa == FALSE & round(population_inmunity_df[["ipv2"]],0)  > 100 ~ 3
  )
  return(score)
}

# Score succesfull campaign
score_succesfull_inmunization_campaign <- function(population_inmunity_df) {
  population_and_pfa <- population_and_pfa(population_inmunity_df)
  score = case_when(
    population_and_pfa == TRUE & population_inmunity_df[["effective_inmunization_campaign"]] == lang_label("no") ~ 6,
    population_and_pfa == TRUE & population_inmunity_df[["effective_inmunization_campaign"]] == lang_label("yes") ~ 0,
    population_and_pfa == FALSE & population_inmunity_df[["effective_inmunization_campaign"]] == lang_label("no") ~ 8,
    population_and_pfa == FALSE & population_inmunity_df[["effective_inmunization_campaign"]] == lang_label("yes") ~ 0,
    TRUE ~ 0
   )
  return(score)
}



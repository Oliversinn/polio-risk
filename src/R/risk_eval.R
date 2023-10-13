# Organización Panamericana de la Salud
# Autor: Oliver Mazariegos
# Última fecha de modificación: 2023-10-09
# R 4.3.1
##############################################################

Sys.setlocale(locale = "es_ES.UTF-8")

population_inmunity = read_excel(PATH_country_data, sheet = 3, skip = 2, col_names = FALSE)
colnames(population_inmunity) = c('admin1', 'admin2', 'subnational', 'region', 'population', 'pfa','year1','year2','year3','year4','year5','ipv2','effective_inmunization_campaign')
survaillance = read_excel(PATH_country_data, sheet = 4, skip = 2, col_names = FALSE)
colnames(survaillance) = c('admin1', 'admin2', 'subnational', 'region', 'population', 'pfa', 'compliant_units', 'pfa_rate', 'pfa_notified', 'pfa_investigated', 'suitable_samples', 'followups', 'active_search')
determinants = read_excel(PATH_country_data, sheet = 5, skip = 2, col_names = FALSE)
colnames(determinants) = c('admin1', 'admin2', 'subnational', 'region', 'population', 'pfa', 'drinking_water', 'sanitation_services')
outbreaks = read_excel(PATH_country_data, sheet = 6, skip = 2, col_names = FALSE)
colnames(outbreaks) =  c('admin1', 'admin2', 'subnational', 'region', 'measles', 'rubella', 'diphtheria', 'yellow_fever', 'tetanus')

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

## Population immunity ----

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
    population_and_pfa & round(population_inmunity_df[[year_column]],0)  < 80 ~ 8,
    population_and_pfa & (round(population_inmunity_df[[year_column]],0) >= 80 & round(population_inmunity_df[[year_column]],0) < 90) ~ 5,
    population_and_pfa & (round(population_inmunity_df[[year_column]],0) >= 90 & round(population_inmunity_df[[year_column]],0) < 95) ~ 2,
    population_and_pfa & (round(population_inmunity_df[[year_column]],0) >= 95 & round(population_inmunity_df[[year_column]],0) <= 100) ~ 0,
    population_and_pfa & round(population_inmunity_df[[year_column]],0)  > 100 ~ 2,
    !population_and_pfa & round(population_inmunity_df[[year_column]],0)  < 80 ~ 10,
    !population_and_pfa & (round(population_inmunity_df[[year_column]],0) >= 80 & round(population_inmunity_df[[year_column]],0) < 90) ~ 6,
    !population_and_pfa & (round(population_inmunity_df[[year_column]],0) >= 90 & round(population_inmunity_df[[year_column]],0) < 95) ~ 3,
    !population_and_pfa & (round(population_inmunity_df[[year_column]],0) >= 95 & round(population_inmunity_df[[year_column]],0) <= 100) ~ 0,
    !population_and_pfa & round(population_inmunity_df[[year_column]],0)  > 100 ~ 3
  )
  return(score)
}

# Score IPV2
score_ipv2 <- function(population_inmunity_df) {
  population_and_pfa <- population_and_pfa(population_inmunity_df)
  score = case_when(
    population_and_pfa & round(population_inmunity_df[["ipv2"]],0)  < 80 ~ 8,
    population_and_pfa & (round(population_inmunity_df[["ipv2"]],0) >= 80 & round(population_inmunity_df[["ipv2"]],0) < 90) ~ 5,
    population_and_pfa & (round(population_inmunity_df[["ipv2"]],0) >= 90 & round(population_inmunity_df[["ipv2"]],0) < 95) ~ 2,
    population_and_pfa & (round(population_inmunity_df[["ipv2"]],0) >= 95 & round(population_inmunity_df[["ipv2"]],0) <= 100) ~ 0,
    population_and_pfa & round(population_inmunity_df[["ipv2"]],0)  > 100 ~ 2,
    !population_and_pfa & round(population_inmunity_df[["ipv2"]],0)  < 80 ~ 10,
    !population_and_pfa & (round(population_inmunity_df[["ipv2"]],0) >= 80 & round(population_inmunity_df[["ipv2"]],0) < 90) ~ 6,
    !population_and_pfa & (round(population_inmunity_df[["ipv2"]],0) >= 90 & round(population_inmunity_df[["ipv2"]],0) < 95) ~ 3,
    !population_and_pfa & (round(population_inmunity_df[["ipv2"]],0) >= 95 & round(population_inmunity_df[["ipv2"]],0) <= 100) ~ 0,
    !population_and_pfa & round(population_inmunity_df[["ipv2"]],0)  > 100 ~ 3
  )
  return(score)
}

# Score succesfull campaign
score_succesfull_inmunization_campaign <- function(population_inmunity_df) {
  population_and_pfa <- population_and_pfa(population_inmunity_df)
  score = case_when(
    population_and_pfa & population_inmunity_df[["effective_inmunization_campaign"]] == lang_label("no") ~ 6,
    population_and_pfa & population_inmunity_df[["effective_inmunization_campaign"]] == lang_label("yes") ~ 0,
    !population_and_pfa & population_inmunity_df[["effective_inmunization_campaign"]] == lang_label("no") ~ 8,
    !population_and_pfa & population_inmunity_df[["effective_inmunization_campaign"]] == lang_label("yes") ~ 0,
    TRUE ~ 0
   )
  return(score)
}

## Survaillance ----

# Score reporting units
score_reporting_units <- function(survaillance_df) {
  population_and_pfa = population_and_pfa(survaillance_df)
  score = case_when(
    population_and_pfa & survaillance_df[["compliant_units"]] < 80 ~ 8,
    !population_and_pfa & survaillance_df[["compliant_units"]] < 80 ~ 10,
    TRUE ~ 0
  )
  return(score)
}

# Score PFA rate
score_pfa_rate <- function(survaillance_df) {
  population_and_pfa <- population_and_pfa(survaillance_df)
  score <- case_when(
    population_and_pfa & survaillance_df[['pfa_rate']] < 1 ~ 8,
    population_and_pfa & survaillance_df[['pfa_rate']] >= 1 ~ 0,
    TRUE ~ NA
  )
  return(score)
}

# Score PFA notified < 14 days
score_pfa_notified <- function(survaillance_df) {
  population_and_pfa <- population_and_pfa(survaillance_df)
  score <- case_when(
    population_and_pfa & survaillance_df[["pfa_notified"]] < 80 ~ 5,
    population_and_pfa & survaillance_df[["pfa_notified"]] >= 80 ~ 0,
    TRUE ~ NA
  )
  return(score)
}

# Score PFA investigated < 48 hr
score_pfa_investigated <- function(survaillance_df) {
  population_and_pfa <- population_and_pfa(survaillance_df)
  score <- case_when(
    population_and_pfa & survaillance_df[["pfa_investigated"]] < 80 ~ 5,
    population_and_pfa & survaillance_df[["pfa_investigated"]] >= 80 ~ 0,
    TRUE ~ NA
  )
  return(score)
}

# Score suitable samples
score_suitable_samples <- function(survaillance_df) {
  population_and_pfa <- population_and_pfa(survaillance_df)
  score <- case_when(
    population_and_pfa & survaillance_df[["suitable_samples"]] < 80 ~ 5,
    population_and_pfa & survaillance_df[["suitable_samples"]] >= 80 ~ 0,
    TRUE ~ NA
  )
  return(score)
}

# Score followups
score_followups <- function(survaillance_df) {
  population_and_pfa <- population_and_pfa(survaillance_df)
  score <- case_when(
    population_and_pfa & survaillance_df[["followups"]] < 80 ~ 5,
    population_and_pfa & survaillance_df[["followups"]] >= 80 ~ 0,
    TRUE ~ NA
  )
  return(score)
}

# Score active search
score_active_search <- function(survaillance_df) {
  population_and_pfa <- population_and_pfa(survaillance_df)
  score <- case_when(
    !population_and_pfa & survaillance_df[["active_search"]] == lang_label("no") ~ 10,
    !population_and_pfa & survaillance_df[["active_search"]] == lang_label("yes") ~ 0,
    TRUE ~ NA
  )
  return(score)
}

## Determinants ----

# score drinking water
score_drinking_water <- function(determinants_df) {
  population_and_pfa <- population_and_pfa(determinants_df)
  score <- case_when(
    population_and_pfa & determinants_df[["drinking_water"]] < 90 ~ 5,
    population_and_pfa & determinants_df[["drinking_water"]] >= 90 ~ 0,
    !population_and_pfa & determinants_df[["drinking_water"]] < 90 ~ 6,
    !population_and_pfa & determinants_df[["drinking_water"]] >= 90 ~ 0,
  )
  return(score)
}

# Score sanitation services
score_sanitation_services <- function(determinants_df) {
  population_and_pfa <- population_and_pfa(determinants_df)
  score <- case_when(
    population_and_pfa & determinants_df[["sanitation_services"]] < 90 ~ 5,
    population_and_pfa & determinants_df[["sanitation_services"]] >= 90 ~ 0,
    !population_and_pfa & determinants_df[["sanitation_services"]] < 90 ~ 6,
    !population_and_pfa & determinants_df[["sanitation_services"]] >= 90 ~ 0,
  )
  return(score)
}


## Outbreaks ----

# Score outbreaks
score_outbreak <- function(outbreaks_df, disease) {
  score <- case_when(
    outbreaks_df[[disease]] == lang_label("yes") ~ 2,
    outbreaks_df[[disease]] == lang_label("no") ~ 0
  )
  return(score)
}






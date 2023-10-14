# AUTORSHIP ----
# Organización Panamericana de la Salud
# Autor: Oliver Mazariegos
# Última fecha de modificación: 2023-10-09
# R 4.3.1

# SETUP ----

Sys.setlocale(locale = "es_ES.UTF-8")

# outbreaks = read_excel(PATH_country_data, sheet = 6, skip = 2, col_names = FALSE)
# colnames(outbreaks) =  c('admin1', 'admin2', 'subnational', 'region', 'measles', 'rubella', 'diphtheria', 'yellow_fever', 'tetanus')

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
LANG_TLS <- read_excel("R/translations.xlsx",sheet = "DASHBOARD") %>% 
  select(LABEL,all_of(LANG))
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

geocodes_cleansing <- function(df) {
  df <- df %>% 
    filter(!is.na(`ADMIN1 GEO_ID`) & !is.na(GEO_ID)) %>% 
    mutate(`ADMIN1 GEO_ID` = as.character(`ADMIN1 GEO_ID`),GEO_ID = as.character(GEO_ID))
  return(df)
} 

# SCORING Functions ----

## Population immunity ----

# Population and PFA
population_and_pfa <- function(population_inmunity_df) {
    score <- case_when(
      population_inmunity_df$POB >= 100000 | population_inmunity_df$pfa == lang_label("yes") ~ TRUE,
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
score_effective_campaign <- function(population_inmunity_df) {
  population_and_pfa <- population_and_pfa(population_inmunity_df)
  score = case_when(
    population_and_pfa & population_inmunity_df[["effective_campaign"]] == lang_label("no") ~ 6,
    population_and_pfa & population_inmunity_df[["effective_campaign"]] == lang_label("yes") ~ 0,
    !population_and_pfa & population_inmunity_df[["effective_campaign"]] == lang_label("no") ~ 8,
    !population_and_pfa & population_inmunity_df[["effective_campaign"]] == lang_label("yes") ~ 0,
    TRUE ~ 0
   )
  return(score)
}

## Survaillance ----

# Score reporting units
score_compliant_units <- function(survaillance_df) {
  population_and_pfa = population_and_pfa(survaillance_df)
  score = case_when(
    population_and_pfa & survaillance_df[["compliant_units_percent"]] < 80 ~ 8,
    !population_and_pfa & survaillance_df[["compliant_units_percent"]] < 80 ~ 10,
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
    population_and_pfa & survaillance_df[["pfa_notified_percent"]] < 80 ~ 5,
    population_and_pfa & survaillance_df[["pfa_notified_percent"]] >= 80 ~ 0,
    TRUE ~ NA
  )
  return(score)
}

# Score PFA investigated < 48 hr
score_pfa_investigated <- function(survaillance_df) {
  population_and_pfa <- population_and_pfa(survaillance_df)
  score <- case_when(
    population_and_pfa & survaillance_df[["pfa_investigated_percent"]] < 80 ~ 5,
    population_and_pfa & survaillance_df[["pfa_investigated_percent"]] >= 80 ~ 0,
    TRUE ~ NA
  )
  return(score)
}

# Score suitable samples
score_suitable_samples <- function(survaillance_df) {
  population_and_pfa <- population_and_pfa(survaillance_df)
  score <- case_when(
    population_and_pfa & survaillance_df[["suitable_samples_percent"]] < 80 ~ 5,
    population_and_pfa & survaillance_df[["suitable_samples_percent"]] >= 80 ~ 0,
    TRUE ~ NA
  )
  return(score)
}

# Score followups
score_followups <- function(survaillance_df) {
  population_and_pfa <- population_and_pfa(survaillance_df)
  score <- case_when(
    population_and_pfa & survaillance_df[["followups_percent"]] < 80 ~ 5,
    population_and_pfa & survaillance_df[["followups_percent"]] >= 80 ~ 0,
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


# OPTS ----
OPTS_DF <- read_xlsx(PATH_country_data,sheet = "_ListValues")
sex_opts <- unique(OPTS_DF$Sex)
sex_opts <- sex_opts[!is.na(sex_opts)]
yes_no_opts <- unique(OPTS_DF$`Yes No`)
yes_no_opts <- yes_no_opts[!is.na(yes_no_opts)]
outbreak_opts <- unique(OPTS_DF$Outbreaks)
outbreak_opts <- outbreak_opts[!is.na(outbreak_opts)]

# GENERAL ----
id_data <- read_excel(PATH_country_data,sheet = 2) %>% select(1,2,3,4)
colnames(id_data) <- c("ADMIN1 GEO_ID","GEO_ID","ADMIN1","ADMIN2")
id_data$`ADMIN1 GEO_ID` <- as.character(id_data$`ADMIN1 GEO_ID`)
id_data$GEO_ID <- as.character(id_data$GEO_ID)
id_data <- id_data %>% filter(!is.na(`ADMIN1 GEO_ID`) & !is.na(GEO_ID))
config_data <- read_excel(PATH_country_data,sheet = 1)
colnames(config_data) <- c("var","val")
YEAR_EVAL <- as.integer(config_data$val[2])
YEAR_CAMP_SR <- as.integer(config_data$val[4])
YEAR_1 = YEAR_EVAL - 5
YEAR_2 = YEAR_EVAL - 4
YEAR_3 = YEAR_EVAL - 3
YEAR_4 = YEAR_EVAL - 2
YEAR_5 = YEAR_EVAL - 1
COUNTRY_NAME <- config_data$val[1]
REPORT_FILE_FORMAT <- config_data$val[8]
scores_data <- id_data

# POPULATION AREA ----
pop_data <- read_excel(PATH_country_data,sheet = 2)
colnames(pop_data) <- c("ADMIN1 GEO_ID","GEO_ID","ADMIN1","ADMIN2","POB")
pop_data$`ADMIN1 GEO_ID` <- as.character(pop_data$`ADMIN1 GEO_ID`)
pop_data$GEO_ID <- as.character(pop_data$GEO_ID)
pop_data <- pop_data %>% filter(!is.na(`ADMIN1 GEO_ID`) & !is.na(GEO_ID))
pop_data$POB <- as.numeric(pop_data$POB)
ZERO_POB_LIST <- pop_data %>% filter(POB <= 0) %>% select(GEO_ID)
ZERO_POB_LIST <- ZERO_POB_LIST$GEO_ID

# POPULATION IMMUNITY ----

## Read data ----
immunity_data = read_excel(PATH_country_data, sheet = 3, skip = 2, col_names = FALSE)
colnames(immunity_data) = c('ADMIN1 GEO_ID', 'GEO_ID', 'ADMIN1', 'ADMIN2', 
                            'POB', 'pfa','year1','year2','year3',
                            'year4','year5','ipv2','effective_campaign')

## Filter missing GEO codes ----
immunity_data <- geocodes_cleansing(immunity_data)

## Scores calculation ----
immunity_scores <- immunity_data %>% 
  mutate_at(
    vars(contains('year')), ~ round(.,digits = 0)
  ) %>% 
  mutate(
    ipv2 = round(ipv2, digits = 0),
    population_and_pfa_bool = population_and_pfa(immunity_data),
    year1_score = score_coverage_polio3(immunity_data,'year1'),
    year2_score = score_coverage_polio3(immunity_data,'year2'),
    year3_score = score_coverage_polio3(immunity_data,'year3'),
    year4_score = score_coverage_polio3(immunity_data,'year4'),
    year5_score = score_coverage_polio3(immunity_data,'year5'),
    ipv_score = score_ipv2(immunity_data),
    effective_campaign_score = score_effective_campaign(immunity_data),
  ) %>% 
  rowwise() %>% 
  mutate(
      immunity_score = sum(c_across(matches('score')), na.rm = T) 
  )
  
## Adding to scores_data ----
immunity_scores_join <- immunity_scores %>% 
  select(
    'ADMIN1 GEO_ID', 
    'GEO_ID',
    'immunity_score'
  )
scores_data <- left_join(scores_data, immunity_scores_join)

# SURVAILLANCE ----

## Read data ----
survaillance <- read_excel(PATH_country_data, sheet = 4, skip = 2, col_names = FALSE)
colnames(survaillance) <- c('ADMIN1 GEO_ID', 'GEO_ID', 'ADMIN1', 'ADMIN2', 
                           'POB', 'pfa', 'compliant_units_percent', 'pfa_rate', 
                           'pfa_notified_percent', 'pfa_investigated_percent', 
                           'suitable_samples_percent', 'followups_percent', 'active_search')

## Filtering missing GEO codes ----
survaillance_data <- geocodes_cleansing(survaillance)

## Scores calculation ----
survaillance_scores <- survaillance_data %>% 
  mutate_at(
    vars(contains('percent')), ~ round(.,digits = 0)
  ) %>% 
  mutate(
    compliant_units_score = score_compliant_units(survaillance_data),
    pfa_rate_score = score_pfa_rate(survaillance_data),
    pfa_notified_score = score_pfa_notified(survaillance_data),
    pfa_investigated_score = score_pfa_investigated(survaillance_data),
    suitable_samples_score = score_suitable_samples(survaillance_data),
    followups_score = score_followups(survaillance_data),
    active_search_score = score_active_search(survaillance)
  ) %>% 
  rowwise() %>% 
  mutate(
    survaillance_score = sum(c_across(matches('score')), na.rm = T) 
  )
  
## Adding to scores_data ----
survaillance_scores_join <- survaillance_scores %>% 
  select(
    'ADMIN1 GEO_ID', 
    'GEO_ID',
    'survaillance_score'
  )
scores_data <- left_join(scores_data, survaillance_scores_join)

# DETERMINANTS ----

## Read data ----


## Filtering missing GEO codes ----


## Scores calculation ----


## Adding to scores_data ----










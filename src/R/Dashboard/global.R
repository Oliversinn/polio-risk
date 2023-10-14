##############################################################
# Herramienta digital Análisis de Riesgo SR - app.R
# Organización Panamericana de la Salud
# Autor: Luis Quezada
# Última fecha de modificación: 2023-08-18
# R 4.3.0
##############################################################

Sys.setlocale(locale = "es_ES.UTF-8")

# LIBS ----
library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinycssloaders)
library(fontawesome)
library(plotly)
library(leaflet)
library(readxl)
library(data.table)
library(tidyr)
library(DT)
library(janitor)
library(RColorBrewer)
library(sf)
library(htmltools)
library(tidyverse)
library(scales)
library(mapview)
library(webshot)
webshot::install_phantomjs()


# LOAD DATA ----
load(file = "SR_BD.RData")

# FUNCS ----
get_a1_geo_id <- function(admin1) {
  return(admin1_geo_id_df$`ADMIN1 GEO_ID`[admin1_geo_id_df$ADMIN1 == admin1])
}

lang_label <- function(label) {
  return(LANG_TLS$LANG[LANG_TLS$LABEL == label])
}

# TITLES ----

title_map_box <- function(indicator,admin1) {
  indicator = tolower(indicator)
  if (indicator == tolower(lang_label("menuitem_general_label"))) {
    indicator = lang_label("total")
  } else {
    indicator = paste0("(",indicator,")")
  }
  if (admin1 == toupper(lang_label("rep_label_all"))) {
    title_text <- paste0(lang_label("general_title_map_box")," ",indicator," - ",toupper(COUNTRY_NAME))
  } else {
    title_text <- paste0(lang_label("general_title_map_box")," ",indicator," - ",admin1,", ",toupper(COUNTRY_NAME))
  }
  return(title_text)
}

title_bar_box <- function(indicator,admin1) {
  indicator = tolower(indicator)
  if (indicator == tolower(lang_label("menuitem_general_label"))) {
    indicator = lang_label("total")
  } else {
    indicator = paste0("(",indicator,")")
  }
  if (admin1 == toupper(lang_label("rep_label_all"))) {
    title_text <- paste0(lang_label("general_title_bar_box")," ",indicator," - ",toupper(COUNTRY_NAME))
  } else {
    title_text <- paste0(lang_label("general_title_bar_box")," ",indicator," - ",admin1,", ",toupper(COUNTRY_NAME))
  }
  return(title_text)
}

title_data_box <- function(indicator,admin1) {
  indicator = tolower(indicator)
  if (indicator == tolower(lang_label("menuitem_general_label"))) {
    indicator = lang_label("total")
  } else {
    indicator = paste0("(",indicator,")")
  }
  if (admin1 == toupper(lang_label("rep_label_all"))) {
    title_text <- paste0(lang_label("general_title_data_box")," ",indicator," - ",toupper(COUNTRY_NAME))
  } else {
    title_text <- paste0(lang_label("general_title_data_box")," ",indicator," - ",admin1,", ",toupper(COUNTRY_NAME))
  }
  return(title_text)
}

title_pie_box <- function(indicator,admin1) {
  indicator = tolower(indicator)
  if (admin1 == toupper(lang_label("rep_label_all"))) {
    title_text <- paste0(lang_label("title_pie_box")," (",indicator,") - ",toupper(COUNTRY_NAME))
  } else {
    title_text <- paste0(lang_label("title_pie_box")," (",indicator,") - ",admin1,", ",toupper(COUNTRY_NAME))
  }
  return(title_text)
}


# SOURCE ----
source("general.R")
source("inm_pob.R")
source("surv_qual.R")
source("prog_del.R")
source("vul_group.R")
source("thre_asse.R")
source("rap_res.R")

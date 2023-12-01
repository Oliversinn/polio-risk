
# Herramienta digital Análisis de Riesgo Polio - qa.R
# Organización Panamericana de la Salud
# Autor: Oliver Mazariegos
# Última fecha de modificación: 2023-11-30
# R 4.3.0


Sys.setlocale(locale = "es_ES.UTF-8")

# Working dir ----
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
library(pacman)
p_load(tidyverse,tidyr,readxl,shiny,
       webshot,sf,sp,mapview,leaflet,
       htmltools,DT,data.table,writexl)

# Data QA ----
rmarkdown::render("R/QA_report.Rmd")
file.copy(from = "R/QA_report.docx",to ="R/Modals/www/QA_report.docx",overwrite = T)
file.remove(from = "R/QA_report.docx")

# LANG MSG ----
LANG <- as.character(read_excel("Data/country_data.xlsx",sheet = 1)[8,2])
if (!(LANG %in% c("SPA","ENG","FRA","POR"))) {LANG = "SPA"}
LANG_TLS <- read_excel("R/translations.xlsx",sheet="MSG") %>% select(LABEL,all_of(LANG))
colnames(LANG_TLS) <- c("LABEL","LANG")
if (report_has_errors) {
  LANG_TLS$LABEL[LANG_TLS$LABEL == "qa_report_incorrect"] = "qa_report_result_msg"
  LANG_TLS$LABEL[LANG_TLS$LABEL == "qa_report_result_incorrect"] = "qa_report_result_title"
  LANG_TLS <- rbind(LANG_TLS,c("qa_result","FALSE"))
} else {
  LANG_TLS$LABEL[LANG_TLS$LABEL == "qa_report_correct"] = "qa_report_result_msg"
  LANG_TLS$LABEL[LANG_TLS$LABEL == "qa_report_result_correct"] = "qa_report_result_title"
  LANG_TLS <- rbind(LANG_TLS,c("qa_result","TRUE"))
}
write_xlsx(LANG_TLS,"R/Modals/qarm_lang.xlsx")

# Modal ----
shiny::runApp("R/Modals/qa_modal.R")





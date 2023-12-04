Sys.setlocale(locale = "es_ES.UTF-8")
# grep -r -o 'lang_label(".*")' . | awk -F'"' '{ print $2 }' | sort | uniq > labels.txt
library(readxl)
library(readr)
library(dplyr)
library(writexl)

file_path = rstudioapi::getSourceEditorContext()$path
file_path_index = unlist(gregexec('R/lang_label_cleanup.R',file_path))[1]
PATH_global = substr(file_path,1,file_path_index - 1)
PATH_translations   = paste0(PATH_global,"R/translations.xlsx")
PATH_labels_txt = paste0(PATH_global, "R/labels.txt")

labels_used <- read_csv(PATH_labels_txt, col_names = "label")

# REPORT ----
labels_report <- read_excel(PATH_translations, "REPORT")
labels_report_cleand <- labels_report %>% 
  filter(
    LABEL %in% labels_used$label
  )

# QA REPORT ----
labels_qa_report <- read_excel(PATH_translations, "QA_REPORT")
labels_qa_report_cleand <- labels_qa_report %>% 
  filter(
    LABEL %in% labels_used$label
  )

# DASHBOARD ----
labels_dashboard <- read_excel(PATH_translations, "DASHBOARD")
labels_dashboard_cleand <- labels_dashboard %>% 
  filter(
    LABEL %in% labels_used$label
  )

# MSG ----
labels_msg <- read_excel(PATH_translations, "MSG")
labels_msg_cleand <- labels_msg %>% 
  filter(
    LABEL %in% labels_used$label
  )

write_xlsx(
  list(
    REPORT = labels_report_cleand,
    QA_REPORT = labels_qa_report_cleand,
    DASHBOARD = labels_dashboard_cleand,
    MSG = labels_msg_cleand
  ),
  PATH_translations
)
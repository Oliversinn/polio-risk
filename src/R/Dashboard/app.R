# AUTORSHIP ----
# Pan American Health Organization
# Author: Luiz Quezada & Oliver Mazariegos
# Last Update: 2023-10-09
# R 4.3.1

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

options(shiny.fullstacktrace=TRUE)


# LOAD DATA ----
load(file = "POLIO.RData")
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

# UI ----
ui <- fluidPage(
  ## HEADER ----
  fluidRow(
    box(width = 12, background = "maroon",
        HTML(paste0('<center><div style = "text-align: left; padding-left: 30px; padding-right: 30px; padding-top: 10px;">
                        <img src="',lang_label("logo_org"),'" height="35"> <img id="country_flag" style = "right: 30px !important; position: absolute; padding-top: 1px; padding-bottom: 1px; padding-right: 1px; padding-left: 1px; margin-bottom: 10px; background-color: white;" src="country_flag.png" height="50">
                        </div> <h2>',lang_label("dashboard_title"),' <b>',toupper(COUNTRY_NAME),'</b></h2> </center>'))
    )
  ),
  ## DASHBOARD SKIN ----
  dashboardPage(
    skin = "purple",
    title = lang_label('dashboard_tab_title'),
    
    ## DASHBOARD HEADER ----
    header = dashboardHeader(
      titleWidth = 300,
      title = paste0(lang_label("header_year_eval"),": ", YEAR_EVAL)
    ),
    
    ## DASHBOARD SIDEBAR ----
    dashboardSidebar(
      width = 300,
      
      sidebarMenu(
        id = "sidebarid",
        
        ### GENERAL SCORES ----
        menuItem(
          text = lang_label("menuitem_general"),
          tabName = "GENERAL",
          icon = icon("square-check")
        ),
        
        #### CONDICTIONAL PANEL ----
        conditionalPanel(
          'input.sidebarid == "GENERAL"',
          
          ##### SOCRE TYPE SELECTOR
          selectInput("indicadores_select_indicador",
                      label = paste0(lang_label("general_select_ind"), ":"),
                      choices = c(
                        lang_label("menuitem_general_label")
                        #lang_label("menuitem_inm_pob"),
                        #lang_label("menuitem_determinants"),
                        #lang_label("menuitem_surv_qual"),
                        #lang_label("menuitem_outbreaks"),
                        #lang_label("menuitem_rap_res")
                      ),
                      selected = lang_label("menuitem_general_label")
          ),
          bsTooltip("indicadores_select_indicador", lang_label("tooltip_select_ind"), placement = "right", trigger = "hover",options = NULL),
          ##### ADMIN1 SELECTOR ----
          selectInput("indicadores_select_admin1", label = paste0(lang_label("general_select_admin1"), ":"), choices = admin1_list, selected = admin1_list[1]),
          bsTooltip("indicadores_select_admin1", lang_label("tooltip_select_admin1"), placement = "right", trigger = "hover",options = NULL),
          #### CUT OFF SELECTOR
          selectInput("indicadores_select_risk",label = paste0(lang_label("general_select_risk"),":"),
                      choices = c(toupper(lang_label("rep_label_all")),
                                  lang_label("cut_offs_VHR"),
                                  lang_label("cut_offs_HR"),
                                  lang_label("cut_offs_MR"),
                                  lang_label("cut_offs_LR")
                      )
          ),
          bsTooltip("indicadores_select_risk", lang_label("tooltip_select_risk"), placement = "right", trigger = "hover",options = NULL)
          
        )
      )
    ),
    ## DASHBOARD BODY ----
    dashboardBody(
      fluidPage(
        ### JS ----
        tags$head(tags$script(src = "message-handler.js")),
        ### CSS ----
        tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "style.css")),
        
        tabItems(
          #### TAB GENERAL SCORES  
          tabItem(
            tabName = "GENERAL",
            h2(textOutput("indicadores_title")),
            br(),

            fluidRow(
              valueBoxOutput("ind_box_1",width = 3),
              valueBoxOutput("ind_box_2",width = 3),
              valueBoxOutput("ind_box_3",width = 3),
              valueBoxOutput("ind_box_4",width = 3)
            )
          )
        )
      )
    )
    
  )
)

# SERVER ----
server <- function(input, output, session) {
  
  # SERVER GENERAL IND ----
  ind_rename <- function(selected_ind) {
    renamed <- case_when(
      lang_label("menuitem_general_label") == selected_ind ~ "total_score",
      lang_label("menuitem_immunity") == selected_ind ~ "immunity_score",
      lang_label("menuitem_survaillance") == selected_ind ~ "survailance_score",
      lang_label("menuitem_determinants") == selected_ind ~ "determinants_score",
      lang_label("menuitem_outbreaks") == selected_ind ~ "outbreaks_score"
    )
    return(renamed)
  }
  
  risk_rename <- function(selected_risk) {
    return(
      case_when(
        toupper(lang_label("rep_label_all")) == selected_risk ~ "ALL",
        lang_label("cut_offs_VHR") == selected_risk ~ "VHR",
        lang_label("cut_offs_HR") == selected_risk ~ "HR",
        lang_label("cut_offs_MR") == selected_risk ~ "MR",
        lang_label("cut_offs_LR") == selected_risk ~ "LR"
      )
    )
  }
  
  box_data <- reactiveValues()
  box_data$a1 <- 0
  box_data$a2 <- 0
  box_data$a3 <- 0
  box_data$a4 <- 0
  box_data$at <- 0
  
  observeEvent(input$indicadores_select_indicador, {
    new_box_data <- datos_boxes(LANG_TLS,indicadores_prep_box_data())
    box_data$a1 <- new_box_data[1]
    box_data$a2 <- new_box_data[2]
    box_data$a3 <- new_box_data[3]
    box_data$a4 <- new_box_data[4]
    box_data$at <- new_box_data[5]
  })

  observeEvent(input$indicadores_select_admin1, {
    new_box_data <- datos_boxes(LANG_TLS,indicadores_prep_box_data())
    box_data$a1 <- new_box_data[1]
    box_data$a2 <- new_box_data[2]
    box_data$a3 <- new_box_data[3]
    box_data$a4 <- new_box_data[4]
    box_data$at <- new_box_data[5]
  })
  
  
  
  box_lugar <- function(admin1) {
    if (admin1 == toupper(lang_label("rep_label_all"))) {
      return(toupper(COUNTRY_NAME))
    } else {
      return(toupper(admin1))
    }
  }
  
  output$ind_box_1 <- renderValueBox({
    valueBox(
      VB_style(get_box_text(box_data$a1,box_data$at,"LR"),"font-size: 90%;"),
      VB_style(paste(lang_label("box_LR_admin2"),box_lugar(input$indicadores_select_admin1)),"font-size: 95%;"),
      icon = icon('ok-sign', lib = 'glyphicon'),
      color = "purple"
    )
  })
  
  output$ind_box_2 <- renderValueBox({
    valueBox(
      VB_style(get_box_text(box_data$a2,box_data$at,"MR"),"font-size: 85%;"),
      VB_style(paste(lang_label("box_MR_admin2"),box_lugar(input$indicadores_select_admin1)),"font-size: 95%;"),
      icon = icon('minus-sign', lib = 'glyphicon'),
      color = "purple"
    )
  })
  
  output$ind_box_3 <- renderValueBox({
    valueBox(
      VB_style(get_box_text(box_data$a3,box_data$at,"HR"),"font-size: 85%;"),
      VB_style(paste(lang_label("box_HR_admin2"),box_lugar(input$indicadores_select_admin1)),"font-size: 95%;"),
      icon = icon('exclamation-sign', lib = 'glyphicon'),
      color = "purple"
    )
  })
  
  output$ind_box_4 <- renderValueBox({
    valueBox(
      VB_style(get_box_text(box_data$a4,box_data$at,"VHR"),"font-size: 85%;"),
      VB_style(paste(lang_label("box_VHR_admin2"),box_lugar(input$indicadores_select_admin1)),"font-size: 95%;"),
      icon = icon('alert', lib = 'glyphicon'),
      color = "purple"
    )
  })
  
  output$indicadores_title <- renderText({
    input$indicadores_select_indicador
  })
  
  ## REACTIVE ----
  ### indicadores_prep_box_data ----
  indicadores_prep_box_data <- reactive({
    ind_prep_box_data(LANG_TLS,
                      CUT_OFFS,
                      scores_data,
                      ind_rename(input$indicadores_select_indicador),
                      unique(get_a1_geo_id(input$indicadores_select_admin1)))
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)


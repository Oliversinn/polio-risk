# AUTORSHIP ----
# Pan American Health Organization
# Author: Oliver Mazariegos
# Last Update: 2023-10-09
# R 4.3.1

# SERVER ----
function(input, output, session) {
  
  # GENERAL FUNCTIONS ----
  ind_rename <- function(selected_ind) {
    renamed <- case_when(
      lang_label("menuitem_general_label") == selected_ind ~ "total_score",
      lang_label("menuitem_immunity") == selected_ind ~ "immunity_score",
      lang_label("menuitem_survaillance") == selected_ind ~ "survaillance_score",
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
  
  box_lugar <- function(admin1) {
    if (admin1 == toupper(lang_label("rep_label_all"))) {
      return(toupper(COUNTRY_NAME))
    } else {
      return(toupper(admin1))
    }
  }
  
  
  #### VALUE BOXES ----
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
  
  indicadores_prep_box_data <- reactive({
    ind_prep_box_data(LANG_TLS,
                      CUT_OFFS,
                      scores_data,
                      ind_rename(input$indicadores_select_indicador),
                      unique(get_a1_geo_id(input$indicadores_select_admin1)))
  })
  
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
  
  #### MAP ----
  indicadores_prep_map_data <- reactive({
    ind_prep_map_data(LANG_TLS,ZERO_POB_LIST,CUT_OFFS,country_shapes,scores_data,ind_rename(input$indicadores_select_indicador),get_a1_geo_id(input$indicadores_select_admin1),risk_rename(input$indicadores_select_risk))
  })
  
  ind_map <- reactiveValues(dat = 0)
  ind_map_2 <- reactiveValues(dat = 0)
  
  
  output$indicadores_title_map_box <- renderText({
    text_title <- title_map_box(input$indicadores_select_indicador,input$indicadores_select_admin1)
    text_title <- paste0(text_title," (",YEAR_1," - ",YEAR_5,")")
    text_title
  })
  
  output$indicadores_plot_map <- renderLeaflet({
    ind_map$dat <- ind_plot_map_data(LANG_TLS,ZERO_POB_LIST,CUT_OFFS,indicadores_prep_map_data(),ind_rename(input$indicadores_select_indicador),get_a1_geo_id(input$indicadores_select_admin1),risk_rename(input$indicadores_select_risk))
    ind_map$dat
  })
  
  output$dl_indicadores_plot_map <- downloadHandler(
    filename = function() {
      paste0(lang_label("map")," ",toupper(COUNTRY_NAME)," ",input$indicadores_select_indicador," (",YEAR_1,"-",YEAR_5,").png")
    },
    content = function(file) {
      mapshot(ind_map$dat, file = file)
    }
  )
  
  output$indicadores_table <- renderDataTable(server = FALSE,{
    ind_get_bar_table(LANG_TLS,CUT_OFFS,scores_data,ind_rename(input$indicadores_select_indicador),get_a1_geo_id(input$indicadores_select_admin1),risk_rename(input$indicadores_select_risk))
  })
  
  output$indicadores_plot_map_2 <- renderLeaflet({
    ind_map_2$dat <- ind_plot_map_data(LANG_TLS,ZERO_POB_LIST,CUT_OFFS,indicadores_prep_map_data(),ind_rename(input$indicadores_select_indicador),get_a1_geo_id(input$indicadores_select_admin1),risk_rename(input$indicadores_select_risk))
    ind_map_2$dat
  })
  
  output$dl_indicadores_plot_map_2 <- downloadHandler(
    filename = function() {
      paste0(lang_label("map")," ",input$indicadores_select_admin1," ",toupper(COUNTRY_NAME)," ",input$indicadores_select_indicador," (",YEAR_1,"-",YEAR_5,").png")
    },
    content = function(file) {
      mapshot(ind_map_2$dat, file = file)
    }
  )
  
  output$indicadores_table_2 <- renderDataTable(server = FALSE,{
    ind_get_bar_table(LANG_TLS,CUT_OFFS,scores_data,ind_rename(input$indicadores_select_indicador),get_a1_geo_id(input$indicadores_select_admin1),risk_rename(input$indicadores_select_risk))
  })
  
  ### TOTAL POINTS ----
  indicadores_prep_bar_data <- reactive({
    ind_prep_bar_data(LANG_TLS,CUT_OFFS,scores_data,ind_rename(input$indicadores_select_indicador),get_a1_geo_id(input$indicadores_select_admin1),risk_rename(input$indicadores_select_risk))
  })
  
  
  output$indicadores_title_bar_box <- renderText({
    text_title <- title_bar_box(input$indicadores_select_indicador,input$indicadores_select_admin1)
    text_title <- paste0(text_title," (",YEAR_1," - ",YEAR_5,")")
    text_title
  })
  
  output$indicadores_plot_bar <- renderPlotly({
    pfa_filter <- case_when(
      input$general_title_plot_bar_filter == lang_label("population_pfa_filter") ~ TRUE,
      input$general_title_plot_bar_filter == lang_label("population_pfa_no_filter") ~ FALSE,
    )
    ind_plot_bar_data(LANG_TLS,CUT_OFFS,indicadores_prep_bar_data(),ind_rename(input$indicadores_select_indicador),get_a1_geo_id(input$indicadores_select_admin1), pfa_filter)
  })
  
  output$indicadores_plot_multibar <- renderPlotly({
    pfa_filter <- case_when(
      input$general_title_plot_multibar_filter == lang_label("population_pfa_filter") ~ TRUE,
      input$general_title_plot_multibar_filter == lang_label("population_pfa_no_filter") ~ FALSE,
    )
    ind_plot_multibar_data(LANG_TLS,CUT_OFFS,scores_data,get_a1_geo_id(input$indicadores_select_admin1),ind_rename(input$indicadores_select_indicador),risk_rename(input$indicadores_select_risk), pfa_filter)
  })
  
  ### INDICATORS CHEAT SHEET ----
  output$indicadores_rangos_table <- renderDataTable(server = FALSE,{
    ind_rangos_table(LANG_TLS,CUT_OFFS,ind_rename(input$indicadores_select_indicador), TRUE)
  })
  
  
}
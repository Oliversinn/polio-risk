# AUTHORSHIP ----

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
      lang_label("menuitem_surveillance") == selected_ind ~ "surveillance_score",
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
    text_title <- paste0(text_title," (",YEAR_1," - ",YEAR_EVAL,")")
    text_title
  })
  
  output$indicadores_plot_map <- renderLeaflet({
    ind_map$dat <- ind_plot_map_data(LANG_TLS,ZERO_POB_LIST,CUT_OFFS,indicadores_prep_map_data(),ind_rename(input$indicadores_select_indicador),get_a1_geo_id(input$indicadores_select_admin1),risk_rename(input$indicadores_select_risk))
    ind_map$dat
  })
  
  output$dl_indicadores_plot_map <- downloadHandler(
    filename = function() {
      paste0(lang_label("map")," ",toupper(COUNTRY_NAME)," ",input$indicadores_select_indicador," (",YEAR_1,"-",YEAR_EVAL,").png")
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
      paste0(lang_label("map")," ",input$indicadores_select_admin1," ",toupper(COUNTRY_NAME)," ",input$indicadores_select_indicador," (",YEAR_1,"-",YEAR_EVAL,").png")
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
    pfa_filter <- case_when(
      input$general_limits_table_filter == lang_label("population_pfa_filter") ~ TRUE,
      input$general_limits_table_filter == lang_label("population_pfa_no_filter") ~ FALSE,
    )
    ind_rangos_table(LANG_TLS,CUT_OFFS,ind_rename(input$indicadores_select_indicador), pfa_filter)
  })
  
  # IMMUNITY ----
  ## MAP ----
  ### IMMUNITY SCORE ----
  output$inmunidad_title_map_box <- renderText({
    title_map_box(lang_label("INM_POB"),input$inmunidad_select_admin1)
  })
  
  inmu_map_total <- reactiveValues(dat = 0)
  
  output$inmunidad_map_total <- renderLeaflet({
    inmu_map_total$dat <- inmu_plot_map_data(LANG_TLS,YEAR_CAMP_SR,toupper(COUNTRY_NAME),YEAR_LIST,ZERO_POB_LIST,CUT_OFFS,country_shapes,immunity_scores,"immunity_score",input$inmunidad_select_admin1,get_a1_geo_id(input$inmunidad_select_admin1),admin1_geo_id_df)
    inmu_map_total$dat
  })
  
  output$dl_inmunidad_map_total <- downloadHandler(
    filename = function() {
      paste0(lang_label("map")," ",input$inmunidad_select_admin1," ",toupper(COUNTRY_NAME)," ",lang_label("INM_POB")," (",YEAR_1,"-",YEAR_5,").png")
    },
    content = function(file) {
      mapshot(inmu_map_total$dat, file = file)
    }
  )
  
  ### POLIO COVERAGE ----
  inmu_map_cob_1 <- reactiveValues(dat = 0)
  
  output$inmunidad_map_cob_1 <- renderLeaflet({
    var_srp1 <- case_when(
      input$radio_inmunidad_cob_1 == paste(lang_label("vac_coverage"),YEAR_1) ~ "year1",
      input$radio_inmunidad_cob_1 == paste(lang_label("vac_coverage"),YEAR_2) ~ "year2",
      input$radio_inmunidad_cob_1 == paste(lang_label("vac_coverage"),YEAR_3) ~ "year3",
      input$radio_inmunidad_cob_1 == paste(lang_label("vac_coverage"),YEAR_4) ~ "year4",
      input$radio_inmunidad_cob_1 == paste(lang_label("vac_coverage"),YEAR_5) ~ "year5",
      input$radio_inmunidad_cob_1 == lang_label("risk_points") ~ "SRP1_PR"
    )
    inmu_map_cob_1$dat <- inmu_plot_map_data(LANG_TLS,YEAR_CAMP_SR,toupper(COUNTRY_NAME),YEAR_LIST,ZERO_POB_LIST,CUT_OFFS,country_shapes,immunity_scores,var_srp1,input$inmunidad_select_admin1,get_a1_geo_id(input$inmunidad_select_admin1),admin1_geo_id_df)
    inmu_map_cob_1$dat
  })
  
  output$dl_inmunidad_map_cob_1 <- downloadHandler(
    filename = function() {
      paste0(lang_label("map")," ",input$inmunidad_select_admin1," ",toupper(COUNTRY_NAME)," ",lang_label("immunity_polio_cob")," (",input$radio_inmunidad_cob_1,").png")
    },
    content = function(file) {
      mapshot(inmu_map_cob_1$dat, file = file)
    }
  )
  
  ### IPV2 COVERAGE ----
  inmu_map_cob_2 <- reactiveValues(dat = 0)
  
  output$inmunidad_map_cob_2 <- renderLeaflet({
    inmu_map_cob_2$dat <- inmu_plot_map_data(LANG_TLS,YEAR_CAMP_SR,toupper(COUNTRY_NAME),YEAR_LIST,ZERO_POB_LIST,CUT_OFFS,country_shapes,immunity_scores,"ipv2",input$inmunidad_select_admin1,get_a1_geo_id(input$inmunidad_select_admin1),admin1_geo_id_df)
    inmu_map_cob_2$dat
  })
  
  output$dl_inmunidad_map_cob_2 <- downloadHandler(
    filename = function() {
      paste0(lang_label("map")," ",input$inmunidad_select_admin1," ",toupper(COUNTRY_NAME)," ",lang_label("immunity_ipv2_cob")," (",YEAR_EVAL,").png")
    },
    content = function(file) {
      mapshot(inmu_map_cob_2$dat, file = file)
    }
  )
  
  ### EFFECTIVE COVERAGE ----
  inmu_map_effective <- reactiveValues(dat = 0)
  
  output$inmunidad_map_effective <- renderLeaflet({
    inmu_map_effective$dat <- inmu_plot_map_data(LANG_TLS,YEAR_CAMP_SR,toupper(COUNTRY_NAME),YEAR_LIST,ZERO_POB_LIST,CUT_OFFS,country_shapes,immunity_scores,"effective_campaign",input$inmunidad_select_admin1,get_a1_geo_id(input$inmunidad_select_admin1),admin1_geo_id_df)
    inmu_map_effective$dat
  })
  
  output$dl_inmunidad_map_cob_2 <- downloadHandler(
    filename = function() {
      paste0(lang_label("map")," ",input$inmunidad_select_admin1," ",toupper(COUNTRY_NAME)," ",lang_label("immunity_effective_cob")," (",YEAR_1, "-", YEAR_EVAL,").png")
    },
    content = function(file) {
      mapshot(inmu_map_effective$dat, file = file)
    }
  )
  
  ## PIE ----
  output$inmunidad_title_pie_box <- renderText({
    title_pie_box(lang_label("INM_POB"),input$inmunidad_select_admin1)
  })
  
  output$inmunidad_plot_pie <- renderPlotly({
    plot_pie_data(LANG_TLS,ZERO_POB_LIST,CUT_OFFS,"immunity_score",immunity_scores,get_a1_geo_id(input$inmunidad_select_admin1),return_table=F)
  })
  
  output$inmunidad_table_dist <- renderDataTable(server = FALSE,{
    plot_pie_data(LANG_TLS,ZERO_POB_LIST,CUT_OFFS,"immunity_score",immunity_scores,get_a1_geo_id(input$inmunidad_select_admin1),return_table=T)
  })
  
  ## DATATABLE ----
  output$inmunidad_title_data_box <- renderText({
    title_data_box(lang_label("INM_POB"),input$inmunidad_select_admin1)
  })
  
  output$inmunidad_table <- renderDataTable(server = FALSE,{
    inmu_get_data_table(LANG_TLS,YEAR_LIST,CUT_OFFS,immunity_scores,get_a1_geo_id(input$inmunidad_select_admin1))
  })
  
  ## CHEAT SHEET ----
  output$inmu_rangos_table <- renderDataTable(server = FALSE,{
    pfa_filter <- case_when(
      input$immunity_limits_table_filter == lang_label("population_pfa_filter") ~ TRUE,
      input$immunity_limits_table_filter == lang_label("population_pfa_no_filter") ~ FALSE,
    )
    ind_rangos_table(LANG_TLS,CUT_OFFS,"immunity_score", pfa_filter)
  })
  
  # SURVEILLANCE ----
  ## MAP ----
  output$surveillance_title_map_box <- renderText({
    title_map_box(lang_label("SURV_QUAL"),input$surveillance_select_admin1)
  })
  
  ### SURVEILLANCE SCORE ----
  cal_map_total <- reactiveValues(dat = 0)
  
  output$calidad_map_total <- renderLeaflet({
    cal_map_total$dat <- cal_plot_map_data(LANG_TLS,toupper(COUNTRY_NAME),YEAR_LIST,ZERO_POB_LIST,CUT_OFFS,country_shapes,surveillance_scores,"surveillance_score",input$surveillance_select_admin1,get_a1_geo_id(input$surveillance_select_admin1),admin1_geo_id_df)
    cal_map_total$dat
  })
  
  output$dl_surveillance_map_total <- downloadHandler(
    filename = function() {
      paste0(lang_label("map")," ",input$calidad_select_admin1," ",toupper(COUNTRY_NAME)," ",lang_label("SURV_QUAL")," (",YEAR_1,"-",YEAR_5,").png")
    },
    content = function(file) {
      mapshot(cal_map_total$dat, file = file)
    }
  )
  
  ### REPORTING UNITS ----
  cal_map_2 <- reactiveValues(dat = 0)
  output$calidad_map_2 <- renderLeaflet({
    cal_map_2$dat <- cal_plot_map_data(LANG_TLS,toupper(COUNTRY_NAME),YEAR_LIST,ZERO_POB_LIST,CUT_OFFS,country_shapes,surveillance_scores,"compliant_units_percent",input$surveillance_select_admin1,get_a1_geo_id(input$surveillance_select_admin1),admin1_geo_id_df)
    cal_map_2$dat
  })
  
  
  
  
}
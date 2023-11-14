# AUTHORSHIP ----

# Pan American Health Organization
# Author: Oliver Mazariegos
# Última fecha de modificación: 2023-11-06
# R 4.3.1

determinants_title_map <- function(LANG_TLS,COUNTRY_NAME,YEAR_LIST,admin1,var) {
  var_text <- case_when(
    var == "determinants_score" ~ paste0(lang_label_tls(LANG_TLS,"determinants_title_map_total_pr")," ",admin1_transform(LANG_TLS,COUNTRY_NAME,admin1)," (",YEAR_LIST[5],")"),
    var == "drinking_water_percent" ~ paste0(lang_label_tls(LANG_TLS,"determinants_title_map_water")," ",admin1_transform(LANG_TLS,COUNTRY_NAME,admin1)," (",YEAR_LIST[5],")"),
    var == "sanitation_services_percent" ~ paste0(lang_label_tls(LANG_TLS,"determinants_title_map_sanitation")," ",admin1_transform(LANG_TLS,COUNTRY_NAME,admin1)," (",YEAR_LIST[5],")"),
  )
  return(var_text)
}


determinants_plot_map_data <- function(LANG_TLS,COUNTRY_NAME,YEAR_LIST,ZERO_POB_LIST,CUT_OFFS,map_data,data,var_to_summarise,admin1,admin1_id,admin1_geo_id_df, pop_filter = lang_label("filter_all")) {

  indicator <- "determinants_score"
  data <- data %>% select(-ADMIN1,-ADMIN2)
  if ("ADMIN1 GEO_ID" %in% colnames(data)) {
    data <- data %>% select(-`ADMIN1 GEO_ID`)
  }
  map_data <- full_join(map_data,data,by = "GEO_ID")
  
  map_data$`ADMIN1 GEO_ID`[is.na(map_data$`ADMIN1 GEO_ID`) & map_data$ADMIN1 == admin1] <- admin1_geo_id_df$`ADMIN1 GEO_ID`[admin1_geo_id_df$ADMIN1 == admin1]
  
  if (var_to_summarise == "determinants_score") {
    map_data$risk_level <- get_risk_level(LANG_TLS,CUT_OFFS,indicator,map_data$determinants_score, map_data$population_and_pfa_bool)
    map_data$risk_level[map_data$GEO_ID %in% ZERO_POB_LIST] <- "NO_HAB"
    
    if (admin1_id == 0) {
      map_data <- map_data %>% select(GEO_ID,ADMIN1,ADMIN2,determinants_score,risk_level,geometry, POB15)
    } else {
      map_data <- map_data %>% filter(`ADMIN1 GEO_ID` == admin1_id) %>% select(GEO_ID,ADMIN1,ADMIN2,determinants_score,risk_level,geometry, POB15)
    }
    
    if (pop_filter != lang_label("filter_all")) {
      if (pop_filter == lang_label("less_than_100000")) {
        map_data <- map_data %>% 
          mutate(
            risk_level = case_when(
              POB15 >= 100000 ~ lang_label("na"),
              T ~ risk_level
            )
          )
      } else if (pop_filter == lang_label("greater_than_100000")) {
        map_data <- map_data %>%
          mutate(
            risk_level = case_when(
              POB15 < 100000 ~ lang_label("na"),
              T ~ risk_level
            )
          )
      }
    }
    
    map_data <- map_data %>% mutate(
      risk_level_num = case_when(
        is.na(risk_level) ~ 0,
        risk_level == lang_label_tls(LANG_TLS,"LR") ~ 1,
        risk_level == lang_label_tls(LANG_TLS,"MR") ~ 2,
        risk_level == lang_label_tls(LANG_TLS,"HR") ~ 3,
        risk_level == "NO_HAB" ~ 4,
        risk_level == lang_label("na") ~ 5
      ),
      risk_level_word = case_when(
        is.na(risk_level) ~ lang_label_tls(LANG_TLS,"no_data"),
        risk_level == "NO_HAB" ~ lang_label_tls(LANG_TLS,"no_hab"),
        T ~ risk_level
      )
    )
    
    pal_gradient <- colorNumeric(
      c("#666666","#92d050","#fec000","#e8132b","#9bc2e6","#111111"),
      domain = c(0,5)
    )
    
    legend_colors = c("#e8132b","#fec000","#92d050")
    legend_values = c(lang_label_tls(LANG_TLS,"cut_offs_HR"),
                      lang_label_tls(LANG_TLS,"cut_offs_MR"),
                      lang_label_tls(LANG_TLS,"cut_offs_LR"))
    
    if (0 %in% map_data$risk_level_num) {
      legend_colors = c("#666666",legend_colors)
      legend_values = c(lang_label_tls(LANG_TLS,"no_data"),legend_values)
    }
    
    if (length(ZERO_POB_LIST) > 0) {
      legend_colors = c(legend_colors,"#9bc2e6")
      legend_values = c(legend_values,lang_label_tls(LANG_TLS,"no_hab"))
    }
    
    if (5 %in% map_data$risk_level_num) {
      legend_colors = c(legend_colors,"#111111")
      legend_values = c(legend_values,lang_label_tls(LANG_TLS,"na"))
    }
    
    shape_label <- sprintf("<strong>%s</strong>, %s<br/>%s: %s<br/>%s: %s",
                           map_data$ADMIN2,
                           map_data$ADMIN1,
                           lang_label_tls(LANG_TLS,"risk_points"),
                           map_data$determinants_score,
                           lang_label_tls(LANG_TLS,"risk_level"),
                           map_data$risk_level_word
    ) %>% lapply(HTML)
    
    
    # MAPA
    map <- leaflet(map_data,options = leafletOptions(doubleClickZoom = T, attributionControl = F, zoomSnap=0.1, zoomDelta=0.1)) %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
      addPolygons(
        fillColor   = ~pal_gradient(risk_level_num),
        fillOpacity = 0.7,
        dashArray   = "",
        weight      = 1,
        color       = "#333333",
        opacity     = 1,
        highlight = highlightOptions(
          weight = 2,
          color = "#333333",
          dashArray = "",
          fillOpacity = 1,
          bringToFront = TRUE),
        label = shape_label,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")
      ) %>%
      addLegend(layerId = "map_title","topright",color = "white", opacity = 0,labels=HTML(paste0("<strong>",determinants_title_map(LANG_TLS,COUNTRY_NAME,YEAR_LIST,admin1,var_to_summarise),"</strong>"))) %>%
      addLegend(title = lang_label_tls(LANG_TLS,"legend_risk_class"),colors = legend_colors,labels = legend_values, opacity = 0.5, position = 'topright')
  } 
  else if (var_to_summarise %in% c("drinking_water_percent", "sanitation_services_percent")) {
    # Cob map
    map_data <- map_data %>% rename("COB"=var_to_summarise)
    
    
    if (admin1_id == 0) {
      map_data <- map_data %>% select(GEO_ID,ADMIN1,ADMIN2,COB,geometry, POB15)
    } else {
      map_data <- map_data %>% filter(`ADMIN1 GEO_ID` == admin1_id) %>% select(GEO_ID,ADMIN1,ADMIN2,COB,geometry, POB15)
    }
    
    map_data <- map_data %>% mutate(
      var_num = case_when(
        GEO_ID %in% ZERO_POB_LIST ~ 3,
        is.na(COB) ~ 0,
        COB < 90 ~ 2,
        COB >= 90 ~ 1
      ),
      var_word = case_when(
        GEO_ID %in% ZERO_POB_LIST ~ lang_label_tls(LANG_TLS,"no_hab"),
        is.na(COB) ~ lang_label_tls(LANG_TLS,"no_data"),
        COB < 90 ~ "<90%",
        COB >= 90 ~ ">90%"
      )
    )
    
    
    if (pop_filter != lang_label("filter_all")) {
      if (pop_filter == lang_label("less_than_100000")) {
        map_data <- map_data %>% 
          mutate(
            var_num = case_when(
              POB15 >= 100000 ~ 4,
              T ~ var_num
            ),
            var_word = case_when(
              POB15 >= 100000 ~ lang_label("na"),
              T ~ var_word
            )
          )
      } else if (pop_filter == lang_label("greater_than_100000")) {
        map_data <- map_data %>%
          mutate(
            var_num = case_when(
              POB15 < 100000 ~ 4,
              T ~ var_num
            ),
            var_word = case_when(
              POB15 < 100000 ~ lang_label("na"),
              T ~ var_word
            )
          )
      }
    }
    
    
    
    
    pal_gradient <- colorNumeric(
      c("#666666","#92d050","#e8132b","#9bc2e6", "#111111"),
      domain = c(0,4)
    )
    legend_colors = c("#e8132b","#92d050")
    legend_values = c("<90%","≥90%")
    
    if (0 %in% map_data$var_num) {
      legend_colors = c("#666666",legend_colors)
      legend_values = c(lang_label_tls(LANG_TLS,"no_data"),legend_values)
    }
    
    if (length(ZERO_POB_LIST) > 0) {
      legend_colors = c(legend_colors,"#9bc2e6")
      legend_values = c(legend_values,lang_label_tls(LANG_TLS,"no_hab"))
    }
    
    if (4 %in% map_data$var_num) {
      legend_colors = c("#111111",legend_colors)
      legend_values = c(lang_label_tls(LANG_TLS,"na"),legend_values)
    }
    
    shape_label <- sprintf("<strong>%s</strong>, %s<br/>%s: %s",
                           map_data$ADMIN2,
                           map_data$ADMIN1,
                           lang_label_tls(LANG_TLS,"determinants_population_legend"),
                           as.character(map_data$COB)
                           
    ) %>% lapply(HTML)
    
    # MAPA
    map <- leaflet(map_data,options = leafletOptions(doubleClickZoom = T, attributionControl = F, zoomSnap=0.1, zoomDelta=0.1)) %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
      addPolygons(
        fillColor   = ~pal_gradient(var_num),
        fillOpacity = 0.7,
        dashArray   = "",
        weight      = 1,
        color       = "#333333",
        opacity     = 1,
        highlight = highlightOptions(
          weight = 2,
          color = "#333333",
          dashArray = "",
          fillOpacity = 1,
          bringToFront = TRUE),
        label = shape_label,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")
      ) %>% 
      addLegend(layerId = "map_title","topright",color = "white", opacity = 0,labels=HTML(paste0("<strong>",determinants_title_map(LANG_TLS,COUNTRY_NAME,YEAR_LIST,admin1,var_to_summarise),"</strong>"))) %>%
      addLegend(title = lang_label_tls(LANG_TLS,"determinants_population_legend"),colors = legend_colors,labels = legend_values, opacity = 0.5, position = 'topright')
  }
  
  return(map)
}



determinants_get_data_table <- function(LANG_TLS,CUT_OFFS,data,admin1_id) {

  data$risk_level <- get_risk_level(LANG_TLS,CUT_OFFS,"determinants_score",data$determinants_score, data$population_and_pfa_bool)
  
  data <- data %>% 
    select(`ADMIN1 GEO_ID`,
           ADMIN1,ADMIN2,determinants_score,risk_level, POB1, POB5, POB15,
           drinking_water_percent, drinking_water_score,
           sanitation_services_percent, sanitation_services_score) 
  
  if (admin1_id == 0) {
    data <- data %>% select(-`ADMIN1 GEO_ID`)
    colnames(data) <- c(lang_label_tls(LANG_TLS,"table_admin1_name"),lang_label_tls(LANG_TLS,"table_admin2_name"),lang_label_tls(LANG_TLS,"total_pr"),lang_label_tls(LANG_TLS,"risk_level"),
                        "POB1", "POB5", "POB15",
                        paste(lang_label_tls(LANG_TLS,"determinants_water_column"),"(%)"),
                        paste(lang_label_tls(LANG_TLS,"determinants_water_score")),
                        paste(lang_label_tls(LANG_TLS,"determinants_sanitation_column"),"(%)"),
                        paste(lang_label_tls(LANG_TLS,"determinants_sanitation_score"))
                        )
  } else {
    data <- data %>% filter(`ADMIN1 GEO_ID` == admin1_id) %>% select(-ADMIN1,-`ADMIN1 GEO_ID`)
    colnames(data) <- c(lang_label_tls(LANG_TLS,"table_admin2_name"),lang_label_tls(LANG_TLS,"total_pr"),lang_label_tls(LANG_TLS,"risk_level"),
                        "POB1", "POB5", "POB15",
                        paste(lang_label_tls(LANG_TLS,"determinants_water_column"),"(%)"),
                        paste(lang_label_tls(LANG_TLS,"determinants_water_score")),
                        paste(lang_label_tls(LANG_TLS,"determinants_sanitation_column"),"(%)"),
                        paste(lang_label_tls(LANG_TLS,"determinants_sanitation_score"))
                        )
  }
  
  spr_cob_colnames <- c(
    paste(lang_label_tls(LANG_TLS,"determinants_water_column"),"(%)"),
    paste(lang_label_tls(LANG_TLS,"determinants_sanitation_column"),"(%)")
    )
  
  datos_table <- data %>%
    datatable(
      rownames = F,
      extensions = 'Buttons',
      options = list(
        scrollX=TRUE, scrollCollapse=TRUE,
        language = list(
          info = paste0(lang_label_tls(LANG_TLS,"data_table_showing")," _START_ ",lang_label_tls(LANG_TLS,"data_table_to")," _END_ ",lang_label_tls(LANG_TLS,"data_table_of")," _TOTAL_ ",lang_label_tls(LANG_TLS,"data_table_rows")),
          paginate = list(previous = lang_label_tls(LANG_TLS,"data_table_prev"), `next` = lang_label_tls(LANG_TLS,"data_table_next"))
        ),
        searching = TRUE,fixedColumns = TRUE,autoWidth = FALSE,
        ordering = TRUE,scrollY = TRUE, scrollX = TRUE,pageLength = 8,
        columnDefs = list(list(className = 'dt-right', targets = 0:(ncol(data)-1))),
        dom = 'Brtip',
        buttons = list(
          list(extend = "copy",text = lang_label_tls(LANG_TLS,"button_copy")),
          list(extend='csv',filename=paste(lang_label_tls(LANG_TLS,"data"),lang_label_tls(LANG_TLS,"determinants_score"),admin1_id)),
          list(extend='excel', filename=paste(lang_label_tls(LANG_TLS,"data"),lang_label_tls(LANG_TLS,"determinants_score"),admin1_id))
        ),
        class = "display"
      )
    ) %>% formatStyle(
      lang_label_tls(LANG_TLS,"risk_level"),
      backgroundColor = styleEqual(
        c(lang_label_tls(LANG_TLS,"LR"),lang_label_tls(LANG_TLS,"MR"),
          lang_label_tls(LANG_TLS,"HR"),lang_label_tls(LANG_TLS,"VHR")),
        c("rgba(146, 208, 80, 0.7)","rgba(254, 192, 0, 0.7)",
          "rgba(232, 19, 43, 0.7)","rgba(146, 0, 0, 0.7)"))
    ) %>% formatStyle(
      c(lang_label_tls(LANG_TLS,"total_pr"),
        lang_label_tls(LANG_TLS,"determinants_water_score"),
        lang_label_tls(LANG_TLS,"determinants_sanitation_score")
        ),
      backgroundColor = "#e3e3e3"
    ) %>% formatStyle(
      spr_cob_colnames,
      color = styleInterval(c(100),c("black","#0097e6"))
    )
  
  return(datos_table)
}
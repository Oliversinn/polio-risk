# AUTHORSHIP ----

# Pan American Health Organization
# Author: Oliver Mazariegos
# Última fecha de modificación: 2023-11-06
# R 4.3.1

determinants_title_map <- function(LANG_TLS,COUNTRY_NAME,YEAR_LIST,admin1,var) {
  var_text <- case_when(
    var == "determinants_score" ~ paste0(lang_label_tls(LANG_TLS,"determinants_title_map_total_pr")," ",admin1_transform(LANG_TLS,COUNTRY_NAME,admin1)," (",YEAR_LIST[5],")"),
  )
  return(var_text)
}


determinants_plot_map_data <- function(LANG_TLS,COUNTRY_NAME,YEAR_LIST,ZERO_POB_LIST,CUT_OFFS,map_data,data,var_to_summarise,admin1,admin1_id,admin1_geo_id_df) {

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
      map_data <- map_data %>% select(GEO_ID,ADMIN1,ADMIN2,determinants_score,risk_level,geometry)
    } else {
      map_data <- map_data %>% filter(`ADMIN1 GEO_ID` == admin1_id) %>% select(GEO_ID,ADMIN1,ADMIN2,determinants_score,risk_level,geometry)
    }
    
    map_data <- map_data %>% mutate(
      risk_level_num = case_when(
        is.na(risk_level) ~ 0,
        risk_level == lang_label_tls(LANG_TLS,"LR") ~ 1,
        risk_level == lang_label_tls(LANG_TLS,"MR") ~ 2,
        risk_level == lang_label_tls(LANG_TLS,"HR") ~ 3,
        risk_level == lang_label_tls(LANG_TLS,"VHR") ~ 4,
        risk_level == "NO_HAB" ~ 5
      ),
      risk_level_word = case_when(
        is.na(risk_level) ~ lang_label_tls(LANG_TLS,"no_data"),
        risk_level == "NO_HAB" ~ lang_label_tls(LANG_TLS,"no_hab"),
        T ~ risk_level
      )
    )
    
    pal_gradient <- colorNumeric(
      c("#666666","#92d050","#fec000","#e8132b","#920000","#9bc2e6"),
      domain = c(0,5)
    )
    
    legend_colors = c("#920000","#e8132b","#fec000","#92d050")
    legend_values = c(lang_label_tls(LANG_TLS,"cut_offs_VHR"),
                      lang_label_tls(LANG_TLS,"cut_offs_HR"),
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
  
  return(map)
}
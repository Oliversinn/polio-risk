# AUTHORSHIP ----

# Pan American Health Organization
# Author: Oliver Mazariegos & Luis Quezada
# Última fecha de modificación: 2023-11-06
# R 4.3.1


cal_title_map <- function(LANG_TLS,COUNTRY_NAME,YEAR_LIST,admin1,var) {
  YEAR_1=YEAR_LIST[1];YEAR_2=YEAR_LIST[2];YEAR_3=YEAR_LIST[3];YEAR_4=YEAR_LIST[4];YEAR_5=YEAR_LIST[5];
  var_text <- case_when(
    var == "surveillance_score" ~ paste0(lang_label_tls(LANG_TLS,"surv_title_map_total_pr")," ",admin1_transform(LANG_TLS,COUNTRY_NAME,admin1)," (",YEAR_5,")"),
    var == "compliant_units_percent" ~ paste0(lang_label_tls(LANG_TLS,"surveillance_title_map_reporting_units")," ",admin1_transform(LANG_TLS,COUNTRY_NAME,admin1)," (",YEAR_5,")"),
    var == "pfa_rate" ~ paste0(lang_label_tls(LANG_TLS,"surveillance_title_map_pfa_rate")," ",admin1_transform(LANG_TLS,COUNTRY_NAME,admin1)," (",YEAR_5,")"),
    var == "pfa_notified_percent" ~ paste0(lang_label_tls(LANG_TLS,"surveillance_title_map_pfa_notification")," ",admin1_transform(LANG_TLS,COUNTRY_NAME,admin1)," (",YEAR_5,")"),
    var == "pfa_investigated_percent" ~ paste0(lang_label_tls(LANG_TLS,"surveillance_title_map_pfa_investigated")," ",admin1_transform(LANG_TLS,COUNTRY_NAME,admin1)," (",YEAR_5,")"),
    var == "suitable_samples_percent" ~ paste0(lang_label_tls(LANG_TLS,"surveillance_title_map_suitable_samples")," ",admin1_transform(LANG_TLS,COUNTRY_NAME,admin1)," (",YEAR_5,")"),
    var == "followups_percent" ~ paste0(lang_label_tls(LANG_TLS,"surveillance_title_map_followups")," ",admin1_transform(LANG_TLS,COUNTRY_NAME,admin1)," (",YEAR_5,")"),
    var == "active_search" ~ paste0(lang_label_tls(LANG_TLS,"surveillance_title_map_active_search")," ",admin1_transform(LANG_TLS,COUNTRY_NAME,admin1)," (",YEAR_5,")")
  )
  return(var_text)
}


cal_plot_map_data <- function(LANG_TLS,COUNTRY_NAME,YEAR_LIST,ZERO_POB_LIST,CUT_OFFS,map_data,data,var_to_summarise,admin1,admin1_id,admin1_geo_id_df, pop_filter = lang_label("filter_all"),risk_filter=toupper(lang_label("filter_all"))) {

  indicator <- "surveillance_score"
  data <- data %>% select(-ADMIN1,-ADMIN2)
  if ("ADMIN1 GEO_ID" %in% colnames(data)) {
    data <- data %>% select(-`ADMIN1 GEO_ID`)
  }
  map_data <- full_join(map_data,data,by="GEO_ID")

  map_data$`ADMIN1 GEO_ID`[is.na(map_data$`ADMIN1 GEO_ID`) & map_data$ADMIN1 == admin1] <- admin1_geo_id_df$`ADMIN1 GEO_ID`[admin1_geo_id_df$ADMIN1 == admin1]
  
  map_data$risk_level <- get_risk_level(LANG_TLS,CUT_OFFS,indicator,map_data$surveillance_score, map_data$population_and_pfa_bool)
  
    
  if (var_to_summarise == "surveillance_score") {
    map_data$risk_level <- get_risk_level(LANG_TLS,CUT_OFFS,indicator,map_data$surveillance_score, map_data$population_and_pfa_bool)
    map_data$risk_level[map_data$GEO_ID %in% ZERO_POB_LIST] <- "NO_HAB"
    
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
    
    if (risk_filter != toupper(lang_label("filter_all"))) {
      map_data$risk_level[map_data$risk_level != risk_filter] <- lang_label("na")
    }
    
    if (admin1_id == 0) {
      map_data <- map_data %>% select(GEO_ID,ADMIN1,ADMIN2,surveillance_score,risk_level,geometry)
    } else {
      map_data <- map_data %>% filter(`ADMIN1 GEO_ID` == admin1_id) %>% select(GEO_ID,ADMIN1,ADMIN2,surveillance_score,risk_level,geometry)
    }
    
    map_data <- map_data %>% mutate(
      risk_level_num = case_when(
        is.na(risk_level) ~ 0,
        risk_level == lang_label_tls(LANG_TLS,"LR") ~ 1,
        risk_level == lang_label_tls(LANG_TLS,"MR") ~ 2,
        risk_level == lang_label_tls(LANG_TLS,"HR") ~ 3,
        risk_level == lang_label_tls(LANG_TLS,"VHR") ~ 4,
        risk_level == "NO_HAB" ~ 5,
        risk_level == lang_label("na") ~ 6
      ),
      risk_level_word = case_when(
        is.na(risk_level) ~ lang_label_tls(LANG_TLS,"no_data"),
        risk_level == "NO_HAB" ~ lang_label_tls(LANG_TLS,"no_hab"),
        T ~ risk_level
      )
    )
    
    pal_gradient <- colorNumeric(
      c("#666666","#92d050","#fec000","#e8132b","#920000","#9bc2e6","#111111"),
      domain = c(0,6)
    )
    
    if (risk_filter == lang_label("VHR")) {
      legend_colors = c("#920000")
      legend_values = c(lang_label_tls(LANG_TLS,"cut_offs_VHR"))
      
    } else if (risk_filter == lang_label("VHR")) {
      legend_colors = c("#e8132b")
      legend_values = c(lang_label_tls(LANG_TLS,"cut_offs_HR"))
      
    } else if (risk_filter == lang_label("MR")) {
      legend_colors = c("#fec000")
      legend_values = c(lang_label_tls(LANG_TLS,"cut_offs_MR"))
      
    } else if (risk_filter == lang_label("LR")) {
      legend_colors = c("#92d050")
      legend_values = c(lang_label_tls(LANG_TLS,"cut_offs_LR"))
      
    } else {
      legend_colors = c("#920000","#e8132b","#fec000","#92d050")
      legend_values = c(lang_label_tls(LANG_TLS,"cut_offs_VHR"),
                        lang_label_tls(LANG_TLS,"cut_offs_HR"),
                        lang_label_tls(LANG_TLS,"cut_offs_MR"),
                        lang_label_tls(LANG_TLS,"cut_offs_LR"))
    }
    
    if (0 %in% map_data$risk_level_num) {
      legend_colors = c("#666666",legend_colors)
      legend_values = c(lang_label_tls(LANG_TLS,"no_data"),legend_values)
    }
    
    if (length(ZERO_POB_LIST) > 0) {
      legend_colors = c(legend_colors,"#9bc2e6")
      legend_values = c(legend_values,lang_label_tls(LANG_TLS,"no_hab"))
    }
    
    if (6 %in% map_data$risk_level_num) {
      legend_colors = c(legend_colors,"#111111")
      legend_values = c(legend_values,lang_label_tls(LANG_TLS,"na"))
    }
    
    shape_label <- sprintf("<strong>%s</strong>, %s<br/>%s: %s<br/>%s: %s",
                           map_data$ADMIN2,
                           map_data$ADMIN1,
                           lang_label_tls(LANG_TLS,"risk_points"),
                           map_data$surveillance_score,
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
      addLegend(layerId = "map_title","topright",color = "white", opacity = 0,labels=HTML(paste0("<strong>",cal_title_map(LANG_TLS,COUNTRY_NAME,YEAR_LIST,admin1,var_to_summarise),"</strong>"))) %>%
      addLegend(title = lang_label_tls(LANG_TLS,"legend_risk_class"),colors = legend_colors,labels = legend_values, opacity = 0.5, position = 'topright')
    
  } else if (var_to_summarise == "compliant_units_percent") {
    # % de casos o muestras
    map_data <- map_data %>% rename("var"=var_to_summarise)
    map_data$var <- round(map_data$var,1)
    
    if (var_to_summarise == "compliant_units_percent") {
      legend_title = lang_label_tls(LANG_TLS,"surveillance_prop_reporting_units")
    } 
    
    if (admin1_id == 0) {
      map_data <- map_data %>% select(GEO_ID,ADMIN1,ADMIN2,var,geometry, POB15, risk_level)
    } else {
      map_data <- map_data %>% filter(`ADMIN1 GEO_ID` == admin1_id) %>% select(GEO_ID,ADMIN1,ADMIN2,var,geometry, POB15, risk_level)
    }
    
    map_data <- map_data %>% mutate(
      var_level_num = case_when(
        GEO_ID %in% ZERO_POB_LIST ~ 3,
        is.na(var) ~ 0,
        var < 80 ~ 1,
        var >= 80 ~ 2
      )
    )
    
    if (pop_filter != lang_label("filter_all")) {
      if (pop_filter == lang_label("less_than_100000")) {
        map_data <- map_data %>% 
          mutate(
            var_level_num = case_when(
              POB15 >= 100000 ~ 4,
              T ~ var_level_num
            )
          )
      } else if (pop_filter == lang_label("greater_than_100000")) {
        map_data <- map_data %>%
          mutate(
            var_level_num = case_when(
              POB15 < 100000 ~ 4,
              T ~ var_level_num
            )
          )
      }
    }
    
    if (risk_filter != toupper(lang_label("filter_all"))) {
      map_data$var_level_num[map_data$risk_level != risk_filter] <- 4
    }
    
    pal_gradient <- colorNumeric(
      c("#666666","#e8132b","#92d050","#9bc2e6", "#111111"),
      domain = c(0,4)
    )
    legend_colors = c("#e8132b","#92d050")
    legend_values = c("< 80%","≥ 80%")
    
    if (0 %in% map_data$var_level_num) {
      legend_colors = c("#666666",legend_colors)
      legend_values = c(lang_label_tls(LANG_TLS,"no_data"),legend_values)
    }
    
    if (length(ZERO_POB_LIST) > 0) {
      legend_colors = c(legend_colors,"#9bc2e6")
      legend_values = c(legend_values,lang_label_tls(LANG_TLS,"no_hab"))
    }
    
    if (4 %in% map_data$cob_level_num) {
      legend_colors = c(legend_colors, "#0097e6")
      legend_values = c(legend_values,lang_label_tls(LANG_TLS,"over_100"))
      
    }
    
    shape_label <- sprintf("<strong>%s</strong>, %s<br/>%s: %s%s",
                           map_data$ADMIN2,
                           map_data$ADMIN1,
                           lang_label_tls(LANG_TLS,"proportion"),
                           map_data$var,
                           "%"
    ) %>% lapply(HTML)
    
    # MAPA
    map <- leaflet(map_data,options = leafletOptions(doubleClickZoom = T, attributionControl = F, zoomSnap=0.1, zoomDelta=0.1)) %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
      addPolygons(
        fillColor   = ~pal_gradient(var_level_num),
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
      addLegend(layerId = "map_title","topright",color = "white", opacity = 0,labels=HTML(paste0("<strong>",cal_title_map(LANG_TLS,COUNTRY_NAME,YEAR_LIST,admin1,var_to_summarise),"</strong>"))) %>%
      addLegend(title = legend_title,colors = legend_colors,labels = legend_values, opacity = 0.5, position = 'topright')
  
    } else if (var_to_summarise == "pfa_rate") {
    
    map_data <- map_data %>% rename("var"=var_to_summarise)
    legend_title = lang_label_tls(LANG_TLS,"surveillance_pfa_rate")
    
    if (admin1_id == 0) {
      map_data <- map_data %>% select(GEO_ID,ADMIN1,ADMIN2,population_and_pfa_bool, var,geometry, POB15, risk_level)
    } else {
      map_data <- map_data %>% filter(`ADMIN1 GEO_ID` == admin1_id) %>% select(GEO_ID,ADMIN1,ADMIN2,var,geometry, population_and_pfa_bool, POB15, risk_level)
    }
    
    map_data <- map_data %>% mutate(
      var_level_num = case_when(
        !population_and_pfa_bool ~ 4,
        GEO_ID %in% ZERO_POB_LIST ~ 3,
        is.na(var) ~ 0,
        var < 1 ~ 1,
        var >= 1 ~ 2
      )
    )
    
    if (pop_filter != lang_label("filter_all")) {
      if (pop_filter == lang_label("less_than_100000")) {
        map_data <- map_data %>% 
          mutate(
            var_level_num = case_when(
              POB15 >= 100000 ~ 4,
              T ~ var_level_num
            )
          )
      } else if (pop_filter == lang_label("greater_than_100000")) {
        map_data <- map_data %>%
          mutate(
            var_level_num = case_when(
              POB15 < 100000 ~ 4,
              T ~ var_level_num
            )
          )
      }
    }
    
    if (risk_filter != toupper(lang_label("filter_all"))) {
      map_data$var_level_num[map_data$risk_level != risk_filter] <- 4
    }
    
    pal_gradient <- colorNumeric(
      c("#666666","#e8132b","#92d050","#9bc2e6", "#111111"),
      domain = c(0,4)
    )
    legend_colors = c("#e8132b","#92d050")
    legend_values = c("< 1","≥ 1")
    
    if (0 %in% map_data$var_level_num) {
      legend_colors = c("#666666",legend_colors)
      legend_values = c(lang_label_tls(LANG_TLS,"no_data"),legend_values)
    }
    
    if (length(ZERO_POB_LIST) > 0) {
      legend_colors = c(legend_colors,"#9bc2e6")
      legend_values = c(legend_values,lang_label_tls(LANG_TLS,"no_hab"))
    }
    
    if (FALSE %in% map_data$population_and_pfa_bool) {
      legend_colors = c(legend_colors,"#111111")
      legend_values = c(legend_values,lang_label_tls(LANG_TLS,"na"))
    }
    
    shape_label <- sprintf("<strong>%s</strong>, %s<br/>%s: %s",
                           map_data$ADMIN2,
                           map_data$ADMIN1,
                           lang_label_tls(LANG_TLS,"rate"),
                           map_data$var
    ) %>% lapply(HTML)
    
    # MAPA
    map <- leaflet(map_data,options = leafletOptions(doubleClickZoom = T, attributionControl = F, zoomSnap=0.1, zoomDelta=0.1)) %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
      addPolygons(
        fillColor   = ~pal_gradient(var_level_num),
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
      addLegend(layerId = "map_title","topright",color = "white", opacity = 0,labels=HTML(paste0("<strong>",cal_title_map(LANG_TLS,COUNTRY_NAME,YEAR_LIST,admin1,var_to_summarise),"</strong>"))) %>%
      addLegend(title = legend_title,colors = legend_colors,labels = legend_values, opacity = 0.5, position = 'topright')
    
    
    } else if (var_to_summarise %in% c("pfa_notified_percent", "pfa_investigated_percent", 
                                       "suitable_samples_percent", "followups_percent")) {
      # % de casos o muestras
      map_data <- map_data %>% rename("var"=var_to_summarise)
      map_data$var <- round(map_data$var,1)
      
      legend_title = lang_label_tls(LANG_TLS,"surveillance_prop_pfa_cases")
      
      if (admin1_id == 0) {
        map_data <- map_data %>% select(GEO_ID,ADMIN1,ADMIN2,var,geometry, population_and_pfa_bool, POB15, risk_level)
      } else {
        map_data <- map_data %>% filter(`ADMIN1 GEO_ID` == admin1_id) %>% select(GEO_ID,ADMIN1,ADMIN2,var,geometry, population_and_pfa_bool, POB15, risk_level)
      }
      
      map_data <- map_data %>% mutate(
        var_level_num = case_when(
          !population_and_pfa_bool ~ 4,
          GEO_ID %in% ZERO_POB_LIST ~ 3,
          is.na(var) ~ 0,
          var < 80 ~ 1,
          var >= 80 ~ 2
        )
      )
      
      if (pop_filter != lang_label("filter_all")) {
        if (pop_filter == lang_label("less_than_100000")) {
          map_data <- map_data %>% 
            mutate(
              var_level_num = case_when(
                POB15 >= 100000 ~ 4,
                T ~ var_level_num
              )
            )
        } else if (pop_filter == lang_label("greater_than_100000")) {
          map_data <- map_data %>%
            mutate(
              var_level_num = case_when(
                POB15 < 100000 ~ 4,
                T ~ var_level_num
              )
            )
        }
      }
      
      if (risk_filter != toupper(lang_label("filter_all"))) {
        map_data$var_level_num[map_data$risk_level != risk_filter] <- 4
      }
      
      pal_gradient <- colorNumeric(
        c("#666666","#e8132b","#92d050","#9bc2e6", "#111111"),
        domain = c(0,4)
      )
      legend_colors = c("#e8132b","#92d050")
      legend_values = c("< 80%","≥ 80%")
      
      if (0 %in% map_data$var_level_num) {
        legend_colors = c("#666666",legend_colors)
        legend_values = c(lang_label_tls(LANG_TLS,"no_data"),legend_values)
      }
      
      if (length(ZERO_POB_LIST) > 0) {
        legend_colors = c(legend_colors,"#9bc2e6")
        legend_values = c(legend_values,lang_label_tls(LANG_TLS,"no_hab"))
      }
      
      if (FALSE %in% map_data$population_and_pfa_bool) {
        legend_colors = c(legend_colors,"#111111")
        legend_values = c(legend_values,lang_label_tls(LANG_TLS,"na"))
      }
      
      shape_label <- sprintf("<strong>%s</strong>, %s<br/>%s: %s%s",
                             map_data$ADMIN2,
                             map_data$ADMIN1,
                             lang_label_tls(LANG_TLS,"proportion"),
                             map_data$var,
                             "%"
      ) %>% lapply(HTML)
      
      # MAPA
      map <- leaflet(map_data,options = leafletOptions(doubleClickZoom = T, attributionControl = F, zoomSnap=0.1, zoomDelta=0.1)) %>%
        addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
        addPolygons(
          fillColor   = ~pal_gradient(var_level_num),
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
        addLegend(layerId = "map_title","topright",color = "white", opacity = 0,labels=HTML(paste0("<strong>",cal_title_map(LANG_TLS,COUNTRY_NAME,YEAR_LIST,admin1,var_to_summarise),"</strong>"))) %>%
        addLegend(title = legend_title,colors = legend_colors,labels = legend_values, opacity = 0.5, position = 'topright')
      
    } else if (var_to_summarise == "active_search") {
      
      map_data <- map_data %>% rename("var"=var_to_summarise)

      legend_title = lang_label_tls(LANG_TLS,"surveillance_active_search")
      
      if (admin1_id == 0) {
        map_data <- map_data %>% select(GEO_ID,ADMIN1,ADMIN2,var,geometry, population_and_pfa_bool, POB15, risk_level)
      } else {
        map_data <- map_data %>% filter(`ADMIN1 GEO_ID` == admin1_id) %>% select(GEO_ID,ADMIN1,ADMIN2,var,geometry, population_and_pfa_bool, POB15, risk_level)
      }
      
      map_data <- map_data %>% mutate(
        var_num = case_when(
          population_and_pfa_bool ~ 4,
          GEO_ID %in% ZERO_POB_LIST ~ 3,
          is.na(var) ~ 0,
          var == lang_label_tls(LANG_TLS,"yes") | var == lang_label_tls(LANG_TLS,"yes_upper") ~ 1,
          var == lang_label_tls(LANG_TLS,"no") | var == lang_label_tls(LANG_TLS,"no_upper") ~ 2
        ),
        var_word = case_when(
          population_and_pfa_bool ~ lang_label_tls(LANG_TLS,"na"),
          GEO_ID %in% ZERO_POB_LIST ~ lang_label_tls(LANG_TLS,"no_hab"),
          is.na(var) ~ lang_label_tls(LANG_TLS,"no_data"),
          var == lang_label_tls(LANG_TLS,"yes") | var == lang_label_tls(LANG_TLS,"yes_upper") ~ lang_label_tls(LANG_TLS,"yes"),
          var == lang_label_tls(LANG_TLS,"no") | var == lang_label_tls(LANG_TLS,"no_upper") ~ lang_label_tls(LANG_TLS,"no")
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
      
      if (risk_filter != toupper(lang_label("filter_all"))) {
        map_data$var_num[map_data$risk_level != risk_filter] <- 4
        map_data$var_word[map_data$risk_level != risk_filter] <- lang_label("na")
      }
      
      pal_gradient <- colorNumeric(
        c("#666666","#92d050","#e8132b","#9bc2e6", "#111111"),
        domain = c(0,4)
      )
      legend_colors = c("#e8132b","#92d050")
      legend_values = c(lang_label_tls(LANG_TLS,"no"),lang_label_tls(LANG_TLS,"yes"))
      
      if (0 %in% map_data$var_num) {
        legend_colors = c("#666666",legend_colors)
        legend_values = c(lang_label_tls(LANG_TLS,"no_data"),legend_values)
      }
      
      if (length(ZERO_POB_LIST) > 0) {
        legend_colors = c(legend_colors,"#9bc2e6")
        legend_values = c(legend_values,lang_label_tls(LANG_TLS,"no_hab"))
      }
      
      if (TRUE %in% map_data$population_and_pfa_bool) {
        legend_colors = c(legend_colors,"#111111")
        legend_values = c(legend_values,lang_label_tls(LANG_TLS,"na"))
      }
      
      shape_label <- sprintf("<strong>%s</strong>, %s<br/>%s: %s",
                             map_data$ADMIN2,
                             map_data$ADMIN1,
                             lang_label_tls(LANG_TLS,"surveillance_active_search"),
                             tolower(map_data$var_word)
      ) %>% lapply(HTML)
      
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
        addLegend(layerId = "map_title","topright",color = "white", opacity = 0,labels=HTML(paste0("<strong>",cal_title_map(LANG_TLS,COUNTRY_NAME,YEAR_LIST,admin1,var_to_summarise),"</strong>"))) %>%
        addLegend(title = lang_label_tls(LANG_TLS,"rap_pres_team"),colors = legend_colors,labels = legend_values, opacity = 0.5, position = 'topright')
      
    }
  
  return(map)
  
}

cal_get_data_table <- function(LANG_TLS,CUT_OFFS,data,admin1_id,pop_filter,risk_filter=toupper(lang_label("filter_all"))) {
  
  data$risk_level <- get_risk_level(LANG_TLS,CUT_OFFS,"surveillance_score",data$surveillance_score, data$population_and_pfa_bool)
  data <- data %>% 
    select(
      `ADMIN1 GEO_ID`, ADMIN1, ADMIN2, surveillance_score, risk_level, POB1, POB5,
      POB15, compliant_units_percent, compliant_units_score,
      pfa_rate, pfa_rate_score, pfa_notified_percent, pfa_notified_score,
      pfa_investigated_percent, pfa_investigated_score, suitable_samples_percent,
      suitable_samples_score, followups_percent, followups_score, active_search,
      active_search_score, population_and_pfa_bool
      ) %>% 
    mutate(
      compliant_units_percent = round(compliant_units_percent, 0),
      pfa_rate = case_when(
        population_and_pfa_bool ~ cFormat(pfa_rate, 2),
        !population_and_pfa_bool ~ lang_label("na")
      ),
      pfa_rate_score = case_when(
        population_and_pfa_bool ~ cFormat(pfa_rate_score, 0),
        !population_and_pfa_bool ~ lang_label("na")
      ),
      pfa_notified_percent = case_when(
        population_and_pfa_bool ~ cFormat(pfa_notified_percent, 0),
        !population_and_pfa_bool ~ lang_label("na")
      ),
      pfa_notified_score = case_when(
        population_and_pfa_bool ~ cFormat(pfa_notified_score, 0),
        !population_and_pfa_bool ~ lang_label("na")
      ),
      pfa_investigated_percent = case_when(
        population_and_pfa_bool ~ cFormat(pfa_investigated_percent, 0),
        !population_and_pfa_bool ~ lang_label("na")
      ),
      pfa_investigated_score = case_when(
        population_and_pfa_bool ~ cFormat(pfa_investigated_score, 0),
        !population_and_pfa_bool ~ lang_label("na")
      ),
      suitable_samples_percent = case_when(
        population_and_pfa_bool ~ cFormat(suitable_samples_percent, 0),
        !population_and_pfa_bool ~ lang_label("na")
      ),
      suitable_samples_score = case_when(
        population_and_pfa_bool ~ cFormat(suitable_samples_score, 0),
        !population_and_pfa_bool ~ lang_label("na")
      ),
      followups_percent = case_when(
        population_and_pfa_bool ~ cFormat(followups_percent, 0),
        !population_and_pfa_bool ~ lang_label("na")
      ),
      followups_score = case_when(
        population_and_pfa_bool ~ cFormat(followups_score, 0),
        !population_and_pfa_bool ~ lang_label("na")
      ),
      active_search = case_when(
        !population_and_pfa_bool ~ case_when(
          active_search == lang_label("yes") ~ lang_label("yes"),
          active_search == lang_label("yes_upper") ~ lang_label("yes"),
          active_search == lang_label("no") ~ lang_label("no"),
          active_search == lang_label("no_upper") ~ lang_label("no"),
          # population_and_pfa_bool == lang_label("yes") ~ lang_label("na"),
          # is.na(active_search) ~ lang_label("no_data")
        ),
        population_and_pfa_bool ~ lang_label("na")
      ),
      active_search_score = case_when(
        !population_and_pfa_bool ~ cFormat(active_search_score, 0),
        population_and_pfa_bool ~ lang_label("na")
      ),
    )
  if (pop_filter != lang_label("filter_all")) {
    if (pop_filter == lang_label("less_than_100000")) {
      data <- data %>% filter(POB15 < 100000)
    } else if (pop_filter == lang_label("greater_than_100000")) {
      data <- data %>% filter(POB15 >= 100000)
    }
  }
  
  data <- data %>% 
    select(-population_and_pfa_bool) %>% 
    mutate(
      POB1 = cFormat(POB1,0),
      POB5 = cFormat(POB5,0),
      POB15 = cFormat(POB15,0)
    )
  
  if (risk_filter != toupper(lang_label("filter_all"))) {
    data <- data %>% filter(risk_level == risk_filter)
  }
  
  if (admin1_id == 0) {
    data <- data %>% select(-`ADMIN1 GEO_ID`)
    colnames(data) <- c(
      lang_label_tls(LANG_TLS,"table_admin1_name"),lang_label_tls(LANG_TLS,"table_admin2_name"),
      lang_label_tls(LANG_TLS,"total_pr"),lang_label_tls(LANG_TLS,"risk_level"),
      "POB1", "POB5", "POB15", 
      lang_label_tls(LANG_TLS,"surveillance_title_map_reporting_units"),lang_label_tls(LANG_TLS,"surveillance_reporting_units_score"),
      lang_label_tls(LANG_TLS,"surveillance_pfa_rate"),lang_label_tls(LANG_TLS,"surveillance_pfa_rate_score"),
      lang_label_tls(LANG_TLS,"surveillance_title_map_pfa_notification"),lang_label_tls(LANG_TLS,"surveillance_pfa_notification_score"),
      lang_label_tls(LANG_TLS,"surveillance_title_map_pfa_investigated"),lang_label_tls(LANG_TLS,"surveillance_pfa_investigated_score"),
      lang_label_tls(LANG_TLS,"surveillance_title_map_suitable_samples"),lang_label_tls(LANG_TLS,"surveillance_suitable_samples_score"),
      lang_label_tls(LANG_TLS,"surveillance_title_map_followups"),lang_label_tls(LANG_TLS,"surveillance_followups_score"),
      lang_label_tls(LANG_TLS,"surveillance_title_map_active_search"),lang_label_tls(LANG_TLS,"surveillance_active_search_score"),
      "population_and_pfa_bool"
    )
  } else {
    data <- data %>% filter(`ADMIN1 GEO_ID` == admin1_id) %>% select(-ADMIN1,-`ADMIN1 GEO_ID`)
    colnames(data) <- c(
      lang_label_tls(LANG_TLS,"table_admin2_name"),
      lang_label_tls(LANG_TLS,"total_pr"),lang_label_tls(LANG_TLS,"risk_level"),
      "POB1", "POB5", "POB15", 
      lang_label_tls(LANG_TLS,"surveillance_title_map_reporting_units"),lang_label_tls(LANG_TLS,"surveillance_reporting_units_score"),
      lang_label_tls(LANG_TLS,"surveillance_pfa_rate"),lang_label_tls(LANG_TLS,"surveillance_pfa_rate_score"),
      lang_label_tls(LANG_TLS,"surveillance_title_map_pfa_notification"),lang_label_tls(LANG_TLS,"surveillance_pfa_notification_score"),
      lang_label_tls(LANG_TLS,"surveillance_title_map_pfa_investigated"),lang_label_tls(LANG_TLS,"surveillance_pfa_investigated_score"),
      lang_label_tls(LANG_TLS,"surveillance_title_map_suitable_samples"),lang_label_tls(LANG_TLS,"surveillance_suitable_samples_score"),
      lang_label_tls(LANG_TLS,"surveillance_title_map_followups"),lang_label_tls(LANG_TLS,"surveillance_followups_score"),
      lang_label_tls(LANG_TLS,"surveillance_title_map_active_search"),lang_label_tls(LANG_TLS,"surveillance_active_search_score"),
      "population_and_pfa_bool"
    )
  }
  
  
  
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
        ordering = TRUE,scrollY = TRUE,pageLength = 8,
        columnDefs = list(list(className = 'dt-right', targets = 0:(ncol(data)-1))),
        dom = 'Brtip',
        buttons = list(
          list(extend = "copy",text = lang_label_tls(LANG_TLS,"button_copy")),
          list(extend='csv',filename=paste(lang_label_tls(LANG_TLS,"data"),lang_label_tls(LANG_TLS,"SURV_QUAL"),admin1_id)),
          list(extend='excel', filename=paste(lang_label_tls(LANG_TLS,"data"),lang_label_tls(LANG_TLS,"SURV_QUAL"),admin1_id))
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
      c(
        lang_label_tls(LANG_TLS,"total_pr"),
        lang_label_tls(LANG_TLS,"surveillance_reporting_units_score"),
        lang_label_tls(LANG_TLS,"surveillance_pfa_rate_score"),
        lang_label_tls(LANG_TLS,"surveillance_pfa_notification_score"),
        lang_label_tls(LANG_TLS,"surveillance_pfa_investigated_score"),
        lang_label_tls(LANG_TLS,"surveillance_suitable_samples_score"),
        lang_label_tls(LANG_TLS,"surveillance_followups_score"),
        lang_label_tls(LANG_TLS,"surveillance_active_search_score")
      ),
      backgroundColor = "#e3e3e3"
    )
  
  return(datos_table)
}


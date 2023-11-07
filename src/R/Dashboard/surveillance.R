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
    var == "tasa_casos" ~ paste0(lang_label_tls(LANG_TLS,"surv_rate_novac")," ",admin1_transform(LANG_TLS,COUNTRY_NAME,admin1)," (",YEAR_5,")"),
    var == "p_casos_inv" ~ paste0(lang_label_tls(LANG_TLS,"surv_adeq_inv")," ",admin1_transform(LANG_TLS,COUNTRY_NAME,admin1)," (",YEAR_5,")"),
    var == "p_casos_muestra" ~ paste0(lang_label_tls(LANG_TLS,"surv_adeq_sample")," ",admin1_transform(LANG_TLS,COUNTRY_NAME,admin1)," (",YEAR_5,")"),
    var == "p_muestras_lab" ~ paste0(lang_label_tls(LANG_TLS,"surv_timely_lab")," ",admin1_transform(LANG_TLS,COUNTRY_NAME,admin1)," (",YEAR_5,")")
  )
  return(var_text)
}


cal_plot_map_data <- function(LANG_TLS,COUNTRY_NAME,YEAR_LIST,ZERO_POB_LIST,CUT_OFFS,map_data,data,var_to_summarise,admin1,admin1_id,admin1_geo_id_df) {

  indicator <- "surveillance_score"
  data <- data %>% select(-ADMIN1,-ADMIN2)
  if ("ADMIN1 GEO_ID" %in% colnames(data)) {
    data <- data %>% select(-`ADMIN1 GEO_ID`)
  }
  map_data <- full_join(map_data,data,by="GEO_ID")

  map_data$`ADMIN1 GEO_ID`[is.na(map_data$`ADMIN1 GEO_ID`) & map_data$ADMIN1 == admin1] <- admin1_geo_id_df$`ADMIN1 GEO_ID`[admin1_geo_id_df$ADMIN1 == admin1]
  
  if (var_to_summarise != "tasa_casos") {
    
    if (var_to_summarise == "surveillance_score") {
      map_data$risk_level <- get_risk_level(LANG_TLS,CUT_OFFS,indicator,map_data$surveillance_score, map_data$population_and_pfa_bool)
      map_data$risk_level[map_data$GEO_ID %in% ZERO_POB_LIST] <- "NO_HAB"
      
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
        map_data <- map_data %>% select(GEO_ID,ADMIN1,ADMIN2,var,geometry)
      } else {
        map_data <- map_data %>% filter(`ADMIN1 GEO_ID` == admin1_id) %>% select(GEO_ID,ADMIN1,ADMIN2,var,geometry)
      }
      
      map_data <- map_data %>% mutate(
        var_level_num = case_when(
          GEO_ID %in% ZERO_POB_LIST ~ 3,
          is.na(var) ~ 0,
          var < 80 ~ 1,
          var >= 80 ~ 2
        )
      )
      
      pal_gradient <- colorNumeric(
        c("#666666","#e8132b","#92d050","#9bc2e6"),
        domain = c(0,3)
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
        map_data <- map_data %>% select(GEO_ID,ADMIN1,ADMIN2,population_and_pfa_bool, var,geometry)
      } else {
        map_data <- map_data %>% filter(`ADMIN1 GEO_ID` == admin1_id) %>% select(GEO_ID,ADMIN1,ADMIN2,var,geometry, population_and_pfa_bool)
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
        
        if (var_to_summarise == "pfa_notified_percent") {
          legend_title = lang_label_tls(LANG_TLS,"surveillance_prop_pfa_cases")
        } else if (var_to_summarise == "pfa_investigated_percent") {
          legend_title = lang_label_tls(LANG_TLS,"surveillance_prop_pfa_cases")
        }
        
        if (admin1_id == 0) {
          map_data <- map_data %>% select(GEO_ID,ADMIN1,ADMIN2,var,geometry, population_and_pfa_bool)
        } else {
          map_data <- map_data %>% filter(`ADMIN1 GEO_ID` == admin1_id) %>% select(GEO_ID,ADMIN1,ADMIN2,var,geometry, population_and_pfa_bool)
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
        
      }
    
  } else {
    map_data <- map_data %>% rename("var"=var_to_summarise)
    map_data$var <- round(map_data$var,0)
    
    if (admin1_id == 0) {
      map_data <- map_data %>% select(GEO_ID,ADMIN1,ADMIN2,var,pob=POB,geometry)
    } else {
      map_data <- map_data %>% filter(`ADMIN1 GEO_ID` == admin1_id) %>% select(GEO_ID,ADMIN1,ADMIN2,var,pob=POB,geometry)
    }
    
    
    map_data <- map_data %>% mutate(
      tasa_level_num = case_when(
        GEO_ID %in% ZERO_POB_LIST ~ 4,
        is.na(var) ~ 0,
        var < 1 ~ 1,
        var >= 1 & var < 2 & pob > 100000 ~ 2,
        (var >= 2 & pob > 100000) | (var >= 1 & pob <= 100000) ~ 3
      )
    )
    
    pal_gradient <- colorNumeric(
      c("#666666","#e8132b","#fec000","#92d050","#9bc2e6"),
      domain = c(0,4)
    )
    legend_colors = c("#e8132b","#fec000","#92d050")
    legend_values = c(lang_label_tls(LANG_TLS,"surv_legend_cases_1"),
                      lang_label_tls(LANG_TLS,"surv_legend_cases_2"),
                      lang_label_tls(LANG_TLS,"surv_legend_cases_3"))
    
    if (0 %in% map_data$tasa_level_num) {
      legend_colors = c("#666666",legend_colors)
      legend_values = c(lang_label_tls(LANG_TLS,"no_data"),legend_values)
    }
    
    if (length(ZERO_POB_LIST) > 0) {
      legend_colors = c(legend_colors,"#9bc2e6")
      legend_values = c(legend_values,lang_label_tls(LANG_TLS,"no_hab"))
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
        fillColor   = ~pal_gradient(tasa_level_num),
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
      addLegend(title = lang_label_tls(LANG_TLS,"surv_title_map_rate"),colors = legend_colors,labels = legend_values, opacity = 0.5, position = 'topright')
  }
  
  return(map)
  
}

cal_get_data_table <- function(LANG_TLS,CUT_OFFS,data,admin1_id) {
  
  data$risk_level <- get_risk_level(LANG_TLS,CUT_OFFS,"SURV_QUAL",data$surveillance_score)
  data <- data %>% select(`ADMIN1 GEO_ID`,ADMIN1,ADMIN2,surveillance_score,risk_level,Suspected_Case,POB,tasa_casos, tasa_casos_PR, p_casos_inv, p_casos_inv_PR, p_casos_muestra, p_casos_muestra_PR, p_muestras_lab, p_muestras_lab_PR) %>% 
    mutate(
      POB = cFormat(POB,0),
      tasa_casos = round((tasa_casos),1),
      tasa_casos_PR = round((tasa_casos_PR),0),
      p_casos_inv = round((p_casos_inv),1),
      p_casos_inv_PR = round((p_casos_inv_PR),0),
      p_casos_muestra = round((p_casos_muestra),1),
      p_casos_muestra_PR = round((p_casos_muestra_PR),0),
      p_muestras_lab = round((p_muestras_lab),1),
      p_muestras_lab_PR = round((p_muestras_lab_PR),0),
      surveillance_score = round((surveillance_score),0)
    )
  
  if (admin1_id == 0) {
    data <- data %>% select(-`ADMIN1 GEO_ID`)
    colnames(data) <- c(lang_label_tls(LANG_TLS,"table_admin1_name"),lang_label_tls(LANG_TLS,"table_admin2_name"),
                        lang_label_tls(LANG_TLS,"total_pr"),lang_label_tls(LANG_TLS,"risk_level"),
                        lang_label_tls(LANG_TLS,"surv_table_cases"),lang_label_tls(LANG_TLS,"surv_table_pob"),
                        lang_label_tls(LANG_TLS,"surv_table_rate"),lang_label_tls(LANG_TLS,"surv_table_rate_pr"),
                        lang_label_tls(LANG_TLS,"surv_table_adeq_inv"),lang_label_tls(LANG_TLS,"surv_table_adeq_inv_pr"),
                        lang_label_tls(LANG_TLS,"surv_table_adeq_sample"),lang_label_tls(LANG_TLS,"surv_table_adeq_sample_pr"),
                        lang_label_tls(LANG_TLS,"surv_table_timely_lab"),lang_label_tls(LANG_TLS,"surv_table_timely_lab_pr"))
  } else {
    data <- data %>% filter(`ADMIN1 GEO_ID` == admin1_id) %>% select(-ADMIN1,-`ADMIN1 GEO_ID`)
    colnames(data) <- c(lang_label_tls(LANG_TLS,"table_admin2_name"),
                        lang_label_tls(LANG_TLS,"total_pr"),lang_label_tls(LANG_TLS,"risk_level"),
                        lang_label_tls(LANG_TLS,"surv_table_cases"),lang_label_tls(LANG_TLS,"surv_table_pob"),
                        lang_label_tls(LANG_TLS,"surv_table_rate"),lang_label_tls(LANG_TLS,"surv_table_rate_pr"),
                        lang_label_tls(LANG_TLS,"surv_table_adeq_inv"),lang_label_tls(LANG_TLS,"surv_table_adeq_inv_pr"),
                        lang_label_tls(LANG_TLS,"surv_table_adeq_sample"),lang_label_tls(LANG_TLS,"surv_table_adeq_sample_pr"),
                        lang_label_tls(LANG_TLS,"surv_table_timely_lab"),lang_label_tls(LANG_TLS,"surv_table_timely_lab_pr"))
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
      c(lang_label_tls(LANG_TLS,"total_pr"),
        lang_label_tls(LANG_TLS,"surv_table_rate_pr"),lang_label_tls(LANG_TLS,"surv_table_adeq_inv_pr"),
        lang_label_tls(LANG_TLS,"surv_table_adeq_sample_pr"),lang_label_tls(LANG_TLS,"surv_table_timely_lab_pr")),
      backgroundColor = "#e3e3e3"
    )
  
  return(datos_table)
}


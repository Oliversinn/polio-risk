##############################################################
# Herramienta digital Análisis de Riesgo SR - inm_pob.R
# Organización Panamericana de la Salud
# Autor: Luis Quezada
# Última fecha de modificación: 2023-08-17
# R 4.3.0
##############################################################

# Utils ----
admin1_transform <- function(LANG_TLS,COUNTRY_NAME,admin1) {
  if (admin1 == toupper(lang_label_tls(LANG_TLS,"rep_label_all"))) {
    return(paste0("- ",toupper(COUNTRY_NAME)))
  } else {
    return(paste0("- ",admin1,", ",toupper(COUNTRY_NAME)))
  }
}

plot_pie_data <- function(LANG_TLS,ZERO_POB_LIST,CUT_OFFS,indicator,data,admin1_id,return_table=F) {
  data$risk_level <- get_risk_level(LANG_TLS,CUT_OFFS,indicator,data$immunity_score, data$population_and_pfa_bool)
  
  if (admin1_id == 0) {
    pie_data <- data %>% filter(!is.na(immunity_score)) %>% select(risk_level) %>% count(risk_level)
  } else {
    pie_data <- data %>% filter(`ADMIN1 GEO_ID` == admin1_id,!is.na(immunity_score)) %>% select(risk_level) %>% count(risk_level)
  }
  
  if (sum(is.na(data$immunity_score)) > 0) {pie_data <- rbind(pie_data,c(lang_label_tls(LANG_TLS,"no_data"),sum(is.na(data$immunity_score))))}
  if (length(ZERO_POB_LIST) > 0) {pie_data <- rbind(pie_data,c(lang_label_tls(LANG_TLS,"no_hab"),length(ZERO_POB_LIST)))}
  
  pie_data <- pie_data %>% mutate(
    risk_level_word = case_when(
      risk_level == lang_label_tls(LANG_TLS,"LR") ~ lang_label_tls(LANG_TLS,"cut_offs_LR"),
      risk_level == lang_label_tls(LANG_TLS,"MR") ~ lang_label_tls(LANG_TLS,"cut_offs_MR"),
      risk_level == lang_label_tls(LANG_TLS,"HR") ~ lang_label_tls(LANG_TLS,"cut_offs_HR"),
      risk_level == lang_label_tls(LANG_TLS,"VHR") ~ lang_label_tls(LANG_TLS,"cut_offs_VHR"),
      risk_level == "NA" | is.na(risk_level) ~ lang_label_tls(LANG_TLS,"no_data"),
      risk_level == "NO_HAB" ~ lang_label_tls(LANG_TLS,"no_hab")
    ),
    clas_color = case_when(
      risk_level == lang_label_tls(LANG_TLS,"LR") ~ "rgba(146, 208, 80, 0.7)",
      risk_level == lang_label_tls(LANG_TLS,"MR") ~ "rgba(254, 192, 0, 0.7)",
      risk_level == lang_label_tls(LANG_TLS,"HR") ~ "rgba(232, 19, 43, 0.7)",
      risk_level == lang_label_tls(LANG_TLS,"VHR") ~ "rgba(146, 0, 0, 0.7)",
      risk_level == "NA" | is.na(risk_level) ~ "rgba(0, 0, 0, 0.5)",
      risk_level == "NO_HAB" ~ "rgba(155, 194, 230, 0.7)"
    ),
    clas_order = case_when(
      risk_level == lang_label_tls(LANG_TLS,"LR") ~ 1,
      risk_level == lang_label_tls(LANG_TLS,"MR") ~ 2,
      risk_level == lang_label_tls(LANG_TLS,"HR") ~ 3,
      risk_level == lang_label_tls(LANG_TLS,"VHR") ~ 4,
      risk_level == "NA" | is.na(risk_level) ~ 5,
      risk_level == "NO_HAB" ~ 6
    )
  ) %>% arrange(clas_order)
  
  if (!return_table) {
    fig <- plot_ly(
      pie_data, labels = ~risk_level_word, values = ~n, 
      type = 'pie',
      textposition="inside",
      textinfo="value+percent",
      texttemplate="%{value}<br>%{percent:.1%}",
      sort = FALSE,
      marker = list(colors = ~clas_color)) %>% 
      layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) %>%
      layout(legend = list(orientation = 'h',x = 0.05, y = 5, bgcolor = 'rgba(0,0,0,0)', font = list(size = 12))) %>%
      config(displaylogo = FALSE) %>%
      config(modeBarButtonsToRemove = c("sendDataToCloud", "editInChartStudio","pan2d","select2d","drawclosedpath",
                                        "drawline","drawrect","drawopenpath","drawcircle","eraseshape","autoScale2d",
                                        "zoomIn2d","zoomOut2d","toggleSpikelines","lasso2d")) %>% 
      layout(hovermode = 'x')
    
    return(fig)
  } else {
    pie_data <- pie_data %>% select(risk_level_word,n)
    colnames(pie_data) = c(lang_label_tls(LANG_TLS,"risk_level"),"n")
    
    datos_table <- pie_data %>%
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
          dom = 'Brtip',
          buttons = list(
            list(extend = "copy",text = lang_label_tls(LANG_TLS,"button_copy")),
            list(extend='csv',filename=paste(lang_label_tls(LANG_TLS,"distribution"),lang_label_tls(LANG_TLS,indicator),admin1_id)),
            list(extend='excel', filename=paste(lang_label_tls(LANG_TLS,"distribution"),lang_label_tls(LANG_TLS,indicator),admin1_id))
          ),
          class = "display"
        )
      )
  }
}


# Dashboard ----

inmu_title_map <- function(LANG_TLS,YEAR_CAMP_SR,COUNTRY_NAME,YEAR_LIST,admin1,var) {
  YEAR_1=YEAR_LIST[1];YEAR_2=YEAR_LIST[2];YEAR_3=YEAR_LIST[3];YEAR_4=YEAR_LIST[4];YEAR_5=YEAR_LIST[5];
  var_text <- case_when(
    var == "immunity_score" ~ paste0(lang_label_tls(LANG_TLS,"inm_title_map_total_pr")," ",admin1_transform(LANG_TLS,COUNTRY_NAME,admin1)," (",YEAR_1," - ",YEAR_5,")"),
    var == "SRP1_PR" ~ paste0(lang_label_tls(LANG_TLS,"inm_title_map_mmr1_pr")," ",admin1_transform(LANG_TLS,COUNTRY_NAME,admin1)," (",YEAR_1," - ",YEAR_5,")"),
    var == "SRP2_PR" ~ paste0(lang_label_tls(LANG_TLS,"inm_title_map_mmr2_pr")," ",admin1_transform(LANG_TLS,COUNTRY_NAME,admin1)," (",YEAR_1," - ",YEAR_5,")"),
    var == "cob_last_camp_PR" ~ paste0(lang_label_tls(LANG_TLS,"inm_title_map_last_camp_pr")," ",admin1_transform(LANG_TLS,COUNTRY_NAME,admin1)," (",YEAR_CAMP_SR,")"),
    var == "p_sospechosos_novac_PR" ~ paste0(lang_label_tls(LANG_TLS,"inm_title_map_novac_pr")," ",admin1_transform(LANG_TLS,COUNTRY_NAME,admin1)," (",YEAR_5,")"),
    var == "cob_last_camp" ~ paste0(lang_label_tls(LANG_TLS,"inm_title_map_last_camp")," ",admin1_transform(LANG_TLS,COUNTRY_NAME,admin1)," (",YEAR_CAMP_SR,")"),
    var == "p_sospechosos_novac" ~ paste0(lang_label_tls(LANG_TLS,"inm_title_map_novac")," ",admin1_transform(LANG_TLS,COUNTRY_NAME,admin1)," (",YEAR_5,")"),
    var == "year1" ~ paste0(lang_label_tls(LANG_TLS,"immunity_polio_cob")," ",admin1_transform(LANG_TLS,COUNTRY_NAME,admin1)," (",YEAR_1,")"),
    var == "year2" ~ paste0(lang_label_tls(LANG_TLS,"immunity_polio_cob")," ",admin1_transform(LANG_TLS,COUNTRY_NAME,admin1)," (",YEAR_2,")"),
    var == "year3" ~ paste0(lang_label_tls(LANG_TLS,"immunity_polio_cob")," ",admin1_transform(LANG_TLS,COUNTRY_NAME,admin1)," (",YEAR_3,")"),
    var == "year4" ~ paste0(lang_label_tls(LANG_TLS,"immunity_polio_cob")," ",admin1_transform(LANG_TLS,COUNTRY_NAME,admin1)," (",YEAR_4,")"),
    var == "year5" ~ paste0(lang_label_tls(LANG_TLS,"immunity_polio_cob")," ",admin1_transform(LANG_TLS,COUNTRY_NAME,admin1)," (",YEAR_5,")"),
    var == "ipv2" ~ paste0(lang_label_tls(LANG_TLS,"immunity_ipv2_cob")," ",admin1_transform(LANG_TLS,COUNTRY_NAME,admin1)," (",YEAR_EVAL,")"),
    var == "effective_campaign" ~ paste0(lang_label_tls(LANG_TLS,"immunity_effective_cob")," ",admin1_transform(LANG_TLS,COUNTRY_NAME,admin1)," (",YEAR_1, "-", YEAR_EVAL,")")
  )
  return(var_text)
}



inmu_plot_map_data <- function(LANG_TLS,YEAR_CAMP_SR,COUNTRY_NAME,YEAR_LIST,ZERO_POB_LIST,CUT_OFFS,map_data,data,var_to_summarise,admin1,admin1_id,admin1_geo_id_df) {
  pfa <- population_and_pfa(data)
  indicator <- "immunity_score"
  data <- data %>% select(-ADMIN1,-ADMIN2)
  map_data <- full_join(map_data,data,by = c("GEO_ID" = "GEO_ID", "ADMIN1 GEO_ID" = "ADMIN1 GEO_ID"))
  map_data$`ADMIN1 GEO_ID`[is.na(map_data$`ADMIN1 GEO_ID`) & map_data$ADMIN1 == admin1] <- admin1_geo_id_df$`ADMIN1 GEO_ID`[admin1_geo_id_df$ADMIN1 == admin1]
  
  if (var_to_summarise %in% c("immunity_score")) {
    map_data <- map_data %>% rename("PR" = var_to_summarise)
    map_data$risk_level <- get_risk_level(LANG_TLS,CUT_OFFS,indicator,map_data$PR,map_data$population_and_pfa_bool)
    
    if (var_to_summarise == "immunity_score") {
      map_data$risk_level[map_data$GEO_ID %in% ZERO_POB_LIST] <- "NO_HAB"
    }
    
    
    if (admin1_id == 0) {
      map_data <- map_data %>% select(GEO_ID,ADMIN1,ADMIN2,PR,risk_level,geometry)
    } else {
      map_data <- map_data %>% filter(`ADMIN1 GEO_ID` == admin1_id) %>% select(GEO_ID,ADMIN1,ADMIN2,PR,risk_level,geometry)
    }
    
    
    if (var_to_summarise == "immunity_score") {
      
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
                             map_data$PR,
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
        addLegend(layerId = "map_title","topright",color = "white", opacity = 0,labels=HTML(paste0("<strong>",inmu_title_map(LANG_TLS,YEAR_CAMP_SR,COUNTRY_NAME,YEAR_LIST,admin1,var_to_summarise),"</strong>"))) %>%
        addLegend(title = lang_label_tls(LANG_TLS,"legend_risk_class"),colors = legend_colors,labels = legend_values, opacity = 0.5, position = 'topright')
    } else {
      # No es total, es algun PR
      pal_gradient <- colorNumeric(
        colorRampPalette(brewer.pal(9,"YlOrRd"))(6),
        domain = c(0,10)
      )
      legend_colors = c("#800026","#D30F20","#FC5A2D","#FDAA48","#FEE186","#FFFFCC")
      legend_values = c("10","8","6","4","2","0")
      
      shape_label <- sprintf("<strong>%s</strong>, %s<br/>%s: %s",
                             map_data$ADMIN2,
                             map_data$ADMIN1,
                             lang_label_tls(LANG_TLS,"risk_points"),
                             map_data$PR
      ) %>% lapply(HTML)
      
      # MAPA
      map <- leaflet(map_data,options = leafletOptions(doubleClickZoom = T, attributionControl = F, zoomSnap=0.1, zoomDelta=0.1)) %>%
        addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
        addPolygons(
          fillColor   = ~pal_gradient(PR),
          fillOpacity = 0.8,
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
        ) %>% addLegend(layerId = "map_title","topright",color = "white", opacity = 0,labels=HTML(paste0("<strong>",inmu_title_map(LANG_TLS,YEAR_CAMP_SR,COUNTRY_NAME,YEAR_LIST,admin1,var_to_summarise),"</strong>"))) %>%
        addLegend(title = lang_label_tls(LANG_TLS,"risk_points"),colors = legend_colors,labels = legend_values, opacity = 0.5, position = 'topright')
    }

  } else if (var_to_summarise %in% c("ipv2",
    "year1","year2","year3","year4","year5")) {
    # Cob map
    map_data <- map_data %>% rename("COB"=var_to_summarise)
    map_data$COB <- round(map_data$COB,1)
    
    if (admin1_id == 0) {
      map_data <- map_data %>% select(GEO_ID,ADMIN1,ADMIN2,COB,geometry)
    } else {
      map_data <- map_data %>% filter(`ADMIN1 GEO_ID` == admin1_id) %>% select(GEO_ID,ADMIN1,ADMIN2,COB,geometry)
    }
    
    map_data <- map_data %>% mutate(
      cob_level_num = case_when(
        GEO_ID %in% ZERO_POB_LIST ~ 5,
        is.na(COB) ~ 0,
        COB < 80 ~ 4,
        COB >= 80 & COB < 90 ~ 3,
        COB >= 90 & COB < 95 ~ 2,
        COB >= 95 ~ 1
      )
    )
    
    pal_gradient <- colorNumeric(
      c("#666666","#92d050","#fec000","#e8132b","#920000","#9bc2e6"),
      domain = c(0,5)
    )
    legend_colors = c("#920000","#e8132b","#fec000","#92d050")
    legend_values = c("< 80%","≥ 80% <b>;</b> < 90%","≥ 90% <b>;</b> < 95%","≥ 95%")
    
    if (0 %in% map_data$cob_level_num) {
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
                           lang_label_tls(LANG_TLS,"vac_coverage"),
                           map_data$COB,
                           "%"
    ) %>% lapply(HTML)
    
    # MAPA
    map <- leaflet(map_data,options = leafletOptions(doubleClickZoom = T, attributionControl = F, zoomSnap=0.1, zoomDelta=0.1)) %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
      addPolygons(
        fillColor   = ~pal_gradient(cob_level_num),
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
      ) %>% addLegend(layerId = "map_title","topright",color = "white", opacity = 0,labels=HTML(paste0("<strong>",inmu_title_map(LANG_TLS,YEAR_CAMP_SR,COUNTRY_NAME,YEAR_LIST,admin1,var_to_summarise),"</strong>"))) %>%
      addLegend(title = lang_label_tls(LANG_TLS,"inm_legend_cob_range"),colors = legend_colors,labels = legend_values, opacity = 0.5, position = 'topright')
    
    
  } else if (var_to_summarise == "effective_campaign") {
    
    # Cob map
    map_data <- map_data %>% rename("COB"=var_to_summarise)
    

    if (admin1_id == 0) {
      map_data <- map_data %>% select(GEO_ID,ADMIN1,ADMIN2,COB,geometry)
    } else {
      map_data <- map_data %>% filter(`ADMIN1 GEO_ID` == admin1_id) %>% select(GEO_ID,ADMIN1,ADMIN2,COB,geometry)
    }
    
    map_data <- map_data %>% mutate(
      var_num = case_when(
        GEO_ID %in% ZERO_POB_LIST ~ 3,
        is.na(COB) ~ 0,
        COB == "Si" ~ 1,
        COB == "No" ~ 2
      ),
      var_word = case_when(
        GEO_ID %in% ZERO_POB_LIST ~ lang_label_tls(LANG_TLS,"no_hab"),
        is.na(COB) ~ lang_label_tls(LANG_TLS,"no_data"),
        COB == "Si" ~ lang_label_tls(LANG_TLS,"yes"),
        COB == "No" ~ lang_label_tls(LANG_TLS,"no")
      )
    )


    pal_gradient <- colorNumeric(
      c("#666666","#92d050","#e8132b","#9bc2e6"),
      domain = c(0,3)
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
    
    shape_label <- sprintf("<strong>%s</strong>, %s<br/>%s: %s",
                           map_data$ADMIN2,
                           map_data$ADMIN1,
                           lang_label_tls(LANG_TLS,"immunity_legend_effective"),
                           tolower(map_data$var_word)
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
      addLegend(layerId = "map_title","topright",color = "white", opacity = 0,labels=HTML(paste0("<strong>",inmu_title_map(LANG_TLS,YEAR_CAMP_SR,COUNTRY_NAME,YEAR_LIST,admin1,var_to_summarise),"</strong>"))) %>%
      addLegend(title = lang_label_tls(LANG_TLS,"rap_pres_team"),colors = legend_colors,labels = legend_values, opacity = 0.5, position = 'topright')
  }

  return(map)
  
}

inmu_get_data_table <- function(LANG_TLS,YEAR_LIST,CUT_OFFS,data,admin1_id) {
  YEAR_1=YEAR_LIST[1];YEAR_2=YEAR_LIST[2];YEAR_3=YEAR_LIST[3];YEAR_4=YEAR_LIST[4];YEAR_5=YEAR_LIST[5];
  
  data$risk_level <- get_risk_level(LANG_TLS,CUT_OFFS,"immunity_score",data$immunity_score, data$population_and_pfa_bool)
  
  data <- data %>% 
    select(`ADMIN1 GEO_ID`,
           ADMIN1,ADMIN2,immunity_score,risk_level,
           year1,year2,year3,year4,year5,years_score,
           ipv2,ipv_score,
           effective_campaign,effective_campaign_score,immunity_score) %>%
    mutate(
      year1=round(year1,0),
      year2=round(year2,0),
      year3=round(year3,0),
      year4=round(year4,0),
      year5=round(year5,0)
    )
  
  if (admin1_id == 0) {
    data <- data %>% select(-`ADMIN1 GEO_ID`)
    colnames(data) <- c(lang_label_tls(LANG_TLS,"table_admin1_name"),lang_label_tls(LANG_TLS,"table_admin2_name"),lang_label_tls(LANG_TLS,"total_pr"),lang_label_tls(LANG_TLS,"risk_level"),
                        paste(lang_label_tls(LANG_TLS,"immunity_polio_cob"),YEAR_1,"(%)"),
                        paste(lang_label_tls(LANG_TLS,"immunity_polio_cob"),YEAR_2,"(%)"),
                        paste(lang_label_tls(LANG_TLS,"immunity_polio_cob"),YEAR_3,"(%)"),
                        paste(lang_label_tls(LANG_TLS,"immunity_polio_cob"),YEAR_4,"(%)"),
                        paste(lang_label_tls(LANG_TLS,"immunity_polio_cob"),YEAR_5,"(%)"),
                        lang_label_tls(LANG_TLS,"immunity_polio_score"),
                        lang_label_tls(LANG_TLS, "immunity_ipv2_cob"),
                        lang_label_tls(LANG_TLS, "immunity_ipv2_score"),
                        lang_label_tls(LANG_TLS,"immunity_effective_cob"),
                        lang_label_tls(LANG_TLS,"immunity_effective_score"))
  } else {
    data <- data %>% filter(`ADMIN1 GEO_ID` == admin1_id) %>% select(-ADMIN1,-`ADMIN1 GEO_ID`)
    colnames(data) <- c(lang_label_tls(LANG_TLS,"table_admin2_name"),lang_label_tls(LANG_TLS,"total_pr"),lang_label_tls(LANG_TLS,"risk_level"),
                        paste(lang_label_tls(LANG_TLS,"immunity_polio_cob"),YEAR_1,"(%)"),
                        paste(lang_label_tls(LANG_TLS,"immunity_polio_cob"),YEAR_2,"(%)"),
                        paste(lang_label_tls(LANG_TLS,"immunity_polio_cob"),YEAR_3,"(%)"),
                        paste(lang_label_tls(LANG_TLS,"immunity_polio_cob"),YEAR_4,"(%)"),
                        paste(lang_label_tls(LANG_TLS,"immunity_polio_cob"),YEAR_5,"(%)"),
                        lang_label_tls(LANG_TLS,"immunity_polio_score"),
                        lang_label_tls(LANG_TLS, "immunity_ipv2_cob"),
                        lang_label_tls(LANG_TLS, "immunity_ipv2_score"),
                        lang_label_tls(LANG_TLS,"immunity_effective_cob"),
                        lang_label_tls(LANG_TLS,"immunity_effective_score"))
  }
  
  spr_cob_colnames <- c(
    paste(lang_label_tls(LANG_TLS,"immunity_polio_cob"),YEAR_1,"(%)"),
    paste(lang_label_tls(LANG_TLS,"immunity_polio_cob"),YEAR_2,"(%)"),
    paste(lang_label_tls(LANG_TLS,"immunity_polio_cob"),YEAR_3,"(%)"),
    paste(lang_label_tls(LANG_TLS,"immunity_polio_cob"),YEAR_4,"(%)"),
    paste(lang_label_tls(LANG_TLS,"immunity_polio_cob"),YEAR_5,"(%)"),
    lang_label_tls(LANG_TLS, "immunity_ipv2_cob"),
    lang_label_tls(LANG_TLS,"immunity_effective_cob")
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
          list(extend='csv',filename=paste(lang_label_tls(LANG_TLS,"data"),lang_label_tls(LANG_TLS,"INM_POB"),admin1_id)),
          list(extend='excel', filename=paste(lang_label_tls(LANG_TLS,"data"),lang_label_tls(LANG_TLS,"INM_POB"),admin1_id))
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
        lang_label_tls(LANG_TLS,"immunity_polio_score"),
        lang_label_tls(LANG_TLS,"immunity_ipv2_score"),
        lang_label_tls(LANG_TLS,"immunity_effective_score")),
      backgroundColor = "#e3e3e3"
    ) %>% formatStyle(
      spr_cob_colnames,
      color = styleInterval(c(100),c("black","#0097e6"))
    )
  
  return(datos_table)
}



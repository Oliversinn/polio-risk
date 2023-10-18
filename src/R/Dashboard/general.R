# AUTHORSHIP ----

# Pan American Health Organization
# Author: Luis Quezada & Oliver Mazariegos
# Last Update: 2023-10-09
# R 4.3.1


# Utils ----
cFormat <- function(x,n) {
  cf <- format(round(as.numeric(x),n), nsmall = n, big.mark = ",")
  return(cf)
}

VB_style <- function(msg = '', style="font-size: 100%;") {
  tags$p( msg , style = style )
}

hline <- function(y = 0, color = "#666666") {
  list(
    type = "line",
    x0 = 0,
    x1 = 1,
    xref = "paper",
    y0 = y,
    y1 = y,
    line = list(color = color)
  )
}

hdash <- function(y = 0, color = "#666666") {
  list(
    type = "line",
    x0 = 0,
    x1 = 1,
    xref = "paper",
    y0 = y,
    y1 = y,
    line = list(color = color,dash = "dash")
  )
}

lang_label_tls <- function(LANG_TLS,label) {
  return(LANG_TLS$LANG[LANG_TLS$LABEL == label])
}


# Risk funcs ----
get_risk_level <- function(LANG_TLS,CUT_OFFS,indicator,risk_points) {
  rp_LR <- CUT_OFFS$value[CUT_OFFS$RV == indicator & CUT_OFFS$risk_level == "LR"]
  rp_MD <- CUT_OFFS$value[CUT_OFFS$RV == indicator & CUT_OFFS$risk_level == "MR"]
  rp_HR <- CUT_OFFS$value[CUT_OFFS$RV == indicator & CUT_OFFS$risk_level == "HR"]
  rp_VHR <- CUT_OFFS$value[CUT_OFFS$RV == indicator & CUT_OFFS$risk_level == "VHR"]
  risk_levels <- c()
  for (r_point in risk_points) {
    if (is.na(r_point)) {r_level = lang_label_tls(LANG_TLS,"no_data")}
    else if (r_point >= 0 & r_point <= rp_LR) {r_level = lang_label_tls(LANG_TLS,"LR")}
    else if (r_point > rp_LR & r_point <= rp_MD) {r_level = lang_label_tls(LANG_TLS,"MR")}
    else if (r_point > rp_MD & r_point <= rp_HR) {r_level = lang_label_tls(LANG_TLS,"HR")}
    else if (r_point > rp_HR) {r_level = lang_label_tls(LANG_TLS,"VHR")}
    risk_levels <- c(risk_levels,r_level)
  }
  return(risk_levels)
}

get_risk_level_point_limit <- function(CUT_OFFS,indicator,risk_level) {
  risk_point <- CUT_OFFS$value[CUT_OFFS$RV == indicator & CUT_OFFS$risk_level == risk_level]
  return(risk_point)
}


# Dashboard ----

ind_rangos_table <- function(LANG_TLS,CUT_OFFS,indicator) {
  
  table_percentages <- c(lang_label_tls(LANG_TLS,"LR"),lang_label_tls(LANG_TLS,"MR"),lang_label_tls(LANG_TLS,"HR"),lang_label_tls(LANG_TLS,"VHR"))
  table_intervals_min <- c(0,get_risk_level_point_limit(CUT_OFFS,indicator,"LR") + 1,get_risk_level_point_limit(CUT_OFFS,indicator,"MR") + 1,get_risk_level_point_limit(CUT_OFFS,indicator,"HR") + 1)
  table_intervals_max <- c(get_risk_level_point_limit(CUT_OFFS,indicator,"LR"),get_risk_level_point_limit(CUT_OFFS,indicator,"MR"),get_risk_level_point_limit(CUT_OFFS,indicator,"HR"),get_risk_level_point_limit(CUT_OFFS,indicator,"VHR"))
  table_colors <- c("rgba(146, 208, 80, 0.7)","rgba(254, 192, 0, 0.7)","rgba(232, 19, 43, 0.7)","rgba(146, 0, 0, 0.7)")
  
  rangos_df <- data.frame(table_percentages,table_intervals_min)
  rangos_df <- rangos_df %>% pivot_wider(names_from = table_percentages, values_from = table_intervals_min)
  rangos_df <- rbind(rangos_df,table_intervals_max)
  rangos_df <- cbind(c(lang_label_tls(LANG_TLS,"limit_min"),lang_label_tls(LANG_TLS,"limit_max")),rangos_df)
  colnames(rangos_df)[1] <- lang_label_tls(LANG_TLS,"risk")
  
  datos_table <- rangos_df %>%
    datatable(
      rownames = F,
      extensions = 'Buttons',
      options = list(
        scrollX = TRUE, scrollCollapse = TRUE,
        searching = TRUE,fixedColumns = TRUE,autoWidth = FALSE,
        ordering = TRUE,scrollY = 700,pageLength = nrow(rangos_df),
        dom = 't'
      ),
      class = "display"
    ) %>% formatStyle(
      colnames(rangos_df)[-1],
      backgroundColor = styleInterval(table_intervals_max %>% head(3),table_colors)
    ) %>% formatStyle(0, target = 'row',lineHeight = '150%')
  
  return(datos_table)
}


ind_prep_bar_data <- function(LANG_TLS,CUT_OFFS,data,indicator,admin1_id,risk) {
  
  var_to_summarise <- case_when(
    indicator == "total_score" ~ "total_score",
    indicator == "immunity_score" ~ "immunity_score",
    indicator == "survaillance_score" ~ "survaillance_score",
    indicator == "determinants_score" ~ "determinants_score",
    indicator == "outbreaks_score" ~ "outbreaks_score"
  )
  
  prep_data <- data %>% rename("PR" = var_to_summarise)
  
  if (admin1_id == 0) {
    prep_data <- prep_data %>% filter(!is.na(PR)) %>% select(ADMIN2,PR)
  } else {
    prep_data <- prep_data %>% filter(`ADMIN1 GEO_ID` == admin1_id) %>% filter(!is.na(PR)) %>% select(ADMIN2,PR)
  }
  
  prep_data$risk_level <- get_risk_level(LANG_TLS,CUT_OFFS,indicator,prep_data$PR)
  if (risk != "ALL") {
    prep_data <- prep_data %>% filter(risk_level == lang_label_tls(LANG_TLS,risk))
  }
  
  return(prep_data)
}

ind_prep_map_data <- function(LANG_TLS,ZERO_POB_LIST,CUT_OFFS,map_data,data,indicator,admin1_id,risk) {
  data <- data %>% select(-ADMIN1,-ADMIN2)
  map_data <- full_join(map_data,data,by = c("GEO_ID" = "GEO_ID", "ADMIN1 GEO_ID" = "ADMIN1 GEO_ID") )
  print(map_data)
  var_to_summarise <- case_when(
    indicator == "total_score" ~ "total_score",
    indicator == "immunity_score" ~ "immunity_score",
    indicator == "survaillance_score" ~ "survaillance_score",
    indicator == "determinants_score" ~ "determinants_score",
    indicator == "outbreaks_score" ~ "outbreaks_score"
  )
  map_data <- map_data %>% rename("PR" = var_to_summarise)
  map_data$risk_level <- get_risk_level(LANG_TLS,CUT_OFFS,indicator,map_data$PR)
  map_data$risk_level[map_data$GEO_ID %in% ZERO_POB_LIST] <- "NO_HAB"
  if (admin1_id == 0) {
    map_data <- map_data %>% select(ADMIN1,ADMIN2,PR,risk_level,geometry)
  } else {
    map_data <- map_data %>% filter(`ADMIN1 GEO_ID` == admin1_id) %>% select(ADMIN1,ADMIN2,PR,risk_level,geometry)
  }
  
  if (risk != "ALL") {
    map_data$PR[map_data$risk_level != lang_label_tls(LANG_TLS,risk)] <- NA
    map_data$risk_level[map_data$risk_level != lang_label_tls(LANG_TLS,risk)] <- NA
  }
  return(map_data)
}

ind_get_bar_table <- function(LANG_TLS,CUT_OFFS,data,indicator,admin1_id,risk) {
  
  var_to_summarise <- case_when(
    indicator == "total_score" ~ "total_score",
    indicator == "immunity_score" ~ "immunity_score",
    indicator == "survaillance_score" ~ "survaillance_score",
    indicator == "determinants_score" ~ "determinants_score",
    indicator == "outbreaks_score" ~ "outbreaks_score"
  )
  
  
  
  if (indicator != "total_score") {
    data <- data %>% rename("PR" = var_to_summarise)
    
    if (admin1_id == 0) {
      data <- data %>% filter(!is.na(PR)) %>% select(ADMIN1,ADMIN2,PR)
    } else {
      data <- data %>% filter(`ADMIN1 GEO_ID` == admin1_id) %>% filter(!is.na(PR)) %>% select(ADMIN1,ADMIN2,PR)
      data <- data %>% arrange(desc(ADMIN2))
    }
    
    data$risk_level <- get_risk_level(LANG_TLS,CUT_OFFS,indicator,data$PR)
    if (risk != "ALL") {
      data <- data %>% filter(risk_level == lang_label_tls(LANG_TLS,risk))
    }
    
    colnames(data) = c(
      lang_label_tls(LANG_TLS,"table_admin1_name"),
      lang_label_tls(LANG_TLS,"table_admin2_name"),
      lang_label_tls(LANG_TLS,"risk_points"),
      lang_label_tls(LANG_TLS,"risk_level")
    )
  } else {
    if (admin1_id == 0) {
      data <- data %>% select(ADMIN1,ADMIN2,immunity_score,survaillance_score,determinants_score,outbreaks_score,total_score)
    } else {
      data <- data %>% filter(`ADMIN1 GEO_ID` == admin1_id) %>% select(ADMIN1,ADMIN2,immunity_score,survaillance_score,determinants_score,outbreaks_score,total_score)
      data <- data %>% arrange(desc(ADMIN2))
    }
    
    data$risk_level <- get_risk_level(LANG_TLS,CUT_OFFS,indicator,data$total_score)
    if (risk != "ALL") {
      data <- data %>% filter(risk_level == lang_label_tls(LANG_TLS,risk))
    }
    
    colnames(data) = c(
      lang_label_tls(LANG_TLS,"table_admin1_name"),
      lang_label_tls(LANG_TLS,"table_admin2_name"),
      lang_label_tls(LANG_TLS,"menuitem_immunity"),
      lang_label_tls(LANG_TLS,"menuitem_survaillance"),
      lang_label_tls(LANG_TLS,"menuitem_determinants"),
      lang_label_tls(LANG_TLS,"menuitem_outbreaks"),
      lang_label_tls(LANG_TLS,"risk_points"),
      lang_label_tls(LANG_TLS,"risk_level")
    )
  }
  
  datos_table <- data %>%
    datatable(
      rownames = F,
      extensions = 'Buttons',
      options = list(
        scrollX = TRUE, scrollCollapse = TRUE,
        searching = TRUE,fixedColumns = TRUE,autoWidth = FALSE,
        ordering = TRUE,scrollY = TRUE,pageLength = 13,
        dom = 'Brtip',
        buttons = list(
          list(extend = "copy",text = lang_label_tls(LANG_TLS,"button_copy")),
          list(extend = 'csv',filename = paste(indicator,admin1_id)),
          list(extend = 'excel', filename = paste(indicator,admin1_id))
        ),
        language = list(
          info = paste0(lang_label_tls(LANG_TLS,"data_table_showing")," _START_ ",lang_label_tls(LANG_TLS,"data_table_to")," _END_ ",lang_label_tls(LANG_TLS,"data_table_of")," _TOTAL_ ",lang_label_tls(LANG_TLS,"data_table_rows")),
          paginate = list(previous = lang_label_tls(LANG_TLS,"data_table_prev"), `next` = lang_label_tls(LANG_TLS,"data_table_next"))
        ),
        class = "display"
      )
    ) %>% formatStyle(
      lang_label_tls(LANG_TLS,"risk_level"),
      backgroundColor = styleEqual(
        c(lang_label_tls(LANG_TLS,"LR"),lang_label_tls(LANG_TLS,"MR"),
          lang_label_tls(LANG_TLS,"HR"),lang_label_tls(LANG_TLS,"VHR")),
        c("rgba(146, 208, 80, 0.7)","rgba(254, 192, 0, 0.7)",
          "rgba(232, 19, 43, 0.7)","rgba(146, 0, 0, 0.7)")
      )
    ) %>% formatStyle(
      lang_label_tls(LANG_TLS,"risk_points"),
      backgroundColor = "#e3e3e3"
    )
  
  return(datos_table)
}


ind_plot_bar_data <- function(LANG_TLS,CUT_OFFS,bar_data,indicator,admin1_id) {
  fig <- NULL
  
  if (admin1_id != 0) {
    y_axis_title <- str_to_title(lang_label_tls(LANG_TLS,"rep_label_admin2_name_plural"))
    x_axis_title <- paste0(lang_label_tls(LANG_TLS,"risk_points")," (",lang_label_tls(LANG_TLS,indicator),")")
    
    colnames(bar_data) = c("LUGAR","PR")
    bar_color = "#5b9bd5"
    
    fig <- plot_ly(bar_data, type = 'bar',orientation = 'h',y = ~LUGAR, x = ~PR, name = "", marker = list(color = bar_color),text = ~PR, textposition = 'outside') %>%
      layout(xaxis = list(title = x_axis_title, tickfont = list(size = 12)), 
             barmode = 'group',
             yaxis = list(title = y_axis_title, tickangle = 0, tickfont = list(size = 10))
      ) %>%
      layout(shapes = list(
        list(type = "rect",fillcolor = "#92d050", line = list(color = "#92d050"),
             opacity = 0.4, layer = "below",
             x0 = 0, x1 = get_risk_level_point_limit(CUT_OFFS,indicator,"LR"), xref = "x",
             y0 = -1, y1 = nrow(bar_data), yref = "y"),
        list(type = "rect",fillcolor = "#fec000", line = list(color = "#fec000"),
             opacity = 0.3, layer = "below",
             x0 = get_risk_level_point_limit(CUT_OFFS,indicator,"LR"), x1 = get_risk_level_point_limit(CUT_OFFS,indicator,"MR"), xref = "x",
             y0 = -1, y1 = nrow(bar_data), yref = "y"),
        list(type = "rect",fillcolor = "#e8132b", line = list(color = "#e8132b"),
             opacity = 0.2, layer = "below",
             x0 = get_risk_level_point_limit(CUT_OFFS,indicator,"MR"), x1 = get_risk_level_point_limit(CUT_OFFS,indicator,"HR"), xref = "x",
             y0 = -1, y1 = nrow(bar_data), yref = "y"),
        list(type = "rect",fillcolor = "#920000", line = list(color = "#920000"),
             opacity = 0.3, layer = "below",
             x0 = get_risk_level_point_limit(CUT_OFFS,indicator,"HR"), x1 = get_risk_level_point_limit(CUT_OFFS,indicator,"VHR"), xref = "x",
             y0 = -1, y1 = nrow(bar_data), yref = "y")
      )) %>%
      config(displaylogo = FALSE) %>%
      config(modeBarButtonsToRemove = c("sendDataToCloud", "editInChartStudio",
                                        "pan2d","select2d","drawclosedpath",
                                        "drawline","drawrect","drawopenpath",
                                        "drawcircle","eraseshape",
                                        "zoomIn2d","zoomOut2d","toggleSpikelines","lasso2d")) #%>% layout(hovermode = 'x')
  }
  
  return(fig)
}


ind_plot_multibar_data <- function(LANG_TLS,CUT_OFFS,bar_data,admin1_id,selected_indicador,risk) {
  fig <- NULL
  y_axis_title <- lang_label_tls(LANG_TLS,"rep_label_admin2_name_plural")
  x_axis_title <- lang_label_tls(LANG_TLS,"risk_points_general")
  indicator = "total_score"
  max_y_point <- get_risk_level_point_limit(CUT_OFFS,"total_score","VHR")
  bar_data <- bar_data %>% filter(`ADMIN1 GEO_ID` == admin1_id) %>% filter(!is.na(total_score))
  bar_data <- bar_data %>% rename(LUGAR = ADMIN2)

  if (admin1_id != 0) {
    
    if (selected_indicador == "total_score") {
      bar_data$risk_level <- get_risk_level(LANG_TLS,CUT_OFFS,indicator,bar_data$total_score)
      if (risk != "ALL") {
        bar_data <- bar_data %>% filter(risk_level == lang_label_tls(LANG_TLS,risk))
      }

      fig <- plot_ly(bar_data, type = 'bar',orientation = 'h',y = ~LUGAR,
                     x = ~immunity_score, name = lang_label_tls(LANG_TLS,"menuitem_immunity"),marker = list(color = "#8DB1CC"),text = ~immunity_score, textposition = 'inside',textangle = 0) %>%
        add_trace(x = ~survaillance_score, name = lang_label_tls(LANG_TLS,"menuitem_survaillance"), marker = list(color = "#2165A4"),text = ~survaillance_score, textposition = 'inside') %>%
        add_trace(x = ~determinants_score, name = lang_label_tls(LANG_TLS,"menuitem_determinants"), marker = list(color = "#253E80"),text = ~determinants_score, textposition = 'inside') %>%
        add_trace(x = ~outbreaks_score, name = lang_label_tls(LANG_TLS,"menuitem_outbreaks"), marker = list(color = "#6436A5"),text = ~outbreaks_score, textposition = 'inside') %>% 

        layout(xaxis = list(title = x_axis_title, tickfont = list(size = 12)), 
               barmode = 'stack',
               yaxis = list(title = y_axis_title, tickangle = 0, tickfont = list(size = 10))
        ) %>%
        layout(shapes = list(
          list(type = "rect",fillcolor = "#92d050", line = list(color = "#92d050"),
               opacity = 0.4, layer = "below",
               x0 = 0, x1 = get_risk_level_point_limit(CUT_OFFS,indicator,"LR"), xref = "x",
               y0 = -1, y1 = nrow(bar_data), yref = "y"),
          list(type = "rect",fillcolor = "#fec000", line = list(color = "#fec000"),
               opacity = 0.3, layer = "below",
               x0 = get_risk_level_point_limit(CUT_OFFS,indicator,"LR"), x1 = get_risk_level_point_limit(CUT_OFFS,indicator,"MR"), xref = "x",
               y0 = -1, y1 = nrow(bar_data), yref = "y"),
          list(type = "rect",fillcolor = "#e8132b", line = list(color = "#e8132b"),
               opacity = 0.2, layer = "below",
               x0 = get_risk_level_point_limit(CUT_OFFS,indicator,"MR"), x1 = get_risk_level_point_limit(CUT_OFFS,indicator,"HR"), xref = "x",
               y0 = -1, y1 = nrow(bar_data), yref = "y"),
          list(type = "rect",fillcolor = "#920000", line = list(color = "#920000"),
               opacity = 0.3, layer = "below",
               x0 = get_risk_level_point_limit(CUT_OFFS,indicator,"HR"), x1 = max_y_point, xref = "x",
               y0 = -1, y1 = nrow(bar_data), yref = "y")
        )) %>%
        layout(legend = list(orientation = 'h',x = 0, y = 50, bgcolor = 'rgba(0,0,0,0)', font = list(size = 10))) %>%
        config(displaylogo = FALSE) %>%
        config(modeBarButtonsToRemove = c("sendDataToCloud", "editInChartStudio",
                                          "pan2d","select2d","drawclosedpath",
                                          "drawline","drawrect","drawopenpath",
                                          "drawcircle","eraseshape",
                                          "zoomIn2d","zoomOut2d","toggleSpikelines","lasso2d","hoverCompareCartesian"))
    } else {
      var_to_summarise <- case_when(
        selected_indicador == "total_score" ~ "total_score",
        selected_indicador == "immunity_score" ~ "immunity_score",
        selected_indicador == "srvaillance_score" ~ "srvaillance_score",
        selected_indicador == "determinants_score" ~ "determinants_score",
        selected_indicador == "outbreaks_score" ~ "outbreaks_score"
      )
      
      bar_data <- bar_data %>% rename(VAR = var_to_summarise)
      bar_data$other_PR <- bar_data$total_score - bar_data$VAR
      
      bar_data$risk_level <- get_risk_level(LANG_TLS,CUT_OFFS,selected_indicador,bar_data$VAR)
      if (risk != "ALL") {
        bar_data <- bar_data %>% filter(risk_level == lang_label_tls(LANG_TLS,risk))
      }
      
      fig <- plot_ly(bar_data, type = 'bar',orientation = 'h',y = ~LUGAR,
                     x = ~VAR, name = lang_label_tls(LANG_TLS,selected_indicador),marker = list(color = "#5b9bd5"),text = ~VAR, textposition = 'inside',textangle = 0) %>%
        add_trace(x = ~other_PR, name = lang_label_tls(LANG_TLS,"general_other_ind"), marker = list(color = "rgba(0,0,0,0.2)"),text = ~other_PR, textposition = 'inside') %>%
        
        layout(xaxis = list(title = x_axis_title, tickfont = list(size = 12)), 
               barmode = 'stack',
               yaxis = list(title = y_axis_title, tickangle = 0, tickfont = list(size = 10))
        ) %>%
        layout(shapes = list(
          list(type = "rect",fillcolor = "#92d050", line = list(color = "#92d050"),
               opacity = 0.4, layer = "below",
               x0 = 0, x1 = get_risk_level_point_limit(CUT_OFFS,indicator,"LR"), xref = "x",
               y0 = -1, y1 = nrow(bar_data), yref = "y"),
          list(type = "rect",fillcolor = "#fec000", line = list(color = "#fec000"),
               opacity = 0.3, layer = "below",
               x0 = get_risk_level_point_limit(CUT_OFFS,indicator,"LR"), x1 = get_risk_level_point_limit(CUT_OFFS,indicator,"MR"), xref = "x",
               y0 = -1, y1 = nrow(bar_data), yref = "y"),
          list(type = "rect",fillcolor = "#e8132b", line = list(color = "#e8132b"),
               opacity = 0.2, layer = "below",
               x0 = get_risk_level_point_limit(CUT_OFFS,indicator,"MR"), x1 = get_risk_level_point_limit(CUT_OFFS,indicator,"HR"), xref = "x",
               y0 = -1, y1 = nrow(bar_data), yref = "y"),
          list(type = "rect",fillcolor = "#920000", line = list(color = "#920000"),
               opacity = 0.3, layer = "below",
               x0 = get_risk_level_point_limit(CUT_OFFS,indicator,"HR"), x1 = max_y_point, xref = "x",
               y0 = -1, y1 = nrow(bar_data), yref = "y")
        )) %>%
        layout(legend = list(orientation = 'h',x = 0, y = 50, bgcolor = 'rgba(0,0,0,0)', font = list(size = 10))) %>%
        config(displaylogo = FALSE) %>%
        config(modeBarButtonsToRemove = c("sendDataToCloud", "editInChartStudio",
                                          "pan2d","select2d","drawclosedpath",
                                          "drawline","drawrect","drawopenpath",
                                          "drawcircle","eraseshape",
                                          "zoomIn2d","zoomOut2d","toggleSpikelines","lasso2d","hoverCompareCartesian"))
    }
    
    
  }
  
  return(fig)
}


ind_plot_map_data <- function(LANG_TLS,ZERO_POB_LIST,CUT_OFFS,map_data,indicator,admin1_id,risk) {
  table_intervals <- c(
    get_risk_level_point_limit(CUT_OFFS,indicator,"LR"),
    get_risk_level_point_limit(CUT_OFFS,indicator,"MR"),
    get_risk_level_point_limit(CUT_OFFS,indicator,"HR"),
    get_risk_level_point_limit(CUT_OFFS,indicator,"VHR")
  )

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
  
  if (risk == "VHR") {
    legend_colors = c("#920000")
    legend_values = c(lang_label_tls(LANG_TLS,"cut_offs_VHR"))
    
  } else if (risk == "HR") {
    legend_colors = c("#e8132b")
    legend_values = c(lang_label_tls(LANG_TLS,"cut_offs_HR"))
    
  } else if (risk == "MR") {
    legend_colors = c("#fec000")
    legend_values = c(lang_label_tls(LANG_TLS,"cut_offs_MR"))
    
  } else if (risk == "LR") {
    legend_colors = c("#92d050")
    legend_values = c(lang_label_tls(LANG_TLS,"cut_offs_LR"))
    
  } else {
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
  }
  
  map_data <- st_as_sf(map_data)

  shape_label <- sprintf("<strong>%s</strong>, %s<br/>%s: %s<br/>%s: %s",
                         map_data$ADMIN2,
                         map_data$ADMIN1,
                         lang_label_tls(LANG_TLS,"risk_points"),
                         map_data$PR,
                         lang_label_tls(LANG_TLS,"risk_level"),
                         map_data$risk_level_word
  ) %>% lapply(HTML)
  
  # MAPA
  map <- leaflet(map_data,options = leafletOptions(doubleClickZoom = T, attributionControl = F, zoomSnap = 0.1, zoomDelta = 0.1)) %>%
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
    addLegend(title = lang_label_tls(LANG_TLS,"legend_risk_class"),colors = legend_colors,labels = legend_values, opacity = 0.5, position = 'topright')
  
  return(map)
  
}



ind_prep_box_data <- function(LANG_TLS,CUT_OFFS,data,indicator,admin1_id) {
  var_to_summarise <- case_when(
    indicator == "total_score" ~ "total_score",
    indicator == "immunity_score" ~ "immunity_score",
    indicator == "survaillance_score" ~ "survaillance_score",
    indicator == "determinants_score" ~ "determinants_score",
    indicator == "outbreaks_score" ~ "outbreaks_score"
  )
  
  prep_data <- data %>% rename("PR" = var_to_summarise)
  if (admin1_id == 0) {
    prep_data <- prep_data %>% filter(!is.na(PR)) %>% select(LUGAR = ADMIN2,PR)
  } else {
    prep_data <- prep_data %>% filter(`ADMIN1 GEO_ID` == admin1_id) %>% filter(!is.na(PR)) %>% select(LUGAR = ADMIN2,PR)
  }
  prep_data$risk_level <- get_risk_level(LANG_TLS,CUT_OFFS,indicator,prep_data$PR)

  
  return(prep_data)
}


get_box_text <- function(dat_box,total,risk_type) {
  color_p <- case_when(
    risk_type == "LR" ~ "#9ced8c",
    risk_type == "MR" ~ "#ffd683",
    risk_type == "HR" ~ "#ffaaa7",
    risk_type == "VHR" ~ "#ad4d4e"
  )
  
  txt <- HTML(paste0("<div>",cFormat(dat_box,0),"<p style='color:",color_p,";'>(",cFormat(dat_box/total*100,1),"%)</p></div>"))
  return(txt)
}


datos_boxes <- function(LANG_TLS,ind_data) {
  
  ind_data$CANTIDAD <- 1
  dat_LR <- sum(ind_data$CANTIDAD[ind_data$risk_level == lang_label_tls(LANG_TLS,"LR")])
  dat_MR <- sum(ind_data$CANTIDAD[ind_data$risk_level == lang_label_tls(LANG_TLS,"MR")])
  dat_HR <- sum(ind_data$CANTIDAD[ind_data$risk_level == lang_label_tls(LANG_TLS,"HR")])
  dat_VHR <- sum(ind_data$CANTIDAD[ind_data$risk_level == lang_label_tls(LANG_TLS,"VHR")])
  dat_total <- nrow(ind_data)
  
  if (is.na(dat_LR)) {dat_LR = 0.0}
  if (is.na(dat_MR)) {dat_MR = 0.0}
  if (is.na(dat_HR)) {dat_HR = 0.0}
  if (is.na(dat_VHR)) {dat_VHR = 0.0}
  if (is.na(dat_total)) {dat_total = 0.0}
  
  res <- c(dat_LR,dat_MR,dat_HR,dat_VHR,dat_total)
  
  return(res)
}

ind_rename <- function(selected_ind) {
  return(
    case_when(
      lang_label("menuitem_general_label") == selected_ind ~ "GENERAL",
      lang_label("menuitem_immunity") == selected_ind ~ "immunity_score",
      lang_label("menuitem_survaillance") == selected_ind ~ "survaillance_score",
      lang_label("menuitem_determinants") == selected_ind ~ "determinants_score",
      lang_label("menuitem_outbreaks") == selected_ind ~ "outbreaks_score"
    )
  )
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


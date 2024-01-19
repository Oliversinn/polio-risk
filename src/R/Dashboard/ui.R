# AUTHORSHIP ----

# Pan American Health Organization
# Author: Oliver Mazariegos
# Last Update: 2023-10-09
# R 4.3.1

# UI ----
fluidPage(
  ## HEADER ----
  fluidRow(
    box(width = 12, background = "maroon",
        HTML(paste0('<center><div style = "text-align: left; padding-left: 30px; padding-right: 30px; padding-top: 10px;">
                        <img src="',lang_label("logo_org"),'" height="35"> <img id="country_flag" style = "right: 30px !important; position: absolute; padding-top: 1px; padding-bottom: 1px; padding-right: 1px; padding-left: 1px; margin-bottom: 10px; background-color: white;" src="country_flag.png" height="50">
                        </div> <h2>',lang_label("dashboard_title"),'<br> <b>',toupper(COUNTRY_NAME),'</b></h2> </center>'))
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
        ### REPORTS ----
        div(
          downloadButton("download_report_word",lang_label("download_report_word"), icon=icon("file-lines"), class = "button_word"),
          downloadButton("download_report_html",lang_label("download_report_html"), icon=icon("file-lines"), class = "button_html")
        ),
        
        ### GLOBAL FILTERS ----
        #### ADMIN1 ----
        selectInput(
          "admin1_filter", 
          label = paste0(lang_label("general_select_admin1"), ":"),
          choices = admin1_list, 
          selected = admin1_list[1]
        ),
        bsTooltip(
          "admin1_filter", 
          lang_label("tooltip_select_admin1"), 
          placement = "right", trigger = "hover",options = NULL
        ),
        
        #### POPULATION15 ----
        selectInput("population15_filter",
                    label = paste0(lang_label("population15_filter"), ":"),
                    choices = c(
                      lang_label("filter_all"),
                      lang_label("less_than_100000"),
                      lang_label("greater_than_100000")
                    ),
                    selected = lang_label("filter_all")
        ), 
        bsTooltip(
          "population15_filter", 
          lang_label("tooltip_population15_filter"), 
          placement = "right", trigger = "hover",options = NULL
        ),
        
        #### CUT OFF SELECTOR ----
        selectInput("indicadores_select_risk",label = paste0(lang_label("general_select_risk"),":"),
                    choices = c(toupper(lang_label("rep_label_all")),
                                lang_label("cut_offs_VHR"),
                                lang_label("cut_offs_HR"),
                                lang_label("cut_offs_MR"),
                                lang_label("cut_offs_LR")
                    )
        ),
        bsTooltip("indicadores_select_risk", lang_label("tooltip_select_risk"), placement = "right", trigger = "hover",options = NULL),

        
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
                        lang_label("menuitem_general_label"),
                        lang_label("general_select_immunity"),
                        lang_label("general_select_surveillance"),
                        lang_label("general_select_determinants"),
                        lang_label("general_select_outbreaks")
                      ),
                      selected = lang_label("menuitem_general_label")
          ),
          bsTooltip("indicadores_select_indicador", lang_label("tooltip_select_ind"), placement = "right", trigger = "hover",options = NULL),
          
        ),
        
        ### IMMUNITY SCORES ----
        menuItem(
          text = lang_label("menuitem_immunity"),
          tabName = "IMMUNITY",
          icon = icon("syringe")
        ),
        
        
        ### SURVEILLANCE SCORES ----
        menuItem(
          text = lang_label("menuitem_surveillance"),
          tabName = "SURVEILLANCE",
          icon = icon("eye",class = "fa-solid fa-eye")
        ),
        
        ### DETERMINANTS SCORES ----
        menuItem(
          text = lang_label("menuitem_determinants"),
          tabName = "DETERMINANTS",
          icon = icon("person-dots-from-line")
        ),
        
        ### OUTBREAKS SCORES ----
        menuItem(
          text = lang_label("menuitem_outbreaks"),
          tabName = "OUTBREAKS",
          icon = icon("viruses")
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
          ### TAB GENERAL SCORES ----
          tabItem(
            tabName = "GENERAL",
            h2(textOutput("indicadores_title")),
            br(),
            
            #### VALUE BOXES ----
            fluidRow(
              valueBoxOutput("ind_box_4",width = 3),
              valueBoxOutput("ind_box_3",width = 3),
              valueBoxOutput("ind_box_2",width = 3),
              valueBoxOutput("ind_box_1",width = 3)
            ),
            
            #### ADMIN1 == ALL ----
            conditionalPanel(
              paste0('input.admin1_filter == "',toupper(lang_label("rep_label_all")),'"'),
              fluidRow(
                box(
                  width = 12,
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  title = textOutput("indicadores_title_map_box"),
                  tabBox(
                    width = 12,
                    height = NULL,
                    ###### MAP TOTALS----
                    tabPanel(
                      title = lang_label("menuitem_general_label"), icon = icon("map",class = "fa-solid fa-map"),
                      shinycssloaders::withSpinner(leafletOutput("indicadores_plot_map",height = 600),color = "#1c9ad6", type = "8", size = 0.5),
                      br(),div(style = "text-align: center;",downloadButton(outputId = "dl_indicadores_plot_map",lang_label("button_download_map"),icon = icon('camera')))
                    ),
                    ###### MAP IMMUNITY----
                    tabPanel(
                      title = lang_label("immunity_score"), icon = icon("map",class = "fa-solid fa-map"),
                      shinycssloaders::withSpinner(leafletOutput("indicadores_plot_map_immunity",height = 600),color = "#1c9ad6", type = "8", size = 0.5),
                      br(),div(style = "text-align: center;",downloadButton(outputId = "dl_indicadores_plot_map_immunity",lang_label("button_download_map"),icon = icon('camera')))
                    ),
                    ###### MAP SURVEILLANCE ----
                    tabPanel(
                      title = lang_label("surveillance_score"), icon = icon("map",class = "fa-solid fa-map"),
                      shinycssloaders::withSpinner(leafletOutput("indicadores_plot_map_surveillance",height = 600),color = "#1c9ad6", type = "8", size = 0.5),
                      br(),div(style = "text-align: center;",downloadButton(outputId = "dl_indicadores_plot_map_surveillance",lang_label("button_download_map"),icon = icon('camera')))
                    ),
                    ###### MAP DETERMINANTS ----
                    tabPanel(
                      title = lang_label("determinants_score"), icon = icon("map",class = "fa-solid fa-map"),
                      shinycssloaders::withSpinner(leafletOutput("indicadores_plot_map_determinants",height = 600),color = "#1c9ad6", type = "8", size = 0.5),
                      br(),div(style = "text-align: center;",downloadButton(outputId = "dl_indicadores_plot_map_determinants",lang_label("button_download_map"),icon = icon('camera')))
                    ),
                    ###### MAP OUTBREAKS ----
                    tabPanel(
                      title = lang_label("outbreaks_score"), icon = icon("map",class = "fa-solid fa-map"),
                      shinycssloaders::withSpinner(leafletOutput("indicadores_plot_map_outbreaks",height = 600),color = "#1c9ad6", type = "8", size = 0.5),
                      br(),div(style = "text-align: center;",downloadButton(outputId = "dl_indicadores_plot_map_outbreaks",lang_label("button_download_map"),icon = icon('camera')))
                    ),
                    ###### TABLE ----
                    tabPanel(
                      title = lang_label("button_datatable"),icon = icon("table"),
                      shinycssloaders::withSpinner(DT::dataTableOutput("indicadores_table"),color = "#1c9ad6", type = "8", size = 0.5)
                    )
                  )
                )
              )
            ),
            
            #### ADMIN1 != ALL ----
            conditionalPanel(
              paste0('input.admin1_filter != "',toupper(lang_label("rep_label_all")),'"'),
              fluidRow(
                box(
                  width = 6,
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  title = textOutput("indicadores_title_map_box_2"),
                  tabBox(
                    width = 12,
                    height = NULL,
                    ##### MAP TOTALS ----
                    tabPanel(
                      title = lang_label("total_score"), icon = icon("map",class = "fa-solid fa-map"),
                      shinycssloaders::withSpinner(leafletOutput("indicadores_plot_map_2",height = 600),color = "#1c9ad6", type = "8", size = 0.5),
                      br(),div(style = "text-align: center;",downloadButton(outputId = "dl_indicadores_plot_map_2",lang_label("button_download_map"),icon = icon('camera')))
                    ),
                    ##### MAP IMMUNITY ----
                    tabPanel(
                      title = lang_label("immunity_score"), icon = icon("map",class = "fa-solid fa-map"),
                      shinycssloaders::withSpinner(leafletOutput("indicadores_plot_map_immunity_2",height = 600),color = "#1c9ad6", type = "8", size = 0.5),
                      br(),div(style = "text-align: center;",downloadButton(outputId = "dl_indicadores_plot_map_immunity_2",lang_label("button_download_map"),icon = icon('camera')))
                    ),
                    ##### MAP SURVEILLANCE ----
                    tabPanel(
                      title = lang_label("surveillance_score"), icon = icon("map",class = "fa-solid fa-map"),
                      shinycssloaders::withSpinner(leafletOutput("indicadores_plot_map_surveillance_2",height = 600),color = "#1c9ad6", type = "8", size = 0.5),
                      br(),div(style = "text-align: center;",downloadButton(outputId = "dl_indicadores_plot_map_surveillance_2",lang_label("button_download_map"),icon = icon('camera')))
                    ),
                    ##### MAP DETERMINANTS ----
                    tabPanel(
                      title = lang_label("determinants_score"), icon = icon("map",class = "fa-solid fa-map"),
                      shinycssloaders::withSpinner(leafletOutput("indicadores_plot_map_determinants_2",height = 600),color = "#1c9ad6", type = "8", size = 0.5),
                      br(),div(style = "text-align: center;",downloadButton(outputId = "dl_indicadores_plot_map_determinants_2",lang_label("button_download_map"),icon = icon('camera')))
                    ),
                    ##### MAP OUTBREAKS ----
                    tabPanel(
                      title = lang_label("outbreaks_score"), icon = icon("map",class = "fa-solid fa-map"),
                      shinycssloaders::withSpinner(leafletOutput("indicadores_plot_map_outbreaks_2",height = 600),color = "#1c9ad6", type = "8", size = 0.5),
                      br(),div(style = "text-align: center;",downloadButton(outputId = "dl_indicadores_plot_map_outbreaks_2",lang_label("button_download_map"),icon = icon('camera')))
                    ),
                    ##### TABLE ----
                    tabPanel(
                      title = lang_label("button_datatable"),icon = icon("table"),
                      shinycssloaders::withSpinner(DT::dataTableOutput("indicadores_table_2"),color = "#1c9ad6", type = "8", size = 0.5)
                    )
                  )
                ),
                box(
                  width = 6,
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  title = textOutput("indicadores_title_bar_box"),
                  tabBox(
                    width = 12,
                    height = NULL,
                    ##### TOTAL BAR PLOT ----
                    tabPanel(
                      title = lang_label("general_title_plot_bar"),icon = icon("bar-chart"),
                      selectInput(
                        "general_title_plot_bar_filter", 
                        label = "", 
                        choices = c(
                          lang_label("population_pfa_filter"),
                          lang_label("population_pfa_no_filter")
                        ),
                      ),
                      shinycssloaders::withSpinner(plotlyOutput("indicadores_plot_bar",height = 595),color = "#1c9ad6", type = "8", size = 0.5)
                    ),
                    ##### PER SCORE BARPLOT ----
                    tabPanel(
                      title = lang_label("general_title_plot_multibar"),icon = icon("square-check"),
                      selectInput(
                        "general_title_plot_multibar_filter", 
                        label = "", 
                        choices = c(
                          lang_label("population_pfa_filter"),
                          lang_label("population_pfa_no_filter")
                        ),
                      ),
                      shinycssloaders::withSpinner(plotlyOutput("indicadores_plot_multibar",height = 595),color = "#1c9ad6", type = "8", size = 0.5)
                    )
                  )
                )
              )
            ),
            #### SCORE CHEAT SHEET ----
            fluidRow(
              column(
                width = 12,
                box(
                  width = 12,
                  solidHeader = TRUE,collapsible = TRUE,title = lang_label("general_title_limits_table"),
                  selectInput(
                    "general_limits_table_filter", 
                    label = "", 
                    choices = c(
                      lang_label("population_pfa_filter"),
                      lang_label("population_pfa_no_filter")
                    ),
                  ),
                  shinycssloaders::withSpinner(DT::dataTableOutput("indicadores_rangos_table"),color = "#1c9ad6", type = "8", size = 0.3),
                  div(
                    class = "text-center",
                    downloadButton("download_cutoffs_excel",lang_label("download_cutoffs_excel"), icon=icon("file-lines"), class = "button_word")
                  )
                )
              )
            )
            
          ),
          
          ### TAB IMMUNITY SCORES ----
          tabItem(
            tabName = "IMMUNITY",
            h2(lang_label("menuitem_immunity")),
            br(),
            
            fluidRow(
              #### MAP ----
              box(
                width = 7,
                solidHeader = TRUE,
                collapsible = TRUE,
                title = textOutput("inmunidad_title_map_box"),
                tabBox(
                  width = 12,
                  height = NULL,
                  ##### TOTLA PR ----
                  tabPanel(
                    title = lang_label("total_pr"),
                    icon = icon("square-check"),
                    shinycssloaders::withSpinner(leafletOutput("inmunidad_map_total",height = 600),color = "#1c9ad6", type = "8", size = 0.5),
                    br(),div(style = "text-align: center;",downloadButton(outputId = "dl_inmunidad_map_total",lang_label("button_download_map"),icon = icon('camera')))
                  ),
                  
                  ##### POLIO COB ----
                  tabPanel(
                    title = lang_label("immunity_polio_cob"),icon = icon("syringe"),
                    column(width = 12,
                           selectInput("radio_inmunidad_cob_1", label = "", 
                                       choices = c(
                                         paste(lang_label("vac_coverage"),YEAR_5),
                                         paste(lang_label("vac_coverage"),YEAR_4),
                                         paste(lang_label("vac_coverage"),YEAR_3),
                                         paste(lang_label("vac_coverage"),YEAR_2),
                                         paste(lang_label("vac_coverage"),YEAR_1)
                                       ),
                           ),style = "z-index:2000;"),
                    shinycssloaders::withSpinner(leafletOutput("inmunidad_map_cob_1",height = 600),color = "#1c9ad6", type = "8", size = 0.5),
                    br(),div(style = "text-align: center;",downloadButton(outputId = "dl_inmunidad_map_cob_1",lang_label("button_download_map"),icon = icon('camera')))
                  ),
                  
                  ##### IPV2 COB ----
                  tabPanel(
                    title = lang_label("immunity_ipv2_cob"),icon = icon("syringe"),
                    shinycssloaders::withSpinner(leafletOutput("inmunidad_map_cob_2",height = 600),color = "#1c9ad6", type = "8", size = 0.5),
                    br(),div(style = "text-align: center;",downloadButton(outputId = "dl_inmunidad_map_cob_2",lang_label("button_download_map"),icon = icon('camera')))
                  ),
                  
                  ##### EFFECTIVE CAMPAIGN ----
                  tabPanel(
                    title = lang_label("immunity_effective_cob"),icon = icon("question-circle"),
                    p(style = "text-align: center;",lang_label("immunity_effective_cob_text")),
                    shinycssloaders::withSpinner(leafletOutput("inmunidad_map_effective",height = 600),color = "#1c9ad6", type = "8", size = 0.5),
                    br(),div(style = "text-align: center;",downloadButton(outputId = "dl_inmunidad_map_effective",lang_label("button_download_map"),icon = icon('camera')))
                  )
                )
              ),
              
              #### PIE CHART ----
              box(
                width = 5,
                solidHeader = TRUE,
                collapsible = TRUE,
                title = textOutput("inmunidad_title_pie_box"),
                
                tabBox(
                  width = 12,
                  height = NULL,
                  tabPanel(
                    title = lang_label("button_plot"),icon = icon("pie-chart"),
                    br(),
                    shinycssloaders::withSpinner(plotlyOutput("inmunidad_plot_pie", height = 600),color = "#1c9ad6", type = "8", size = 0.5)
                  ),
                  tabPanel(
                    title = lang_label("button_datatable"),icon = icon("table"),
                    br(),
                    shinycssloaders::withSpinner(DT::dataTableOutput("inmunidad_table_dist"),color = "#1c9ad6", type = "8", size = 0.5)
                  )
                )
              )
            ),
            #### DATATABLE ----
            fluidRow(
              box(width = 12,
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  title = textOutput("inmunidad_title_data_box"),
                  column(width = 12,shinycssloaders::withSpinner(DT::dataTableOutput("inmunidad_table"),color = "#1c9ad6", type = "8", size = 0.5))
              )
            ),
            
            #### SCORE CHEAT SHEET ----
            fluidRow(
              box(
                width = 12,
                olidHeader = TRUE,collapsible = TRUE,title = lang_label("general_title_limits_table"),
                selectInput(
                  "immunity_limits_table_filter", 
                  label = "", 
                  choices = c(
                    lang_label("population_pfa_filter"),
                    lang_label("population_pfa_no_filter")
                  ),
                ),
                shinycssloaders::withSpinner(DT::dataTableOutput("inmu_rangos_table"),color = "#1c9ad6", type = "8", size = 0.3)
              )
              
            )
          ),
          
          ### TAB SURVEILLANCE ----
          tabItem(
            tabName = "SURVEILLANCE",
            h2(lang_label("menuitem_surveillance")),
            br(),
            
            fluidRow(
              
              #### MAP ----
              box(
                width = 7,
                solidHeader = TRUE,
                collapsible = TRUE,
                title = textOutput("surveillance_title_map_box"),
                tabBox(
                  width = 12,
                  height = NULL,
                  
                  ##### TOTAL PR ----
                  tabPanel(
                    title = lang_label("total_pr"),icon = icon("square-check"),
                    shinycssloaders::withSpinner(leafletOutput("calidad_map_total",height = 600),color = "#1c9ad6", type = "8", size = 0.5),
                    br(),div(style = "text-align: center;",downloadButton(outputId = "dl_surveillance_map_total",lang_label("button_download_map"),icon = icon('camera')))
                  ),
                  
                  ##### REPORTING UNITS ----
                  tabPanel(
                    title = lang_label("surveillance_reporting_units"),icon = icon("house-medical"),
                    shinycssloaders::withSpinner(leafletOutput("calidad_map_2",height = 600),color = "#1c9ad6", type = "8", size = 0.5),
                    br(),div(style = "text-align: center;",downloadButton(outputId = "dl_calidad_map_2",lang_label("button_download_map"),icon = icon('camera')))
                  ),
                  
                  ##### PFA RATE ----
                  tabPanel(
                    title = lang_label("surveillance_pfa_rate"),icon = icon("calculator"),
                    p(style = "text-align: center;",lang_label("surveillance_pfa_population_text")),
                    shinycssloaders::withSpinner(leafletOutput("calidad_map_3",height = 600),color = "#1c9ad6", type = "8", size = 0.5),
                    br(),div(style = "text-align: center;",downloadButton(outputId = "dl_calidad_map_3",lang_label("button_download_map"),icon = icon('camera')))
                  ),
                  
                  ##### PFA NOTIFICATION ----
                  tabPanel(
                    title = lang_label("surveillance_pfa_notification"),icon = icon("house-medical-circle-check", class = "fa-solid fa-house-medical-circle-check"),
                    p(style = "text-align: center;",lang_label("surveillance_pfa_population_text")),
                    shinycssloaders::withSpinner(leafletOutput("calidad_map_4",height = 600),color = "#1c9ad6", type = "8", size = 0.5),
                    br(),div(style = "text-align: center;",downloadButton(outputId = "dl_calidad_map_4",lang_label("button_download_map"),icon = icon('camera')))
                  ),
                  
                  ##### PFA INVESTIGTED ----
                  tabPanel(
                    title = lang_label("surveillance_pfa_investigated"),icon = icon("search-plus"),
                    p(style = "text-align: center;",lang_label("surveillance_pfa_population_text")),
                    shinycssloaders::withSpinner(leafletOutput("calidad_map_5",height = 600),color = "#1c9ad6", type = "8", size = 0.5),
                    br(),div(style = "text-align: center;",downloadButton(outputId = "dl_calidad_map_5",lang_label("button_download_map"),icon = icon('camera')))
                  ),
                  
                  ##### SUITABLE SAMPLES ----
                  tabPanel(
                    title = lang_label("surveillance_suitable_samples"),icon = icon("vials"),
                    p(style = "text-align: center;",lang_label("surveillance_pfa_population_text")),
                    shinycssloaders::withSpinner(leafletOutput("calidad_map_6",height = 600),color = "#1c9ad6", type = "8", size = 0.5),
                    br(),div(style = "text-align: center;",downloadButton(outputId = "dl_calidad_map_6",lang_label("button_download_map"),icon = icon('camera')))
                  ),
                  
                  ##### FOLLOWUPS ----
                  tabPanel(
                    title = lang_label("surveillance_followups"),icon = icon("notes-medical"),
                    p(style = "text-align: center;",lang_label("surveillance_pfa_population_text")),
                    shinycssloaders::withSpinner(leafletOutput("calidad_map_7",height = 600),color = "#1c9ad6", type = "8", size = 0.5),
                    br(),div(style = "text-align: center;",downloadButton(outputId = "dl_calidad_map_7",lang_label("button_download_map"),icon = icon('camera')))
                  ),
                  
                  ##### ACTIVE SEARCH ----
                  tabPanel(
                    title = lang_label("surveillance_active_search"),icon = icon("magnifying-glass-location"),
                    p(style = "text-align: center;",lang_label("surveillance_active_search_text")),
                    shinycssloaders::withSpinner(leafletOutput("calidad_map_8",height = 600),color = "#1c9ad6", type = "8", size = 0.5),
                    br(),div(style = "text-align: center;",downloadButton(outputId = "dl_calidad_map_8",lang_label("button_download_map"),icon = icon('camera')))
                  )
                )
              ),
              
              #### PIE CHART ----
              box(
                width = 5,
                solidHeader = TRUE,
                collapsible = TRUE,
                title = textOutput("surveillance_title_pie_box"),
                
                tabBox(
                  width = 12,
                  height = NULL,
                  tabPanel(
                    title = lang_label("button_plot"),icon = icon("pie-chart"),
                    br(),
                    shinycssloaders::withSpinner(plotlyOutput("calidad_plot_pie", height = 600),color = "#1c9ad6", type = "8", size = 0.5)
                  ),
                  tabPanel(
                    title = lang_label("button_datatable"),icon = icon("table"),
                    br(),
                    shinycssloaders::withSpinner(DT::dataTableOutput("calidad_table_dist"),color = "#1c9ad6", type = "8", size = 0.5)
                  )
                )
              )
            ),
            
            #### DATATABLE ----
            fluidRow(
              box(
                width = 12,
                solidHeader = TRUE,
                collapsible = TRUE,
                title = textOutput("calidad_title_data_box"),
                column(width = 12,shinycssloaders::withSpinner(DT::dataTableOutput("calidad_table"),color = "#1c9ad6", type = "8", size = 0.5))
              )
            ),
            
            #### SCORE DATASHEET ----
            fluidRow(
              box(
                width = 12,
                olidHeader = TRUE,collapsible = TRUE,title = lang_label("general_title_limits_table"),
                selectInput(
                  "surveillance_limits_table_filter", 
                  label = "", 
                  choices = c(
                    lang_label("population_pfa_filter"),
                    lang_label("population_pfa_no_filter")
                  ),
                ),
                shinycssloaders::withSpinner(DT::dataTableOutput("surveillance_rangos_table"),color = "#1c9ad6", type = "8", size = 0.3)
              )
            )
          ),
          
          ### TAB DETERMINANTS ----
          tabItem(
            tabName = "DETERMINANTS",
            h2(lang_label("determinants_title")),
            br(),
            
            fluidRow(
              #### MAP ----
              box(
                width = 7,
                solidHeader = TRUE,
                collapsible = TRUE,
                title = textOutput("determinants_map_box_title"),
                tabBox(
                  width = 12,
                  height = NULL,
                  ##### TOTAL PR ----
                  tabPanel(
                    title = lang_label("total_pr"),
                    icon = icon("square-check"),
                    shinycssloaders::withSpinner(leafletOutput("determinants_map_total",height = 600),color = "#1c9ad6", type = "8", size = 0.5),
                    br(),div(style = "text-align: center;",downloadButton(outputId = "dl_determinants_map_total",lang_label("button_download_map"),icon = icon('camera')))
                  ),
                  
                  ##### DRINKING WATER ----
                  tabPanel(
                    title = lang_label("determinants_drinking_water"),
                    icon = icon("bottle-water"),
                    shinycssloaders::withSpinner(leafletOutput("determinants_map_water",height = 600),color = "#1c9ad6", type = "8", size = 0.5),
                    br(),div(style = "text-align: center;",downloadButton(outputId = "dl_determinants_map_water",lang_label("button_download_map"),icon = icon('camera')))
                  ),
                  
                  ##### SANITATION SERVICES ----
                  tabPanel(
                    title = lang_label("determinants_sanitation_services"),
                    icon = icon("hand-holding-droplet"),
                    shinycssloaders::withSpinner(leafletOutput("determinants_map_sanitation",height = 600),color = "#1c9ad6", type = "8", size = 0.5),
                    br(),div(style = "text-align: center;",downloadButton(outputId = "dl_determinants_map_sanitation",lang_label("button_download_map"),icon = icon('camera')))
                  )
                )
              ),
              
              #### PIE CHART ----
              box(
                width = 5,
                solidHeader = TRUE,
                collapsible = TRUE,
                title = textOutput("determinants_title_pie_box"),
                
                tabBox(
                  width = 12,
                  height = NULL,
                  tabPanel(
                    title = lang_label("button_plot"),icon = icon("pie-chart"),
                    br(),
                    shinycssloaders::withSpinner(plotlyOutput("determinants_plot_pie", height = 600),color = "#1c9ad6", type = "8", size = 0.5)
                  ),
                  tabPanel(
                    title = lang_label("button_datatable"),icon = icon("table"),
                    br(),
                    shinycssloaders::withSpinner(DT::dataTableOutput("determinants_table_dist"),color = "#1c9ad6", type = "8", size = 0.5)
                  )
                )
              )
            ),
            
            #### DATATABLE ----
            fluidRow(
              box(
                width = 12,
                solidHeader = TRUE,
                collapsible = TRUE,
                title = textOutput("determinants_title_data_box"),
                column(width = 12,shinycssloaders::withSpinner(DT::dataTableOutput("determinants_table"),color = "#1c9ad6", type = "8", size = 0.5))
              )
            ),
            
            #### SCORE CHEAT SHEET ----
            fluidRow(
              box(
                width = 12,
                olidHeader = TRUE,collapsible = TRUE,title = lang_label("general_title_limits_table"),
                selectInput(
                  "determinants_limits_table_filter", 
                  label = "", 
                  choices = c(
                    lang_label("population_pfa_filter"),
                    lang_label("population_pfa_no_filter")
                  ),
                ),
                shinycssloaders::withSpinner(DT::dataTableOutput("determinants_rangos_table"),color = "#1c9ad6", type = "8", size = 0.3)
              )
            )
          ),
          
          ### TAB OUTBREAKS ----
          tabItem(
            tabName = "OUTBREAKS",
            h2(lang_label("outbreaks_title")),
            br(),
            
            fluidRow(
              #### MAP ----
              box(
                width = 7,
                solidHeader = TRUE,
                collapsible = TRUE,
                title = textOutput("outbreaks_map_box_title"),
                tabBox(
                  width = 12,
                  height = NULL,
                  ##### TOTAL PR ----
                  tabPanel(
                    title = lang_label("total_pr"),
                    icon = icon("square-check"),
                    shinycssloaders::withSpinner(leafletOutput("outbreaks_map_total",height = 600),color = "#1c9ad6", type = "8", size = 0.5),
                    br(),div(style = "text-align: center;",downloadButton(outputId = "dl_outbreaks_map_total",lang_label("button_download_map"),icon = icon('camera')))
                  ),
                  tabPanel(
                    title = lang_label("outbreaks_coverage"),icon = icon("syringe"),
                    column(width = 12,
                           selectInput("outbreaks_disease_filter", label = "", 
                                       choices = c(
                                         paste(lang_label("outbreaks_polio")),
                                         paste(lang_label("outbreaks_measles")),
                                         paste(lang_label("outbreaks_rubella")),
                                         paste(lang_label("outbreaks_diphtheria")),
                                         paste(lang_label("outbreaks_yellow_fever")),
                                         paste(lang_label("outbreaks_tetanus"))
                                       ),
                           ),style = "z-index:2000;"),
                    shinycssloaders::withSpinner(leafletOutput("outbreaks_disease_map",height = 600),color = "#1c9ad6", type = "8", size = 0.5),
                    br(),div(style = "text-align: center;",downloadButton(outputId = "dl_outbreaks_disease_map",lang_label("button_download_map"),icon = icon('camera')))
                  )
                )
              ),
              
              #### PIE CHART ----
              box(
                width = 5,
                solidHeader = TRUE,
                collapsible = TRUE,
                title = textOutput("outbreaks_title_pie_box"),
                
                tabBox(
                  width = 12,
                  height = NULL,
                  tabPanel(
                    title = lang_label("button_plot"),icon = icon("pie-chart"),
                    br(),
                    shinycssloaders::withSpinner(plotlyOutput("outbreaks_plot_pie", height = 600),color = "#1c9ad6", type = "8", size = 0.5)
                  ),
                  tabPanel(
                    title = lang_label("button_datatable"),icon = icon("table"),
                    br(),
                    shinycssloaders::withSpinner(DT::dataTableOutput("outbreaks_table_dist"),color = "#1c9ad6", type = "8", size = 0.5)
                  )
                )
              )
            ),
            
            #### DATATABLE ----
            fluidRow(
              box(width = 12,
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  title = textOutput("outbreaks_title_data_box"),
                  column(width = 12,shinycssloaders::withSpinner(DT::dataTableOutput("outbreaks_table"),color = "#1c9ad6", type = "8", size = 0.5))
              )
            ),
            
            #### SCORE CHEAT SHEET ----
            fluidRow(
              box(
                width = 12,
                olidHeader = TRUE,collapsible = TRUE,title = lang_label("general_title_limits_table"),
                selectInput(
                  "outbreaks_limits_table_filter", 
                  label = "", 
                  choices = c(
                    lang_label("population_pfa_filter"),
                    lang_label("population_pfa_no_filter")
                  ),
                ),
                shinycssloaders::withSpinner(DT::dataTableOutput("outbreaks_rangos_table"),color = "#1c9ad6", type = "8", size = 0.3)
              )
            )
          )
          ### NEW TABITEM ----
        )
      )
    )
    
  )
)

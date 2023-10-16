# AUTORSHIP ----
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
          ### TAB GENERAL SCORES ----
          tabItem(
            tabName = "GENERAL",
            h2(textOutput("indicadores_title")),
            br(),
            
            #### VALUE BOXES ----
            fluidRow(
              valueBoxOutput("ind_box_1",width = 3),
              valueBoxOutput("ind_box_2",width = 3),
              valueBoxOutput("ind_box_3",width = 3),
              valueBoxOutput("ind_box_4",width = 3)
            ),
            
            #### ADMIN1 == ALL ----
            conditionalPanel(
              paste0('input.indicadores_select_admin1 == "',toupper(lang_label("rep_label_all")),'"'),
              fluidRow(
                box(
                  width = 12,
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  title = textOutput("indicadores_title_map_box"),
                  tabBox(
                    width = 12,
                    height = NULL,
                    ###### MAP ----
                    tabPanel(
                      title = lang_label("button_map"), icon = icon("map",class = "fa-solid fa-map"),
                      shinycssloaders::withSpinner(leafletOutput("indicadores_plot_map",height = 600),color = "#1c9ad6", type = "8", size = 0.5),
                      br(),div(style = "text-align: center;",downloadButton(outputId = "dl_indicadores_plot_map",lang_label("button_download_map"),icon = icon('camera')))
                    ),
                    ###### TABLE ----
                    tabPanel(
                      title = lang_label("button_datatable"),icon = icon("table"),
                      shinycssloaders::withSpinner(dataTableOutput("indicadores_table",height = 620),color = "#1c9ad6", type = "8", size = 0.5)
                    )
                  )
                )
              )
            ),
            
            #### ADMIN1 != ALL ----
            conditionalPanel(
              paste0('input.indicadores_select_admin1 != "',toupper(lang_label("rep_label_all")),'"'),
              fluidRow(
                box(
                  width = 6,
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  title = textOutput("indicadores_title_map_box_2"),
                  tabBox(
                    width = 12,
                    height = NULL,
                    ##### MAP ----
                    tabPanel(
                      title = lang_label("button_map"), icon = icon("map",class = "fa-solid fa-map"),
                      shinycssloaders::withSpinner(leafletOutput("indicadores_plot_map_2",height = 600),color = "#1c9ad6", type = "8", size = 0.5),
                      br(),div(style = "text-align: center;",downloadButton(outputId = "dl_indicadores_plot_map_2",lang_label("button_download_map"),icon = icon('camera')))
                    ),
                    ##### TABLE ----
                    tabPanel(
                      title = lang_label("button_datatable"),icon = icon("table"),
                      shinycssloaders::withSpinner(dataTableOutput("indicadores_table_2",height = 620),color = "#1c9ad6", type = "8", size = 0.5)
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
                      shinycssloaders::withSpinner(plotlyOutput("indicadores_plot_bar",height = 595),color = "#1c9ad6", type = "8", size = 0.5)
                    ),
                    ##### PER SCORE BARPLOT ----
                    tabPanel(
                      title = lang_label("general_title_plot_multibar"),icon = icon("square-check"),
                      shinycssloaders::withSpinner(plotlyOutput("indicadores_plot_multibar",height = 595),color = "#1c9ad6", type = "8", size = 0.5)
                    )
                  )
                )
              )
            ),
            
            fluidRow(
              column(
                width = 6, offset = 3,
                box(
                  width = 12,
                  solidHeader = TRUE,collapsible = TRUE,title = lang_label("general_title_limits_table"),
                  shinycssloaders::withSpinner(dataTableOutput("indicadores_rangos_table"),color = "#1c9ad6", type = "8", size = 0.3)
                )
              )
            )
            
          )
        )
      )
    )
    
  )
)
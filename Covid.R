# Initial required package 
#if (!require("dplyr")) install.packages("dplyr"); library(dplyr)


#options(shiny.trace = TRUE)  

# Setting working directory
#setwd(dirname(sys.frame(1)$ofile))
#options(shiny.maxRequestSize = 300*1024^2)

#List all required packages - dplyr
packages = c("shiny", "shinydashboard", "shinyjs", "DT",  "readr", "shinycssloaders", "shinybusy", "reshape2",
             "shinyalert","ggplot2", "htmlwidgets","plotly","shinyWidgets", "tidyverse","countrycode","dplyr",
             "plyr","stringr", "leaflet","maps","rgeos","rworldmap","rworldxtra","lubridate")



# Install packages not yet installed
# installed_packages = packages %in% rownames(installed.packages())
# if (any(installed_packages == FALSE)) {
#    install.packages(packages[!installed_packages])
# }

#################################################################################
##                               LIBRARIES                                     ##   
#################################################################################


library(shiny)
library(shinydashboard)
library(shinyjs)
library(DT)
library(readr)
library(shinycssloaders)
library(shinybusy)
library(reshape2)
library(shinyalert)
library(ggplot2)
library(htmlwidgets)
library(plotly)
library(shinyWidgets)
library(tidyverse)
library(countrycode)
library(plyr)
library(dplyr)
library(plotly)
library(stringr)
library(leaflet)
library(maps)
library(rgeos)
library(rworldmap)
library(rworldxtra)
library(lubridate)
library(gapminder)
library(highcharter)
library(xts)
library(caret)
library(Metrics)

library(forecast)
library(prophet)
library(ggthemes)

library(randomForest)
library(xgboost)



# Packages loading
# lapply(packages, library, character.only = TRUE) %>%  invisible()

formatThousands <- JS(
  "function(data) {",
  "return (data / 1000).toFixed(1) + 'K'",
  "}")

  
############################ menu


sidebar = dashboardSidebar(uiOutput("sidebarpanel") , collapsed = FALSE, disable = FALSE) 

l_today =  format(today(), "%m-%d-%Y")
l_prefix = 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/'
l_file = paste0(l_prefix,l_today,'.csv')


res = try( readr::read_csv( l_file  , locale = readr::locale(encoding = "latin1") ) )
print(res)

if (res==0) {
  l_today = format(today() , "%m-%d-%Y")  
   }  else  {
    l_today = format(today() - 1 , "%m-%d-%Y")
    l_file = paste0(l_prefix,l_today,'.csv')
    res = try( readr::read_csv( l_file  , locale = readr::locale(encoding = "latin1") ) )
    print(res) 
      if (res==0) {
      l_today = format(today() -  1 , "%m-%d-%Y") 
      }  else {
      l_today = format(today() -  2 , "%m-%d-%Y") 
      }
  }
  

  
l_file = paste0(l_prefix,l_today,'.csv')

res = try( readr::read_csv( l_file  , locale = readr::locale(encoding = "latin1") ) )
print(res)

if (!file.exists(l_file)) {
  l_today = format(today() - 1 , "%m-%d-%Y")
}






l_title = paste0( "COVID-19 APP (Updated: ", l_today,')')

body <- dashboardBody(
      shinyjs::useShinyjs(),
      
      useShinyalert(),
      tags$head(
        tags$style(type = "text/css",
                   HTML("th { text-align: center; }")
                  )
               ),
  
    tags$head(tags$style(HTML(
        '.myClass { 
        font-size: 21px;
        line-height: 50px;
        text-align: center;
        font-family: "Helvetica Neue",Helvetica,Arial,sans-serif;
        padding: 0 15px;
        overflow: hidden;
        color: white;
      }
    '))),
    tags$script(HTML(paste0('
      $(document).ready(function() {
        $("header").find("nav").append(\'<span class="myClass"> ', l_title ,'</span>\');
      })
     '))),
    
    tabItems(
    
    tabItem("dashboard", class = "active",
            
            
            fluidRow(
                valueBoxOutput("exploration_pa_dat1", width = 3),
                valueBoxOutput("exploration_pa_dat2", width = 3),
                valueBoxOutput("exploration_pa_dat3", width = 3),
                valueBoxOutput("exploration_pa_dat4", width = 3)
            ), #fluidrow
            
            fluidRow(
                
                box(width=6, collapsible = TRUE,  title="Cases by Country",status="primary",solidHeader = TRUE,
                    column(width=12,
                           radioButtons(inputId ="case_type_chart1", inline=TRUE, "",
                                        c("Confirmed" = "Confirmed",
                                          "Recovered" = "Recovered",
                                          "Deaths" = "Deaths", 
                                          "Active" = "Active")),
                           leafletOutput("exploration_pa_geo", width = '100%' ,height="400" ) %>% withSpinner(color="#4262a8") )
                    
                ),# box close
                box(width=6, collapsible = TRUE,  title="Cumulative Cases Over Time",status="primary",solidHeader = TRUE,
                    
                    column(width=12, 
                           radioButtons(inputId ="case_type_chart2", inline=TRUE, "",
                                        c("Confirmed" = "Confirmed",
                                          "Recovered" = "Recovered",
                                          "Deaths" = "Deaths", 
                                          "Active" = "Active")),
                           highchartOutput("exploration_active"  )%>% withSpinner(color="#4262a8") 
                          
                         )
                           
                )# box close
                
            ), #fluidrow    
            
            fluidRow(
                
                box(width=6, collapsible = TRUE,  title="Data Summary",status="primary",solidHeader = TRUE,
                    column(width=12, DT::dataTableOutput("descriptive_dt" ,height="500")  %>% withSpinner(color="#4262a8") )
                    
                ),# box close
                box(width=6, collapsible = TRUE,  title="Cumulative Cases",status="primary",solidHeader = TRUE,
                    column(width=12, plotlyOutput("exploration_ani" , width = '100%' ,height="500") %>% withSpinner(color="#4262a8") )
                )# box close
                
            ) #fluidrow    
            
            
    ),
    
    tabItem("forecast", 
            
            
            fluidRow( 
              
              tabBox( title = tagList(shiny::icon("project-diagram")),width = 12,
                     
                     tabPanel( strong("Data Collection"),   icon = icon("database"),
                        fluidRow(        
                            box(width=12, collapsible = TRUE,  title="Dataset",status="primary",solidHeader = TRUE,
                              column(width=12, DT::dataTableOutput("raw_dataset" )  %>% withSpinner(color="#4262a8") )
                            )# box close
                         ) #fluidrow close 
                       ), # tabpanel close                          
                     tabPanel( strong("Model Training"),   icon = icon("chalkboard-teacher"),
                               
                               fluidRow(        
                                 box(width=12, collapsible = TRUE,  title="Filter",status="primary",solidHeader = TRUE,
                                     column(width=6, selectInput(inputId = "country_filter","Country",choices=c(NULL)  )),
                                     column(width=6, selectInput(inputId = "target_filter","Target",choices=c('Confirmed','Recovered','Deaths','Active') ,selected = "Deaths"))
                                 )# box close
                               ), #fluidrow close 
                               
                               fluidRow(        
                                 box(width=12, collapsible = TRUE,  title="Model Settings",status="primary",solidHeader = TRUE,
                                     column(width=4, textInput(inputId ="random_seed","Ramdom Seed",value="999") ),
                                     column(width=4, selectInput(inputId = "split","Splitting",choices=c("70/30","75/25","80/20","90/10") ,selected = "80/20")),
                                     column(width=4,  selectInput(inputId = "kfold","K-Fold",choices=c("5-fold CV","10-fold CV") ,selected = "5-fold CV"))
                                     
                                 )# box close
                               ), #fluidrow close 
                               
                               fluidRow(
                                 div(style = "text-align: center;",actionButton(inputId = "train", " Train ", style = "color: white; background-color:#3c8dbc;padding: 10px 15px; width: 120px; cursor: pointer;
                                               font-size: 14px; font-weight: 600;" ,  icon = icon('caret-square-right'))) 
                               ), #fluidrow close 
                               
                               fluidRow(    
                                 column(width=6,
                                     box(width=12, collapsible = TRUE,  title="",solidHeader = TRUE,
                                         checkboxInput("lm","Linear Regression")
                                         )# box close,
                                       ),#column close,
                                  column(width=6,
                                     box(width=12, collapsible = TRUE,  title="",solidHeader = TRUE,
                                         verbatimTextOutput("lm_output")
                                        )# box close,                                    
                                     )#column close,
                               ), #fluidrow close 
                               fluidRow(   
                                  column(width=6,
                                        box(width=12, collapsible = TRUE,  title="",solidHeader = TRUE,
                                            checkboxInput("rf","Random Forests")
                                        )# box close,
                                 ),#column close,
                                 column(width=6,
                                        box(width=12, collapsible = TRUE,  title="",solidHeader = TRUE,
                                            verbatimTextOutput("rf_output")
                                        )# box close,                                    
                                 )#column close,
                                ), #fluidrow close 
    
                               
                               fluidRow(   
                                 column(width=6,
                                        box(width=12, collapsible = TRUE,  title="",solidHeader = TRUE,
                                            checkboxInput("xgboost","XGBoost")
                                        )# box close,
                                 ),#column close,
                                 column(width=6,
                                        box(width=12, collapsible = TRUE,  title="",solidHeader = TRUE,
                                            verbatimTextOutput("xgboost_output")
                                        )# box close,                                    
                                 )#column close,                                 
                                ), #fluidrow close 
                               
                               fluidRow(   
                                 column(width=6,
                                        box(width=12, collapsible = TRUE,  title="",solidHeader = TRUE,
                                           checkboxInput("prophet","Prophet", value= TRUE)
                                        )# box close,
                                 ),#column close,
                                 column(width=6,
                                        box(width=12, collapsible = TRUE,  title="",solidHeader = TRUE,
                                            verbatimTextOutput("prophet_output"),
                                            tags$head(tags$style("#prophet_output{overflow-y:scroll; max-height: 400px; }"))
                                        )# box close,                                    
                                 )#column close,               
                               ) #fluidrow close 

                               
                     ), # tabpanel close 
                     tabPanel( strong("Model Evaluation"),   icon = icon("eye"),
                               fluidRow(  
                                 box(width=12, collapsible = TRUE, title="Model Performance Comparison using Test partition", status="primary",solidHeader = TRUE,
                                     column(width=12, plotlyOutput("model_evaluation" , width = '100%' ,height="600"))
                                 )# box close
                               ) #fluidrow
                               
                     ), # tabpanel close 
                     
                     tabPanel( strong("Forecasting"),   icon = icon("chart-area"),
                               
                               fluidRow(   
                                 
                                        box(width=12, collapsible = TRUE,  title="14 days forecasts",status="primary",solidHeader = TRUE,
                                            column(width=6,
                                             DT::dataTableOutput("forecasting_dataset" )
                                            ),#column close,
                                            column(width=6,
                                              plotlyOutput("forecasting_plot", height = "600px")
                                            )#column close,
                                          )# box close,
                                ) #fluidrow close 
                               
                               
                     ) # tabpanel close 
                     
                      
              ) # tabbox close
            ) #fluidrow close
                                
                                
                                
    ),
            
            
    
    tabItem("about",
            tags$div(
              tags$h4("Data Science Hackathon"), 
              "This app was built as part of the UCLA Extension Data Science Hackathon.",tags$br(),
              "The data used was obtained from COVID-19 Data Repository by the Center for Systems Science and Engineering (CSSE) at Johns Hopkins University",tags$br(),
              tags$a(href="https://github.com/CSSEGISandData/COVID-19", "GitHub JHU CSSE account"),
              tags$br(),
              tags$h5("Team"),
              tags$ul(
                tags$li("Paramjit Singh"), 
                tags$li("Saul Ventura"), 
                tags$li("Ian  May")
              )
            )
      )
    
 ) #tabitems close


)




header = dashboardHeader(
    title = 'UCLA Hackaton' )






ui = dashboardPage(header, sidebar, body)


#################################################################################
##                               SERVER                                        ##   
#################################################################################



server = function(input, output, session) {    
    
    USER = reactiveValues(df_covid_countries = NULL,df_covid_countries_top = NULL, df_covid_all = NULL, covid_merge_csv = NULL,df_covid_merge_top =NULL,
                          df_animation = NULL , last_day= NULL, a_day_before = NULL, lm_output = NULL, rf_output = NULL, xgboost_output= NULL,
                          prophet_output = NULL, prophet_output_rmse= NULL, rmse_data = NULL , prophet.model = NULL, forecast_df = NULL
                          )
    


#################################################################################
##                               MENU CREATION                                 ##   
#################################################################################
    
    
    output$sidebarpanel = renderUI({
        
        
        dashboardSidebar(
            
            
            sidebarMenu(
                menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")) ,
                hr(),
                h5(strong("Filters"), align = "center"),
                
                sliderInput(inputId = "top_n",
                            label = h5("Top N Countries:"),
                            min = 1,
                            max = 20,
                            value = 5),
                
                radioButtons(inputId ="case_type", "Sort by:",
                             c("Confirmed" = "Confirmed",
                               "Recovered" = "Recovered",
                               "Deaths" = "Deaths",
                               "Active" = "Active",
                               "GDP" = "GDP",
                               "Population" = "Population"
                               )),
                
                radioButtons(inputId ="order_type", "Sort order:",selected = "Descending",
                             c("Ascending" = "Ascending",
                               "Descending" = "Descending"
                             )),    
                
                
                
                checkboxInput("manual_selection","Manual Selection?"),
                disabled(selectizeInput(inputId = "manual_countries","Select countries",choices=c(NULL), multiple = T )),
               
                hr(),
                
                menuItem("Forecasting", tabName = "forecast", icon = icon("chart-line")) ,
                
                menuItem("About", tabName = "about", icon = icon("question"))

            )
            
        )
        
        
        
    })    
    
    
    
    
#################################################################################
##                               DATA LOAD                                     ##   
#################################################################################
    
    
    f_file = function (l_date) {
        l_today =  format(l_date, "%m-%d-%Y")
        l_prefix = 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/'
        l_file = paste0(l_prefix,l_today,'.csv')
        return(l_file)
    }
    

     res = try( readr::read_csv( f_file(today())  , locale = readr::locale(encoding = "latin1") ) )

     if (res==0) {
           l_last_day_file = f_file(today())
           l_a_day_before_file = f_file(today()-1)
           l_2_day_before_file = f_file(today()-2)
           l_last_day = today()
           } else {
             
             res = try( readr::read_csv( f_file(today() -1 )  , locale = readr::locale(encoding = "latin1") ) )

                 if (res==0) {
                   l_last_day_file = f_file(today()-1)
                   l_a_day_before_file = f_file(today()-2)
                   l_2_day_before_file = f_file(today()-3)
                   l_last_day = today()-1
                 } else {
                   l_last_day_file = f_file(today()-2)
                   l_a_day_before_file = f_file(today()-3)
                   l_2_day_before_file = f_file(today()-4)
                   l_last_day = today() - 1
                   }
           }
            
     
    #  
    # if (   ! is.null(      ) {
    #     l_last_day_file = f_file(today()) 
    #     l_a_day_before_file = f_file(today()-1) 
    #     l_2_day_before_file = f_file(today()-2) 
    #     l_last_day = today()
    #     
    # } else {
    #     l_last_day_file = f_file(today()-1) 
    #     l_a_day_before_file = f_file(today()-2) 
    #     l_2_day_before_file = f_file(today()-3) 
    #     l_last_day = today() - 1
    # }
    
    USER$last_day = as.character(l_last_day)
    USER$a_day_before = as.character(l_last_day - 1)
    
    
    
    covid_csv = readr::read_csv(l_last_day_file  , locale = readr::locale(encoding = "latin1") )
    covid_adb_csv = readr::read_csv(l_a_day_before_file  , locale = readr::locale(encoding = "latin1") )
    covid_2db_csv = readr::read_csv(l_2_day_before_file  , locale = readr::locale(encoding = "latin1") )
    
    
    df_covid_countries = covid_csv %>% dplyr::group_by(Country_Region,Last_Update) %>% 
      dplyr::summarise(Confirmed=sum(Confirmed), Deaths=sum(Deaths) ,Recovered=sum(Recovered), 
                       Active=sum(Active)) %>%
      dplyr::select(-c('Last_Update'))
    
    df_covid_countries = as.data.frame(df_covid_countries)
    

    df_covid_adb_countries = covid_adb_csv %>% 
      dplyr::select(c('Country_Region','Confirmed','Deaths','Recovered','Active','Last_Update'))
    
    df_covid_adb_countries = df_covid_adb_countries %>% dplyr::group_by(Country_Region,Last_Update) %>% 
        dplyr::summarise(Confirmed=sum(Confirmed), Deaths=sum(Deaths) ,Recovered=sum(Recovered), 
                         Active=sum(Active)) %>%
        dplyr::select(-c('Last_Update'))
    
    df_covid_adb_countries = df_covid_adb_countries %>% dplyr::rename(Confirmed_adb ='Confirmed' , Deaths_adb = 'Deaths',
                                                   Recovered_adb ='Recovered' , Active_adb = 'Active' )
    
    
    df_covid_2db_countries = covid_2db_csv %>% 
      dplyr::select(c('Country_Region','Confirmed','Deaths','Recovered','Active','Last_Update'))
    
    df_covid_2db_countries = df_covid_2db_countries %>% dplyr::group_by(Country_Region,Last_Update) %>% 
      dplyr::summarise(Confirmed=sum(Confirmed), Deaths=sum(Deaths) ,Recovered=sum(Recovered), 
                       Active=sum(Active)) %>%
      dplyr::select(-c('Last_Update'))
    
    df_covid_2db_countries = df_covid_2db_countries %>% dplyr::rename(Confirmed_2db ='Confirmed' , Deaths_2db = 'Deaths',
                                                                      Recovered_2db ='Recovered' , Active_2db = 'Active' )
    

    df_covid_join = df_covid_countries %>% 
        dplyr::inner_join(df_covid_adb_countries , by = c('Country_Region'='Country_Region') ) %>% 
        dplyr::inner_join(df_covid_2db_countries , by = c('Country_Region'='Country_Region') )
    
    df_gdp = gapminder
    df_gdp = df_gdp %>% dplyr::filter(year== 2007)
    df_gdp$GDP = df_gdp$pop * df_gdp$gdpPercap
    df_gdp$Population = df_gdp$pop 
    df_gdp = df_gdp %>% dplyr::select(c('country','GDP','Population'))
    df_gdp$country = as.character(df_gdp$country)
    df_gdp$country[df_gdp$country == "United States"] = "US"
    
    df_covid_join = df_covid_join %>% 
      dplyr::left_join(df_gdp , by = c('Country_Region'='country') )
    
    #################################################################################
    ##                       ADDING  GEOGRAPHICAL DATA                             ##   
    #################################################################################
    
    
    wmap <- getMap(resolution="high")
    # get centroids
    centroids <- gCentroid(wmap, byid=TRUE)
    # get a data.frame with centroids
    df_long_lat <- as.data.frame(centroids)
    names(df_long_lat) = c("long","lat")
    df_long_lat <- tibble::rownames_to_column(df_long_lat, "COUNTRY_NAME")
    
    df_covid_join$Country_Region[df_covid_join$Country_Region == "US"] = "United States of America"
    
    df_covid_join =  merge(df_covid_join, df_long_lat, by.x = "Country_Region", by.y = "COUNTRY_NAME")
    
    
    

    df_covid_join =df_covid_join %>% dplyr::rename(Lat ='lat' , Long_ = 'long' )
    
    df_covid_join$Mortality = df_covid_join$Deaths/df_covid_join$Confirmed
    df_covid_join$New_Cases_adb = ifelse(df_covid_join$Confirmed_adb - df_covid_join$Confirmed_2db>0,df_covid_join$Confirmed_adb - df_covid_join$Confirmed_2db,0)
    df_covid_join$New_Cases = ifelse(df_covid_join$Confirmed - df_covid_join$Confirmed_adb>0,df_covid_join$Confirmed - df_covid_join$Confirmed_adb,0)
    df_covid_join$Change =  ifelse(df_covid_join$New_Cases_adb != 0,  (df_covid_join$New_Cases -df_covid_join$New_Cases_adb )/df_covid_join$New_Cases_adb,NaN)
    
    df_covid_join =df_covid_join %>% dplyr::rename(`New Cases`= 'New_Cases' , `New Cases from previous day`= 'New_Cases_adb','Country' = 'Country_Region' )
    

    
    df_covid_all = covid_csv %>% 
        dplyr::summarise(Confirmed=sum(Confirmed, na.rm = TRUE), Deaths=sum(Deaths, na.rm = TRUE) ,Recovered=sum(Recovered, na.rm = TRUE),  Active=sum(Active, na.rm = TRUE) ) 
    
    df_covid_join$Country[df_covid_join$Country == "United States of America"] = "US"
    
    USER$df_covid_countries = as.data.frame( df_covid_join)
    USER$df_covid_all = as.data.frame( df_covid_all)
    
    
    
    #################################################################################
    ##                               MANUAL SELECTION                              ##   
    #################################################################################    
    
    observeEvent(input$manual_selection,{
      if (input$manual_selection) {
        disable("top_n")
        disable("case_type")
        disable("order_type")
        shinyjs::enable("manual_countries")
        updateSelectizeInput(session, "manual_countries", choices = c(unique(USER$covid_merge_csv$Country)) )
      }
      else {
        enable("top_n")
        enable("case_type")
        enable("order_type")
        shinyjs::disable("manual_countries")          
        
      }
      
      })
    
    
    #################################################################################
    ##                               MAIN FILTER                                   ##   
    #################################################################################
    
    
    
    observeEvent(c(input$order_type,input$top_n, input$case_type, input$manual_countries),{


      
      df_covid_countries_top = USER$df_covid_countries
      df_covid_merge_top = USER$covid_merge_csv
      
      print("input$order_type")
      print(input$order_type)
      print(str(df_covid_countries_top))
      print(str(input$top_n))
      print(str(input$case_type))
      
      
      if (! input$manual_selection) {
      
            if (input$order_type =='Ascending') {
              l_top  = input$top_n*-1
            } else {
       
              l_top  = input$top_n
            }
      
            df_covid_countries_top =  df_covid_countries_top %>% 
              dplyr::filter(  df_covid_countries_top[[input$case_type]]  > 0) 
            
            df_covid_countries_top =  df_covid_countries_top %>% 
              dplyr::top_n(l_top, df_covid_countries_top[[input$case_type]]  ) 
            
            df_covid_countries_top = df_covid_countries_top %>% dplyr::arrange(desc(df_covid_countries_top[[input$case_type]])) 
          }    
      else      
        {
      
          df_covid_countries_top = df_covid_countries_top %>% dplyr::filter(Country %in% input$manual_countries)
       
          }
     
      

      l_countries = unique(df_covid_countries_top$Country)
      
      #df_covid_merge_top$Country[df_covid_merge_top$Country == "US"] = "United States of America"
      
      df_covid_merge_top =  df_covid_merge_top %>%  dplyr::filter(Country %in% l_countries) 
      
     # df_covid_merge_top$Country[df_covid_merge_top$Country == "United States of America"] = "US"
      
     # df_covid_countries_top$Country[df_covid_countries_top$Country == "United States of America"] = "US"
      
      USER$df_covid_countries_top = df_covid_countries_top
      
      USER$df_covid_merge_top = df_covid_merge_top
      

      updateSelectInput(session, "country_filter", choices = c(unique(USER$covid_merge_csv$Country)),selected = "US"  )
      

      shinyjs::disable("prophet")
                      
    
    } )
    

    
    
    
    #################################################################################
    ##                               DATA OVER TIME                                ##   
    #################################################################################
    
    f_df_covid = function (l_file_type, l_type) {
      
      l_prefix = 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_'
      l_file = paste0(l_prefix,l_file_type,'_global.csv')
      
      l_covid_csv = readr::read_csv(l_file, locale = readr::locale(encoding = "latin1") )
      
      l_covid_csv = l_covid_csv %>% dplyr::select(-c('Province/State','Lat','Long'))
      
      l_covid_csv =l_covid_csv %>% dplyr::rename(Country = `Country/Region`)
      l_covid_csv = l_covid_csv %>% dplyr::group_by(Country) %>%
        dplyr::summarise_all(sum) %>%  dplyr::ungroup()
      l_covid_csv = melt(l_covid_csv, id.vars="Country")
      l_covid_csv =l_covid_csv %>% dplyr::rename_at(vars(c('variable','value')), ~ c('Date',l_type))
      
      
      return(l_covid_csv)
    }
    
    
    covid_confirmed_csv = f_df_covid('confirmed','Confirmed')
    covid_recovered_csv = f_df_covid('recovered','Recovered')
    covid_deaths_csv = f_df_covid('deaths','Deaths')
    
    covid_merge_csv = merge(covid_confirmed_csv,covid_recovered_csv)
    covid_merge_csv = merge(covid_merge_csv,covid_deaths_csv)
    covid_merge_csv$Active = covid_merge_csv$Confirmed - covid_merge_csv$Recovered - covid_merge_csv$Deaths
   

    USER$covid_merge_csv = covid_merge_csv


    
    #################################################################################
    ##                               DASHBOARD                                     ##   
    #################################################################################
    
    
    
    
    
    
    output$exploration_pa_dat1 <- shinydashboard::renderValueBox({
        
        l_rate = USER$df_covid_all$Confirmed
        
        l_label = 'Confirmed (worldwide)'
        shinydashboard::valueBox(
            formatC(l_rate, format="d", big.mark=',')
            ,paste(l_label,'')
            ,icon = icon("user")
            ,color = "teal")  
        
        
    })
    
    output$exploration_pa_dat2 <- renderValueBox({ 
        l_rate = USER$df_covid_all$Deaths
        l_label = 'Deaths (worldwide)'
        shinydashboard::valueBox(
            formatC(l_rate, format="d", big.mark=',')
            ,paste(l_label,'')
            ,icon = icon("user-times")
            ,color = "aqua")  
    })
    output$exploration_pa_dat3 <- renderValueBox({ 
        l_rate = USER$df_covid_all$Recovered
        l_label = 'Recovered (worldwide)'
        shinydashboard::valueBox(
            formatC(l_rate, format="d", big.mark=',')
            ,paste(l_label,'')
            ,icon = icon("user-shield")
            ,color = "teal")  
    })
    
    output$exploration_pa_dat4 <- renderValueBox({
        l_rate = USER$df_covid_all$Active
        l_label = 'Active (worldwide)'
        shinydashboard::valueBox(
            formatC(l_rate, format="d", big.mark=',')
            ,paste(l_label,'')
            ,icon = icon("procedures")
            ,color = "aqua")  
    })
    
    
    
    output$exploration_pa_geo <- renderLeaflet({
        
      if(is.null(USER$df_covid_countries_top)){return()}
      
      
      
      l_column_filter = input$case_type_chart1

      if (l_column_filter == 'Confirmed') { l_resize = 1.0 }
      if (l_column_filter == 'Recovered') { l_resize = 3.0 }
      if (l_column_filter == 'Deaths') { l_resize = 3.0 }
      if (l_column_filter == 'Active') { l_resize = 1.0 }
      
      pal <- colorFactor(
        palette = c('orange', 'yellow', 'red', 'green'),
        domain = c('Active', 'Confirmed', 'Deaths', 'Recovered')
      )
      
      
      l_label =   paste0("<strong>",USER$df_covid_countries_top$Country,"</strong><br>", l_column_filter ,
                         ": <strong>", formatC(USER$df_covid_countries_top[[l_column_filter]], format="d", big.mark=',') , "</strong>")
      

      leaflet(USER$df_covid_countries_top) %>% addTiles() %>%
            
            setView(lng = -25, lat = 40, zoom = 2) %>%
            
            addCircles(lng = ~Long_, lat = ~Lat, weight = 1, fillOpacity = 0.5,
                       radius = USER$df_covid_countries_top[[l_column_filter]]*l_resize ,  highlightOptions = highlightOptions(color = "black", weight = 2) ,
                       label = lapply(l_label , HTML) , color = ~pal(input$case_type_chart1)   )  %>%
            addMarkers(lng = ~Long_, lat = ~Lat, icon= NULL,
                       popup = l_label   )  
        
    })
    
    
    
    output$exploration_active <- renderHighchart({
      
      if(is.null(USER$df_covid_merge_top) | nrow(USER$df_covid_merge_top) ==0){return()}
      
      l_column_filter = input$case_type_chart2
      
      l_df_ts = USER$df_covid_merge_top %>% dplyr::select(c('Country','Date',l_column_filter))
      
      l_countries = unique(l_df_ts$Country)
      Hchc <- highchart(type = "stock") 
      
      for (i in 1:length(l_countries)){
        l_df =  l_df_ts %>%  dplyr::filter(Country== l_countries[[i]] ) 
        l_df = dcast(l_df, Date ~ Country, value.var =l_column_filter)
        l_df = xts(l_df[, -1], order.by= as.Date(l_df$Date, format="%m/%d/%y")  )
        Hchc = Hchc %>%
          hc_add_series(name = l_countries[[i]], l_df , id= as.character(l_df ) )
        
      }
      
      Hchc = Hchc %>% 
        hc_legend(enabled = T, align = "right", verticalAlign = "top",layout = "vertical", x = 0, y = 100)
      
      Hchc
      
    } )
    

    
    output$descriptive_dt =  DT::renderDataTable({
      
      if(is.null(USER$df_covid_countries_top)){return()}
      
      sketch = htmltools::withTags(table(
        class = 'display',
        thead( 
          tr(
            th(rowspan = 2, 'Country'),
            th(rowspan = 2, 'Confirmed'),
            th(colspan = 3, 'New Cases'),
            th(rowspan = 2, 'Deaths'),
            th(rowspan = 2, 'Mortality'),
            th(rowspan = 2, 'Recovered'),
            th(rowspan = 2, 'Active'),
            th(rowspan = 2, 'GDP'),
            th(rowspan = 2, 'Population')
            
          ),
          tr(
            lapply(rep(c(USER$a_day_before, USER$last_day,'Change')), th)
          )
        )
      ))
      

        datatable(USER$df_covid_countries_top %>% dplyr::select(c('Country','Confirmed','New Cases from previous day','New Cases','Change','Deaths',
                                                                  'Mortality','Recovered','Active','GDP','Population'))  , container = sketch, rownames = FALSE,
                  options = list( pageLength = 10,scroller = TRUE,
                                  scrollX = TRUE  ,paging = TRUE,
                                  columnDefs = list(list(visible=FALSE, targets=c(9,10)))
                  ), 
                  class = 'white-space: nowrap'
        )  %>% 
            formatPercentage(c("Change","Mortality"), 1)  %>% 
            formatStyle(c(input$case_type), backgroundColor = "#5ad5fa")  %>% 
            formatStyle(c("Change"), color = styleInterval(0,c("Blue","Red")))  %>% 
            formatRound(columns=c(2,3,4,5,6,7,8,9), digits=0) 
                        
    },  server = TRUE) 
    
    
    
    output$exploration_ani <- renderPlotly({
        
      if(is.null(USER$df_covid_countries_top)){return()}
      
      p4 <- plot_ly(USER$df_covid_countries_top,
                    y = ~Country,
                    x = ~Active,
                    type = "bar",orientation = 'h',
                    name = "Active" , text = ~scales::comma(Active, 1), textposition = 'auto', marker = list(color = 'orange', opacity = 0.7)) %>% 
        add_trace(x = ~Recovered,
                  name = "Recovered" , text = ~scales::comma(Recovered, 1), textposition = 'auto',marker = list(color = 'green', opacity = 0.7)) %>% 
        add_trace(x = ~Deaths,
                  name = "Deaths" , text = ~scales::comma(Deaths, 1), textposition = 'auto', marker = list(color = 'red', opacity = 0.7)) %>% 
        layout(xaxis = list(title = ""),yaxis = list(title = ""),
               barmode = "stack") 
      
      p4
        
    } )
    
    
    #################################################################################
    ##                               FORECASTING                                   ##   
    #################################################################################
    
    output$raw_dataset =  DT::renderDataTable({
      
      if(is.null(USER$covid_merge_csv )){return()}
      
      datatable(USER$covid_merge_csv ,
                options = list( pageLength = 20,scroller = TRUE,
                                scrollX = TRUE  ,paging = TRUE
                ), 
                class = 'white-space: nowrap'
      ) 
      
    },  server = TRUE) 
    
    
    
    observeEvent(input$train,{

      if (!(input$lm | input$rf | input$xgboost | input$prophet)) { 
        shinyalert::shinyalert(title = "",text = "Please select at least one model",type = "error")
        return()
        }
      
      
      #show_spinner
      show_modal_spinner(spin = "fading-circle", color = "#112446",text = "Training model(s).....Please wait", session = shiny::getDefaultReactiveDomain())
      
      
      
      set.seed(input$random_seed)
      
      l_df = USER$covid_merge_csv 
      
      
      l_target_column = input$target_filter
      l_filter_column = input$country_filter
      
      l_df = l_df %>% dplyr::filter(Country == l_filter_column) %>% dplyr::select(c('Country',Date,l_target_column))
      l_df$Date = as.Date(l_df$Date, format="%m/%d/%y")
      lmin = min(l_df$Date)
      l_df$Day = as.numeric(l_df$Date -lmin)
      l_df$Weekday = wday(ymd(l_df$Date))
      l_df$Month = month(ymd(l_df$Date))
      
      print('training l_df')
      print(str(l_df))
      
      
      if (input$split == "70/30" ) { l_partition = .7 }
      if (input$split == "75/25" ) { l_partition = .75 }
      if (input$split == "80/20" ) { l_partition = .8 }
      if (input$split == "90/10" ) { l_partition = .9 }
      

      #inTraining = sample(seq_len(nrow(l_df)), size = l_partition)  #createDataPartition( l_df[[l_target_column]] , p = l_partition, list = FALSE)
      
      l_df = l_df %>% dplyr::arrange(l_df[[l_target_column]]) 
      
      N = nrow(l_df)
      n = l_partition*N
      training = l_df[1:n, ]
      testing  = l_df[(n+1):N,  ]
      

      column(width=4,  selectInput(inputId = "kfold","K-Fold",choices=c("5-fold CV","10-fold CV") ,selected = "5-fold CV"))
      
      if (input$kfold == "5-fold CV" ) { l_fold = 5 }
      if (input$kfold == "10-fold CV" ) { l_fold = 10 }
      
      
      fitControl = trainControl(method = "cv",number = l_fold , verboseIter = FALSE)
      

      lm_rmse = 0
      rf_rmse = 0
      xgb_rmse=0
      prophet_rmse= 0
      
      
      ## LINEAR REGRESSION

      if (input$lm) {
      
        lm.model = train( formula(paste0(l_target_column,  ' ~ Day + Weekday + Month ')) , data = training, method = "lm",
                           tuneLength = 10,
                           preProcess = c("center", "scale"),
                           trControl = fitControl,
                           metric="RMSE")
  
        
        lm.predict = predict(lm.model, testing)
  
        lm_rmse = rmse(testing[[l_target_column]], lm.predict)
  
        USER$lm_output = lm.model
      
      }
      
      
      ## RANDOM FORESTS
      
      if (input$rf) {
      
        rf.model = train(formula(paste0(l_target_column,  ' ~ Day + Weekday + Month ')), data = training, method = "rf", 
                          tuneLength = 10,
                          ntree = 100,
                          preProcess = c("center", "scale"),
                          trControl = fitControl,
                          metric="RMSE")
        
        rf.predict = predict(rf.model, testing)
  
        rf_rmse = rmse(testing[[l_target_column]], rf.predict)
        
        USER$rf_output = rf.model
      
      }
      
      
      ## XGBOOST
      
      if (input$xgboost) {
      
        xgbGrid <- expand.grid(nrounds = c(50), 
                               max_depth = c(15),
                               colsample_bytree = seq(0.5, 0.9, length.out = 5),
                               eta = 0.1,
                               gamma=0,
                               min_child_weight = 1,
                               subsample = 1
        )
        xgb.model <- train(formula(paste0(l_target_column,  ' ~ Day + Weekday + Month ')), data = training,  method = "xgbTree",
                           trControl = fitControl,
                           tuneGrid = xgbGrid)
        
        xgb.predict = predict(xgb.model, testing)
        
        xgb_rmse = rmse(testing[[l_target_column]], xgb.predict)
        
        USER$xgboost_output = xgb.model
      
      }
      
      ##PROPHET
      
      if (input$prophet) {
      

        train = training %>% dplyr::select(c(2,3))
        test = testing %>% dplyr::select(c(2,3))
        colnames(train) = c('ds','y')
        colnames(test) = c('ds','y')
        

        prophet.model = prophet::prophet(train ) 
        
        # predictions
        forecast_df <- tbl_df(predict(prophet.model, train))
        
        dataprediction <- data.frame(forecast_df$ds,forecast_df$yhat)
        
        
        #rmse train
        forecast::accuracy(dataprediction$forecast_df.yhat ,train$y)
        
        USER$prophet_output = prophet.model
        USER$prophet_output_rmse = forecast::accuracy(dataprediction$forecast_df.yhat ,train$y)
        
        forecast_df <- tbl_df(predict(prophet.model, test))
        dataprediction <- data.frame(forecast_df$ds,forecast_df$yhat)
  
        #rmse test
        prophet_rmse_list = forecast::accuracy(dataprediction$forecast_df.yhat ,test$y)
        prophet_rmse = prophet_rmse_list[3]
        
        # forecasting next days

        l_df = l_df %>% dplyr::select(c(2,3))
        colnames(l_df) = c('ds','y')
        
        prophet.model = prophet::prophet(l_df) 
        future = make_future_dataframe(prophet.model, periods =14, freq = 'day')
        forecast_df = tbl_df(predict(prophet.model, future))
        forecast_df = data.frame(forecast_df[c('ds','yhat', 'yhat_lower', 'yhat_upper')])
        
        USER$prophet.model = prophet.model
        USER$forecast_df = forecast_df
        

      }
      
             
      rmse_results <- c(ifelse(is.na(lm_rmse),0,lm_rmse), ifelse(is.na(rf_rmse),0,rf_rmse), ifelse(is.na(xgb_rmse),0,xgb_rmse), ifelse(is.na(prophet_rmse),0,prophet_rmse))
      rmse_labels <- c("Linear Regression",  "Random Forest", "XGBoost", "Facebook Prophet")
      rmse_data <- data.frame(model=rmse_labels, rmse=rmse_results)
      USER$rmse_data = rmse_data
      
      remove_modal_spinner(session = getDefaultReactiveDomain())
      
    } )
    
    
    
    output$lm_output <- renderPrint({
      if(is.null(USER$lm_output) ){return(invisible(""))}
      
      USER$lm_output
      
    })
    
    output$rf_output <- renderPrint({
      if(is.null(USER$rf_output) ){return(invisible(""))}
      
      USER$rf_output
      
    })
    
    output$xgboost_output <- renderPrint({
      if(is.null(USER$xgboost_output) ){return(invisible(""))}
      
      USER$xgboost_output
      
    })
    
    output$prophet_output <- renderPrint({
      if(is.null(USER$prophet_output) ){return(invisible(""))}
      
      print('Facebook Prophet')
      print(USER$prophet_output)
      print(USER$prophet_output_rmse) 
      
    })
    
    output$model_evaluation <- renderPlotly({
      if(is.null(USER$rmse_data )){return()}
      
     g = ggplot(USER$rmse_data, aes(x=reorder(model, rmse), y=rmse))+
        geom_bar(stat="identity" , fill="steelblue")+
        ggtitle("Model Comparison")+
        labs(y="RMSE", x="Model")+
        coord_flip() 
     
      ggplotly(g)
      
    } )
    
    
    output$forecasting_dataset =  DT::renderDataTable({
      
      if(is.null(USER$forecast_df )){return()}
      
      l_forecast_df = USER$forecast_df
      
      l_forecast_df =  l_forecast_df %>% 
        dplyr::top_n(14, l_forecast_df$ds  ) 
      
      datatable(l_forecast_df ,
                options = list( pageLength = 14,scroller = TRUE,
                                scrollX = TRUE  ,paging = TRUE
                ), 
                class = 'white-space: nowrap'
      )  %>%  formatRound(columns=c(2,3,4), digits=0) 
      
    },  server = TRUE) 
    
    
    output$forecasting_plot <- renderPlotly({
      if(is.null(USER$forecast_df )){return()}
      
      
      
      g=plot( USER$prophet.model ,USER$forecast_df, main = 'COVID-19 Forecast', 
              xlab = 'Date',
              ylab = 'Cases') 
      
      ggplotly(g)
      
    } )
    
    
}

shinyApp(ui, server)

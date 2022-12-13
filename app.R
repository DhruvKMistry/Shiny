
library(ggplot2) # data visualization
library(magrittr) # pipe operator
library(dplyr) # manipulate data

library(readr)
library(stringr)
library(hrbrthemes)
library(raster)
library(tidyr)
library(data.table)
library(socviz)
library(tidyverse)

# Make dashboard
library(shiny)
library(shinydashboard)

library(geojsonio)
library(rmapshaper)
library(rgdal)
library(plotly)

library(RColorBrewer)
library(sf) # the base package manipulating shapes
library(spdplyr) # the `dplyr` counterpart for shapes
library(magrittr) # data wrangling
library(leaflet)# map view

# Importing Data
df <- read.csv(file = 'Data.csv')

# Data Processing
df[is.na(df)] <- 0
df$Province[df$Province == "BC"] <- "BRITISH COLUMBIA"
df$Province[df$Province == "NL"] <- "NEWFOUNDLAND AND LABRADOR"
df$Province[df$Province == "NWT"] <- "NORTHWEST TERRITORIES"
df$Province[df$Province == "PEI"] <- "PRINCE EDWARD ISLAND"
df<-subset(df, Province != "REPATRIATED" & Province != "REPATRIATED CDN")

# Changing the day based data into weekly
df$SummaryDate <- sapply(strsplit(df$SummaryDate," "), `[`, 1) 
df[c('year', 'month')] <- str_split_fixed(df$SummaryDate, '/', 2)
df[c('year', 'month','day')] <- str_split_fixed(df$SummaryDate, '/', 3)
df <- subset(df, select = -c(SummaryDate))
df$day <- floor(as.double(df$day)/7)
df <- df%>%unite(month, month, day, sep="/")





#Create a ui (user interface); commands that create the overall look of the dashboard
ui = dashboardPage(
    skin='blue',
    dashboardHeader(title = "Covid"),
    dashboardSidebar(
        sidebarMenu(id="menu",
            menuItem("Provincial Analysis", tabName="FirstPage"),
            menuItem("Comparative Analysis", tabName="SecondPage"),
            menuItem("Map View", tabName = "map")
        ),
        
        # Provincial Analysis Navigation Bar
        conditionalPanel(condition = "input.menu == 'FirstPage'",
            tags$hr(),
            p("Choose the Province and Year to Analytics"),
            selectInput(
                inputId = "province",
                label = "Province",
                choices = c("Canada",
                            "Alberta",
                            "British Columbia",
                            "Manitoba",
                            "New Brunswick",
                            "Newfoundland and Labrador",
                            "Northwest Territories",
                            "Nova Scotia",
                            "Nunavut",
                            "Ontario",
                            "Prince Edward Island",
                            "Quebec",
                            "Saskatchewan",
                            "Yukon"),
                selected = c("New Brunswick")
            ),
            selectInput(
              inputId = "year",
              label = "Year",
              choices = c("2020","2021","2022"),
              selected = c("2020")
            )
            
        ),
        
        # Comparative Analysis Navigation Bar
        conditionalPanel(condition = "input.menu == 'SecondPage'",
                         tags$hr(),
                         p("Choose 2 Provinces to Compair"),
                         selectInput(
                           inputId = "province1",
                           label = "Province 1",
                           choices = c("Canada",
                                       "Alberta",
                                       "British Columbia",
                                       "Manitoba",
                                       "New Brunswick",
                                       "Newfoundland and Labrador",
                                       "Northwest Territories",
                                       "Nova Scotia",
                                       "Nunavut",
                                       "Ontario",
                                       "Prince Edward Island",
                                       "Quebec",
                                       "Saskatchewan",
                                       "Yukon"),
                           selected = c("New Brunswick")
                         ),
                         selectInput(
                           inputId = "province2",
                           label = "Province 2",
                           choices = c("Canada",
                                       "Alberta",
                                       "British Columbia",
                                       "Manitoba",
                                       "New Brunswick",
                                       "Newfoundland and Labrador",
                                       "Northwest Territories",
                                       "Nova Scotia",
                                       "Nunavut",
                                       "Ontario",
                                       "Prince Edward Island",
                                       "Quebec",
                                       "Saskatchewan",
                                       "Yukon"),
                           selected = c("Manitoba")
                         ),
                         selectInput(
                           inputId = "year1",
                           label = "Year",
                           choices = c("2020","2021","2022"),
                           selected = c("2020")
                         )
                         
        ),
        
        # Map View Navigation Bar
        conditionalPanel(condition = "input.menu == 'map'",
                         tags$hr(),
                         p("Choose the type and  year to show the map"),
                         selectInput(
                           inputId = "total",
                           label = "Type",
                           choices =c("ICU",
                                     "Cases",
                                     "Recovered",
                                     "Death",
                                     "Hospitalized"
                             ),
                           selected = c("ICU")
                         ),
                         selectInput(
                           inputId = "year2",
                           label = "Year",
                           choices = c("2020","2021","2022"),
                           selected = c("2020")
                         )
        )   
    ),
    dashboardBody(
      tabItems(
        
        # Provincial Analysis UI
        tabItem(tabName = "FirstPage",
                h2("Weekly COVID-19 Updatess"),
                tags$br(),
                p("The plots below shows the weekly Cases, Deaths and recovery for patinses in canada between 2020 to 2022."),
                fluidRow(
                  tabBox(width=12,
                         
                         id = "tabchat1",
                         
                         tabPanel("Cases",plotOutput("plot1")),
                         tabPanel("Recovered", plotOutput("plot2")),
                         tabPanel("Deaths", plotOutput("plot3")))
                  
                  
                ),
                
                tags$br(),
                p("The plots below show the weekly patinses hospitalized and in ICU "),
                fluidRow(
                  tabBox(width=12,
                         id = "tabchat2",
                         tabPanel("Hospitalized",plotOutput("plot4")),
                         tabPanel("ICU", plotOutput("plot5")))
                )
          
              ),
        
        # Comparative Analysis UI
        tabItem(tabName = "SecondPage",
                
                h2('Comparitive Analysis'),
                tags$br(),
                p('The table below allows as to identify the yearly total(cases, deaths, recovery, hospitalized and ICU) for 2 provinces'),
                
                  fluidRow(
                      box(width=12,tableOutput("table1"))
                    
                  ),
                
                tags$br(),
                p('The plots below show the weekly vaccination,also the weekly doses teken'),
                  fluidRow(
                    tabBox(width=12,
                           id = "tabchat3",
                          tabPanel("Vaccine", plotOutput("plot6")),
                          tabPanel("Details", plotOutput("plot7"))
                          )
                    
                  )
                ),
        
        # Map UI 
        tabItem(tabName = "map",
                
                h2('Map View'),
                tags$br(),
                p('The Map below can display the yearly totals for each provence(by hovering over them)'),
                fluidRow(
                   box(width=12,plotlyOutput("map"))
                )  )
        
      )
        
    )
)

server = function(input,output){
  
  provincetag= c("Canada"="CANADA",
              "Alberta"="ALBERTA",
              "British Columbia"="BRITISH COLUMBIA",
              "Manitoba"="MANITOBA",
              "New Brunswick"="NEW BRUNSWICK",
              "Newfoundland and Labrador"="NEWFOUNDLAND AND LABRADOR",
              "Northwest Territories"="NORTHWEST TERRITORIES",
              "Nova Scotia"="NOVA SCOTIA",
              "Nunavut"="NUNAVUT",
              "Ontario"="ONTARIO",
              "Prince Edward Island"="PRINCE EDWARD ISLAND",
              "Quebec"="QUEBEC",
              "Saskatchewan"="SASKATCHEWAN",
              "Yukon"="YUKON")
  
  totaltag=c("ICU"="TotalICU",
          "Cases"="TotalCases",
          "Recovered"="TotalRecovered",
          "Death"="TotalDeath",
          "Hospitalized"="TotalHospitalized"
          )
  
  
  # Line Plot for yearly Cases
  output$plot1 = renderPlot({
    data <- reactive({ 
      req(input$province,input$year)
      df1 <- df %>% filter(Province %in% as.character(provincetag[input$province])  &  year %in% input$year) %>% group_by(month) %>% summarise(TotalCases = sum(DailyTotals),.groups = 'drop')
    }) 
    
    ggplot(data(), aes(x=month,y=TotalCases,group=1)) + geom_line(linetype = "dashed") + geom_point(color="red",size=3) +  theme_ipsum()
  })
  
  
  # Line Plot for yearly Recovered
  output$plot2 = renderPlot({
    data <- reactive({ 
      req(input$province,input$year)
      df1 <- df %>% filter(Province %in% as.character(provincetag[input$province])  &  year %in% input$year) %>% group_by(month) %>% summarise(TotalRecovered = sum(DailyRecovered),.groups = 'drop')
    }) 
    
    ggplot(data(), aes(x=month,y=TotalRecovered,group=1)) + geom_line(linetype = "dashed") + geom_point(color="#008252",size=3) +  theme_ipsum()
  })
  
  # Line Plot for yearly Deaths
  output$plot3 = renderPlot({
    data <- reactive({ 
      req(input$province,input$year)
      df1 <- df %>% filter(Province %in% as.character(provincetag[input$province])  &  year %in% input$year) %>% group_by(month) %>% summarise(TotalDeaths = sum(DailyDeaths),.groups = 'drop')
    }) 
    
    ggplot(data(), aes(x=month,y=TotalDeaths,group=1)) + geom_line(linetype = "dashed") + geom_point(color="black",size=3) +  theme_ipsum()
  })
  
  # Bar Plot for yearly Hospitalized
  output$plot4 = renderPlot({
    data <- reactive({ 
      req(input$province,input$year)
      df1 <- df %>% filter(Province %in% as.character(provincetag[input$province])  &  year %in% input$year) %>% group_by(month) %>% summarise(TotalHospitalized = sum(TotalHospitalized),.groups = 'drop')
    }) 
    
    ggplot(data(), aes(x=month,y=TotalHospitalized)) + geom_bar(stat="identity",fill="#800000") +  theme_ipsum()
  })
  
  # Bar Plot for yearly ICU
  output$plot5 = renderPlot({
    data <- reactive({ 
      req(input$province,input$year)
      df1 <- df %>% filter(Province %in% as.character(provincetag[input$province])  &  year %in% input$year) %>% group_by(month) %>% summarise(TotalICU = sum(TotalICU),.groups = 'drop')
    }) 
    
    ggplot(data(), aes(x=month,y=TotalICU)) + geom_bar(stat="identity",fill="purple") +  theme_ipsum()
  })
  
  # Map Plot to display Canada's Map 
  output$map = renderPlotly({
    
    # Importing Canada's map
    canada_cd <- st_read("data/canada_cd_sim.geojson", quiet = TRUE)
    crs_string = "+proj=lcc +lat_1=49 +lat_2=77 +lon_0=-91.52 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs" # 2
    
    # To remove all the unnecessary things from the plot
    theme_map <- function(base_size=9, base_family="") { 
      require(grid)
      theme_bw(base_size=base_size, base_family=base_family) %+replace%
        theme(axis.line=element_blank(),
              axis.text=element_blank(),
              axis.ticks=element_blank(),
              axis.title=element_blank(),
              panel.background=element_blank(),
              panel.border=element_blank(),
              panel.grid=element_blank(),
              panel.spacing=unit(0, "lines"),
              plot.background=element_blank(),
              legend.justification = c(0,0),
              legend.position = c(0,0)
        )
    }
    map_colors <- RColorBrewer::brewer.pal(9, "Pastel1") %>% rep(37)
    
    
    data1 <- reactive({
    
    req(input$total,input$year2)
      
    data <- df  %>%filter(year %in% input$year2) %>% group_by(Province) %>% 
      summarise(TotalICU = as.integer(sum(TotalICU)),
                TotalCases = as.integer(sum(DailyTotals)),
                TotalRecovered = as.integer(sum(DailyRecovered)),
                TotalDeaths = as.integer(sum(DailyDeaths)),
                TotalHospitalized = as.integer(sum(TotalHospitalized)),
                .groups = 'drop') 
    data <-  data[colnames(data)==as.character(totaltag[input$total]) || colnames(data)=='Province']
    
    data <- subset(data, Province != "Canada")
    
    # creating a column to connect data to the map data
    data$Province = str_to_title(data$Province)
    prov.list <- list(
      '48' = c('Alberta'),
      '59' = c('British Columbia'),
      '46' = c('Manitoba'),
      '13' = c('New Brunswick'),
      '10' = c('Newfoundland And Labrador'),
      '61' = c('Northwest Territories'),
      '12' = c('Nova Scotia'),
      '62' = c('Nunavut'),
      '35' = c('Ontario'),
      '11' = c('Prince Edward Island'),
      '24' = c('Quebec'),
      '47' = c('Saskatchewan'),
      '60' = c('Yukon'))
    
    data$PRUID <- `levels<-`(factor(data$Province),prov.list)
    
    # merging map data to covid data
    canada_cd = merge(x=canada_cd,y=data,by="PRUID",all.x=TRUE)
    
    p <- ggplot(canada_cd) +
      geom_sf(aes(fill = factor(get(as.character(totaltag[input$total])))), color = "gray60", size = 0.00001, data = canada_cd) +
      coord_sf(crs = crs_string) +
      guides(fill = FALSE) +
      theme_map() +
      theme(panel.grid.major = element_line(color = "white"),
            legend.key = element_rect(color = "gray40", size = 0.000001)) 
    return(p)
    })
    data1()
  })
  
  # Displaying Yearly average for the data
  output$table1 = renderTable({
    req(input$province1,input$province1,input$year1)
    data <- df %>% filter((Province %in% as.character(provincetag[input$province1]) | Province %in% as.character(provincetag[input$province2]))  &  year %in% input$year1) %>%  group_by(Province) %>% 
        summarise(TotalICU = as.integer(sum(TotalICU)),
                  TotalCases = as.integer(sum(DailyTotals)),
                  TotalRecovered = as.integer(sum(DailyRecovered)),
                  TotalDeaths = as.integer(sum(DailyDeaths)),
                  TotalHospitalized = as.integer(sum(TotalHospitalized)),
                  .groups = 'drop')
      dt <- as.data.table(data, TRUE)
      return(dt)
  })
  
  
  # Displaying the line plot for the total vaccinated 
  output$plot6 = renderPlot({
    data <- reactive({ 
      req(input$province1,input$year1)
      df1 <- df %>% filter(Province %in% as.character(provincetag[input$province1])  &  year %in% input$year1) %>% group_by(month) %>% summarise(TotalVaccinated = sum(DailyVaccinated),.groups = 'drop')
    })
    ggplot(data(), aes(x=month,y=TotalVaccinated,group=1)) + geom_line(linetype = "dashed") + geom_point(color="black",size=3) +  theme_ipsum()
  })
  
  # displaying the line plot of each dose of vacination
  output$plot7 = renderPlot({
    data <- reactive({ 
      req(input$province1,input$year1)
     df1 <- df %>% filter(Province %in% as.character(provincetag[input$province1])  &  year %in% input$year1) %>% group_by(month) %>% summarise(TotalDose1 = sum(DailyDose1), TotalDose2= sum(DailyDose2), TotalBooster= sum(DailyBooster),.groups = 'drop')
    })
    ggplot(data(),aes(x= month, group=1)) + 
      geom_line(linetype = "dashed",col="red", aes(y=TotalDose1))  +
      geom_line(linetype = "twodash",col="blue", aes(y=TotalDose2))
  })
  
}
shinyApp(ui, server)

library(tidyverse)
library(shiny)
library(plotly)
library(DT)

covid_stats <- read_csv('Project_Covid19/data/dataset.csv',col_types = cols(.default = "c"))
covid_stats <- covid_stats %>% 
  mutate(Province.State = ifelse(is.na(Province.State),Province_State, Province.State),
                  Country.Region = ifelse(is.na(Country.Region),Country_Region, Country.Region),
                  Last.Update = ifelse(is.na(Last.Update),Last_Update, Last.Update),
                  Latitude = ifelse(is.na(Latitude),Lat,Latitude),
                  Longitude = ifelse(is.na(Longitude),Long_,Longitude)
                  ) %>%
  select(c("Province.State","Country.Region","Last.Update","Confirmed",
           "Deaths","Recovered","Latitude","Longitude","FIPS","Admin2","Active","Combined_Key","filename"))
  

covid_stats <- covid_stats %>% mutate_at(c('Confirmed','Deaths','Recovered'), as.numeric)
covid_stats <- covid_stats %>% mutate(data_date = lubridate::mdy(str_sub(filename,1,10)))%>%
  mutate(Province.State = replace_na(Province.State,'No Province'))



ui <- fluidPage(
  fluidRow(
    column(2, 
           selectInput('country', 'Country', choices = unique(covid_stats$Country.Region)),
           selectInput('province','Province/State',choices = NULL)),
    column(5, plotlyOutput('countryplot')),
    column(5, DT::dataTableOutput('summary'))
  )
)

server <- function(input, output, session){
  country <- reactive({
    filter(covid_stats, Country.Region == input$country)
  })
  observeEvent(country(), {
    choices <- unique(country()$Province.State)
    updateSelectInput(session, "province", choices = choices) 
  })
  
  chart_table <-covid_stats %>% 
      group_by(Country.Region,Province.State,data_date) %>% 
      summarise(Confirmed = sum(Confirmed),
                Recovered = sum(Recovered),
                Deaths = sum(Deaths)) %>%
      mutate(Active = Confirmed-Recovered-Deaths,
             'Recovery %' = round((Recovered/Confirmed*100),2)) %>%
      arrange(desc(Confirmed))%>%
      ungroup()
  
  chart_data <- reactive({chart_table %>% 
      filter(Country.Region==input$country)%>%
      filter(Province.State==input$province)})
  
  summary_table <-  covid_stats %>% filter(data_date == max(data_date)) %>% 
    group_by(Country.Region) %>% 
    summarise(Confirmed = sum(Confirmed),
              Recovered = sum(Recovered),
              Deaths = sum(Deaths)) %>%
    mutate(Active = Confirmed-Recovered-Deaths,
           'Recovery %' = round((Recovered/Confirmed*100),2)) %>%
    arrange(desc(Confirmed))
  
  output$countryplot <-renderPlotly(
    fig <- plot_ly(x = chart_data()$data_date, y = chart_data()$Confirmed, type = 'scatter',
                  name ='Confirmed', mode = 'markers') %>%
      add_trace(y = chart_data()$Recovered, name ='Recovered', mode = 'markers') 
  )
  
   output$summary <- DT::renderDataTable(summary_table,  
                                   options = list(pageLength = 5, orderClasses = TRUE))}


shinyApp(ui, server)


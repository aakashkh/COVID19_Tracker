library(tidyverse)

covid_stats <- read_csv('Project_Covid19/data/dataset.csv',col_types = cols(.default = "c"))
colnames(covid_stats)


#Lats <- covid_stats %>% filter(is.na(Latitude)) %>% select(Lat,Longitude,Long_)
#lats <- covid_stats %>% filter(is.na(Lat)) %>% select(Latitude,Longitude,Long_)

# check data.table
# library(data.table)
#setDT(s)[Group=="", Group:= Group2]
#https://stackoverflow.com/questions/36151572/replace-missing-values-with-a-value-from-another-column
covid_stats <- covid_stats %>% 
  mutate(Province.State = ifelse(is.na(Province.State),Province_State, Province.State),
         Country.Region = ifelse(is.na(Country.Region),Country_Region, Country.Region),
         Last.Update = ifelse(is.na(Last.Update),Last_Update, Last.Update),
         Latitude = ifelse(is.na(Latitude),Lat,Latitude),
         Longitude = ifelse(is.na(Longitude),Long_,Longitude)
  ) %>%
  select(c("Province.State","Country.Region","Last.Update","Confirmed",
           "Deaths","Recovered","Latitude","Longitude","FIPS","Admin2","Active","Combined_Key","filename"))


#covid_stats$Last.Update <- lubridate::parse_date_time(covid_stats$Last.Update, orders = c('%Y-%m-%d %H:%M;%S','%m/%d/%Y %H:%M', '%m/%d/%y %H:%M'))
#covid_stats$Last.Update <- lubridate::as_date(covid_stats$Last.Update)

covid_stats <- covid_stats %>% mutate_at(c('Confirmed','Deaths','Recovered'), as.numeric)
covid_stats <- covid_stats %>% mutate(data_date = lubridate::mdy(str_sub(filename,1,10)))

#covid_stats %>%
#  mutate_at(vars('Confirmed','Deaths','Recovered'), ~replace_na(., 0)) %>%
#  mutate_at(vars('Country.Region','Province.State'), ~replace_na(., ""))

a <- covid_stats %>% 
  group_by(data_date,Country.Region,Province.State) %>% 
  summarise(Confirmed = sum(Confirmed),Deaths = sum(Deaths), Recovered = sum(Recovered)) %>%
  gather('metric','value', -c(data_date,Country.Region,Province.State))

a <- a %>% drop_na(value) %>% mutate(Province.State = replace_na(Province.State,'No Province'))

covid_stats %>% filter(data_date=='2020-04-24') %>% 
  group_by(Country.Region,Last.Update) %>% 
  summarise(confirmed <- sum(Confirmed))

covid_stats %>% filter((data_date=='2020-04-24')&(Country.Region=='US')) %>% 
  group_by(data_date) %>% summarise(recov <- sum(Deaths))


library(shiny)
library(plotly)
library(DT)


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
    filter(a, Country.Region == input$country)
  })
  observeEvent(country(), {
    choices <- unique(country()$Province.State)
    updateSelectInput(session, "province", choices = choices) 
  })
  
  ab<-reactive({a %>% 
      filter(Country.Region==input$country)%>%
      filter(Province.State==input$province)})
  
  output$countryplot <-renderPlotly(
    fig<- plot_ly(x = ab()$data_date, y = ab()$value, split= ab()$metric, type = 'scatter', mode = 'lines'),
  )
  output$summary <- DT::renderDataTable(covid_stats %>% 
                                          filter(data_date == max(data_date)) %>% 
                                          group_by(Country.Region) %>% 
                                          summarise(Confirmed = sum(Confirmed),
                                                    Recovered = sum(Recovered),
                                                    Deaths = sum(Deaths)) %>%
                                          mutate(Active = Confirmed-Recovered-Deaths,
                                                 'Recovery %' = round((Recovered/Confirmed*100),2)) %>%
                                          arrange(desc(Confirmed)),  
                                        options = list(pageLength = 5, orderClasses = TRUE))}


shinyApp(ui, server)


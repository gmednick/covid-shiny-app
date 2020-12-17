library(tidyverse)
library(lubridate)
library(shiny)
library(shinydashboard)
library(usmap)
library(ggrepel)
library(plotly)
library(viridis)
theme_set(theme_light())
scale_colour_discrete <- scale_colour_viridis_d

url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/"
data <- c("time_series_covid19_confirmed_global.csv",
          "time_series_covid19_deaths_global.csv",
          "time_series_covid19_confirmed_US.csv",
          "time_series_covid19_deaths_US.csv")
urls <- str_c(url, data)

us_confirmed <- read_csv(urls[3]) %>%
  pivot_longer(cols = -(UID:Combined_Key), names_to = "date", values_to = "cases") %>%
  select(Admin2:cases) %>%
  janitor::clean_names() %>% 
  mutate(date = mdy(date))
us_deaths <- read_csv(urls[4]) %>%
  pivot_longer(cols = -(UID:Population), names_to = "date", values_to ="deaths") %>%
  select(Admin2:deaths) %>%
  janitor::clean_names() %>% 
  mutate(date = mdy(date))

us_data <- us_deaths %>%
  select(deaths, date, combined_key, population) %>% 
  left_join(us_confirmed, by = c('date', 'combined_key')) %>% 
  select(date, province_state, cases, deaths, population, lon = long, lat) %>% 
  filter(!province_state %in% c('Diamond Princess', 'Grand Princess')) 

us_state <- us_data %>%
  group_by(province_state, date) %>%
  summarize(cases = sum(cases), 
            deaths = sum(deaths),
            population = sum(population)) %>%
  ungroup()

us_totals <- us_state %>% 
  group_by(province_state) %>% 
  summarize(cases = max(cases),
            deaths = max(deaths),
            population = max(population)) %>%
  filter(cases > 0) %>%
  mutate(deaths_per_1e6 = 1000000 * (deaths / population),
         cases_per_1e6 = 1000000 * (cases / population),
         state = province_state) %>%
  filter(!is.na(deaths_per_1e6)) %>% 
  arrange(desc(cases))

us_total_sums <- us_data %>% 
  group_by(date) %>%
  summarize(total_cases = sum(cases), 
            total_deaths = sum(deaths))

cbs <- plot_usmap(data = us_totals, 
                  values = "cases_per_1e6",
                  color = "black",
                  labels = FALSE) +
  ggplot2::aes(text = paste0('State: ', province_state,
                             '\nPopulation: ', format(population, big.mark = ','),
                             '\nTotal cases: ', format(cases, big.mark = ','),
                             '\nCases per mill: ', format(round(cases_per_1e6, 0), big.mark = ',')
  )) +
  scale_fill_viridis_c(name = "Cases per million",
                       alpha = 0.5) +
  theme(legend.position = "right",  plot.title = element_text(face = 'bold', size = 18, color = '#367588')) +
  labs(title = paste0("Total Cases in the USA: ", formatC(max(us_total_sums$total_cases), format = 'd', big.mark = ',')))

dbs <- plot_usmap(data = us_totals, 
                  values = "deaths_per_1e6",
                  color = "black",
                  labels = FALSE) +
  ggplot2::aes(text = paste0('State: ', province_state,
                             '\nPopulation: ', format(population, big.mark = ','),
                             '\nTotal deaths: ', format(deaths, big.mark = ','),
                             '\nDeaths per mill: ', format(round(deaths_per_1e6, 0), big.mark = ',')
  )) +
  scale_fill_viridis_c(name = "Deaths per million",
                       alpha = 0.5) +
  theme(legend.position = "right",  plot.title = element_text(face = 'bold', size = 18, color = '#367588')) +
  labs(title = paste0("Total Deaths in the USA: ", formatC(max(us_total_sums$total_deaths), format = 'd', big.mark = ",")))
#------------------------------------------------------------

ui <- dashboardPage(
  dashboardHeader(title = 'Covid Case Tracker'),
  
  dashboardSidebar(
    selectInput("province_state", 
                label = "State:",
                choices = unique(us_data$province_state), 
                selected = c('California', 'Hawaii', 'Florida'),
                selectize = TRUE,
                multiple = TRUE),
    tabItem(tabName = "covid",
            imageOutput("picture", height='auto'))
    
  ),
  
  dashboardBody(
    fluidRow(box(plotOutput('cases')), box(plotlyOutput('caseMap'))),
    fluidRow(box(plotOutput('deaths')), box(plotlyOutput('deathsMap')))
  )
)
server <- function(input, output) {
  # sidebar
  output$picture <- renderImage({
    return(list(src = "covid.jpg", contentType = "image/jpg", alt = "covid", height = 195))
  }, deleteFile = FALSE) #where the src is wherever you have the picture
  # dashboard body
  output$cases <- renderPlot({
    us_data %>% 
      group_by(province_state, date) %>% 
      summarise(cases_n = sum(cases),
                deaths_n = sum(deaths)
      ) %>% 
      mutate(new_cases_n = cases_n - lag(cases_n, default = 0),
             new_deaths_n = deaths_n - lag(deaths_n, default = 0)) %>% 
      ungroup() %>% 
      filter(province_state %in% input$province_state) %>% 
      ggplot(aes(x = date, y = new_cases_n, color = province_state)) +
      geom_line(size = 1, alpha = 0.5) +
      theme(plot.title = element_text(hjust = 0.5, face = 'bold', color = '#367588', size = 18)) +
      labs(
        title = 'Cases per Day',
        subtitle = paste0("latest data from: ", as.character(max(us_total_sums$date))),
        y = 'Cases',
        color = 'State') +
      geom_text_repel(data=. %>% 
                        arrange(desc(date)) %>% 
                        group_by(province_state) %>% 
                        slice(1), 
                      aes(label= new_cases_n), 
                      position=position_nudge(8), hjust= -5, show.legend=FALSE)
  })
  
  output$caseMap <- renderPlotly({
    ggplotly(cbs,
             tooltip = 'text') %>% 
      config(displayModeBar = FALSE)
  })  
  
  output$deaths <- renderPlot({
    us_data %>% 
      group_by(province_state, date) %>% 
      summarise(cases_n = sum(cases),
                deaths_n = sum(deaths)
      ) %>% 
      mutate(new_cases_n = cases_n - lag(cases_n, default = 0),
             new_deaths_n = deaths_n - lag(deaths_n, default = 0)) %>% 
      ungroup() %>% 
      filter(province_state %in% input$province_state) %>% 
      ggplot(aes(x = date, y = new_deaths_n, color = province_state)) +
      geom_line(size = 1, alpha = 0.5)   +
      theme(plot.title = element_text(hjust = 0.5, face = 'bold', color = '#367588', size = 18)) +
      labs(
        title = 'Deaths per Day',
        subtitle = paste0("latest data from: ", as.character(max(us_total_sums$date))),
        y = 'Deaths',
        color = 'State'
      ) +
      geom_text_repel(data=. %>% 
                        arrange(desc(date)) %>% 
                        group_by(province_state) %>% 
                        slice(1), 
                      aes(label= new_deaths_n), 
                      position=position_nudge(8), 
                      hjust=-5, show.legend=FALSE)
  })
  
  output$deathsMap <- renderPlotly({
    ggplotly(dbs,
             tooltip = 'text') %>% 
      config(displayModeBar = FALSE)
  })
}

shinyApp(ui = ui, server = server)

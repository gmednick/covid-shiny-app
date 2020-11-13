library(tidyverse)
library(lubridate)
library(tidymodels)
library(shiny)
library(shinydashboard)
library(usmap)
library(ggrepel)
library(flexdashboard)
theme_set(theme_light())

url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/"
data <- c("time_series_covid19_confirmed_global.csv",
          "time_series_covid19_deaths_global.csv",
          "time_series_covid19_confirmed_US.csv",
          "time_series_covid19_deaths_US.csv")
urls <- str_c(url, data)

# get population data
uid_lookup_url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/UID_ISO_FIPS_LookUp_Table.csv"

uid <- read_csv(uid_lookup_url) %>%
    select(-c(Lat, Long_, Combined_Key, code3, iso2, iso3, Admin2))

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

us_totals <- us_data %>% 
    group_by(province_state) %>% 
    summarize(cases = max(cases),
              deaths = max(deaths),
              population = max(population)) %>%
    filter(cases > 0) %>%
    mutate(deaths_per_1e6 = 1000000 * deaths / population,
           cases_per_1e6 = 1000000 * cases / population,
           state = province_state) %>%
    filter(!is.na(deaths_per_1e6)) %>% 
    arrange(desc(cases))

us_total_sums <- us_data %>% 
    group_by(date) %>%
    summarize(total_cases = sum(cases), 
              total_deaths = sum(deaths))
test <- us_data %>%
    distinct(province_state, .keep_all = T) %>% 
    drop_na(population) %>% 
    summarize(pop_tot = sum(population))
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
                imageOutput("picture", height='auto')),
        fluidRow(box(gaugeOutput(outputId = 'Case_Guage', width = "100%", height = "200px")))
        
        ),
       
    dashboardBody(
        fluidRow(box(plotOutput('cases')), box(plotOutput('caseMap'))),
        fluidRow(box(plotOutput('deaths')), box(plotOutput('deathsMap')))
    )
)
server <- function(input, output) {
    # sidebar
    output$picture <- renderGauge(test, env = parent.frame(), quoted = FALSE)
    
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
            theme(plot.title = element_text(hjust = 0.5, face = 'bold', color = '#367588', size = 17)) +
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
                            position=position_nudge(4), hjust= 1, show.legend=FALSE)
    })
    
    output$caseMap <- renderPlot({
        plot_usmap(data = us_totals, 
                   values = "cases_per_1e6",
                   color = "black",
                   labels = FALSE) + 
            scale_fill_gradient(name = "Cases per million",
                                low = "lightgreen", high = "tomato") +
            theme(legend.position = "right",  plot.title = element_text(hjust = 0.5, face = 'bold', size = 20, color = '#367588')) +
            labs(title = paste0("Total Cases in the USA: ", formatC(max(us_total_sums$total_cases), format = 'd', big.mark = ',')))
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
            theme(plot.title = element_text(hjust = 0.5, face = 'bold', color = '#367588', size = 16)) +
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
                      position=position_nudge(4), 
                      hjust=1, show.legend=FALSE)
    })
    
    output$deathsMap <- renderPlot({
        plot_usmap(data = us_totals, 
                   values = "deaths_per_1e6",
                   color = "black",
                   labels = FALSE) + 
            scale_fill_gradient(name = "Deaths per million",
                                low = "lightgreen", high = "tomato") +
            theme(legend.position = "right",  plot.title = element_text(hjust = 0.5, face = 'bold', size = 20, color = '#367588')) +
            labs(title = paste0("Total Deaths in the USA: ", formatC(max(us_total_sums$total_deaths), format = 'd', big.mark = ",")))
    })
}

shinyApp(ui = ui, server = server)

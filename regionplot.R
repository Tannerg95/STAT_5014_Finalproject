library(shiny)

ui <- fluidPage(
  navbarPage(collapsible = TRUE,
             "COVID-19 tracker", id="nav",
             
             tabPanel("Region plots",
           headerPanel('Covid-19 Dashboard'),
           sidebarPanel(
             selectInput("level_select", "Level:",   
                choices = c("Global", "Country", "US state"), 
                selected = c("Country"),
                multiple = FALSE),
             
             selectInput("region_select", "Country/Region:",   
                choices = c("Albania","Australia","Brazil","China","Cuba","Egypt","France","Iceland","Indonesia","Italy","Japan","US"),
                selected = c("US"),
                multiple = FALSE),
             
             selectInput("outcome_select", "Outcome:",   
                choices = c("Confirmed", "Deaths", "Recovered"), 
                selected = c("Confirmed"),
                multiple = FALSE),
             ),
           mainPanel(
             plotOutput('plot1')
             ))
  )
  
)

server <- function(input,output){
    plottest <- reactive({
      if (("Global" %in% input$level_select)&&("Confirmed" %in% input$outcome_select)) return(globalplot.confirmed)
      if (("Country" %in% input$level_select) &&("Albania" %in% input$region_select)&&("Confirmed" %in% input$outcome_select)) return(Albaniaplot.confirmed)
      if (("Country" %in% input$level_select) &&("Australia" %in% input$region_select)&&("Confirmed" %in% input$outcome_select)) return(Australiaplot.confirmed)
      if (("Country" %in% input$level_select) &&("Brazil" %in% input$region_select)&&("Confirmed" %in% input$outcome_select)) return(Brazilplot.confirmed)
      if (("Country" %in% input$level_select) &&("China" %in% input$region_select)&&("Confirmed" %in% input$outcome_select)) return(Chinaplot.confirmed)
      if (("Country" %in% input$level_select) &&("Cuba" %in% input$region_select)&&("Confirmed" %in% input$outcome_select)) return(Cubaplot.confirmed)
      if (("Country" %in% input$level_select) &&("Egypt" %in% input$region_select)&&("Confirmed" %in% input$outcome_select)) return(Egyptplot.confirmed)
      if (("Country" %in% input$level_select) &&("France" %in% input$region_select)&&("Confirmed" %in% input$outcome_select)) return(Franceplot.confirmed)
      if (("Country" %in% input$level_select) &&("Iceland" %in% input$region_select)&&("Confirmed" %in% input$outcome_select)) return(Icelandplot.confirmed)
      if (("Country" %in% input$level_select) &&("Indonesia" %in% input$region_select)&&("Confirmed" %in% input$outcome_select)) return(Indonesiaplot.confirmed)
      if (("Country" %in% input$level_select) &&("Italy" %in% input$region_select)&&("Confirmed" %in% input$outcome_select)) return(Italyplot.confirmed)
      if (("Country" %in% input$level_select) &&("Japan" %in% input$region_select)&&("Confirmed" %in% input$outcome_select)) return(Japanplot.confirmed)
      if (("Country" %in% input$level_select) &&("US" %in% input$region_select)&&("Confirmed" %in% input$outcome_select)) return(USplot.confirmed)
      
      if (("Global" %in% input$level_select)&&("Recovered" %in% input$outcome_select)) return(globalplot.recovered)
      if (("Country" %in% input$level_select) &&("Albania" %in% input$region_select)&&("Recovered" %in% input$outcome_select)) return(Albaniaplot.recovered)
      if (("Country" %in% input$level_select) &&("Australia" %in% input$region_select)&&("Recovered" %in% input$outcome_select)) return(Australiaplot.recovered)
      if (("Country" %in% input$level_select) &&("Brazil" %in% input$region_select)&&("Recovered" %in% input$outcome_select)) return(Brazilplot.recovered)
      if (("Country" %in% input$level_select) &&("China" %in% input$region_select)&&("Recovered" %in% input$outcome_select)) return(Chinaplot.recovered)
      if (("Country" %in% input$level_select) &&("Cuba" %in% input$region_select)&&("Recovered" %in% input$outcome_select)) return(Cubaplot.recovered)
      if (("Country" %in% input$level_select) &&("Egypt" %in% input$region_select)&&("Recovered" %in% input$outcome_select)) return(Egyptplot.recovered)
      if (("Country" %in% input$level_select) &&("France" %in% input$region_select)&&("Recovered" %in% input$outcome_select)) return(Franceplot.recovered)
      if (("Country" %in% input$level_select) &&("Iceland" %in% input$region_select)&&("Recovered" %in% input$outcome_select)) return(Icelandplot.recovered)
      if (("Country" %in% input$level_select) &&("Indonesia" %in% input$region_select)&&("Recovered" %in% input$outcome_select)) return(Indonesiaplot.recovered)
      if (("Country" %in% input$level_select) &&("Italy" %in% input$region_select)&&("Recovered" %in% input$outcome_select)) return(Italyplot.recovered)
      if (("Country" %in% input$level_select) &&("Japan" %in% input$region_select)&&("Recovered" %in% input$outcome_select)) return(Japanplot.recovered)
      if (("Country" %in% input$level_select) &&("US" %in% input$region_select)&&("Recovered" %in% input$outcome_select)) return(USplot.recovered)
      
      if (("Global" %in% input$level_select)&&("Deaths" %in% input$outcome_select)) return(globalplot.deaths)
      if (("Country" %in% input$level_select) &&("Albania" %in% input$region_select)&&("Deaths" %in% input$outcome_select)) return(Albaniaplot.deaths)
      if (("Country" %in% input$level_select) &&("Australia" %in% input$region_select)&&("Deaths" %in% input$outcome_select)) return(Australiaplot.deaths)
      if (("Country" %in% input$level_select) &&("Brazil" %in% input$region_select)&&("Deaths" %in% input$outcome_select)) return(Brazilplot.deaths)
      if (("Country" %in% input$level_select) &&("China" %in% input$region_select)&&("Deaths" %in% input$outcome_select)) return(Chinaplot.deaths)
      if (("Country" %in% input$level_select) &&("Cuba" %in% input$region_select)&&("Deaths" %in% input$outcome_select)) return(Cubaplot.deaths)
      if (("Country" %in% input$level_select) &&("Egypt" %in% input$region_select)&&("Deaths" %in% input$outcome_select)) return(Egyptplot.deaths)
      if (("Country" %in% input$level_select) &&("France" %in% input$region_select)&&("Deaths" %in% input$outcome_select)) return(Franceplot.deaths)
      if (("Country" %in% input$level_select) &&("Iceland" %in% input$region_select)&&("Deaths" %in% input$outcome_select)) return(Icelandplot.deaths)
      if (("Country" %in% input$level_select) &&("Indonesia" %in% input$region_select)&&("Deaths" %in% input$outcome_select)) return(Indonesiaplot.deaths)
      if (("Country" %in% input$level_select) &&("Italy" %in% input$region_select)&&("Deaths" %in% input$outcome_select)) return(Italyplot.deaths)
      if (("Country" %in% input$level_select) &&("Japan" %in% input$region_select)&&("Deaths" %in% input$outcome_select)) return(Japanplot.deaths)
      if (("Country" %in% input$level_select) &&("US" %in% input$region_select)&&("Deaths" %in% input$outcome_select)) return(USplot.deaths)
    })
    
    output$plot1 <- renderPlot({
    par(mar=c(5.1,4.1,0,1))
      dataplots <- plottest()
      print(dataplots)
    
    })
    
    output$map <- renderPlot(map)
}

## Import data: Source from Johns Hopkins Github data
confirmed_data <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
deaths_data <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
recovered_data <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv")


# Data cleaning: To create country level and global level data
library(tidyr)
library(dplyr)
## Country level
confirmed <- confirmed_data %>% gather(key="date", value="confirmed", -c(Country.Region, Province.State, Lat, Long)) %>% group_by(Country.Region, date) %>% summarize(confirmed=sum(confirmed))

deaths <- deaths_data %>% gather(key="date", value="deaths", -c(Country.Region, Province.State, Lat, Long)) %>% group_by(Country.Region, date) %>% summarize(deaths=sum(deaths))

recovered <- recovered_data %>% gather(key="date", value="recovered", -c(Country.Region, Province.State, Lat, Long)) %>% group_by(Country.Region, date) %>% summarize(recovered=sum(recovered))


# Create final country level dataset: combine all three variables
country_data <- full_join(confirmed, deaths) %>% full_join(recovered)
country_data$date <- country_data$date %>% sub("X", "", .) %>% as.Date("%m.%d.%y")

# Create new variable: number of days
country_data <- country_data %>% group_by(Country.Region) %>% mutate(cumconfirmed=cumsum(confirmed), days = date - first(date) + 1)

## World level
world_data <- country_data %>% group_by(date) %>% summarize(confirmed=sum(confirmed), cumconfirmed=sum(cumconfirmed), deaths=sum(deaths), recovered=sum(recovered)) %>% mutate(days = date - first(date) + 1)

# Specific countries levels
Albania <- country_data %>% filter(Country.Region=="Albania")
Australia <- country_data %>% filter(Country.Region=="Australia")
Brazil <- country_data %>% filter(Country.Region=="Brazil")
China <- country_data %>% filter(Country.Region=="China")
Cuba <- country_data %>% filter(Country.Region=="Cuba")
Egypt <- country_data %>% filter(Country.Region=="Egypt")
France <- country_data %>% filter(Country.Region=="France")
Iceland <- country_data %>% filter(Country.Region=="Iceland")
Indonesia <- country_data %>% filter(Country.Region=="Indonesia")
Italy <- country_data %>% filter(Country.Region=="Italy")
Japan <- country_data %>% filter(Country.Region=="Japan")
US <- country_data %>% filter(Country.Region=="US")

## confirmed
globalplot.confirmed <-ggplot(world_data, aes(x=date, y=confirmed)) + 
  geom_line( color="#69b3a2", size=2, alpha=1.0) +
  labs(title = "Covid-19 Global Confirmed Cases", x="Date (2020)", y="Daily Confirmed Cases")+
  scale_y_continuous(limits = c(0, 7e+07), breaks = seq(0, 7e+07, by = 1e+07))+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

Albaniaplot.confirmed <-ggplot(Albania, aes(x=date, y=confirmed)) + 
  geom_line( color="coral1", size=2, alpha=1.0) +
  labs(title = "Albania Covid-19 Confirmed Cases", x="Date (2020)", y="Daily Confirmed Cases")+
  scale_y_continuous(limits = c(0, 2e+05), breaks = seq(0, 2e+05, by = 0.2e+05))+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

Australiaplot.confirmed <- ggplot(Australia, aes(x=date, y=confirmed)) + 
  geom_line( color="brown", size=2, alpha=1.0) +
  labs(title = "Australia Covid-19 Confirmed Cases", x="Date (2020)", y="Daily Confirmed Cases")+
  scale_y_continuous(limits = c(0, 2e+05), breaks = seq(0, 2e+05, by = 0.2e+05))+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

Brazilplot.confirmed <-ggplot(Brazil, aes(x=date, y=confirmed)) + 
  geom_line( color="darkgreen", size=2, alpha=1.0) +
  labs(title = "Brazil Covid-19 Confirmed Cases", x="Date (2020)", y="Daily Confirmed Cases")+
  scale_y_continuous(limits = c(0, 1.5e+07), breaks = seq(0, 1.5e+07, by = 0.2e+07))+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

Chinaplot.confirmed <-ggplot(China, aes(x=date, y=confirmed)) + 
  geom_line( color="darkorange", size=2, alpha=1.0) +
  labs(title = "China Covid-19 Confirmed Cases", x="Date (2020)", y="Daily Confirmed Cases")+
  scale_y_continuous(limits = c(0, 1e+06), breaks = seq(0, 1e+06, by = 0.2e+06))+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

Cubaplot.confirmed <-ggplot(Cuba, aes(x=date, y=confirmed)) + 
  geom_line( color="darkorchid", size=2, alpha=1.0) +
  labs(title = "Cuba Covid-19 Confirmed Cases", x="Date (2020)", y="Daily Confirmed Cases")+
  scale_y_continuous(limits = c(0, 1e+06), breaks = seq(0, 1e+06, by = 0.2e+06))+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

Egyptplot.confirmed <-ggplot(Egypt, aes(x=date, y=confirmed)) + 
  geom_line( color="cadetblue", size=2, alpha=1.0) +
  labs(title = "Egypt Covid-19 Confirmed Cases", x="Date (2020)", y="Daily Confirmed Cases")+
  scale_y_continuous(limits = c(0, 2e+05), breaks = seq(0, 2e+05, by = 0.2e+05))+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

Franceplot.confirmed <-ggplot(France, aes(x=date, y=confirmed)) + 
  geom_line( color="deeppink", size=2, alpha=1.0) +
  labs(title = "France Covid-19 Confirmed Cases", x="Date (2020)", y="Daily Confirmed Cases")+
  scale_y_continuous(limits = c(0, 3e+06), breaks = seq(0, 3e+06, by = 0.2e+06))+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

Icelandplot.confirmed <-ggplot(Iceland, aes(x=date, y=confirmed)) + 
  geom_line( color="darkseagreen", size=2, alpha=1.0) +
  labs(title = "Iceland Covid-19 Confirmed Cases", x="Date (2020)", y="Daily Confirmed Cases")+
  scale_y_continuous(limits = c(0, 1e+05), breaks = seq(0, 1e+05, by = 0.2e+05))+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

Indonesiaplot.confirmed <-ggplot(Indonesia, aes(x=date, y=confirmed)) + 
  geom_line( color="deepskyblue4", size=2, alpha=1.0) +
  labs(title = "Indonesia Covid-19 Confirmed Cases", x="Date (2020)", y="Daily Confirmed Cases")+
  scale_y_continuous(limits = c(0, 1e+06), breaks = seq(0, 1e+06, by = 0.2e+06))+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

Italyplot.confirmed <-ggplot(Italy, aes(x=date, y=confirmed)) + 
  geom_line( color="coral1", size=2, alpha=1.0) +
  labs(title = "Italy Covid-19 Confirmed Cases", x="Date (2020)", y="Daily Confirmed Cases")+
  scale_y_continuous(limits = c(0, 2e+06), breaks = seq(0, 2e+06, by = 0.2e+06))+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

Japanplot.confirmed <-ggplot(Japan, aes(x=date, y=confirmed)) + 
  geom_line( color="Black", size=2, alpha=1.0) +
  labs(title = "Japan Covid-19 Confirmed Cases", x="Date (2020)", y="Daily Confirmed Cases")+
  scale_y_continuous(limits = c(0, 1e+06), breaks = seq(0, 1e+06, by = 0.2e+06))+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

USplot.confirmed <-ggplot(US, aes(x=date, y=confirmed)) + 
  geom_line( color="darkviolet", size=2, alpha=1.0) +
  labs(title = "US Covid-19 Confirmed Cases", x="Date (2020)", y="Daily Confirmed Cases")+
  scale_y_continuous(limits = c(0, 1.5e+07), breaks = seq(0, 1.5e+07, by = 0.2e+07))+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

## recovered
globalplot.recovered <-ggplot(world_data, aes(x=date, y=recovered)) + 
  geom_line( color="#69b3a2", size=2, alpha=1.0) +
  labs(title = "Covid-19 Global Recovered Cases", x="Date (2020)", y="Daily Recovered Cases")+
  scale_y_continuous(limits = c(0, 7e+07), breaks = seq(0, 7e+07, by = 1e+07))+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

Albaniaplot.recovered <-ggplot(Albania, aes(x=date, y=recovered)) + 
  geom_line( color="coral1", size=2, alpha=1.0) +
  labs(title = "Albania Covid-19 Recovered Cases", x="Date (2020)", y="Daily Recovered Cases")+
  scale_y_continuous(limits = c(0, 2e+05), breaks = seq(0, 2e+05, by = 0.2e+05))+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

Australiaplot.recovered <- ggplot(Australia, aes(x=date, y=recovered)) + 
  geom_line( color="brown", size=2, alpha=1.0) +
  labs(title = "Australia Covid-19 Recovered Cases", x="Date (2020)", y="Daily Recovered Cases")+
  scale_y_continuous(limits = c(0, 2e+05), breaks = seq(0, 2e+05, by = 0.2e+05))+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

Brazilplot.recovered <-ggplot(Brazil, aes(x=date, y=recovered)) + 
  geom_line( color="darkgreen", size=2, alpha=1.0) +
  labs(title = "Brazil Covid-19 Recovered Cases", x="Date (2020)", y="Daily Recovered Cases")+
  scale_y_continuous(limits = c(0, 1.5e+07), breaks = seq(0, 1.5e+07, by = 0.2e+07))+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

Chinaplot.recovered <-ggplot(China, aes(x=date, y=recovered)) + 
  geom_line( color="darkorange", size=2, alpha=1.0) +
  labs(title = "China Covid-19 Recovered Cases", x="Date (2020)", y="Daily Recovered Cases")+
  scale_y_continuous(limits = c(0, 2e+05), breaks = seq(0, 2e+05, by = 0.2e+05))+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

Cubaplot.recovered <-ggplot(Cuba, aes(x=date, y=recovered)) + 
  geom_line( color="darkorchid", size=2, alpha=1.0) +
  labs(title = "Cuba Covid-19 Recovered Cases", x="Date (2020)", y="Daily Recovered Cases")+
  scale_y_continuous(limits = c(0, 2e+05), breaks = seq(0, 2e+05, by = 0.2e+05))+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

Egyptplot.recovered <-ggplot(Egypt, aes(x=date, y=recovered)) + 
  geom_line( color="cadetblue", size=2, alpha=1.0) +
  labs(title = "Egypt Covid-19 Recovered Cases", x="Date (2020)", y="Daily Recovered Cases")+
  scale_y_continuous(limits = c(0, 2e+05), breaks = seq(0, 2e+05, by = 0.2e+05))+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

Franceplot.recovered <-ggplot(France, aes(x=date, y=recovered)) + 
  geom_line( color="deeppink", size=2, alpha=1.0) +
  labs(title = "France Covid-19 Recovered Cases", x="Date (2020)", y="Daily Recovered Cases")+
  scale_y_continuous(limits = c(0, 3e+05), breaks = seq(0, 3e+05, by = 0.3e+05))+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

Icelandplot.recovered <-ggplot(Iceland, aes(x=date, y=recovered)) + 
  geom_line( color="darkseagreen", size=2, alpha=1.0) +
  labs(title = "Iceland Covid-19 Recovered Cases", x="Date (2020)", y="Daily Recovered Cases")+
  scale_y_continuous(limits = c(0, 1e+05), breaks = seq(0, 1e+05, by = 0.2e+05))+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

Indonesiaplot.recovered <-ggplot(Indonesia, aes(x=date, y=recovered)) + 
  geom_line( color="deepskyblue4", size=2, alpha=1.0) +
  labs(title = "Indonesia Covid-19 Recovered Cases", x="Date (2020)", y="Daily Recovered Cases")+
  scale_y_continuous(limits = c(0, 1e+06), breaks = seq(0, 1e+06, by = 0.2e+06))+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

Italyplot.recovered <-ggplot(Italy, aes(x=date, y=recovered)) + 
  geom_line( color="coral1", size=2, alpha=1.0) +
  labs(title = "Italy Covid-19 Recovered Cases", x="Date (2020)", y="Daily Recovered Cases")+
  scale_y_continuous(limits = c(0, 1e+06), breaks = seq(0, 1e+06, by = 0.2e+06))+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

Japanplot.recovered <-ggplot(Japan, aes(x=date, y=recovered)) + 
  geom_line( color="Black", size=2, alpha=1.0) +
  labs(title = "Japan Covid-19 Recovered Cases", x="Date (2020)", y="Daily Recovered Cases")+
  scale_y_continuous(limits = c(0, 2e+05), breaks = seq(0, 2e+05, by = 0.2e+05))+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

USplot.recovered <-ggplot(US, aes(x=date, y=recovered)) + 
  geom_line( color="darkviolet", size=2, alpha=1.0) +
  labs(title = "US Covid-19 Recovered Cases", x="Date (2020)", y="Daily Recovered Cases")+
  scale_y_continuous(limits = c(0, 8e+06), breaks = seq(0, 8e+06, by = 0.8e+06))+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

## death
globalplot.deaths <-ggplot(world_data, aes(x=date, y=deaths)) + 
  geom_line( color="#69b3a2", size=2, alpha=1.0) +
  labs(title = "Covid-19 Global Deaths Cases", x="Date (2020)", y="Daily Deaths Cases")+
  scale_y_continuous(limits = c(0, 2e+06), breaks = seq(0, 2e+06, by = 0.3e+06))+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

Albaniaplot.deaths <-ggplot(Albania, aes(x=date, y=deaths)) + 
  geom_line( color="coral1", size=2, alpha=1.0) +
  labs(title = "Albania Covid-19 Deaths Cases", x="Date (2020)", y="Daily Deaths Cases")+
  scale_y_continuous(limits = c(0, 5e+04), breaks = seq(0, 5e+04, by = 1e+04))+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

Australiaplot.deaths <- ggplot(Australia, aes(x=date, y=deaths)) + 
  geom_line( color="brown", size=2, alpha=1.0) +
  labs(title = "Australia Covid-19 Deaths Cases", x="Date (2020)", y="Daily Deaths Cases")+
  scale_y_continuous(limits = c(0, 5e+04), breaks = seq(0, 5e+04, by = 1e+04))+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

Brazilplot.deaths <-ggplot(Brazil, aes(x=date, y=deaths)) + 
  geom_line( color="darkgreen", size=2, alpha=1.0) +
  labs(title = "Brazil Covid-19 Deaths Cases", x="Date (2020)", y="Daily Deaths Cases")+
  scale_y_continuous(limits = c(0, 3e+05), breaks = seq(0, 3e+05, by = 0.5e+05))+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

Chinaplot.deaths <-ggplot(China, aes(x=date, y=deaths)) + 
  geom_line( color="darkorange", size=2, alpha=1.0) +
  labs(title = "China Covid-19 Deaths Cases", x="Date (2020)", y="Daily Deaths Cases")+
  scale_y_continuous(limits = c(0, 5e+04), breaks = seq(0, 5e+04, by = 1e+04))+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

Cubaplot.deaths <-ggplot(Cuba, aes(x=date, y=deaths)) + 
  geom_line( color="darkorchid", size=2, alpha=1.0) +
  labs(title = "Cuba Covid-19 Deaths Cases", x="Date (2020)", y="Daily Deaths Cases")+
  scale_y_continuous(limits = c(0, 5e+04), breaks = seq(0, 5e+04, by = 1e+04))+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

Egyptplot.deaths <-ggplot(Egypt, aes(x=date, y=deaths)) + 
  geom_line( color="cadetblue", size=2, alpha=1.0) +
  labs(title = "Egypt Covid-19 Deaths Cases", x="Date (2020)", y="Daily Deaths Cases")+
  scale_y_continuous(limits = c(0, 5e+04), breaks = seq(0, 5e+04, by = 1e+04))+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

Franceplot.deaths <-ggplot(France, aes(x=date, y=deaths)) + 
  geom_line( color="deeppink", size=2, alpha=1.0) +
  labs(title = "France Covid-19 Deaths Cases", x="Date (2020)", y="Daily Deaths Cases")+
  scale_y_continuous(limits = c(0, 5e+04), breaks = seq(0, 5e+04, by = 1e+04))+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))
  
Icelandplot.deaths <-ggplot(Iceland, aes(x=date, y=deaths)) + 
  geom_line( color="darkseagreen", size=2, alpha=1.0) +
  labs(title = "Iceland Covid-19 Deaths Cases", x="Date (2020)", y="Daily Deaths Cases")+
  scale_y_continuous(limits = c(0, 5e+04), breaks = seq(0, 5e+04, by = 1e+04))+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

Indonesiaplot.deaths <-ggplot(Indonesia, aes(x=date, y=deaths)) + 
  geom_line( color="deepskyblue4", size=2, alpha=1.0) +
  labs(title = "Indonesia Covid-19 Deaths Cases", x="Date (2020)", y="Daily Deaths Cases")+
  scale_y_continuous(limits = c(0, 5e+04), breaks = seq(0, 5e+04, by = 1e+04))+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

Italyplot.deaths <-ggplot(Italy, aes(x=date, y=deaths)) + 
  geom_line( color="coral1", size=2, alpha=1.0) +
  labs(title = "Italy Covid-19 Deaths Cases", x="Date (2020)", y="Daily Deaths Cases")+
  scale_y_continuous(limits = c(0, 5e+04), breaks = seq(0, 5e+04, by = 1e+04))+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

Japanplot.deaths <-ggplot(Japan, aes(x=date, y=deaths)) + 
  geom_line( color="Black", size=2, alpha=1.0) +
  labs(title = "Japan Covid-19 Deaths Cases", x="Date (2020)", y="Daily Deaths Cases")+
  scale_y_continuous(limits = c(0, 5e+04), breaks = seq(0, 5e+04, by = 1e+04))+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

USplot.deaths <-ggplot(US, aes(x=date, y=deaths)) + 
  geom_line( color="darkviolet", size=2, alpha=1.0) +
  labs(title = "US Covid-19 Deaths Cases", x="Date (2020)", y="Daily Deaths Cases")+
  scale_y_continuous(limits = c(0, 3e+05), breaks = seq(0, 3e+05, by = 0.5e+05))+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))





shinyApp(ui=ui,server = server)

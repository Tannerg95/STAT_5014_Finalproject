# Import data: Source from Johns Hopkins Github data
confirmed_data <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
tail(confirmed_data) # Check latest date at the end of data
deaths_data <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
recovered_data <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv")


# Data cleaning: To create country level and global level data
library(tidyr)
library(dplyr)
## Country level
confirmed <- confirmed_data %>% gather(key="date", value="confirmed", -c(Country.Region, Province.State, Lat, Long)) %>% group_by(Country.Region, date) %>% summarize(confirmed=sum(confirmed))

deaths <- deaths_data %>% gather(key="date", value="deaths", -c(Country.Region, Province.State, Lat, Long)) %>% group_by(Country.Region, date) %>% summarize(deaths=sum(deaths))

recovered <- recovered_data %>% gather(key="date", value="recovered", -c(Country.Region, Province.State, Lat, Long)) %>% group_by(Country.Region, date) %>% summarize(recovered=sum(recovered))

summary(confirmed)

# Create final country level dataset: combine all three variables
country_data <- full_join(confirmed, deaths) %>% full_join(recovered)

# Fix date variable and convert them to date
str(country_data)
country_data$date <- country_data$date %>% sub("X", "", .) %>% as.Date("%m.%d.%y")
str(country_data) 

# Create new variable: number of days
country_data <- country_data %>% group_by(Country.Region) %>% mutate(cumconfirmed=cumsum(confirmed), days = date - first(date) + 1)
head(country_data) # check the variables of data and days 

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


# Summary statistics
summary(country_data)
by(country_data$confirmed, country_data$Country.Region, summary)
by(country_data$cumconfirmed, country_data$Country.Region, summary)
by(country_data$deaths, country_data$Country.Region, summary)
by(country_data$recovered, country_data$Country.Region, summary)
summary(world_data)


# GRAPHS
library(ggplot2)
#comfirmed
# World confirmed
ggplot(world_data, aes(x=date, y=confirmed)) + 
  geom_line( color="#69b3a2", size=2, alpha=1.0) +
  labs(title = "Covid-19 Global Confirmed Cases", x="Date (2020)", y="Daily Confirmed Cases")+
  scale_y_continuous(limits = c(0, 7e+07), breaks = seq(0, 7e+07, by = 1e+07))+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

# Specific countries confirmed
ggplot(Albania, aes(x=date, y=confirmed)) + 
  geom_line( color="coral1", size=2, alpha=1.0) +
  labs(title = "Albania Covid-19 Confirmed Cases", x="Date (2020)", y="Daily Confirmed Cases")+
  scale_y_continuous(limits = c(0, 1e+06), breaks = seq(0, 1e+06, by = 0.2e+06))+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(Australia, aes(x=date, y=confirmed)) + 
  geom_line( color="brown", size=2, alpha=1.0) +
  labs(title = "Australia Covid-19 Confirmed Cases", x="Date (2020)", y="Daily Confirmed Cases")+
  scale_y_continuous(limits = c(0, 1e+06), breaks = seq(0, 1e+06, by = 0.2e+06))+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(Brazil, aes(x=date, y=confirmed)) + 
  geom_line( color="darkgreen", size=2, alpha=1.0) +
  labs(title = "Brazil Covid-19 Confirmed Cases", x="Date (2020)", y="Daily Confirmed Cases")+
  scale_y_continuous(limits = c(0, 1.5e+07), breaks = seq(0, 1.5e+07, by = 0.2e+07))+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(China, aes(x=date, y=confirmed)) + 
  geom_line( color="darkorange", size=2, alpha=1.0) +
  labs(title = "China Covid-19 Confirmed Cases", x="Date (2020)", y="Daily Confirmed Cases")+
  scale_y_continuous(limits = c(0, 1e+06), breaks = seq(0, 1e+06, by = 0.2e+06))+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(Cuba, aes(x=date, y=confirmed)) + 
  geom_line( color="darkorchid", size=2, alpha=1.0) +
  labs(title = "Cuba Covid-19 Confirmed Cases", x="Date (2020)", y="Daily Confirmed Cases")+
  scale_y_continuous(limits = c(0, 1e+06), breaks = seq(0, 1e+06, by = 0.2e+06))+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(Egypt, aes(x=date, y=confirmed)) + 
  geom_line( color="cadetblue", size=2, alpha=1.0) +
  labs(title = "Egypt Covid-19 Confirmed Cases", x="Date (2020)", y="Daily Confirmed Cases")+
  scale_y_continuous(limits = c(0, 1e+06), breaks = seq(0, 1e+06, by = 0.2e+06))+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(France, aes(x=date, y=confirmed)) + 
  geom_line( color="deeppink", size=2, alpha=1.0) +
  labs(title = "France Covid-19 Confirmed Cases", x="Date (2020)", y="Daily Confirmed Cases")+
  scale_y_continuous(limits = c(0, 1e+06), breaks = seq(0, 1e+06, by = 0.2e+06))+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(Iceland, aes(x=date, y=confirmed)) + 
  geom_line( color="darkseagreen", size=2, alpha=1.0) +
  labs(title = "Iceland Covid-19 Confirmed Cases", x="Date (2020)", y="Daily Confirmed Cases")+
  scale_y_continuous(limits = c(0, 1e+06), breaks = seq(0, 1e+06, by = 0.2e+06))+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(Indonesia, aes(x=date, y=confirmed)) + 
  geom_line( color="deepskyblue4", size=2, alpha=1.0) +
  labs(title = "Indonesia Covid-19 Confirmed Cases", x="Date (2020)", y="Daily Confirmed Cases")+
  scale_y_continuous(limits = c(0, 1e+06), breaks = seq(0, 1e+06, by = 0.2e+06))+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(Italy, aes(x=date, y=confirmed)) + 
  geom_line( color="coral1", size=2, alpha=1.0) +
  labs(title = "Italy Covid-19 Confirmed Cases", x="Date (2020)", y="Daily Confirmed Cases")+
  scale_y_continuous(limits = c(0, 1e+06), breaks = seq(0, 1e+06, by = 0.2e+06))+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(Japan, aes(x=date, y=confirmed)) + 
  geom_line( color="Black", size=2, alpha=1.0) +
  labs(title = "Japan Covid-19 Confirmed Cases", x="Date (2020)", y="Daily Confirmed Cases")+
  scale_y_continuous(limits = c(0, 1e+06), breaks = seq(0, 1e+06, by = 0.2e+06))+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(US, aes(x=date, y=confirmed)) + 
  geom_line( color="darkviolet", size=2, alpha=1.0) +
  labs(title = "US Covid-19 Confirmed Cases", x="Date (2020)", y="Daily Confirmed Cases")+
  scale_y_continuous(limits = c(0, 1.5e+07), breaks = seq(0, 1.5e+07, by = 0.2e+07))+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))


#recovered
# World recovered
ggplot(world_data, aes(x=date, y=recovered)) + 
  geom_line( color="#69b3a2", size=2, alpha=1.0) +
  labs(title = "Covid-19 Global Recovered Cases", x="Date (2020)", y="Daily Recovered Cases")+
  scale_y_continuous(limits = c(0, 7e+07), breaks = seq(0, 7e+07, by = 1e+07))+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

# Specific countries recovered
ggplot(Albania, aes(x=date, y=recovered)) + 
  geom_line( color="coral1", size=2, alpha=1.0) +
  labs(title = "Albania Covid-19 Recovered Cases", x="Date (2020)", y="Daily Recovered Cases")+
  scale_y_continuous(limits = c(0, 1e+06), breaks = seq(0, 1e+06, by = 0.2e+06))+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(Australia, aes(x=date, y=recovered)) + 
  geom_line( color="brown", size=2, alpha=1.0) +
  labs(title = "Australia Covid-19 Recovered Cases", x="Date (2020)", y="Daily Recovered Cases")+
  scale_y_continuous(limits = c(0, 1e+06), breaks = seq(0, 1e+06, by = 0.2e+06))+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(Brazil, aes(x=date, y=recovered)) + 
  geom_line( color="darkgreen", size=2, alpha=1.0) +
  labs(title = "Brazil Covid-19 Recovered Cases", x="Date (2020)", y="Daily Recovered Cases")+
  scale_y_continuous(limits = c(0, 1.5e+07), breaks = seq(0, 1.5e+07, by = 0.2e+07))+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(China, aes(x=date, y=recovered)) + 
  geom_line( color="darkorange", size=2, alpha=1.0) +
  labs(title = "China Covid-19 Recovered Cases", x="Date (2020)", y="Daily Recovered Cases")+
  scale_y_continuous(limits = c(0, 1e+06), breaks = seq(0, 1e+06, by = 0.2e+06))+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(Cuba, aes(x=date, y=recovered)) + 
  geom_line( color="darkorchid", size=2, alpha=1.0) +
  labs(title = "Cuba Covid-19 Recovered Cases", x="Date (2020)", y="Daily Recovered Cases")+
  scale_y_continuous(limits = c(0, 1e+06), breaks = seq(0, 1e+06, by = 0.2e+06))+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(Egypt, aes(x=date, y=recovered)) + 
  geom_line( color="cadetblue", size=2, alpha=1.0) +
  labs(title = "Egypt Covid-19 Recovered Cases", x="Date (2020)", y="Daily Recovered Cases")+
  scale_y_continuous(limits = c(0, 1e+06), breaks = seq(0, 1e+06, by = 0.2e+06))+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(France, aes(x=date, y=recovered)) + 
  geom_line( color="deeppink", size=2, alpha=1.0) +
  labs(title = "France Covid-19 Recovered Cases", x="Date (2020)", y="Daily Recovered Cases")+
  scale_y_continuous(limits = c(0, 1e+06), breaks = seq(0, 1e+06, by = 0.2e+06))+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(Iceland, aes(x=date, y=recovered)) + 
  geom_line( color="darkseagreen", size=2, alpha=1.0) +
  labs(title = "Iceland Covid-19 Recovered Cases", x="Date (2020)", y="Daily Recovered Cases")+
  scale_y_continuous(limits = c(0, 1e+06), breaks = seq(0, 1e+06, by = 0.2e+06))+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(Indonesia, aes(x=date, y=recovered)) + 
  geom_line( color="deepskyblue4", size=2, alpha=1.0) +
  labs(title = "Indonesia Covid-19 Recovered Cases", x="Date (2020)", y="Daily Recovered Cases")+
  scale_y_continuous(limits = c(0, 1e+06), breaks = seq(0, 1e+06, by = 0.2e+06))+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(Italy, aes(x=date, y=recovered)) + 
  geom_line( color="coral1", size=2, alpha=1.0) +
  labs(title = "Italy Covid-19 Recovered Cases", x="Date (2020)", y="Daily Recovered Cases")+
  scale_y_continuous(limits = c(0, 1e+06), breaks = seq(0, 1e+06, by = 0.2e+06))+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(Japan, aes(x=date, y=recovered)) + 
  geom_line( color="Black", size=2, alpha=1.0) +
  labs(title = "Japan Covid-19 Recovered Cases", x="Date (2020)", y="Daily Recovered Cases")+
  scale_y_continuous(limits = c(0, 1e+06), breaks = seq(0, 1e+06, by = 0.2e+06))+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(US, aes(x=date, y=recovered)) + 
  geom_line( color="darkviolet", size=2, alpha=1.0) +
  labs(title = "US Covid-19 Recovered Cases", x="Date (2020)", y="Daily Recovered Cases")+
  scale_y_continuous(limits = c(0, 1.5e+07), breaks = seq(0, 1.5e+07, by = 0.2e+07))+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

#deaths
# World deaths
ggplot(world_data, aes(x=date, y=deaths)) + 
  geom_line( color="#69b3a2", size=2, alpha=1.0) +
  labs(title = "Covid-19 Global Deaths Cases", x="Date (2020)", y="Daily Deaths Cases")+
  scale_y_continuous(limits = c(0, 2e+06), breaks = seq(0, 2e+06, by = 0.3e+06))+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

# Specific countries death
ggplot(Albania, aes(x=date, y=deaths)) + 
  geom_line( color="coral1", size=2, alpha=1.0) +
  labs(title = "Albania Covid-19 Deaths Cases", x="Date (2020)", y="Daily Deaths Cases")+
  scale_y_continuous(limits = c(0, 5e+04), breaks = seq(0, 5e+04, by = 1e+04))+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(Australia, aes(x=date, y=deaths)) + 
  geom_line( color="brown", size=2, alpha=1.0) +
  labs(title = "Australia Covid-19 Deaths Cases", x="Date (2020)", y="Daily Deaths Cases")+
  scale_y_continuous(limits = c(0, 5e+04), breaks = seq(0, 5e+04, by = 1e+04))+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(Brazil, aes(x=date, y=deaths)) + 
  geom_line( color="darkgreen", size=2, alpha=1.0) +
  labs(title = "Brazil Covid-19 Deaths Cases", x="Date (2020)", y="Daily Deaths Cases")+
  scale_y_continuous(limits = c(0, 3e+05), breaks = seq(0, 3e+05, by = 0.5e+05))+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(China, aes(x=date, y=deaths)) + 
  geom_line( color="darkorange", size=2, alpha=1.0) +
  labs(title = "China Covid-19 Deaths Cases", x="Date (2020)", y="Daily Deaths Cases")+
  scale_y_continuous(limits = c(0, 5e+04), breaks = seq(0, 5e+04, by = 1e+04))+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(Cuba, aes(x=date, y=deaths)) + 
  geom_line( color="darkorchid", size=2, alpha=1.0) +
  labs(title = "Cuba Covid-19 Deaths Cases", x="Date (2020)", y="Daily Deaths Cases")+
  scale_y_continuous(limits = c(0, 5e+04), breaks = seq(0, 5e+04, by = 1e+04))+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(Egypt, aes(x=date, y=deaths)) + 
  geom_line( color="cadetblue", size=2, alpha=1.0) +
  labs(title = "Egypt Covid-19 Deaths Cases", x="Date (2020)", y="Daily Deaths Cases")+
  scale_y_continuous(limits = c(0, 5e+04), breaks = seq(0, 5e+04, by = 1e+04))+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(France, aes(x=date, y=deaths)) + 
  geom_line( color="deeppink", size=2, alpha=1.0) +
  labs(title = "France Covid-19 Deaths Cases", x="Date (2020)", y="Daily Deaths Cases")+
  scale_y_continuous(limits = c(0, 5e+04), breaks = seq(0, 5e+04, by = 1e+04))+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(Iceland, aes(x=date, y=deaths)) + 
  geom_line( color="darkseagreen", size=2, alpha=1.0) +
  labs(title = "Iceland Covid-19 Deaths Cases", x="Date (2020)", y="Daily Deaths Cases")+
  scale_y_continuous(limits = c(0, 5e+04), breaks = seq(0, 5e+04, by = 1e+04))+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(Indonesia, aes(x=date, y=deaths)) + 
  geom_line( color="deepskyblue4", size=2, alpha=1.0) +
  labs(title = "Indonesia Covid-19 Deaths Cases", x="Date (2020)", y="Daily Deaths Cases")+
  scale_y_continuous(limits = c(0, 5e+04), breaks = seq(0, 5e+04, by = 1e+04))+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(Italy, aes(x=date, y=deaths)) + 
  geom_line( color="coral1", size=2, alpha=1.0) +
  labs(title = "Italy Covid-19 Deaths Cases", x="Date (2020)", y="Daily Deaths Cases")+
  scale_y_continuous(limits = c(0, 5e+04), breaks = seq(0, 5e+04, by = 1e+04))+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(Japan, aes(x=date, y=deaths)) + 
  geom_line( color="Black", size=2, alpha=1.0) +
  labs(title = "Japan Covid-19 Deaths Cases", x="Date (2020)", y="Daily Deaths Cases")+
  scale_y_continuous(limits = c(0, 5e+04), breaks = seq(0, 5e+04, by = 1e+04))+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(US, aes(x=date, y=deaths)) + 
  geom_line( color="darkviolet", size=2, alpha=1.0) +
  labs(title = "US Covid-19 Deaths Cases", x="Date (2020)", y="Daily Deaths Cases")+
  scale_y_continuous(limits = c(0, 3e+05), breaks = seq(0, 3e+05, by = 0.5e+05))+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

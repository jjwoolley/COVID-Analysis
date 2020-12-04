library("readr")
library("tidyverse")
library("lubridate")
library("ggthemes")
library("readxl")
library("rvest")


county.populations <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv") %>%
  select(Combined_Key, Population)

names.us_states <- c(state.name, "Virgin Islands", "Guam", "District of Columbia")

#yesterday.date <- format(Sys.Date() - 1, "%m/%d/%y") 

US_County.Deaths <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv") %>%
  mutate(State = Province_State,
         County = Admin2) %>%
  filter(State %in% names.us_states) %>%
  select(State, County, 
         !(c(Lat, Long_, UID, iso2, iso3, code3, FIPS, Admin2, Province_State, Country_Region))) %>%
  pivot_longer(cols = !(State:Population), names_to = "Date") %>%
  mutate(Total_Deaths = value,
         Deaths.increase = ifelse((Total_Deaths - lag(Total_Deaths, order_by = Combined_Key)) > 0,
                                  Total_Deaths - lag(Total_Deaths, order_by = Combined_Key), 0),
         Deaths.increase.per.thousand = (Deaths.increase / Population) * 1000) %>%
  filter(!(Date == "5/18/20" & 
             Combined_Key %in% 
             c("Kings, New York, US", "Queens, New York, US", "Bronx, New York, US", "	Richmond, New York, US" )),
         Population > 0) %>%
  select(!value) %>%
  mutate(Date = mdy(Date))


US_County.Cases <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv") %>%
  left_join(county.populations) %>%
  select(Population, everything()) %>%
  mutate(State = Province_State,
         County = Admin2) %>%
  filter(State %in% names.us_states) %>%
  select(State, County, Combined_Key, Population,
         !(c(Lat, Long_, UID, iso2, iso3, code3, FIPS, Admin2, Province_State, Country_Region))) %>%
  pivot_longer(cols = !(State:Population), names_to = "Date") %>%
  mutate(Total_Cases = value,
         Cases.increase = ifelse((Total_Cases - lag(Total_Cases, order_by = Combined_Key)) > 0,
                                  Total_Cases - lag(Total_Cases, order_by = Combined_Key), 0),
         Cases.increase.per.thousand = (Cases.increase / Population) * 1000) %>%
  filter(!(Date == "5/18/20" & 
             Combined_Key %in% 
             c("Kings, New York, US", "Queens, New York, US", "Bronx, New York, US", "	Richmond, New York, US" )),
         Population > 0) %>%
  select(!value) %>%
  mutate(Date = mdy(Date))

CA_Counties.Deaths <- US_County.Deaths %>%
  filter(State == "California")


CA_Counties.Cases <- US_County.Cases %>%
  filter(State == "California")

CA_Counties.Cases %>%
  filter(County %in% c("Ventura",  "Los Angeles", "San Diego")) %>%
  ggplot(aes(x = Date, y = Cases.increase.per.thousand, color = County)) +
  geom_smooth(se = F, span = 0.2) + 
  theme_bw()

CA_Counties.Deaths %>%
  filter(County %in% c("Ventura",  "Los Angeles", "San Diego")) %>%
  ggplot(aes(x = Date, y = Deaths.increase.per.thousand, color = County)) +
  geom_smooth(se = F, span = 0.2)  + 
  theme_bw()

x <- bind_cols(CA_Counties.Cases, CA_Counties.Deaths)
full_county <- full_join(CA_Counties.Cases, CA_Counties.Deaths)

full_county %>%
  filter(County %in% c("Ventura", "Orange")) %>%
  ggplot(aes(x = Date, y = Deaths.increase.per.thousand, color = County)) +
  geom_smooth(se = F, span = 0.15, linetype = "dashed") +
  geom_smooth(aes(y = Cases.increase.per.thousand), se = F, span = 0.1) +
  scale_x_date(date_labels = "%B", date_breaks = "1 month") +
  theme_bw() +
  ggtitle("COVID Deaths and Cases in 2020", subtitle = "Best California Counties") +
  xlab("Date") +
  ylab("Metric Per Thousand")


Global_Confirmed <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
Global_Deaths <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")



colnames(CA_Counties.Cases)


country.populations <- read_csv("Population_by_Country.csv")




CA_Counties.Deaths %>%
  filter(County == "San Luis Obispo") %>%
  ggplot(aes(x = mdy(Date), y = Deaths.increase)) +
  #geom_smooth(se = F, span = 0.2) + 
  geom_point() +
  #ylim(0, 35) +
  scale_x_date(date_labels = "%B", date_breaks = "1 month") +
  ggtitle("Daily COVID Metrics", subtitle = "San Luis Obispo County") +
  xlab("Month (2020(") +
  ylab("Metric") +
  theme_fivethirtyeight()

US %>%
  ggplot(aes(x = Date, y = value, color = Metric)) +
  geom_smooth(se = F, span = 0.2) + 
  geom_point(alpha = 0.1) +
  #ylim(0, 150) +
  scale_x_date(date_labels = "%B", date_breaks = "1 month") +
  theme_bw()

International %>%
  filter(Metric == "Deaths_Increase") %>%
  ggplot(aes(x = Date, y = value, color = Country)) +
  geom_smooth(se = F, span = 0.2) + 
  geom_point(alpha = 0.1) +
  ylim(0, 2700) +
  scale_x_date(date_labels = "%B", date_breaks = "1 month") +
  theme_bw() +
  ggtitle("COVID Daily Deaths by Country") + 
  ylab("Deaths")



US_State.Cases <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv") %>%
  left_join(county.populations) %>%
  select(Population, everything()) %>%
  mutate(State = Province_State,
         County = Admin2) %>%
  filter(State %in% names.us_states) %>%
  select(State, County, Combined_Key, Population,
         !(c(Lat, Long_, UID, iso2, iso3, code3, FIPS, Admin2, Province_State, Country_Region))) %>%
  pivot_longer(cols = !(State:Population), names_to = "Date") %>%
  mutate(Total_Cases = value,
         Cases.increase = ifelse((Total_Cases - lag(Total_Cases, order_by = Combined_Key)) > 0,
                                 Total_Cases - lag(Total_Cases, order_by = Combined_Key), 0),
         Cases.increase.per.thousand = (Cases.increase / Population) * 1000) %>%
  filter(!(Date == "5/18/20" & 
             Combined_Key %in% 
             c("Kings, New York, US", "Queens, New York, US", "Bronx, New York, US", "	Richmond, New York, US" )),
         Population > 0) %>%
  select(!value) %>%
  mutate(Date = mdy(Date)) %>%
  group_by(State, Date) %>%
  summarise(S_Cases_Increase = sum(Cases.increase, na.rm = T),
            S_population = sum(Population, na.rm = T),
            S_Cases.increase.per.thousand = ((S_Cases_Increase / S_population) * 1000))

CA_State.cases <- US_State.Cases %>%
  filter(State %in% c("California", "Florida")) %>%
  ggplot(aes(x = Date, y = S_Cases.increase.per.thousand, color = State)) +
  geom_smooth(se = F, span = 0.2)


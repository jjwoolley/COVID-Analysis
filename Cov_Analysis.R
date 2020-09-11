library("readr")
library("tidyverse")
library("lubridate")
library("ggthemes")

# create script that will automatically read in all previous days of data
# df1: groups by country
# df2: US only and grouped by state
# df3: US/California only and grouped by county


# will need to take a start and url then take todays date and go backwards
start_url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/"
end_url <- ".csv"
url1 <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/09-08-2020.csv"


# figure out how to deal with NAs
# how to deal with new countries/states/counties being added

#first day of data was 2020-01-22
date.initial <- as.Date("2020-01-23")

date.yesterday <- Sys.Date() - 1

dates.total <- seq(date.initial, date.yesterday, by = "days") %>%
  format("%m-%d-%Y")


df.covid <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/01-22-2020.csv") %>%
  select(!starts_with("Last Update")) %>%
  mutate(Date = as.Date("2020-01-22"))

# Error: Can't combine `FIPS` <double> and `FIPS` <character>.
for (i in dates.total) {
  lookup <- paste0(start_url, i, end_url)
  daily.covid <- read_csv(lookup) %>%
    select(!starts_with("Last")) %>%
    select(!starts_with("FIPS")) %>%
    mutate(Date = mdy(i)) #%>%
    #mutate(FIPS = ifelse(Date < mdy("03-as.numeric(FIPS))
 
  df.covid <- bind_rows(df.covid, daily.covid, )
}

# switch in data availability at row 7618
# this is start of 2020-03-22
x <- df.covid %>%
  mutate(Latitude = coalesce(Latitude, Lat),
         Longitude = coalesce(Longitude, Long_),
         Country = coalesce(`Country/Region`, Country_Region),
         Province_State = coalesce(`Province/State`, Province_State),
         County = Admin2,
         CK = paste0(ifelse(is.na(County), "", County), 
                     ifelse(is.na(County), "", ", "), 
                     ifelse(is.na(Province_State), "", Province_State),
                     ifelse(is.na(Province_State), "", ", "),
                     Country),
         Combined_Key = coalesce(Combined_Key, CK)) %>%
  select(Country, Date, Province_State, County, 
         Confirmed, Deaths, Recovered, Combined_Key, Incidence_Rate, `Case-Fatality_Ratio`) %>%
  arrange(Country, Date, Province_State, County, 
          Confirmed, Deaths, Recovered, Combined_Key, Incidence_Rate, `Case-Fatality_Ratio`) %>%
  group_by(Combined_Key) %>%
  # for some reason the total Confirmed/deaths decrease occasionally, so changing to 0 if negative
  mutate(Deaths_Increase = ifelse((Deaths - lag(Deaths, order_by = Combined_Key)) > 0,
                                  Deaths - lag(Deaths, order_by = Combined_Key), 0),
         Confirmed_Increase = ifelse((Confirmed - lag(Confirmed, order_by = Combined_Key)) > 0,
                                     Confirmed - lag(Confirmed, order_by = Combined_Key), 0)) %>%
  # filtering out what must be data errors (7000 + deaths in one neighborhood in one day)
  # I am assuming that they added in these areas and possibly all deaths that happened in the area
  # beforehand
  filter(!(Date == "2020-08-31" & 
             Combined_Key %in% 
                c("Kings, New York, US", "Queens, New York, US", "Bronx, New York, US", "	Richmond, New York, US" )))

#8/31 huge increase

ggplot(y, aes(x = Date, y = mean)) +
  geom_smooth(se = F, span = 0.75) 

names.us_states <- c(state.name, "Virgin Islands", "Guam", "District of Columbia")

# issues with too many provinces/states
# it is coding some counties/cities as states
# seems to occur when states are labeled as abbreviations for some and spelled out for others
# fixed by removing any where Province_State name is not exactly state or one of Guam, Virgin Islands, or District of Columbia
# These errors had very few deaths and confirmed cases
us_states <- x %>%
  filter(Country == "US", Province_State %in% names.us_states)  %>% 
  group_by(Province_State) %>%
  summarise(Deaths_Increase = sum(Deaths_Increase, na.rm = T),
            Confirmed_Increase = sum(Confirmed_Increase, na.rm = T)) %>%
  pivot_longer(cols = c(Deaths_Increase, Confirmed_Increase), names_to = "Metric")

US <- x %>%
  filter(Country == "US")  %>% 
  group_by(Country) %>%
  summarise(Deaths_Increase = sum(Deaths_Increase, na.rm = T),
            Confirmed_Increase = sum(Confirmed_Increase, na.rm = T)) %>%
  pivot_longer(cols = c(Deaths_Increase, Confirmed_Increase), names_to = "Metric")

CA_counties <- x %>%
  filter(Country == "US", Province_State == "California", !is.na(County), County != "Unassigned") %>%
  group_by(County, Date) %>%
  summarise(Deaths_Increase = sum(Deaths_Increase, na.rm = T),
            Confirmed_Increase = sum(Confirmed_Increase, na.rm = T)) %>%
  pivot_longer(cols = c(Deaths_Increase, Confirmed_Increase), names_to = "Metric")

International <- x %>%
  filter(Country %in% c("US", "United Kingdom", "Canada")) %>%
  group_by(Country, Date) %>%
  summarise(Deaths_Increase = sum(Deaths_Increase, na.rm = T),
            Confirmed_Increase = sum(Confirmed_Increase, na.rm = T)) %>%
  pivot_longer(cols = c(Deaths_Increase, Confirmed_Increase), names_to = "Metric")

names.countries <- unique(International$Country)

CA_counties %>%
  filter(County == "San Luis Obispo") %>%
  ggplot(aes(x = Date, y = value, color = Metric)) +
  geom_smooth(se = F, span = 0.2) + 
  geom_point(alpha = 0.1) +
  ylim(0, 35) +
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


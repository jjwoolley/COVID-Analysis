#covid tracking usa
#uses covidtracking.com source (theAtlantic)

library(covid19us)
library(dplyr)
library(ggplot2)

#functions from covid19us package
today<-get_states_current()
county<-get_counties_info()
info<-get_info_covid19us()
us_current<-get_us_current()
states_daily<-get_states_daily()
us_daily<-get_us_daily()
dat<-refresh_covid19us()

library(dplyr)
library(ggplot2)

# dat %>% 
#   filter(
#     location == "CA" &
#       data_type %in% 
#       c(
#         "positive_increase",
#         "total_test_results_increase",
#         "death_increase",
#         "hospitalized_increase"
#       )
#   ) %>% 
#   mutate(
#     Type = data_type %>% 
#       stringr::str_replace_all("_", " ") %>% 
#       stringr::str_to_title()
#   ) %>% 
#   arrange(date) %>% 
#   ggplot(aes(date, value, color = Type)) +
#   geom_smooth(se = FALSE) + 
#   scale_x_date(date_breaks = "2 weeks") +
#   labs(title = "COVID in NY") +
#   xlab("Date") +
#   ylab("Value") +
#   theme_minimal(base_family = "Source Sans Pro")



ggplot(us_daily,aes(date,positive_increase))+
  geom_line()+
  geom_smooth() + 
  xlab("Date") +
  ggtitle("Daily Positive Tests")

ggplot(us_daily,aes(date,hospitalized_increase))+
  geom_smooth() +
  geom_line()+
  xlab("Date")+
  ggtitle("Daily COVID Hospitalizations")

ggplot(us_daily,aes(date,death_increase))+
  geom_smooth() +
  geom_line()+
  xlab("Date")+
  ggtitle("Daily COVID Deaths")


Time<-1:11
Vaccine_Success<-c(0,10,20,30,40,50,60,70,80,90,100)
vacc<-data.frame(Time,Vaccine_Success)
ggplot(vacc,aes(Time,Vaccine_Success))+
  geom_line() +
  xlab("Time or Investment (Your Choice)") + 
  ylab("Vaccine Success")


x<-select(us_daily,date,positive_increase)

#more testing 2
#git project for testing

library(covid19us)
library(tidyverse)
library(ggthemes)
library("tidycovid")

#functions from covid19us package
today <- get_states_current()
county <- get_counties_info()
#info <- get_info_covid19us()
us_current <- get_us_current()
states_daily <- get_states_daily()
us_daily <- get_us_daily()
dat <- refresh_covid19us()

dat <- states_daily %>%
  select(date, state, positive, negative, death,
         positive_increase, negative_increase, death_increase, hospitalized_increase) %>%
  mutate(death_increase = ifelse(death_increase < 0, 0, death_increase),
         log_deaths = ifelse(death_increase == 0, 0, log(death_increase))) %>%
  pivot_longer(cols = !c(date, state), names_to = "Metric") %>%
  arrange(state, date)

# count NAs by Metric
dat %>%
  group_by(Metric) %>%
  summarise_each(funs(sum(is.na(.))))
# we only have NA's in the stats death, negative, and positive


dat %>%
  filter(state == "SD") %>%
  filter(Metric %in% c("positive_increase", "death_increase", "hospitalized_increase")) %>%
  ggplot(aes(x = date, y = value, color = Metric)) +
  geom_smooth(se = F, span = 0.1) +
  geom_vline(xintercept = as.numeric(as.Date("2020-08-16")), linetype = 4, color = "purple") +
  xlab("Date") +
  ylab("Value") +
  ggtitle("South Dakota Daily COVID Metrics",
          subtitle = "Response to Sturgis Rally") +
  theme_bw() +
  scale_x_date(date_breaks = "1 month") 


ggplot(us_daily,aes(date,positive_increase))+
  geom_line(alpha = 0.1)+
  geom_smooth(se = F, span = 0.1) + 
  xlab("Date") +
  ylab("Positive Tests") +
  ggtitle("Daily Positive Tests") +
  theme_bw() +
  scale_x_date(date_breaks = "1 month") 

ggplot(us_daily,aes(date,hospitalized_increase))+
  geom_smooth(se = F, span = 0.2) +
  geom_line(alpha = 0.1) +
  xlab("Date") +
  ggtitle("Daily COVID Hospitalizations") +
  theme_bw() +
  scale_x_date(date_breaks = "1 month")

ggplot(us_daily,aes(date,death_increase))+
  geom_smooth(se = F, span = 0.2) +
  geom_line(alpha = 0.1) +
  xlab("Date") +
  ggtitle("Daily COVID Deaths") +
  theme_bw() +
  scale_x_date(date_breaks = "1 month")






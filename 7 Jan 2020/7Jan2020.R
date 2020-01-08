##################################################
## Project: Tidy Tuesday
## Script purpose: Fire/climate data from Australia
## Date: 1/8/2020
## Author: Collin Brehmer
## Contact: collin.brehmer@colostate.edu
##################################################

## Required packages

library(tidyverse)
library(lubridate)
library(viridis)
library(ggridges)

## Read in data

temperature <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-07/temperature.csv')

## Create cleaned data file
# Groups by half decade and finds mean maximum temp

temperature_clean <- temperature %>% 
  filter(!is.na(temperature)) %>% 
  mutate(year = year(date),
         half_decade = floor(year / 5) * 5) %>% 
  group_by(half_decade) %>%
  mutate(mean_half_decade = mean(temperature))

## Make plot
# Density ridge plot by half decade where fill is the mean maximum temp of the half decade

temperature_clean %>% 
ggplot(aes(y = as.factor(half_decade), x = temperature, fill = mean_half_decade))+
  geom_density_ridges_gradient(scale = 3)+
  theme_bw()+
  scale_fill_viridis(name = "Mean temp (°C)") +
  ggtitle("Distribution and mean of the maximum daily temperatures in Australia*")+
  labs(y = "",
       x = 'Temperature (°C)',
       caption = "*Temperature measurements were recorded at weather stations in Perth, Port Lincoln, Kent Town,\nBrisbane, Sydney, Canberra, and Melbourne") +
  theme(plot.caption = element_text(hjust = 0))

ggsave("aus_max_temp_trend.png", width = 7.5, height = 6.5, units = "in")


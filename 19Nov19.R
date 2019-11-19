#Tidy Tuesday November 19 2019
library(tidyverse)
library(viridis)

#Read in raw data
nz_bird_raw <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-11-19/nz_bird.csv")

#Clean data
nz_bird <- nz_bird_raw %>% 
  filter(bird_breed != is.na(bird_breed) & vote_rank == "vote_1") %>% 
  mutate(am_pm = case_when(hour < 12 ~ "AM",
                           hour >= 12 ~ "PM"),
         hour = as.factor(hour),
         bird_breed = as.factor(bird_breed)) %>% 
  group_by(hour, bird_breed, am_pm) %>% 
  count() %>% 
  ungroup() %>% 
  group_by(hour) %>% 
  slice(1:3) %>% 
  ungroup() %>% 
  mutate(hour = fct_recode(hour, "12" = "0",
                           "1" = "13",
                           "2" = "14",
                           "3" = "15",
                           "4" = "16",
                           "5" = "17",
                           "6" = "18",
                           "7" = "19",
                           "8" = "20",
                           "9" = "21",
                           "10" = "22",
                           "11" = "23"))


#Create plot
nz_bird %>% 
  ggplot(aes(x = hour, y = n, fill = fct_rev(bird_breed))) +
  geom_hline(yintercept = seq(0, 1, by = 0.25), colour = "black", size = 0.4)+
  geom_bar(stat = "identity", position = "fill", width = 0.6) +
  facet_wrap(~ am_pm) +
  coord_polar(theta = "x", start = -0.26)+
  labs(y = "% of votes for the top 3 birds")+
  scale_fill_viridis(discrete = TRUE, name = "Bird species") +
  annotate("text", x = 2, y = .25, label = "25%", hjust = 0.55, vjust = -0.75, angle = -15, size = 3.5) +
  annotate("text", x = 2, y = 0.5, label = "50%", hjust = 1, vjust = -0.75, angle = -15, size = 3.5) +
  annotate("text", x = 2, y = 0.75, label = "75%", hjust = 1.45, vjust = -0.75, angle = -15, size = 3.5)+
  annotate("text", x = 2, y = 1, label = "100%", hjust = 1.45, vjust = -0.75, angle = -15, size = 3.5) + 
  theme(axis.ticks = element_blank(),
        panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 12, color = "black"),
        axis.title.x = element_blank(),
        strip.text = element_text(color = "black", size = 12),
        title = element_text(size = 12),
        legend.position = "bottom")+
  ggtitle("Relative percent of first choice votes for the top 3 birds per hour")

ggsave("19nov19.png", width = 7, height = 5, units = "in", dpi = 300)

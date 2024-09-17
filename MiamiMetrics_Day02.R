setwd("~/Desktop/Miami Metrics/Fall 24")
install.packages("tidyverse")
library(tidyverse)

soccer_data <- read_csv("matches_1930_2022.csv") %>%
  select(home_team, away_team, home_score, away_score)


home_data <- soccer_data %>%
  select(Team=home_team, Score=home_score)

away_data <- soccer_data %>%
  select(Team=away_team, Score=away_score)


combined_data <- rbind(home_data, away_data) %>%
  mutate(Team = case_when(
    Team == "China PR" ~ "China",
    Team == "United States" ~ "USA",
    Team == "Germany DR" ~ "Germany",
    Team == "IR Iran" ~ "Iran",
    Team == "England" ~ "UK",
    Team == "Türkiye" ~ "Turkey",
    Team == "West Germany" ~ "Germany",
    TRUE ~ Team
  ))

agg_data <- combined_data %>%
  group_by(Team) %>%
  summarize(Sum_Goals = sum(Score))

fixed_data <- agg_data

install.packages("maps")
library(maps)
world <- map_data("world")

world_combined_data <- full_join(world, fixed_data, by=c("region" = "Team"))

p1 <- ggplot(world_combined_data, aes(x=long, y=lat, group=group)) +
  geom_polygon(aes(fill=Sum_Goals), color="gray10", linewidth=.1) +
  scale_fill_continuous(low = "lightblue", high="darkblue", na.value="gray40") +
  theme_void() +
  labs(title = "Total Goals by Each Country in the World Cup (1930 - 2022)",
       caption = "Source: Petro via Kaggle",
       fill = "Goal Count") +
  theme(plot.title = element_text(hjust=0.5, size=15, face="bold"))


world_cup <- read_csv("world_cup.csv") %>%
  select(Team=Champion) %>%
  mutate(Team = case_when(
    Team == "China PR" ~ "China",
    Team == "United States" ~ "USA",
    Team == "Germany DR" ~ "Germany",
    Team == "IR Iran" ~ "Iran",
    Team == "England" ~ "UK",
    Team == "Türkiye" ~ "Turkey",
    Team == "West Germany" ~ "Germany",
    TRUE ~ Team
  ))

world_cup_agg <- world_cup %>%
  group_by(Team) %>%
  summarise(Num_wins = n())

world_cup_combined <- full_join(world_cup_agg, world, c("Team" = "region"))

p2 <- ggplot(world_cup_combined, aes(x=long, y=lat, group=group)) +
  geom_polygon(aes(fill=Num_wins), color="gray10", linewidth=.1) +
  scale_fill_continuous(low = "lightblue", high="darkblue", na.value="gray40") +
  theme_void() +
  labs(title = "Total Wins by Each Country in the World Cup (1930 - 2022)",
       caption = "Source: Petro via Kaggle",
       fill = "Win Count") +
  theme(plot.title = element_text(hjust=0.5, size=15, face="bold"))

ggsave(plot=p1, "Goals.png")
ggsave(plot=p2, "Wins.png")

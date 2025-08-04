library(tidyverse)
library(dplyr)
library(stringr)
library(readr)
library(lubridate)
library(ggplot2)
library(scales)
library(fmsb)


band = read_csv('Cleaned Data/cleanBand.csv')


bandwidth_south = band %>% 
  filter(County=="SOUTH YORKSHIRE") %>% 
  group_by(District)

bandwidth_south <- bandwidth_south %>%
  mutate(`Avg_download` = as.numeric(`Avg_download`))


town_bandwidth_south <- bandwidth_south %>% 
  drop_na(`Avg_download`) %>% 
  group_by(Town) %>%
  summarise(`Average_download` = mean(`Avg_download`, na.rm = TRUE))


bandwidth_west = band %>% 
  filter(County=="WEST YORKSHIRE") %>% 
  group_by(District)

bandwidth_west <- bandwidth_west %>%
  mutate(`Avg_download` = as.numeric(`Avg_download`))


town_bandwidth_west <- bandwidth_west %>% 
  drop_na(`Avg_download`) %>% 
  group_by(Town) %>%
  summarise(`Average_download` = mean(`Avg_download`, na.rm = TRUE))

ggplot(bandwidth_south, aes(x = District, y = `Avg_download`, fill = District)) +
  geom_boxplot(outlier.size = 1) +
  labs(
    title = "Average Internet Download Speed of South Yorkshire District/Speed",
    x = "District",
    y = "Speed"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(bandwidth_west, aes(x = District, y = `Avg_download`, fill = District)) +
  geom_boxplot(outlier.size = 1) +
  labs(
    title = "Average Internet Download Speed of West Yorkshire District/Speed",
    x = "District",
    y = "Speed"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(town_bandwidth_south, aes(x = Town, y = `Average_download`, fill = Town)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Average Internet Download Speed of South Yorkshire Towns/Speed",
    x = "Town",
    y = "Avg_download"
  ) +
  scale_y_continuous(labels = label_number(accuracy = 0.1)) +  # clean numeric formatting
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(town_bandwidth_west, aes(x = Town, y = `Average_download`, fill = Town)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Average Internet Download Speed of West Yorkshire Towns/Speed",
    x = "Town",
    y = "Avg_download"
  ) +
  scale_y_continuous(labels = label_number(accuracy = 0.1)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
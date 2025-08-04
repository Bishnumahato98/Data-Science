library(tidyverse)
library(dplyr)
library(stringr)
library(readr)
library(lubridate)



school = read_csv('Cleaned Data/cleanSchool.csv')

south_yorkshire <- school %>%
  filter(County == "SOUTH YORKSHIRE") %>% 
  group_by(District)

west_yorkshire <- school %>%
  filter(County == "WEST YORKSHIRE") %>% 
  group_by(District)

district_scores <- school %>%
  group_by(District) %>%
  summarise(avg_score = mean(ATT8SCR, na.rm = TRUE)) 

ggplot(south_yorkshire, aes(x = District, y = ATT8SCR)) +
  geom_boxplot(fill = "skyblue", color = "darkblue") +
  labs(
    title = "Boxplot of Average Attainment 8 Score – South Yorkshire",
    x = "District",
    y = "Attainment 8 Score"
  ) +
  theme_minimal()

ggplot(west_yorkshire, aes(x = District, y = ATT8SCR)) +
  geom_boxplot(fill = "purple", color = "pink") +
  labs(
    title = "Boxplot of Average Attainment 8 Score – South Yorkshire",
    x = "District",
    y = "Attainment 8 Score"
  ) +
  theme_minimal()


ggplot(district_scores, aes(x = District, y = avg_score, group = 1)) +
  geom_point(size = 3, color = "green") +
  labs(
    title = "Average Attainment 8 Score by District (2024)",
    x = "District",
    y = "Average Attainment 8 Score"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

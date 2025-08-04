library(tidyverse)
library(dplyr)
library(stringr)
library(readr)
library(lubridate)
library(ggplot2)
library(scales)
library(fmsb)

house = read_csv('Cleaned Data/cleanHousePrices.csv')


average_price <- house %>% 
  group_by(County, Year) %>% 
  summarise(Avg_Price = mean(Price))

house_price_south = house %>% 
  filter(County=="SOUTH YORKSHIRE") %>% 
  group_by(District)

house_price_west = house %>% 
  filter(County=="WEST YORKSHIRE") %>% 
  group_by(District)

avg_p_2024 <- house %>%
  filter(Year == "2024") %>% 
  group_by(District) %>% 
  summarise(Avg_Price_2024 = mean(Price))

ggplot(average_price, aes(x = Year, y = Avg_Price, color = County, group = County)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = c("SOUTH YORKSHIRE" = "cyan", "WEST YORKSHIRE" = "orange"))+
  scale_y_continuous(
    breaks = seq(0, max(average_price$Avg_Price), by = 10000),
    labels = comma
  ) +
  labs(
    title = "Average House Prices of South and West Yorkshire (2020–2024)",
    x = "Year",
    y = "Average Price",
    color = "County"
  )+theme_minimal()



ggplot(avg_p_2024, aes(x = District, y= Avg_Price_2024, fill = District))+
  geom_bar(stat = "identity")+
  labs(
    title = "Average House Prices of South and West Yorkshire (2024)",
    x = "District",
    y = "Average Price"
  )+
  scale_y_continuous(
    breaks = seq(0, max(avg_p_2024$Avg_Price_2024) * 1.1, by = 50000),
    labels = comma
  ) +
  theme_bw()

ggplot(house_price_south, aes(x = District, y = Price, fill = District)) +
  geom_boxplot(outlier.size = 1) +
  labs(
    title = "House Price Distribution by District of South Yorkshire (2021–2025)",
    x = "District",
    y = "Price"
  )  +
  scale_y_continuous(breaks = seq(0, max(house_price_south$Price, na.rm = TRUE), by = 50000), labels = scales::comma)+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_cartesian(ylim = c(
    quantile(house_price_south$Price, 0.05, na.rm = TRUE),
    quantile(house_price_south$Price, 0.95, na.rm = TRUE)
  ))

ggplot(house_price_west, aes(x = District, y = Price, fill= District)) +
  geom_boxplot(outlier.size = 1) +
  labs(
    title = "House Price Distribution by District of West Yorkshire (2021–2025)",
    x = "District",
    y = "Price"
  )  +
  scale_y_continuous(breaks = seq(0, max(house_price_south$Price, na.rm = TRUE), by = 50000), labels = scales::comma)+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_cartesian(ylim = c(
    quantile(house_price_south$Price, 0.05, na.rm = TRUE),
    quantile(house_price_south$Price, 0.95, na.rm = TRUE)
  ))
library(tidyverse)
library(dplyr)
library(stringr)
library(readr)
library(lubridate)
library(ggplot2)
library(scales)
library(fmsb)

towns = read_csv('Cleaned Data/Towns.csv')
crime = read_csv('Cleaned Data/cleanCrimeRate.csv')

crime_south = crime %>% 
  filter(County == "SOUTH YORKSHIRE") %>% 
  select(Month, `Crime type`, District)

crime_west = crime %>% 
  filter(County == "WEST YORKSHIRE") %>%  
  select(Month, `Crime type`, District)

drug_crime <- crime %>% 
  filter(`Crime type` == "Drugs")

all_south_district_population <- towns %>% 
  filter(County == "SOUTH YORKSHIRE") %>%
  mutate(District = toupper(District)) %>%
  select(-County, -Town, -shortPostcode) %>% 
  group_by(District) %>%
  summarise(Population2024 = sum(Population2024, na.rm = TRUE),
            Population2023 = sum(Population2021, na.rm = TRUE),
            Population2022 = sum(Population2022, na.rm = TRUE),
            Population2021 = sum(Population2023, na.rm = TRUE),
            Population2020 = sum(Population2020, na.rm = TRUE))

all_west_district_population <- towns %>% 
  filter(County == "WEST YORKSHIRE") %>%
  mutate(District = toupper(District)) %>%
  select(-County, -Town, -shortPostcode) %>% 
  group_by(District) %>%
  summarise(Population2024 = sum(Population2024, na.rm = TRUE),
            Population2023 = sum(Population2021, na.rm = TRUE),
            Population2022 = sum(Population2022, na.rm = TRUE),
            Population2021 = sum(Population2023, na.rm = TRUE),
            Population2020 = sum(Population2020, na.rm = TRUE))



south_population_2024 <- towns %>%
  filter(County == "SOUTH YORKSHIRE") %>%
  select(District, Population2024) %>%
  mutate(District = toupper(District)) %>%
  group_by(District)

west_population_2024 <-towns %>%
  filter(County == "WEST YORKSHIRE") %>%
  select(District, Population2024) %>%
  mutate(District = toupper(District)) %>%
  group_by(District)

south_drug_data <- crime_south %>% 
  filter(`Crime type` == "Drugs") %>% 
  group_by(District)

west_drug_data <- crime_west %>% 
  filter(`Crime type` == "Drugs") %>% 
  group_by(District)

all_south_district_drug_rate <- south_drug_data %>% 
  mutate(District = toupper(District)) %>% 
  group_by(District) %>% 
  summarise(drug_offense = n(), .groups = "drop") %>% 
  inner_join(
    all_south_district_population,
    by = "District"
  ) %>%
  mutate(drug_offense_2024 = (drug_offense / Population2024) * 10000,
         drug_offense_2023 = (drug_offense / Population2023) * 10000,
         drug_offense_2022 = (drug_offense / Population2022) * 10000,
         drug_offense_2021 = (drug_offense / Population2021) * 10000,
         drug_offense_2020= (drug_offense / Population2020) * 10000)

all_west_district_drug_rate <- west_drug_data %>% 
  mutate(District = toupper(District)) %>% 
  group_by(District) %>% 
  summarise(drug_offense = n(), .groups = "drop") %>% 
  inner_join(
    all_west_district_population,
    by = c("District"="District")
  ) %>%
  mutate(drug_offense_2024 = (drug_offense / Population2024) * 10000,
         drug_offense_2023 = (drug_offense / Population2023) * 10000,
         drug_offense_2022 = (drug_offense / Population2022) * 10000,
         drug_offense_2021 = (drug_offense / Population2021) * 10000,
         drug_offense_2020= (drug_offense / Population2020) * 10000)

all_west_district_drug_rate <- all_west_district_drug_rate %>%
  mutate(County = "WEST YORKSHIRE")

all_south_district_drug_rate <- all_south_district_drug_rate %>%
  mutate(County = "SOUTH YORKSHIRE")

combined_drug_data <- bind_rows(all_west_district_drug_rate, all_south_district_drug_rate)

long_drug_data <- combined_drug_data %>%
  pivot_longer(
    cols = starts_with("drug_offense_"),
    names_to = "Year",
    values_to = "Rate"
  ) %>%
  mutate(
    Year = gsub("drug_offense_", "", Year),  # Clean year column
    Year = as.integer(Year)
  )

avg_rate_by_county <- long_drug_data %>%
  group_by(County, Year) %>%
  summarise(Average_Rate = mean(Rate, na.rm = TRUE), .groups = "drop")



west_robbery_data <- crime_west %>% 
  filter(`Crime type` == "Robbery" | `Crime type` == "Burglary" | `Crime type` == "Shoplifting" |
           `Crime type` == "Bicycle theft" |`Crime type` == " Other theft" |
           `Crime type` == "  Theft from the person ",
         Month=="2024-05") %>% 
  group_by(District)

district_population <- west_population_2024 %>% 
  mutate(District = toupper(District)) %>%
  group_by(District) %>%
  summarise(Population2024 = sum(Population2024, na.rm = TRUE))


vehicle_crime_data <- crime_west %>%
  filter(`Crime type` == "Vehicle crime",Month == "2024-05")

vehicle_rate_west <- vehicle_crime_data %>%
  mutate(District = toupper(District)) %>%
  group_by(District) %>% 
  summarise(vehicle_offense = n(), .groups = "drop") %>% 
  inner_join(
    district_population,
    by = c("District" = "District")
  )%>% 
  mutate(vehicle_offense_rate = (vehicle_offense / Population2024) * 1000)

robbery_rate_west <- west_robbery_data %>%
  mutate(District = toupper(District)) %>%
  group_by(District) %>% 
  summarise(robbery_offense = n(), .groups = "drop") %>% 
  inner_join(
    district_population,
    by = c("District" = "District")
  )%>% 
  mutate(drug_offense_rate = (robbery_offense / Population2024) * 1000)

robbery_rate_west <- robbery_rate_west %>%
  mutate(percentage = round(robbery_offense / sum(robbery_offense) * 100, 1),
         label = paste0(District, "\n", percentage, "%"))


drug_rate_west <- west_drug_data %>%
  mutate(District = toupper(District)) %>%
  group_by(District) %>% 
  summarise(drug_offense = n(), .groups = "drop") %>% 
  inner_join(
    west_population_2024,
    by = c("District" = "District")
  )%>% 
  mutate(drug_offense_rate = (drug_offense / Population2024) * 1000)

drug_rate <- south_drug_data %>%
  mutate(District = toupper(District)) %>%
  group_by(District) %>% 
  summarise(drug_offense = n(), .groups = "drop") %>% 
  inner_join(
    south_population_2024,
    by = c("District" = "District")
  )%>% 
  mutate(drug_offense_rate = (drug_offense / Population2024) * 1000)




radar_data <- vehicle_rate_west %>%
  select(District, vehicle_offense_rate) %>%
  column_to_rownames("District") %>%
  t() %>%
  as.data.frame()

radar_data <- rbind(
  max = rep(max(radar_data), ncol(radar_data)),
  min = rep(0, ncol(radar_data)),
  radar_data
)
par(mar = c(1, 1, 4, 1))  # Adjust margins

radarchart(radar_data,
           axistype = 0,
           pcol = "purple",
           pfcol = alpha("purple", 0.5),
           plwd = 2,
           cglcol = "grey",
           cglty = 1,
           cglwd = 0.8,
           vlcex = 1.2,
           title = "Vehicle Crime Rate by District – West Yorkshire (May 2024)")

drug_rate %>%
  ggplot(aes(x = District, y = drug_offense_rate)) +
  geom_boxplot(fill = "purple") +
  labs(title = "Drug Offense Rate by District - South Yorkshire",
       x = "District",
       y = "Drug Offense Rate per 1,000 people") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_cartesian(ylim = c(
    quantile(drug_rate$drug_offense_rate, 0.05, na.rm = TRUE),
    quantile(drug_rate$drug_offense_rate, 0.95, na.rm = TRUE)
  ))

drug_rate_west %>%
  ggplot(aes(x = District, y = drug_offense_rate)) +
  geom_boxplot(fill = "pink") +
  labs(title = "Drug Offense Rate by District - West Yorkshire",
       x = "District",
       y = "Drug Offense Rate per 1,000 people") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_cartesian(ylim = c(
    quantile(drug_rate$drug_offense_rate, 0.05, na.rm = TRUE),
    quantile(drug_rate$drug_offense_rate, 0.95, na.rm = TRUE)
  ))

ggplot(robbery_rate_west, aes(x = "", y = robbery_offense, fill = District)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 3) +
  labs(title = "Robbery Offenses by District West Yorkshire", fill = "District") +
  theme_void()


min_rate <- min(avg_rate_by_county$Average_Rate, na.rm = TRUE)
max_rate <- max(avg_rate_by_county$Average_Rate, na.rm = TRUE)

ggplot(avg_rate_by_county, aes(x = Year, y = Average_Rate, color = County)) +
  geom_line(size = 1.5) +
  geom_point(size = 3) +
  scale_color_manual(values = c("WEST YORKSHIRE" = "cyan", "SOUTH YORKSHIRE" = "pink")) +
  scale_y_continuous(limits = c(min_rate - 1, max_rate + 1)) +
  labs(
    title = "Average Drug Offense Rate per 10,000 People (2020–2024)",
    x = "Year",
    y = "Average Drug Offense Rate",
    color = "County"
  ) +
  theme_minimal()

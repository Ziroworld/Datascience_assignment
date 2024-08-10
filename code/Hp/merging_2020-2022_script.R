library(tidyverse)
library(dplyr)
library(ggplot2)
library(readr)

# you can use this same script to clean all 4 property data

# Read in the data
pp_2020 <- read_csv("/home/rohan-manandhar/College Every Semester Data/Semester 4/Data_Science/0assignment/new_data/cleaned_pp-2020.csv")
pp_2021 <- read_csv("/home/rohan-manandhar/College Every Semester Data/Semester 4/Data_Science/0assignment/new_data/cleaned_pp-2021.csv")
pp_2022 <- read_csv("/home/rohan-manandhar/College Every Semester Data/Semester 4/Data_Science/0assignment/new_data/cleaned_pp-2022.csv")
pp_2023 <- read_csv("/home/rohan-manandhar/College Every Semester Data/Semester 4/Data_Science/0assignment/new_data/cleaned_pp-2023.csv")


all_data <- bind_rows(pp_2020, pp_2021, pp_2022, pp_2023)

# View the merged data frame
view(all_data)

write_csv(all_data, "/home/rohan-manandhar/College Every Semester Data/Semester 4/Data_Science/0assignment/new_data/pp_2020_to_2023.csv")

all_data$Date <- as.Date(all_data$Date)


all_data <- all_data %>%
  mutate(Year = lubridate::year(Date))

avg_prices <- all_data %>%
  group_by(County, Year) %>%
  summarise(Avg_Price = mean(Price, na.rm = TRUE))

avg_prices


ggplot(avg_prices, aes(x = Year, y = Avg_Price, color = County, group = County)) +
  geom_line() +
  labs(title = "Average House Prices (2020 - 2023)",
       x = "Year",
       y = "Average Price") +
  scale_x_continuous(breaks = seq(2020, 2023, by = 1)) +
  theme_minimal() +
  theme(legend.position = "top")
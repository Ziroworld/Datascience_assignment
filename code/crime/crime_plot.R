library(tidyverse)
library(ggplot2)
library(dplyr)
library(fmsb)

# Load the datasets

population_data= read_csv("/home/rohan-manandhar/College Every Semester Data/Semester 4/Data_Science/Assignment/data_2024-07-9/Population2011_1656567141570.csv")

population_data <- population_data %>%
  mutate(Population2021 = 1.00561255390388033 * Population)

population_data <- population_data %>%
  mutate(Population2022 = 1.00561255390388033 * Population2021)

population_data <- population_data %>%
  mutate(Population2023 = 1.00561255390388033 * Population2022)

population_data

crime_data = read_csv("/home/rohan-manandhar/College Every Semester Data/Semester 4/Data_Science/Assignment/new_data/Crime/new_crime_data-may2021-dec2023.csv")

# Process the population data to match the postcode format of the crime data
population_data <- population_data %>%
  mutate(shortPostcode = substr(Postcode, 1, 4))

# Process the crime data
crime_data <- crime_data %>%
  mutate(shortPostcode = substr(Postcode, 1, 4))

# Join population data with the crime data
crime_data <- crime_data %>%
  left_join(population_data, by = "shortPostcode") %>% 
  select(Postcode.x, shortPostcode, Year, CrimeType, County, District, Town_City, month, n, Population, 
         Population2021, Population2022, Population2023) %>% 
  distinct()
# Check the resulting data
crime_data


#plotting line chart
drug_offense = crime_data %>% 
  filter(CrimeType=="Drugs") %>%
  group_by(Year,CrimeType,County) %>%
  summarise(
    Total_Crimes = sum(n),
    Total_Population = sum(Population2023)
  ) %>% 
  mutate(Rate = (Total_Crimes / Total_Population) * 10000) %>% 
  arrange(Rate) %>%
  ungroup()%>%
  mutate(row=row_number())

colors <- c("BRISTOL" = "red", "CORNWALL" = "blue")

bristol_data <- filter(drug_offense, County == "BRISTOL")
cornwall_data <- filter(drug_offense, County == "CORNWALL")

# Plotting drug offense rates by Year and County
ggplot(data = drug_offense, aes(x = Year, y = Rate, color = County, group = County)) +
  geom_line() +  # Draw lines for each County
  labs(title = "Drug Offense Rates (per 10,000 people)",
       x = "Year",
       y = "Rate") +
  scale_x_continuous(breaks = seq(min(drug_offense$Year), max(drug_offense$Year), by = 1)) +  # Set breaks for x-axis ticks
  theme_minimal() +  # Minimal theme
  theme(legend.position = "top")  # Position legend at the top

# Filter crime data for robbery crimes in specific month of year 2022, for Cornwall and Bristol
robbery_data <- crime_data %>%
  filter(CrimeType == "Robbery" & Year == 2022 & month == "2022-03")

# Calculate robbery crime rate per 10,000 people
robbery_data <- robbery_data %>%
  group_by(County) %>%
  summarise(
    Total_Crimes = sum(n),
    Total_Population = sum(Population2023)
  ) %>%
  mutate(Rate = (Total_Crimes / Total_Population) * 10000)

# Check the data to ensure it's correctly summarized
print(robbery_data)

# Create pie chart
ggplot(data = robbery_data, aes(x = "", y = Rate, fill = County)) +
  geom_bar(width = 1, stat = "identity") +  # Pie chart representation
  coord_polar(theta = "y") +  # Polar coordinates for pie chart
  labs(title = "Robbery Crime Rate per 10,000 People - April 2022",
       fill = "County",
       x = NULL,
       y = NULL) +
  theme_minimal() +  # Minimal theme
  theme(legend.position = "right")  # Position legend

robbery_data <- crime_data %>%
  filter(CrimeType == "Robbery", 
         Year == 2022, 
         month == "2022-04",
        )

# Calculate robbery crime rate per 10,000 people for Town_City in Cornwall & Cornwall
robbery_summary <- robbery_data %>%
  group_by(Town_City) %>%
  summarise(
    Total_Crimes = sum(n),
    Total_Population = sum(Population2023)
  ) %>%
  mutate(Rate = (Total_Crimes / Total_Population) * 10000)

# Create pie chart
ggplot(data = robbery_summary, aes(x = "", y = Rate, fill = Town_City)) +
  geom_bar(width = 1, stat = "identity") +  # Pie chart representation
  coord_polar(theta = "y") +  # Polar coordinates for pie chart
  labs(title = "Robbery Crime Rate per 10,000 People - April 2022 (Cornwall & Bristols') Towns & Cities",
       fill = "Town_City",
       x = NULL,
       y = NULL) +
  theme_minimal() +  # Minimal theme
  theme(legend.position = "right")  # Position legend

robbery_summary



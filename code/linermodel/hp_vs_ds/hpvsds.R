# Load necessary libraries
library(tidyverse)
library(dplyr)
library(ggplot2)
library(broom)
library(ggfortify)
library(scales)

# Load data from CSV files
pp_data <- read.csv("/home/rohan-manandhar/College Every Semester Data/Semester 4/Data_Science/Assignment/new_data/PP/pp_2020_to_2023.csv")
bb_data <- read.csv("/home/rohan-manandhar/College Every Semester Data/Semester 4/Data_Science/Assignment/new_data/Broadband/bb_cleaned.csv")

# Merge datasets on Postcode
merged_data <- merge(pp_data, bb_data, by = "Postcode")

# Merge Town_City.x and Town_City.y into a single column
merged_data <- merged_data %>%
  mutate(Town_City = coalesce(Town_City.x, Town_City.y),
         County = coalesce(County.x, County.y)) %>%
  select(-Town_City.x, -Town_City.y, -County.x, -County.y)

# Check for null values just in case
null_values <- sapply(merged_data, function(x) sum(is.na(x)))
print(null_values)

# Convert to a regular data frame to avoid issues with autoplot
merged_data <- as.data.frame(merged_data)

# Create a linear model
model <- lm(Price ~ Average.download.speed..Mbit.s., data = merged_data)

# Adjust number format to avoid scientific notation
options(scipen=999)

# Summary statistics of the model
summary(model)

# Plot the data and the line of best fit
ggplot(data = merged_data, aes(x = Average.download.speed..Mbit.s., y = Price)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "House Prices vs Download Speed",
       x = "Download Speed (Mbps)",
       y = "House Price (in $1000s)") +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal()

# Generate diagnostic plots directly from the model object
autoplot(model)

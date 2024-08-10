library(tidyverse)
library(dplyr)
library(ggplot2)
library(readr)


# Read in the data ---accessed (09/07/2024)
data <- read_csv("/home/rohan-manandhar/College Every Semester Data/Semester 4/Data_Science/0assignment/data_2024-07-9/pp-2022.csv")

colnames(data) <- c(
  "ID", "Price", "Date", "Postcode", "Property_Type", "Old_New",
  "Duration", "PAON", "SAON", "Street", "Locality", "Town_City",
  "District", "County", "Category_Type", "Record_Status"
)
#view(data)

#filter data for bristol and cornwall as county
f_data <- data %>% 
  filter(grepl("Cornwall", County, ignore.case = TRUE) | grepl("Bristol", County, ignore.case = TRUE)) %>%
  distinct()  # Remove duplicate rows
#view(filtering)

#missing value check
m_data <- f_data %>%
  summarise_all(~sum(is.na(.))) %>%
  gather(key = "Column", value = "Missing_Values")
print(m_data)

# Impute missing values using the mode while excluding NA values
mode_function <- function(x) {
  ux <- unique(na.omit(x))  
  tab <- tabulate(match(x, ux))  
  ux[tab == max(tab)]  # Return mode(s) for non-NA values
}

mode_function(f_data$Postcode)
mode_function(f_data$Street)
mode_function(f_data$Locality)

# Handle missing values, convert data types, and convert columns to uppercase
c_data <- f_data %>%
  mutate(
    Postcode = na_if(Postcode, ""),
    SAON = na_if(SAON, ""),
    Street = na_if(Street, ""),
    Locality = na_if(Locality, "")
  ) %>%
  mutate(
    Postcode = ifelse(is.na(Postcode), mode_function(Postcode), Postcode),
    Street = ifelse(is.na(Street), mode_function(Street), Street),
    Locality = ifelse(is.na(Locality), mode_function(Locality), Locality),
    SAON = ifelse(is.na(SAON), "UNKNOWN", SAON)
  ) %>%
  mutate(
    Date = as.Date(Date, format = "%Y-%m-%d"),
    Price = as.numeric(Price)
  ) %>%
  mutate(
    across(c(Postcode, Property_Type, Old_New, Duration, PAON, SAON, Street, Locality, Town_City, District, County, Category_Type, Record_Status), toupper)
  )

# View the updated cleaned_data
#view(c_data)

#recheck after handling missing values
m_data <- c_data %>%
  summarise_all(~sum(is.na(.))) %>%
  gather(key = "Column", value = "Missing_Values")

print(m_data)

Q1 <- quantile(c_data$Price, 0.25, na.rm = TRUE)
Q3 <- quantile(c_data$Price, 0.75, na.rm = TRUE)

Q1
Q3

IQR = Q3-Q1
IQR

outlier_threshold <- 1.5 * IQR
outlier_threshold

c_data <- c_data %>%
  filter(Price >= (Q1 - outlier_threshold) & Price <= (Q3 + outlier_threshold))

#view(c_data)

#24398

file_path <- "/home/rohan-manandhar/College Every Semester Data/Semester 4/Data_Science/0assignment/new_data/cleaned_pp-2022.csv"

#Write filtered_data to cleaned_pp-2020.csv
write_csv(x=c_data,file =file_path)

c_data$Price <- as.integer(c_data$Price)

ggplot(c_data, aes(x = County, y = Price, fill = County)) +
  geom_boxplot() +
  scale_y_continuous(labels = scales::comma) + # Adds commas for better readability
  labs(title = "Housing Prices Comparison: Bristol vs Cornwall",
       x = "County",
       y = "Price") +
  theme_minimal()

# Checking Density Distribution of Housing Prices for Town/City within Bristol and Cornwall

#ggplot(cleaned_data, aes(x = Price, fill = Town_City)) +
#  geom_density(alpha = 0.6) +
#  facet_wrap(~ County) +
#  scale_x_continuous(labels = scales::comma) + # Adds commas for better readability
#  labs(title = "Density Distribution of Housing Prices: Bristol vs Cornwall",
#       x = "Price",
#       y = "Density") +
#  theme_minimal()



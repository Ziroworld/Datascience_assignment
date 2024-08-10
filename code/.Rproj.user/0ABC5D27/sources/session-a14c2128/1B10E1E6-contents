library(tidyverse)
library(dplyr)

file_path <- "/home/rohan-manandhar/College Every Semester Data/Semester 4/Data_Science/0assignment/data_2024-07-9/merged_crime_data"

file_list <- list.files(path = file_path, pattern = "*.csv", full.names = TRUE)

file_list

# Function to read and clean individual CSV files
read_and_clean_csv <- function(file) {
  data <- read_csv(file)
  # Ensure column names are consistent (you can adjust this based on your actual column names)
  colnames(data) <- tolower(colnames(data))
  data
}
# Check if all files have the same number of columns
column_counts <- sapply(file_list, function(file) {
  ncol(read_csv(file, n_max = 1))  # Read only the first row to check the number of columns
})

# Check if all files have the same number of columns
if (length(unique(column_counts)) != 1) {
  stop("Not all files have the same number of columns.")
}
crime_data <- file_list %>%
  map_df(~ read_and_clean_csv(.))

rows=nrow(crime_data)
rows

write_csv(crime_data, "/home/rohan-manandhar/College Every Semester Data/Semester 4/Data_Science/0assignment/data_2024-07-9/crime_data/merged_crime_data_may2020-dec2023.csv")

crime <- read_csv("/home/rohan-manandhar/College Every Semester Data/Semester 4/Data_Science/0assignment/data_2024-07-9/crime_data/merged_crime_data_may2020-dec2023.csv")
colnames(crime)
crime <- crime %>%
  mutate(Year = substring(month, 1, 4)) %>%
  rename(lsoa11cd = `lsoa code`, CrimeType = `crime type`) %>%
  select(lsoa11cd, Year, CrimeType, month)
head(crime)
towns <- read_csv("/home/rohan-manandhar/College Every Semester Data/Semester 4/Data_Science/0assignment/new_data/PP/pp_2020_to_2023.csv")
lsoa <- read_csv("/home/rohan-manandhar/College Every Semester Data/Semester 4/Data_Science/0assignment/data_2024-07-9/Postcode to LSOA.csv/Postcode to LSOA.csv")


lsoa = lsoa %>%
  rename(Postcode="pcds") %>% 
  left_join(towns,by="Postcode") %>%
  filter(County=="CORNWALL"|County=="CITY OF BRISTOL") %>% # if you have another name for Counties use that
  group_by(lsoa11cd) %>%
  filter(row_number()==1) %>%
  select(lsoa11cd,Postcode,Town_City,District,County)
lsoa 

#view(lsoa)
clean_crime_data <- left_join(crime, lsoa, by = "lsoa11cd") %>%
  group_by(Postcode, Year, CrimeType, County, District, Town_City, month) %>%
  tally()


clean_crime_data

colSums(is.na(clean_crime_data))


# Example: Remove NA values only from specific columns
clean_crime_data <- na.omit(clean_crime_data)

colSums(is.na(clean_crime_data))

summary(clean_crime_data$n)


Q1 <- quantile(clean_crime_data$n, 0.25, na.rm = TRUE)
Q3 <- quantile(clean_crime_data$n, 0.75, na.rm = TRUE)

Q1
Q3

IQR = Q3-Q1
IQR

outlier_threshold <- 1.5 * IQR
outlier_threshold

clean_crime_data <- clean_crime_data %>%
  filter(n >= (Q1 - outlier_threshold) & n <= (Q3 + outlier_threshold))

summary(clean_crime_data$n)

clean_crime_data<- clean_crime_data %>% 
  distinct()


output_file <- "/home/rohan-manandhar/College Every Semester Data/Semester 4/Data_Science/0assignment/new_data/Crime/new_crime_data-may2021-dec2023.csv"
write_csv(clean_crime_data, output_file)

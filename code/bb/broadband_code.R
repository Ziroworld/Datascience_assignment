library(tidyverse)
library(dplyr)

bb = read_csv("/home/rohan-manandhar/College Every Semester Data/Semester 4/Data_Science/Assignment/data_2024-07-9/Broadband/201805_fixed_pc_performance_r03.csv")

bb = bb %>% 
  rename(Postcode="postcode_space") %>% 
  select(Postcode, `Average download speed (Mbit/s)`, `Average upload speed (Mbit/s)`, `Minimum upload speed (Mbit/s)`,
         `Maximum upload speed (Mbit/s)`, `Minimum download speed (Mbit/s)`, `Maximum download speed (Mbit/s)`) 

summary(bb)

# Count the number of NA values for each column
na_count <- bb %>%
  summarise_all(~sum(is.na(.))) %>%
  gather(key = "Column", value = "Missing_Values")

na_count

#counting number of 0 values in each column
zero_counts <- colSums(bb == 0, na.rm = TRUE)
zero_counts

#mean values calculation (replace with your actual mean values)
mean_values <- summarise(bb, across(where(is.numeric), mean, na.rm = TRUE))

#changing 0 values to na and na to mean values
bb_cleaned <- bb %>% 
  mutate(across(where(is.numeric), ~ na_if(., 0))) %>% 
  mutate(across(where(is.numeric), ~ if_else(is.na(.), mean_values[[cur_column()]], .))) %>% 
  distinct()

#see if there are any na values 
na_count <- bb_cleaned %>%
  summarise_all(~sum(is.na(.))) %>%
  gather(key = "Column", value = "Missing_Values")

#recheck
na_count

# 0 values in each column
zero_recounts <- colSums(bb_cleaned == 0, na.rm = TRUE)
print(zero_recounts)

bb_cleaned

towns <- read_csv("/home/rohan-manandhar/College Every Semester Data/Semester 4/Data_Science/Assignment/new_data/PP/pp_2020_to_2023.csv")

bb_cleaned <- bb_cleaned %>%
  left_join(towns, by = "Postcode") %>%
  filter(County == "CORNWALL" | County =="CITY OF BRISTOL") %>% 
  distinct() %>% 
  select(Postcode,Town_City, `Average download speed (Mbit/s)`, `Average upload speed (Mbit/s)`, `Minimum upload speed (Mbit/s)`,
         `Maximum upload speed (Mbit/s)`, `Minimum download speed (Mbit/s)`, `Maximum download speed (Mbit/s)`, County)

#view(bb_cleaned)

#remove outliers
Q1 <- summarise(bb_cleaned, across(where(is.numeric), quantile, probs = 0.25, na.rm = TRUE))
Q3 <- summarise(bb_cleaned, across(where(is.numeric), quantile, probs = 0.75, na.rm = TRUE))

Q1
Q3

IQR_values <- Q3 - Q1
outlier_threshold <- 1.5 * IQR_values

IQR_values
outlier_threshold

bb_cleaned <- bb_cleaned %>%
  filter(across(where(is.numeric), ~ . >= (Q1[[cur_column()]] - outlier_threshold[[cur_column()]]) &
                  . <= (Q3[[cur_column()]] + outlier_threshold[[cur_column()]])))
colnames(bb_cleaned)
view(bb_cleaned)
summary(bb_cleaned)

file_path <- "/home/rohan-manandhar/College Every Semester Data/Semester 4/Data_Science/Assignment/new_data/Broadband/bb_cleaned.csv"
write_csv(x=bb_cleaned,file =file_path)

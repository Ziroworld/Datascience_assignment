library(tidyverse)
library(dplyr)


bristol_final= read_csv("D:/st5014cem data science for developers/Obtained Data (07-08-2024 accessed)/School/2021-22-bristol/801_ks4final.csv")

bristol_final <- bristol_final %>%
  select(URN, SCHNAME, ADDRESS1, TOTATT8, ATT8SCR) %>%
  rename(
    urn = URN,
    `School Name` = SCHNAME,
    Address = ADDRESS1,
    `Total Attainment 8` = TOTATT8,
    `Attainment 8 Score` = ATT8SCR
    
  )


summary(bristol_final)
view(bristol_final)

bristol_school_info = read_csv("D:/st5014cem data science for developers/Obtained Data (07-08-2024 accessed)/School/2021-22-bristol/801_school_information.csv")

bristol_school_info<- bristol_school_info %>% 
  select(URN, LANAME, SCHNAME, POSTCODE) %>% 
  rename(
    urn=URN,
    County = LANAME,
    `School Name` = SCHNAME,
    Postcode= POSTCODE
  )
bristol_school_info

bristol_combined <- left_join(bristol_final, bristol_school_info, by = "urn") %>% 
  select(urn, `School Name.x`, Address, County, Postcode, `Total Attainment 8`, `Attainment 8 Score`) %>% 
  mutate(County = ifelse(County == "Bristol, City of", "CITY OF BRISTOL", County)) %>% 
  distinct()

bristol_combined <- bristol_combined %>% 
  filter(grepl("^[0-9.]+$", `Total Attainment 8`) & grepl("^[0-9.]+$", `Attainment 8 Score`)) %>% 
  drop_na()

# View the result
view(bristol_combined)


## for cornwall

cornwall_final= read_csv("D:/st5014cem data science for developers/Obtained Data (07-08-2024 accessed)/School/2021-22-cornwall/908_ks4final.csv")
cornwall_final <- cornwall_final %>%
  select(URN, SCHNAME, ADDRESS1, TOTATT8, ATT8SCR) %>%
  rename(
    urn = URN,
    `School Name` = SCHNAME,
    Address = ADDRESS1,
    `Total Attainment 8` = TOTATT8,
    `Attainment 8 Score` = ATT8SCR
    
  )


summary(cornwall_final)
view(cornwall_final)

cornwall_school_info = read_csv("D:/st5014cem data science for developers/Obtained Data (07-08-2024 accessed)/School/2021-22-cornwall/908_school_information.csv")

cornwall_school_info<- cornwall_school_info %>% 
  select(URN, LANAME, SCHNAME, POSTCODE) %>% 
  rename(
    urn=URN,
    County = LANAME,
    `School Name` = SCHNAME,
    Postcode= POSTCODE
  )
cornwall_school_info

cornwall_combined <- left_join(cornwall_final, cornwall_school_info, by = "urn") %>% 
  select(urn, `School Name.x`, Address, County, Postcode, `Total Attainment 8`, `Attainment 8 Score`) %>% 
  mutate(County = ifelse(County == "Cornwall", "CORNWALL", County)) %>% 
  distinct()

cornwall_combined <- cornwall_combined %>% 
  filter(grepl("^[0-9.]+$", `Total Attainment 8`) & grepl("^[0-9.]+$", `Attainment 8 Score`)) %>% 
  drop_na()

# View the result
view(cornwall_combined)

combined_data <- bind_rows(
  bristol_combined,
  cornwall_combined
)

combined_data <- combined_data %>%
  mutate(`Attainment 8 Score` = as.numeric(`Attainment 8 Score`))

view(combined_data)

Q1 <- quantile(combined_data$`Attainment 8 Score`, 0.25, na.rm = TRUE)
Q3 <- quantile(combined_data$`Attainment 8 Score`, 0.75, na.rm = TRUE)

Q1
Q3

IQR = Q3-Q1
IQR

outlier_threshold <- 1.5 * IQR
outlier_threshold

combined_data <- combined_data %>%
  filter(`Attainment 8 Score` >= (Q1 - outlier_threshold) & `Attainment 8 Score` <= (Q3 + outlier_threshold))

ggplot(combined_data, aes(County, `Attainment 8 Score`, fill=County)) +
  geom_boxplot()+
  labs(title = "Average Attainment 8 Score in 2021-2022 Academic Year for Both Counties",
       x = "County",
       y = "Attainment 8 Score") +
  theme_minimal()

#for line chart of bristol
bristol_avg_attainment <- bristol_combined %>%
  group_by(`School Name.x`) %>%
  summarise(Avg_Attainment_8_Score = mean(as.numeric(`Attainment 8 Score`), na.rm = TRUE)) %>%
  arrange(Avg_Attainment_8_Score)

bristol_avg_attainment

# Plot as a line chart
ggplot(data = bristol_avg_attainment, aes(x = `School Name.x`, y = Avg_Attainment_8_Score)) +
  geom_line(color = "blue", size = 3) +  # Use geom_point() to show points
  labs(title = "Average Attainment 8 Score in Academic Year 2021-2022 for Bristol Schools",
       x = "School Name",
       y = "Average Attainment 8 Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  coord_flip()







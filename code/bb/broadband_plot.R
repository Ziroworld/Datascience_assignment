library(tidyverse)
library(dplyr)
library(ggplot2)

#read data
bb_data=read_csv("/home/rohan-manandhar/College Every Semester Data/Semester 4/Data_Science/Assignment/new_data/Broadband/bb_cleaned.csv")
view(bb_data)
# boxplot
ggplot(bb_data, aes(x = County, y = `Average download speed (Mbit/s)`, fill = County)) +
  geom_boxplot() +
  labs(title = "Average Download Speeds in Both Counties",
       x = "County",
       y = "Average Download Speed (Mbit/s)") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")

#BRISTOL
# Aggregate data for Bristol (Average and Maximum Download Speeds)
bristol_speed <- bb_data %>%
  filter(County == "CITY OF BRISTOL") %>%
  summarise(Avg_dl_Speed = mean(`Average download speed (Mbit/s)`, na.rm = TRUE),
            Max_dl_Speed = max(`Average download speed (Mbit/s)`, na.rm = TRUE))

# Reshape the data for plotting
bristol_speed_long <- bristol_speed %>%
  pivot_longer(cols = c(Avg_dl_Speed, Max_dl_Speed), names_to = "Metric", values_to = "Speed")

# Plot bar chart for Bristol
ggplot(bristol_speed_long, aes(x = Metric, y = Speed, fill = Metric)) +
  geom_bar(stat = "identity", width = 0.5) +
  labs(title = "Average and Maximum Download Speeds in City of Bristol",
       x = "Metric", y = "Download Speed (Mbit/s)") +
  theme_minimal()


# CORNWALL
# Aggregate data for Cornwall (Average and Maximum Download Speeds)
cornwall_speed <- bb_data %>%
  filter(County == "CORNWALL") %>%
  summarise(Avg_dl_Speed = mean(`Average download speed (Mbit/s)`, na.rm = TRUE),
            Max_dl_Speed = max(`Average download speed (Mbit/s)`, na.rm = TRUE))

# Reshape the data for plotting
cornwall_speed_long <- cornwall_speed %>%
  pivot_longer(cols = c(Avg_dl_Speed, Max_dl_Speed), names_to = "Metric", values_to = "Speed")

# Plot bar chart for Cornwall
ggplot(cornwall_speed_long, aes(x = Metric, y = Speed, fill = Metric)) +
  geom_bar(stat = "identity", width = 0.5) +
  labs(title = "Average and Maximum Download Speeds in Cornwall",
       x = "Metric", y = "Download Speed (Mbit/s)") +
  theme_minimal()


library(tidyverse)
library(readxl)

emr_data <- read_excel("C:/Users/Tanner/OneDrive - Michigan Technological University/PhD/EMR/EMR Survey Detection Data.xlsx")

library(dplyr)
library(lme4)
library(lubridate)

# Step 2: Ensure date column is in Date format
emr_data <- emr_data %>%
  mutate(date = as.Date(Date))  # adjust if your column name is different

# Step 3: Add camera deployment start date
deployment_start <- as.Date("2024-04-17")

# Step 4: Create 'camera_days' column
emr_data <- emr_data %>%
  mutate(camera_days = as.numeric(date - deployment_start))


emr_data <- emr_data %>%
  mutate(identified = if_else(`Detection ID` == "No ID", FALSE, TRUE))





# Detection rate model
detection_summary <- emr_data %>%
  group_by(Site) %>%
  summarise(detections = n(),
            camera_days = n_distinct(date) * n_distinct(Array))

model <- glm(detections ~ Site, family = "poisson", offset = log(camera_days), data = detection_summary)
summary(model)


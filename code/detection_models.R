library(readxl)
library(dplyr)
library(lubridate)
library(hms)
library(ggplot2)
library(broom)
library(purrr)
library(effects)
# Load your data from your files
emr_data <- read_excel("C:/Users/Tanner/OneDrive - Michigan Technological University/PhD/EMR/EMR Survey Detection Data (version 1).xlsx", 
sheet = "Sheet1")

# Step 2: Ensure date column is in Date format
emr_data <- emr_data %>%
  mutate(date = as.Date(Date))  # adjust if your column name is different

# Step 3: Add camera deployment start date
deployment_start <- as.Date("2024-04-17")

# Step 4: Create 'camera_days' column
emr_data <- emr_data %>%
  mutate(camera_days = as.numeric(date - deployment_start))

# Indicate if the individual was identified or not
emr_data <- emr_data %>%
  mutate(identified = if_else(`Detection ID` == "No ID", FALSE, TRUE))

#Get rid of bogus date from excel
emr_data <- emr_data %>% 
  mutate(Time = as_hms(Time))

# Add month
emr_data_clean <- emr_data %>%
  mutate(month = month(date, label = TRUE),
    hour = hour(hms::as_hms(Time)),
    day = day(date),
    celsius = ((Temperature - 32) * (5/9))
  )

View(emr_data_clean)


#############################################################################################################
# Develop a binomial presence/absence model
# create time-bound absence detections
hour_bounds <- emr_data_clean %>%
  mutate(hour = hour(hms::as_hms(Time))) %>%
  summarise(
    min_hour = min(hour, na.rm = TRUE),
    max_hour = max(hour, na.rm = TRUE)
  )
min_hour <- hour_bounds$min_hour
max_hour <- hour_bounds$max_hour

# Unique dates and sites
all_dates <- seq(min(emr_data_clean$date), max(emr_data_clean$date), by = "1 day")
all_sites <- unique(emr_data_clean$Site)
active_hours <- min_hour:max_hour

# Expand grid
effort_grid <- expand.grid(
  Site = all_sites,
  date = all_dates,
  hour = active_hours
) %>%
  mutate(
    date = as.Date(date),
    hour = as.integer(hour)
  )

#create detections table with site, date, hour
detection_events <- emr_data_clean %>%
  mutate(
    date = as.Date(date),
    hour = hour(hms::as_hms(Time)),
    detected = TRUE,
    temp = as.numeric(celsius),
    month = month(date, label = TRUE)
  ) %>%
  dplyr::select(Site, date, Time, hour, detected, temp, month)  # Include Time here

#join effort to detections
model_data <- effort_grid %>%
  left_join(detection_events, by = c("Site", "date", "hour")) %>%
  mutate(
    detected = ifelse(is.na(detected), FALSE, detected),
    month = month(date, label = TRUE)
  )

glm_month <- glm(
  detected ~ month,
  data = model_data,
  family = binomial
)
summary(glm_month)

model_data <- model_data %>%
  mutate(month = factor(month, ordered = FALSE))  # treat as nominal

glm_month_nominal <- glm(
  detected ~ month,
  data = model_data,
  family = binomial
)

summary(glm_month_nominal)



pred_df <- data.frame(month = levels(model_data$month))
pred_df$pred_prob <- predict(glm_month_nominal, newdata = pred_df, type = "response")
# Define the month order explicitly
month_levels <- c("Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct")
pred_df$month <- factor(pred_df$month, levels = month_levels)

ggplot(pred_df, aes(x = month, y = pred_prob)) +
  geom_col(fill = "forestgreen") +
  labs(title = "Predicted Snake Detection Probability by Month",
       y = "Detection Probability", x = "Month") +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank())


hour_model <- glm(detected ~ month + hour, family = binomial, data = model_data)
summary(hour_model)


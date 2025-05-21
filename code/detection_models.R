library(readxl)
library(dplyr)
library(lubridate)
library(hms)
library(ggplot2)
library(broom)
library(purrr)
library(effects)
library(tidyr)

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

model_data <- model_data %>%
  mutate(month = factor(month, ordered = FALSE), 
        (hour2 = hour^2))

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


# Split model_data by Site and fit a model to each subset
site_models <- model_data %>%
  group_by(Site) %>%
  group_split() %>%
  map(~ glm(detected ~ month + hour + hour2, data = .x, family = binomial()))

# Summarize the models using broom::tidy()
site_model_summaries <- map_dfr(site_models, tidy, .id = "Site")

# View only significant results
site_model_summaries %>% filter(p.value < 0.05)

# Field 3 models
field3_data <- model_data %>% filter(Site == "Field 3")
tg_data <- model_data %>% filter(Site == "Tree Graveyard")
ss_data <- model_data %>% filter(Site == "South Shore")
tg_ss_data <- model_data %>% filter(Site == "South Shore" | Site == "Tree Graveyard")


# Fit a quadratic model per site
field3_quad <- glm(detected ~ month + hour + hour2, 
                    family = binomial, 
                    data = field3_data)
summary(field3_quad)

ss_quad <- glm(detected ~ month + hour + hour2, 
                    family = binomial, 
                    data = ss_data)
summary(ss_quad)

tg_quad <- glm(detected ~ month + hour + hour2, 
                    family = binomial, 
                    data = tg_data)
summary(tg_quad)

tg_ss_quad <- glm(detected ~ month + hour + hour2, 
                    family = binomial, 
                    data = tg_ss_data)
summary(tg_ss_quad)

# Visualize
# Create a prediction dataset
hour_seq <- seq(6, 20, by = 1)  # Assuming snake detection only happens 6am–8pm
months <- levels(model_data$month)

# Create a grid of all combinations of month and hour
new_data <- expand_grid(
  hour = hour_seq,
  month = months
)

names(site_models) <- unique(model_data$Site)

# Get predictions for each site
site_predictions <- map2_dfr(site_models, names(site_models), function(model, site_name) {
  preds <- predict(model, newdata = new_data, type = "response")
  new_data %>%
    mutate(predicted = preds, Site = site_name)
})

# Ensure month is ordered correctly
site_predictions$month <- factor(site_predictions$month, levels = c("Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct"))

# Plot
ggplot(site_predictions, aes(x = hour, y = predicted, color = month)) +
  geom_line(size = 1) +
  facet_wrap(~ Site) +
  labs(
    title = "Predicted Snake Detection Probability by Hour and Site",
    x = "Hour of Day",
    y = "Predicted Detection Probability",
    color = "Month"
  ) +
  theme_bw() +
  theme(panel.grid = element_blank())

# Save the plot
ggsave(
  filename = "prediction_per_site.png",
  path = "C:/Users/Tanner/OneDrive - Michigan Technological University/PhD/EMR/figures",
  width = 10,
  height = 6,
  dpi = 300
)

# Make sure your data includes hour
emr_data_clean <- emr_data_clean %>%
  mutate(
    hour = lubridate::hour(hms::as_hms(Time))
  )

# Group and count detections per hour and site
detections_by_hour <- emr_data_clean %>%
  group_by(Site, hour) %>%
  summarise(detections = n(), .groups = "drop")

# View the table
print(detections_by_hour)

ggplot(detections_by_hour, aes(x = hour, y = detections, color = Site)) +
  geom_line(size = 1) +
  labs(
    title = "Number of Detections by Hour and Site",
    x = "Hour of Day",
    y = "Number of Detections"
  ) +
  theme_bw() +
  theme(panel.grid = element_blank())


# Fit a quadratic model
glm_hour_quad <- glm(detected ~ month + hour + hour2, 
                    family = binomial, 
                    data = model_data)
summary(glm_hour_quad)

# Create prediction grid
new_data <- expand.grid(
  hour = seq(min(model_data$hour, na.rm = TRUE),
             max(model_data$hour, na.rm = TRUE),
             by = 0.1),
  month = "Aug"  # Choose any month for visualization
)
new_data$hour2 <- new_data$hour^2

# Predict
new_data$predicted <- predict(glm_hour_quad, newdata = new_data, type = "response")

# Plot
ggplot(new_data, aes(x = hour, y = predicted)) +
  geom_line(size = 1.2) +
  labs(
    title = "Predicted Detection Probability by Time of Day",
    y = "Predicted Probability",
    x = "Hour of Day"
  ) +
  theme_bw()

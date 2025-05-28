library(readxl)
library(dplyr)
library(lubridate)
library(hms)
library(ggplot2)
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

# Step 1: Add month
emr_data_clean <- emr_data %>%
  mutate(month = format(as.Date(Date), "%Y-%m"))


emr_data_summary <- emr_data_clean %>%
  mutate(
    date = as.Date(Date),
    week = floor_date(date, unit = "week"),
    month = floor_date(date, unit = "month"),
    is_identified = `Detection ID` != "No ID"
  )

detections_month_site <- emr_data_clean %>% 
count(Site, month)

# Detections per week
detections_week <- emr_data_summary %>%
  count(week)

# Detections per month
detections_month <- emr_data_summary %>%
  count(month)
detections_month

detections_per_site <- emr_data_summary %>%
  count(Site)
detections_per_site

id_vs_noid <- emr_data_summary %>%
  count(is_identified)
id_vs_noid

unique_ids_per_site <- emr_data_summary %>%
  filter(is_identified) %>%
  distinct(Site, `Detection ID`) %>%
  count(Site, name = "unique_individuals")
unique_ids_per_site

individuals_more_than_once <- emr_data_clean %>%
  filter(`Detection ID` != "No ID") %>%
  group_by(Site, `Detection ID`) %>%
  summarise(n_detections = n(), .groups = "drop") %>%
  filter(n_detections > 1) %>%
  group_by(Site) %>%
  summarise(individuals_more_than_once = n())
individuals_more_than_once
  
individuals_multiple_sites <- emr_data_clean %>%
  filter(`Detection ID` != "No ID") %>%
  distinct(`Detection ID`, Site) %>%
  group_by(`Detection ID`) %>%
  summarise(n_sites = n_distinct(Site), .groups = "drop") %>%
  filter(n_sites > 1)
individuals_multiple_sites

mean_detections_per_id <- emr_data_summary %>%
  filter(is_identified) %>%
  count(`Detection ID`) %>%
  summarise(
    mean = mean(n),
    sd = sd(n),
    min = min(n),
    max = max(n),
    median = median(n)
  )

# Convert and sort by date
emr_cumulative <- emr_data_clean %>%
  mutate(
    date = as.Date(Date),
    Site = as.factor(Site)
  ) %>%
  arrange(Site, date) %>%
  group_by(Site, date) %>%
  summarise(daily_detections = n(), .groups = "drop") %>%
  group_by(Site) %>%
  arrange(date) %>%
  mutate(cumulative_detections = cumsum(daily_detections))

ggplot(emr_cumulative, aes(x = date, y = cumulative_detections, color = Site)) +
  geom_line(size = 1) +
  labs(
    title = "Cumulative EMR Detections Over Time by Site",
    x = "Date",
    y = "Cumulative Detections",
    color = "Site"
  ) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "right",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Save the plot
ggsave(
  filename = "cumulative_detections_by_site.png",
  path = "C:/Users/Tanner/OneDrive - Michigan Technological University/PhD/EMR/figures",
  width = 10,
  height = 6,
  dpi = 300
)

library(viridis)
scale_color_viridis_d(option = "D", end = 0.85)

ggplot(emr_cumulative, aes(x = date, y = cumulative_detections, color = Site)) +
  geom_line(size = 1) +
  geom_point(size = 2, alpha = 0.7) +
  scale_color_viridis_d(option = "D", end = 0.85) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  labs(
    title = "Cumulative EMR Detections Over Time by Site",
    x = "Month",
    y = "Cumulative Detections",
    color = "Site",
    caption = "Data: Eastern Massasauga Rattlesnake detections (Apr–Oct 2024)"
  ) +
  theme_bw(base_size = 14) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), panel.border = element_rect(color = "black"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_text(face = "bold"),
    legend.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold", hjust = 0.5)
  )
# Save the plot
ggsave(
  filename = "detections_per_site.png",
  path = "C:/Users/Tanner/OneDrive - Michigan Technological University/PhD/EMR/figures",
  width = 10,
  height = 6,
  dpi = 300
)

# presence-only model ################################################################################################
library(broom)
library(purrr)
library(effects)

emr_data <- emr_data %>%
  mutate(
    month = month(date, label = TRUE),
    hour = hour(hms::as_hms(Time)),
    day = day(date)
  )
# Model for entire dataset
glm_model_data <- emr_data %>%
  group_by(month, hour) %>%
  summarise(detections = n(),
            temp = mean(Temperature, na.rm = TRUE),
            .groups = "drop")

#check for colinearity
library(car)
lm_check <- lm(temp ~ hour + month, data = glm_model_data)
vif(lm_check)  # Values > 5 or 10 suggest problematic collinearity




glm_model <- glm(detections ~ hour + temp + month, family = "poisson", data = glm_model_data)
summary(glm_model)

plot(allEffects(glm_model))
dispersion <- sum(residuals(glm_model, type = "pearson")^2) / df.residual(glm_model)
print(dispersion)  # > 1 means overdispersed
library(MASS)
glm_nb <- glm.nb(detections ~ hour + temp + month, data = glm_model_data)
summary(glm_nb)
















# By site
# Field 3 
field3 <- emr_data %>%
  filter(Site == "Field 3") %>%
  group_by(month, hour) %>%
  summarise(
    detections = n(),
    temp = mean(Temperature, na.rm = TRUE),
    .groups = "drop"
  )

field3_model <- glm(detections ~ hour + temp + month, data = field3, family = "poisson")
summary(field3_model)

# South shore
south_shore <- emr_data %>%
  filter(Site == "South Shore") %>%
  group_by(month, hour) %>%
  summarise(
    detections = n(),
    temp = mean(Temperature, na.rm = TRUE),
    .groups = "drop"
  )

south_shore_model <- glm(detections ~ hour + temp + month, data = south_shore, family = "poisson")
summary(south_shore_model)

# Tree Graveyard
tree_graveyard <- emr_data %>%
  filter(Site == "Tree Graveyard") %>%
  group_by(month, hour) %>%
  summarise(
    detections = n(),
    temp = mean(Temperature, na.rm = TRUE),
    .groups = "drop"
  )

tree_graveyard_model <- glm(detections ~ hour + temp + month, data = tree_graveyard, family = "poisson")
summary(tree_graveyard_model)


emr_data %>%
  group_by(Site) %>%
  summarise(
    n = n(),
    temp_range = max(Temperature, na.rm = TRUE) - min(Temperature, na.rm = TRUE),
    hour_range = max(hour, na.rm = TRUE) - min(hour, na.rm = TRUE)
  )

# Check to see if temperature is normally distributed
ggplot(emr_data, aes(x = Temperature)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "skyblue", color = "white") +
  geom_density(color = "darkblue", size = 1.2) +
  facet_wrap(~ Site) +
  labs(title = "Temperature Distribution by Site", x = "Temperature", y = "Density") +
  theme_bw()
emr_data %>%
  group_by(Site) %>%
  summarise(
    shapiro_p = shapiro.test(Temperature)$p.value
  )

# Check to see if hour is normally distributed
ggplot(emr_data, aes(x = hour)) +
  geom_histogram(aes(y = after_stat(density)), bins = 24, fill = "lightgreen", color = "white") +
  geom_density(color = "darkgreen", size = 1.2) +
  facet_wrap(~ Site) +
  labs(title = "Hour of Detection Distribution by Site", x = "Hour", y = "Density") +
  theme_bw()
emr_data %>%
  group_by(Site) %>%
  summarise(
    shapiro_p = shapiro.test(hour)$p.value
  )






# Simplified model
emr_data_filter <- emr_data %>%
  filter(Site != "Inholding") %>%
  mutate(month = month(date, label = TRUE)) %>%
  group_by(Site, date, month) %>%
  summarise(
    detected = n() > 0,
    hour = mean(hour),
    temperature = mean(Temperature, na.rm = TRUE),
    .groups = "drop"
  )

# Run binomial GLM
glm_model <- glm(
  detected ~ hour + temperature + month,
  family = binomial,
  data = emr_data_filter
)

summary(glm_model)

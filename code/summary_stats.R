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

# Detections per week
detections_week <- emr_data_summary %>%
  count(week)

# Detections per month
detections_month <- emr_data_summary %>%
  count(month)

detections_per_site <- emr_data_summary %>%
  count(Site)

id_vs_noid <- emr_data_summary %>%
  count(is_identified)

unique_ids_per_site <- emr_data_summary %>%
  filter(is_identified) %>%
  distinct(Site, `Detection ID`) %>%
  count(Site, name = "unique_individuals")

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

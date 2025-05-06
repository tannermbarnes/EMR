library(tidyverse)
library(readxl)
library(hms)
library(dplyr)
library(lme4)
library(lubridate)

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

####################################################################################################################3
########## Create Population Estimates ###############################
library(Rcapture)
# Step 1: Filter to identified individuals and add month column
emr_data_clean <- emr_data %>%
  filter(`Detection ID` != "No ID") %>%
  mutate(month = format(as.Date(Date), "%Y-%m"))

# Step 2: Define sites of interest
site_list <- list(
  "Field 3" = c("Field 3"),
  "South Shore" = c("South Shore"),
  "Tree Graveyard" = c("Tree Graveyard"),
  "Combined" = c("South Shore", "Tree Graveyard")
)

# Step 3: Create empty list for results
summary_list <- list()

# Step 4: Loop over each site/group
for (site_name in names(site_list)) {
  cat("Processing site:", site_name, "\n")
  
  site_data <- emr_data_clean %>%
    filter(Site %in% site_list[[site_name]])
  
  # Build monthly capture history
  capture_history <- site_data %>%
    group_by(`Detection ID`, month) %>%
    summarise(detected = 1, .groups = "drop") %>%
    pivot_wider(names_from = month, values_from = detected, values_fill = 0)
  
  cat("  Individuals:", nrow(capture_history), 
    "| Months:", ncol(capture_history) - 1, "\n")

  # Skip if too few detections
  if (nrow(capture_history) < 2 || ncol(capture_history) < 3) {
    cat("Skipping", site_name, "- insufficient data\n")
    next
  }

  # Format for Rcapture
  ch_matrix <- as.matrix(capture_history[,-1])
  rownames(ch_matrix) <- capture_history$`Detection ID`

  # Run closed population model
  model <- try(closedp(ch_matrix, dfreq = FALSE), silent = TRUE)

  if (inherits(model, "try-error")) {
    cat("Model failed for", site_name, "\n")
    next
  }

  # Extract best model
  results_df <- as.data.frame(model$results)
  results_df <- results_df[!is.na(results_df$AIC), ]
  if (nrow(results_df) == 0) next

  best_model <- results_df[which.min(results_df$AIC), , drop = FALSE]

  # Store result
  summary_list[[site_name]] <- data.frame(
    SiteID = site_name,
    Model = rownames(best_model),
    abundance = best_model$abundance,
    stderr = best_model$stderr,
    deviance = best_model$deviance,
    df = best_model$df,
    AIC = best_model$AIC,
    BIC = best_model$BIC,
    infoFit = best_model$infoFit
  )
}

# Step 5: Combine results
summary_df <- bind_rows(summary_list)

# Calculate Confidence intervals assumes normality
summary_df <- summary_df %>%
  mutate(
    CI_lower = abundance - 1.96 * stderr,
    CI_upper = abundance + 1.96 * stderr
  )

summary_df <- summary_df %>%
  mutate(across(c(abundance, stderr, CI_lower, CI_upper), \(x) round(x, 2)))

# Step 6: View or export
print(summary_df)
write.csv(summary_df, 
"C:/Users/Tanner/OneDrive - Michigan Technological University/PhD/EMR/EMR_Population_Estimates_By_Site.csv", 
row.names = FALSE)

# Plot data
library(ggplot2)
library(dplyr)

emr_summary <- data.frame(
  Site = c("Field 3", "South Shore", "Tree Graveyard", "Combined"),
  Model = c("Mth Chao (LB)", "Mt", "Mbh", "Mt"),
  Estimate = c(159.88, 115.47, 30.75, 193.26),
  SE = c(58.05, 40.12, 4.72, 52.15)
) %>%
  mutate(
    CI_lower = Estimate - 1.96 * SE,
    CI_upper = Estimate + 1.96 * SE
  )

ggplot(emr_summary, aes(x = Site, y = Estimate)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper), width = 0.2) +
  geom_text(aes(label = Model), vjust = -1.5, size = 3.5, color = "gray30") +
  labs(
    title = "Estimated EMR Population Size by Site",
    y = "Estimated Abundance (N̂)",
    x = "Site"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 25, hjust = 1),
    plot.title = element_text(face = "bold")
  )

library(tidyverse)
library(readxl)
library(hms)
library(dplyr)
library(lme4)
library(lubridate)
library(ggplot2)
library(RMark)
Sys.setenv(MARK_PATH = "C:/Program Files (x86)/MARK")
markpath <- Sys.getenv("MARK_PATH")
file.exists(file.path(markpath, "mark64.exe"))
# Copy or rename mark64.exe to mark.exe (safe workaround)
file.copy(
  from = file.path(markpath, "mark64.exe"),
  to = file.path(markpath, "mark.exe"),
  overwrite = TRUE
)
file.exists(file.path(markpath, "mark.exe"))

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
  filter(`Detection ID` != "No ID") %>%
  mutate(month = format(as.Date(Date), "%Y-%m"))

# Step 2: Build capture histories
all_months <- sort(unique(emr_data_clean$month))

capture_data <- emr_data_clean %>%
  group_by(`Detection ID`, month) %>%
  summarise(detected = 1, .groups = "drop") %>%
  pivot_wider(names_from = month, values_from = detected, values_fill = 0)

# Step 3: Create 'ch' and join site info
capture_data <- capture_data %>%
  unite("ch", all_of(all_months), sep = "") %>%
  left_join(emr_data_clean %>% select(`Detection ID`, Site), by = "Detection ID") %>%
  rename(group = Site)

# Step 4: Collapse by ch + group and count frequency
popan_input <- capture_data %>%
  group_by(ch, group) %>%
  summarise(freq = n(), .groups = "drop")

popan_input_single <- popan_input %>% filter(group == "Field 3")

# Step 5: Process and run model — NO freq.col needed!
mark_data <- process.data(popan_input_single, model = "POPAN")

ddl <- make.design.data(mark_data)

# Step 6: Fit model
model_parameters <- list(
  Phi = list(formula = ~1),
  p = list(formula = ~1),       # constant capture
  pent = list(formula = ~1),    # constant entry
  N = list(formula = ~1)
)


popan_model <- mark(mark_data, ddl, model.parameters = model_parameters, model = "POPAN")

# Step 7: View results
summary(popan_model)

head(popan_input)

###############################################################################################################
###### LOOP THROUGH ALL SITES ########################################################################
#####################################################################################################
# Define model parameters (constant-only)
model_parameters <- list(
  Phi = list(formula = ~1),
  p = list(formula = ~1),
  pent = list(formula = ~1),
  N = list(formula = ~1)
)

# Unique site list
site_list <- unique(capture_data$group)

# Initialize summary list
popan_results <- list()

# Updated loop to extract real$estimate using matching row names
for (site in site_list) {
  cat("Processing site:", site, "\n")
  
  site_input <- capture_data %>%
    filter(group == site) %>%
    group_by(ch, group) %>%
    summarise(freq = n(), .groups = "drop")
  
  if (nrow(site_input) == 0) {
    cat("  Skipping site:", site, "- no data\n")
    next
  }

  mark_data <- process.data(site_input, model = "POPAN")
  ddl <- make.design.data(mark_data)
  
  popan_model <- try(
    mark(mark_data, ddl,
         model.parameters = model_parameters,
         model = "POPAN", silent = TRUE),
    silent = TRUE
  )
  
  if (inherits(popan_model, "try-error")) {
    cat("  Model failed for site:", site, "\n")
    next
  }

  # Look for the row that starts with "N " in real estimates
  real_N <- popan_model$results$real
  beta_N <- popan_model$results$beta

  N_row <- grep("^N", rownames(real_N))
  N_beta_row <- grep("^N", rownames(beta_N))

  if (length(N_row) == 1 && length(N_beta_row) == 1) {
    N_est <- real_N$estimate[N_row]
    N_se <- beta_N[N_beta_row, "se"]
    
    CI_lower <- N_est - 1.96 * N_se
    CI_upper <- N_est + 1.96 * N_se
    
    popan_results[[site]] <- data.frame(
      Site = site,
      Estimate_N = round(N_est, 2),
      SE = round(N_se, 2),
      CI_Lower = round(CI_lower, 2),
      CI_Upper = round(CI_upper, 2),
      AICc = round(popan_model$results$AICc, 2)
    )
  } else {
    cat("  Skipping site:", site, "- could not find N estimate row\n")
  }
}

# Final output
popan_summary_df <- bind_rows(popan_results)
print(popan_summary_df)

# Optional: Export
write.csv(popan_summary_df, "C:/Users/Tanner/OneDrive - Michigan Technological University/PhD/EMR/POPAN_Estimates_AllSites.csv", row.names = FALSE)

# Historical analysis variant. See archive/README.md before running.
# ==========================================
# EXTENSION: IV PROBIT VIA CONTROL FUNCTION
# ==========================================
library(dplyr)
library(boot)
library(broom)
library(gt)

# Define the covariates exactly as they were in your replication script
base_covariates <- "AGEM + mom_age_at_birth + black + hispanic + other_race"

# Formulate the First Stage (Linear) and Second Stage (Probit) equations
# Note: As in Table 7, we control for boy1st and boy2nd
form_stage1 <- as.formula(paste("more_than_2 ~ same_sex + boy1st + boy2nd +", base_covariates))
form_stage2 <- as.formula(paste("worked_for_pay ~ more_than_2 + v_hat + boy1st + boy2nd +", base_covariates))

# ---------------------------------------------------------
# PHASE 1: MANUAL TWO-STEP ESTIMATION (For Diagnostics)
# ---------------------------------------------------------

# Step 1: Run the first stage (OLS) predicting 'more_than_2' using the instrument 'same_sex'
mod_first_stage <- lm(form_stage1, data = clean_data_80)

# Extract the residuals (v_hat) and add them to the dataframe
clean_data_80$v_hat <- residuals(mod_first_stage)

# Step 2: Run the second stage (Probit) including v_hat as a control variable
mod_second_stage <- glm(form_stage2, 
                        family = binomial(link = "probit"), 
                        data = clean_data_80)

### SAVING TABLE

# Convert the second stage Probit model summary to a dataframe
probit_summary_df <- tidy(mod_second_stage)

# Create and format the gt table
publication_table_iv_probit <- probit_summary_df %>%
  gt() %>%
  tab_header(
    title = md("**Second Stage: IV-Probit Results**"),
    subtitle = "Control Function Approach"
  ) %>%
  fmt_number(
    columns = c(estimate, std.error, statistic, p.value),
    decimals = 3
  ) %>%
  cols_label(
    term = "Variable",
    estimate = "Estimate",
    std.error = "Standard Error",
    statistic = "z-value",
    p.value = "p-value"
  ) %>%
  tab_style(
    style = list(cell_fill(color = "lightyellow"), cell_text(weight = "bold")),
    locations = cells_body(rows = term == "v_hat") # Highlights the endogeneity test
  ) %>%
  opt_row_striping()

# Display the table
publication_table_iv_probit

# Save the table as an image
# gtsave(publication_table_iv_probit, "archive/tables/iv_probit_coefficients.png")

# View the results. 
# CRITICAL CHECK: Look at the p-value for 'v_hat'. If it is significant, 
# this is evidence against exogeneity under the control-function assumptions.
summary(mod_second_stage)


# ---------------------------------------------------------
# PHASE 2: BOOTSTRAPPING PROPER STANDARD ERRORS
# ---------------------------------------------------------

# Define the custom bootstrap function
# This function takes the data and a vector of random indices, runs BOTH steps, 
# and returns the coefficients of interest.
cf_boot_function <- function(data, indices) {
  
  # Create the resampled dataset
  d <- data[indices, ]
  
  # Step 1 (on resampled data)
  boot_stage1 <- lm(form_stage1, data = d)
  d$v_hat <- residuals(boot_stage1)
  
  # Step 2 (on resampled data)
  boot_stage2 <- glm(form_stage2, 
                     family = binomial(link = "probit"), 
                     data = d)
  
  # We only really need to extract the coefficients for 'more_than_2' and 'v_hat'
  return(coef(boot_stage2)[c("more_than_2", "v_hat")])
}

# Run the bootstrap. 
# NOTE: R = 500 is used here for time. For your final submission, you might want 
# to bump this up to R = 1000 if your computer can handle the computation time.
set.seed(30413) # Reproducible bootstrap seed.
boot_results <- boot(data = clean_data_80, 
                     statistic = cf_boot_function, 
                     R = 500)

# Display the bootstrapped standard errors
print(boot_results)

# To calculate the 95% Confidence Intervals for the 'more_than_2' coefficient (Index 1):
boot.ci(boot_results, type = "perc", index = 2)




# ==========================================
# EXTENSION: IV PROBIT VIA CONTROL FUNCTION
# ==========================================

# ==============================================================================
# REPLICATION NOTE:
# Please run the cleaning script ("Cleaning.R") prior to 
# executing this file. This script relies on the cleaned data
# being present in your global environment.
# ==============================================================================

library(dplyr)
library(boot)
library(broom)
library(gt)
library(margins)
library(marginaleffects)
library(parallel)

# Define the covariates
base_covariates <- "AGEM + mom_age_at_birth + black + hispanic + other_race"

# Formulate the First Stage (Linear) and Second Stage (Probit) equations
form_stage1 <- as.formula(paste("more_than_2 ~ same_sex + boy1st + boy2nd +", base_covariates))
form_stage2 <- as.formula(paste("worked_for_pay ~ more_than_2 + v_hat + boy1st + boy2nd +", base_covariates))

# ==========================================
# 1. MANUAL TWO-STEP ESTIMATION & PROBIT TABLE
# ==========================================

# Step 1: Run the first stage (OLS) predicting 'more_than_2' using the instrument 'same_sex'
mod_first_stage <- lm(form_stage1, data = clean_data_80)

# Extract the residuals (v_hat) and add them to the dataframe
clean_data_80$v_hat <- residuals(mod_first_stage)

# Step 2: Run the second stage (Probit) including v_hat as a control variable
mod_second_stage <- glm(form_stage2, 
                        family = binomial(link = "probit"), 
                        data = clean_data_80)

# Create and save Probit Results Table
probit_summary_df <- tidy(mod_second_stage)

# Define variable labels
var_labels <- c(
  "more_than_2"      = "More than 2 children",
  "v_hat"            = "Residuals (v-hat)",
  "boy1st"           = "Boy 1st",
  "boy2nd"           = "Boy 2nd",
  "AGEM"             = "Age",
  "mom_age_at_birth" = "Age at first birth",
  "black"            = "Black",
  "hispanic"         = "Hispanic",
  "other_race"       = "Other race"
)


publication_table_iv_probit <- probit_summary_df %>%
  # Rename variables
  mutate(term = recode(term, !!!var_labels)) %>%
  gt() %>%
  tab_header(
    title = md("**TABLE 1 — SECOND STAGE: IV-PROBIT RESULTS**")
  ) %>%
  fmt_number(
    columns = c(estimate, std.error, statistic, p.value),
    decimals = 3
  ) %>%
  cols_label(
    term = "Variable",
    estimate = "Coefficient",
    std.error = "Std. Error",
    statistic = "z-value",
    p.value = "p-value"
  ) %>%
  # Highlight key variables
  tab_style(
    style = list(cell_text(weight = "bold")),
    locations = cells_body(rows = term == "More than 2 children")
  ) %>%
  tab_style(
    style = list(cell_fill(color = "lightyellow")),
    locations = cells_body(rows = term == "Residuals (v-hat)")
  ) %>%
  # Alignments
  cols_align(align = "left", columns = term) %>%
  cols_align(align = "center", columns = -term) %>%
  
  tab_options(
    table.border.top.color = "black",
    table.border.top.width = px(2),
    table.border.bottom.color = "black",
    table.border.bottom.width = px(2),
    table_body.border.bottom.width = px(0)
  ) %>%
  
  tab_source_note(
    source_note = md("Notes: Probit model estimated via control-function approach. Dependent variable: *Worked for pay*. Coefficients are Probit index coefficients, not marginal effects. *v_hat* tests for endogeneity within the control-function framework.")
  )

# Display the final table
publication_table_iv_probit

# ==========================================
# 2. BOOTSTRAPPING PROPER STANDARD ERRORS FOR AMEs
# ==========================================

# Define the custom bootstrap function for AMEs using marginaleffects
cf_boot_function_ame <- function(data, indices) {
  
  # Create the resampled dataset
  d <- data[indices, ]
  
  # Step 1: First stage on the resampled data
  boot_stage1 <- lm(form_stage1, data = d)
  d$v_hat <- residuals(boot_stage1)
  
  # Step 2: Second stage (Probit) on the resampled data
  boot_stage2 <- glm(form_stage2, 
                     family = binomial(link = "probit"), 
                     data = d)
  
  # Step 3: Compute the AMEs *inside* the loop using avg_slopes
  ames <- avg_slopes(boot_stage2, variables = c("more_than_2", "v_hat"))
  
  # Extract the estimate for more_than_2
  ame_more_than_2 <- ames$estimate[ames$term == "more_than_2"]
  
  # Extract the estimate for v_hat
  ame_v_hat <- ames$estimate[ames$term == "v_hat"]
  
  # The function returns a vector with the two marginal effects
  return(c(ame_more_than_2, ame_v_hat))
}

# Set up parallel processing for Mac
num_cores <- detectCores() - 1

# Set the seed for reproducibility
set.seed(30413) 

# Run the bootstrap using parallel processing
boot_results_ame <- boot(data = clean_data_80, 
                         statistic = cf_boot_function_ame, 
                         R = 500,
                         parallel = "multicore",
                         ncpus = num_cores)

# Display the results: t1* is the AME for more_than_2, t2* is the AME for v_hat
print(boot_results_ame)

# Calculate the 95% Confidence Intervals for the 'more_than_2' AME (Index 1)
boot.ci(boot_results_ame, type = "perc", index = 1)

# ==========================================
# 3. COMPUTE BASE AMES AND BUILD FINAL UPDATED TABLE
# ==========================================

# Compute baseline AMEs on the original sample using avg_slopes
ame_results <- avg_slopes(mod_second_stage)

# Create the initial dataframe mapping the output of avg_slopes
ame_df <- ame_results %>%
  as.data.frame() %>%
  rename(
    factor = term,
    AME = estimate,
    SE = std.error,
    z = statistic,
    p = p.value
  )

# Desired order for the table
var_order <- c(
  "more_than_2", "v_hat", "boy1st", "boy2nd", "AGEM", 
  "mom_age_at_birth", "black", "hispanic", "other_race"
)

# Filter and reorder
ame_df <- ame_df %>%
  filter(factor %in% var_order) %>%
  mutate(factor = factor(factor, levels = var_order)) %>%
  arrange(factor)

# Extract the bootstrapped Standard Errors from the boot object
boot_se_more_than_2 <- sd(boot_results_ame$t[, 1])
boot_se_v_hat       <- sd(boot_results_ame$t[, 2])

# Replace the old SEs with the bootstrapped ones and recalculate z/p-values
ame_df <- ame_df %>%
  mutate(
    SE = case_when(
      factor == "more_than_2" ~ boot_se_more_than_2,
      factor == "v_hat"       ~ boot_se_v_hat,
      TRUE ~ SE 
    ),
    z = case_when(
      factor %in% c("more_than_2", "v_hat") ~ AME / SE,
      TRUE ~ z
    ),
    p = case_when(
      factor %in% c("more_than_2", "v_hat") ~ 2 * (1 - pnorm(abs(z))),
      TRUE ~ p
    )
  )

# Build the final stylized table
ame_table_all <- ame_df %>%
  select(factor, AME, SE, z, p) %>%
  mutate(factor = recode(factor, !!!var_labels)) %>%
  gt() %>%
  tab_header(
    title = md("**TABLE 2 — AVERAGE MARGINAL EFFECTS: IV-PROBIT**")
  ) %>%
  fmt_number(
    columns = c(AME, SE, z, p),
    decimals = 3
  ) %>%
  cols_label(
    factor = "Variable",
    AME = "Average Marginal Effect",
    SE = "Std. Error",
    z = "z-value",
    p = "p-value"
  ) %>%
  
  cols_width(
    everything() ~ pct(20) 
  ) %>%

tab_style(
  style = list(cell_text(weight = "bold")),
  locations = cells_body(rows = factor == "More than 2 children")
) %>%
  tab_style(
    style = list(cell_fill(color = "lightyellow")),
    locations = cells_body(rows = factor == "Residuals (v-hat)")
  ) %>%
  cols_align(align = "left", columns = factor) %>%
  cols_align(align = "center", columns = -factor) %>%
  
  # Updated Table Options
  tab_options(
    table.border.top.color = "black",
    table.border.top.width = px(2),
    table.border.bottom.color = "black",
    table.border.bottom.width = px(2),
    table_body.border.bottom.width = px(0)
  ) %>%
  
  tab_source_note(
    source_note = md("Notes: Average marginal effects from IV-Probit control-function model. Dependent variable: *Worked for pay*.")
  )

# Display the final table
ame_table_all

# To save as png
# gtsave(publication_table_iv_probit, "Table_IV_Probit.png")
# gtsave(ame_table_all, "Table_AME_IV_Probit.png")

# ==========================================
# 4. ENVIRONMENT CLEANUP
# ==========================================

# Keep only the final datasets and the publication tables
rm(list = setdiff(ls(), c(
  "clean_data_80", "clean_data_90", 
  "married_data_80", "married_data_90", 
  "publication_table_iv_probit", "ame_table_all"
)))

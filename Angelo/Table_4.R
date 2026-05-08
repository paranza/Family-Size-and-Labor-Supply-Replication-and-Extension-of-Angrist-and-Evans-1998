# ==========================================
# TABLE 4
# ==========================================

# ==============================================================================
# REPLICATION NOTE:
# Please run the cleaning script ("Cleaning.R") prior to 
# executing this file. This script relies on the cleaned data
# being present in your global environment.
# ==============================================================================

# Load necessary libraries
library(dplyr)
library(purrr)
library(broom)
library(gt)
library(tidyr)

# ==========================================
# 1. SETUP VARIABLES
# ==========================================

# Define the exact list of demographic variables to iterate over
demographic_vars <- c(
  "AGEM", 
  "mom_age_at_birth", 
  "black", 
  "white", 
  "other_race", 
  "hispanic", 
  "years_of_education"
)

# ==========================================
# 2. HELPER FUNCTION TO CALCULATE DIFFERENCES
# ==========================================

# This function estimates the difference in means by regressing the 
# demographic variable (Y) on the instrument (X).
# The coefficient of the instrument represents the exact difference in means.
calc_diff_means <- function(y_var, instrument, data, use_weights = FALSE) {
  
  # Construct the formula dynamically: e.g., "AGEM ~ same_sex"
  form <- as.formula(paste(y_var, "~", instrument))
  
  # Run the OLS regression (applying weights for 1990 data if present)
  if(use_weights && "PWGTM1" %in% names(data)) {
    mod <- lm(form, data = data, weights = PWGTM1)
  } else {
    mod <- lm(form, data = data)
  }
  
  # Extract the coefficient and standard error for the instrument
  tidy_mod <- tidy(mod) %>% filter(term == instrument)
  
  # Return as a simple dataframe row
  data.frame(
    Estimate = tidy_mod$estimate,
    StdError = tidy_mod$std.error
  )
}

# ==========================================
# 3. CALCULATE THE THREE COLUMNS
# ==========================================

# Column 1: By Same sex (1980 PUMS) - Unweighted
col1_samesex_80 <- map_dfr(demographic_vars, ~{
  res <- calc_diff_means(.x, "same_sex", clean_data_80, use_weights = FALSE)
  res$Variable <- .x
  res
})

# Column 2: By Same sex (1990 PUMS) - Weighted with PWGTM1
col2_samesex_90 <- map_dfr(demographic_vars, ~{
  res <- calc_diff_means(.x, "same_sex", clean_data_90, use_weights = TRUE)
  res$Variable <- .x
  res
})

# Column 3: By Twins-2 (1980 PUMS) - Unweighted
col3_twins_80 <- map_dfr(demographic_vars, ~{
  res <- calc_diff_means(.x, "twins_2", clean_data_80, use_weights = FALSE)
  res$Variable <- .x
  res
})

# ==========================================
# 4. MERGE AND FORMAT RESULTS
# ==========================================

# Combine the three columns horizontally and format numbers to 4 decimals
combined_results <- data.frame(
  Raw_Var = demographic_vars,
  Col1 = sprintf("%.4f<br>(%.4f)", col1_samesex_80$Estimate, col1_samesex_80$StdError),
  Col2 = sprintf("%.4f<br>(%.4f)", col2_samesex_90$Estimate, col2_samesex_90$StdError),
  Col3 = sprintf("%.4f<br>(%.4f)", col3_twins_80$Estimate, col3_twins_80$StdError)
)

# Map internal variable names to publication-ready labels (italicized)
var_labels <- c(
  "*Age*", 
  "*Age at first birth*", 
  "*Black*", 
  "*White*", 
  "*Other race*", 
  "*Hispanic*", 
  "*Years of education*"
)

# Replace raw names with the publication labels
combined_results$Raw_Var <- var_labels

# ==========================================
# 5. GT TABLE CREATION
# ==========================================

publication_table_4 <- combined_results %>%
  gt() %>%
  # Add the main title
  tab_header(
    title = md("**Table 4 — DIFFERENCES IN MEANS FOR DEMOGRAPHIC VARIABLES BY *SAME SEX* AND *TWINS-2***")
  ) %>%
  # Rename columns to match the paper's specific sub-headers
  cols_label(
    Raw_Var = "Variable",
    Col1 = "1980 PUMS",
    Col2 = "1990 PUMS",
    Col3 = "1980 PUMS"
  ) %>%
  # Add the high-level grouping spanners
  tab_spanner(label = md("*By Same sex*"), columns = c("Col1", "Col2")) %>%
  tab_spanner(label = md("*By Twins-2*"), columns = c("Col3")) %>%
  tab_spanner(label = "Difference in means (standard error)", columns = c("Col1", "Col2", "Col3")) %>%
  # Enable Markdown parsing for italics and line breaks
  fmt_markdown(columns = everything()) %>%
  # Center align the numbers, left align the variable names
  cols_align(align = "center", columns = c("Col1", "Col2", "Col3")) %>%
  cols_align(align = "left", columns = "Raw_Var") %>%
  opt_row_striping() %>%
  # Style the table borders
  tab_options(
    table.border.top.color = "black",
    table.border.top.width = px(2),
    table.border.bottom.color = "black",
    table.border.bottom.width = px(2),
    table_body.border.bottom.width = px(0)
  ) %>%
  # Add the footnote
  tab_source_note(
    source_note = "Notes: The samples are the same as in Table 2. Standard errors are reported in parentheses."
  )

# Display the final table
publication_table_4

# To save as png
# gtsave(publication_table_4, "Table_4.png")

# ==========================================
# 6. ENVIRONMENT CLEANUP
# ==========================================
# Keep the environment clean by removing intermediate datasets and functions
rm(list = setdiff(ls(), c('clean_data_80', 'clean_data_90', 'married_data_80', 'married_data_90', 'publication_table_4')))

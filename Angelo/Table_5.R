# ==========================================
# TABLE 5
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
library(AER)      
library(sandwich) 
library(lmtest)   

# ==========================================
# 1. SETUP OUTCOME VARIABLES AND FORMATTING
# ==========================================

# Standardize weeks and hours variables within the datasets
clean_data_80_5 <- clean_data_80 %>%
  mutate(
    weeks_worked = WEEKSM,
    hours_week = HOURSM
  )

clean_data_90_5 <- clean_data_90 %>%
  mutate(
    weeks_worked = WEEK89M,
    hours_week = HOUR89M
  )

# List of outcome variables in the order they appear in Table 5
outcome_vars <- c(
  "more_than_2", 
  "real_kids", 
  "worked_for_pay", 
  "weeks_worked", 
  "hours_week", 
  "labor_income_95", 
  "ln_fam_inc"
)

# Formatted labels (in markdown) to display in the GT table
var_labels <- c(
  "*More than 2*<br>*children*", 
  "*Number of*<br>*children*", 
  "*Worked for pay*", 
  "*Weeks worked*", 
  "*Hours/week*", 
  "*Labor income*", 
  "*ln(Family*<br>*income)*"
)

# ==========================================
# 2. HELPER FUNCTIONS FOR CALCULATIONS
# ==========================================

# Function to format numbers
format_vals <- function(y_var, est, se, is_wald = FALSE) {
  if (is.na(est)) return("")
  
  # The two explanatory variables at the top (first two rows)
  if (y_var %in% c("more_than_2", "real_kids")) {
    return(sprintf("%.4f<br>(%.4f)", est, se))
  }
  # Worked for pay
  if (y_var == "worked_for_pay") {
    if (is_wald) return(sprintf("%.3f<br>(%.3f)", est, se))
    else return(sprintf("%.4f<br>(%.4f)", est, se))
  }
  # Weeks and Hours
  if (y_var %in% c("weeks_worked", "hours_week")) {
    if (is_wald) return(sprintf("%.2f<br>(%.2f)", est, se))
    else return(sprintf("%.4f<br>(%.4f)", est, se))
  }
  # Labor income (1 decimal)
  if (y_var == "labor_income_95") {
    return(sprintf("%.1f<br>(%.1f)", est, se))
  }
  # Ln Family income
  if (y_var == "ln_fam_inc") {
    if (is_wald) return(sprintf("%.3f<br>(%.3f)", est, se))
    else return(sprintf("%.4f<br>(%.4f)", est, se))
  }
  
  return(sprintf("%.4f<br>(%.4f)", est, se))
}

# Function to calculate Mean Difference and Wald Estimates (via IV) with robust SEs
calc_wald_block <- function(y_var, z_var, data, use_weights = FALSE) {
  
  # 1. Calculate Mean Difference (OLS Regression of Y on Instrument)
  form_ols <- as.formula(paste(y_var, "~", z_var))
  if (use_weights && "PWGTM1" %in% names(data)) {
    mod_ols <- lm(form_ols, data = data, weights = PWGTM1)
  } else {
    mod_ols <- lm(form_ols, data = data)
  }
  # Apply robust standard errors
  robust_ols <- coeftest(mod_ols, vcov = vcovHC(mod_ols, type = "HC1"))
  tidy_ols <- tidy(robust_ols) %>% filter(term == z_var)
  
  mean_diff <- format_vals(y_var, tidy_ols$estimate, tidy_ols$std.error, is_wald = FALSE)
  
  # If the row corresponds to the X variables, output an em-dash as in the paper
  if (y_var %in% c("more_than_2", "real_kids")) {
    wald_kids <- "—"
    wald_more2 <- "—"
  } else {
    # 2. Wald estimate using 'More than 2 children' as the endogenous covariate
    form_iv2 <- as.formula(paste(y_var, "~ more_than_2 |", z_var))
    if (use_weights && "PWGTM1" %in% names(data)) {
      mod_iv2 <- ivreg(form_iv2, data = data, weights = PWGTM1)
    } else {
      mod_iv2 <- ivreg(form_iv2, data = data)
    }
    # Apply robust standard errors
    robust_iv2 <- coeftest(mod_iv2, vcov = vcovHC(mod_iv2, type = "HC1"))
    tidy_iv2 <- tidy(robust_iv2) %>% filter(term == "more_than_2")
    wald_more2 <- format_vals(y_var, tidy_iv2$estimate, tidy_iv2$std.error, is_wald = TRUE)
    
    # 3. Wald estimate using 'Number of children' as the endogenous covariate
    form_iv1 <- as.formula(paste(y_var, "~ real_kids |", z_var))
    if (use_weights && "PWGTM1" %in% names(data)) {
      mod_iv1 <- ivreg(form_iv1, data = data, weights = PWGTM1)
    } else {
      mod_iv1 <- ivreg(form_iv1, data = data)
    }
    # Apply robust standard errors
    robust_iv1 <- coeftest(mod_iv1, vcov = vcovHC(mod_iv1, type = "HC1"))
    tidy_iv1 <- tidy(robust_iv1) %>% filter(term == "real_kids")
    wald_kids <- format_vals(y_var, tidy_iv1$estimate, tidy_iv1$std.error, is_wald = TRUE)
  }
  
  return(data.frame(Mean_Diff = mean_diff, Wald_More2 = wald_more2, Wald_Kids = wald_kids, stringsAsFactors = FALSE))
}

# ==========================================
# 3. CALCULATE THE THREE BLOCKS OF THE TABLE
# ==========================================

# Block 1: 1980 PUMS - Same Sex Instrument (Unweighted)
block_80_samesex <- map_dfr(outcome_vars, ~calc_wald_block(.x, "same_sex", clean_data_80_5, use_weights = FALSE))

# Block 2: 1990 PUMS - Same Sex Instrument (Weighted if PWGTM1 is present)
block_90_samesex <- map_dfr(outcome_vars, ~calc_wald_block(.x, "same_sex", clean_data_90_5, use_weights = TRUE))

# Block 3: 1980 PUMS - Twins-2 Instrument (Unweighted)
block_80_twins <- map_dfr(outcome_vars, ~calc_wald_block(.x, "twins_2", clean_data_80_5, use_weights = FALSE))

# Final dataset composition
combined_results <- data.frame(
  Variable = var_labels,
  
  MD_80_SS = block_80_samesex$Mean_Diff,
  WM_80_SS = block_80_samesex$Wald_More2,
  WK_80_SS = block_80_samesex$Wald_Kids,
  
  MD_90_SS = block_90_samesex$Mean_Diff,
  WM_90_SS = block_90_samesex$Wald_More2,
  WK_90_SS = block_90_samesex$Wald_Kids,
  
  MD_80_TW = block_80_twins$Mean_Diff,
  WM_80_TW = block_80_twins$Wald_More2,
  WK_80_TW = block_80_twins$Wald_Kids
)

# ==========================================
# 4. GT TABLE CREATION
# ==========================================

publication_table_5 <- combined_results %>%
  gt() %>%
  # Main title
  tab_header(
    title = md("**TABLE 5—WALD ESTIMATES OF LABOR-SUPPLY MODELS**")
  ) %>%
  # Column labels
  cols_label(
    Variable = "Variable",
    MD_80_SS = md("Mean<br>difference<br>by *Same*<br>*sex*"),
    WM_80_SS = md("More than<br>2 children"),
    WK_80_SS = md("Number<br>of<br>children"),
    
    MD_90_SS = md("Mean<br>difference<br>by *Same*<br>*sex*"),
    WM_90_SS = md("More than<br>2 children"),
    WK_90_SS = md("Number<br>of<br>children"),
    
    MD_80_TW = md("Mean<br>difference<br>by *Twins-2*"),
    WM_80_TW = md("More than<br>2 children"),
    WK_80_TW = md("Number<br>of<br>children")
  ) %>%
  
  # Internal spanners
  tab_spanner(label = "Wald estimate using as covariate:", columns = c("WM_80_SS", "WK_80_SS"), id = "wald1") %>%
  tab_spanner(label = "Wald estimate using as covariate:", columns = c("WM_90_SS", "WK_90_SS"), id = "wald2") %>%
  tab_spanner(label = "Wald estimate using as covariate:", columns = c("WM_80_TW", "WK_80_TW"), id = "wald3") %>%
  
  # External spanners (Years and Dataset type)
  tab_spanner(label = "1980 PUMS", columns = c("MD_80_SS", "WM_80_SS", "WK_80_SS")) %>%
  tab_spanner(label = "1990 PUMS", columns = c("MD_90_SS", "WM_90_SS", "WK_90_SS")) %>%
  tab_spanner(label = "1980 PUMS ", columns = c("MD_80_TW", "WM_80_TW", "WK_80_TW")) %>% # Space prevents ID conflict
  
  # Apply Markdown and Alignments
  fmt_markdown(columns = everything()) %>%
  cols_align(align = "center", columns = -Variable) %>%
  cols_align(align = "left", columns = "Variable") %>%
  
  # Add borders and remove the grey stripes
  tab_options(
    table.border.top.color = "black",
    table.border.top.width = px(2),
    table.border.bottom.color = "black",
    table.border.bottom.width = px(2),
    table_body.border.bottom.width = px(0)
  ) %>%
  tab_source_note(
    source_note = "Notes: The samples are the same as in Table 2. Standard errors are reported in parentheses."
  )

# Display the result
publication_table_5

# To save as png
# gtsave(publication_table_5, "Table_5.png")

# ==========================================
# 5. ENVIRONMENT CLEANUP
# ==========================================

# Keep only the final datasets and the publication table
rm(list = setdiff(ls(), c(
  "clean_data_80", "clean_data_90", 
  "married_data_80", "married_data_90", 
  "publication_table_5"
)))

# ==========================================
# TABLE 8
# ==========================================

# ==============================================================================
# REPLICATION NOTE:
# Please run the cleaning script ("Cleaning.R") prior to 
# executing this file. This script relies on the cleaned data
# being present in your global environment.
# ==============================================================================

library(dplyr)
library(purrr)
library(broom)
library(AER)
library(gt)
library(tidyr)
library(sandwich)
library(lmtest)

# ==========================================
# 1. SETUP MODEL EQUATIONS AND VARIABLES
# ==========================================

# Define dependent variables
dep_vars_women <- c("worked_for_pay", "WEEK89M", "HOUR89M", "labor_income_95", "ln_fam_inc", "ln_nonwife_inc")
dep_vars_men   <- c("worked_for_pay_dad", "WEEK89D", "HOUR89D", "labor_income_dad_95", "dummy_fam", "dummy_nonwife") 

# Define base covariates
base_covariates <- "AGEM + mom_age_at_birth + black + hispanic + other_race"

# Define the right-hand sides of the equations
# OLS: Include both boy1st and boy2nd as covariates
rhs_ols <- paste("more_than_2 + boy1st + boy2nd +", base_covariates)

# 2SLS (Same Sex instrument): Endogenous = more_than_2. Instrument = same_sex.
rhs_2sls_samesex <- paste("more_than_2 + boy1st + boy2nd +", base_covariates, 
                          "| same_sex + boy1st + boy2nd +", base_covariates)

# 2SLS (Two Boys/Girls instruments): EXCLUDE boy2nd from exogenous covariates
rhs_2sls_twoboys <- paste("more_than_2 + boy1st +", base_covariates,
                          "| two_boys + two_girls + boy1st +", base_covariates)

# ==========================================
# 2. HELPER FUNCTION TO RUN MODELS (WITH WEIGHTS)
# ==========================================

# This function runs all 3 models for a given dependent variable and extracts the 'more_than_2' coefficient
run_models_for_y <- function(y_var, data, is_husband = FALSE) {
  
  # Skip family/non-wife income rows for husband columns
  if (is_husband && y_var %in% c("dummy_fam", "dummy_nonwife")) {
    return(data.frame(
      Model = c("OLS", "2SLS_SameSex", "2SLS_TwoBoys"),
      Estimate = NA, StdError = NA, p_value_overid = NA
    ))
  }
  
  # Build formulas dynamically
  form_ols <- as.formula(paste(y_var, "~", rhs_ols))
  form_iv1 <- as.formula(paste(y_var, "~", rhs_2sls_samesex))
  form_iv2 <- as.formula(paste(y_var, "~", rhs_2sls_twoboys))
  
  # Run OLS with PWGTM1 weights and extract robust SEs
  mod_ols <- lm(form_ols, data = data, weights = PWGTM1)
  tidy_ols <- tidy(coeftest(mod_ols, vcov = vcovHC(mod_ols, type = "HC1"))) %>% filter(term == "more_than_2")
  
  # Run 2SLS (Same Sex) with PWGTM1 weights and extract robust SEs
  mod_iv1 <- ivreg(form_iv1, data = data, weights = PWGTM1)
  tidy_iv1 <- tidy(coeftest(mod_iv1, vcov = vcovHC(mod_iv1, type = "HC1"))) %>% filter(term == "more_than_2")
  
  # Run 2SLS (Two Boys/Two Girls) with PWGTM1 weights and extract robust SEs
  mod_iv2 <- ivreg(form_iv2, data = data, weights = PWGTM1)
  tidy_iv2 <- tidy(coeftest(mod_iv2, vcov = vcovHC(mod_iv2, type = "HC1"))) %>% filter(term == "more_than_2")
  
  # Extract Overidentification Test p-value with robust covariance (Hansen's J conceptually)
  diag_iv2 <- summary(mod_iv2, vcov = vcovHC(mod_iv2, type = "HC1"), diagnostics = TRUE)$diagnostics
  pval_overid <- diag_iv2["Sargan", "p-value"]
  
  # Return combined results
  data.frame(
    Model = c("OLS", "2SLS_SameSex", "2SLS_TwoBoys"),
    Estimate = c(tidy_ols$estimate, tidy_iv1$estimate, tidy_iv2$estimate),
    StdError = c(tidy_ols$std.error, tidy_iv1$std.error, tidy_iv2$std.error),
    p_value_overid = c(NA, NA, pval_overid)
  )
}

# ==========================================
# 3. EXECUTE MODELS OVER ALL VARIABLES
# ==========================================

# All Women Panel
results_all_w <- map_dfr(dep_vars_women, ~{
  res <- run_models_for_y(.x, clean_data_90)
  res$DepVar <- .x
  res$Sample <- "All_Women"
  res
})

# Married Women Panel
results_mar_w <- map_dfr(dep_vars_women, ~{
  res <- run_models_for_y(.x, married_data_90)
  res$DepVar <- .x
  res$Sample <- "Married_Women"
  res
})

# Husbands Panel
results_husb <- map_dfr(dep_vars_men, ~{
  res <- run_models_for_y(.x, married_data_90, is_husband = TRUE)
  res$DepVar <- .x
  res$Sample <- "Husbands"
  res
})

# Combine everything
all_results <- bind_rows(results_all_w, results_mar_w, results_husb)

# ==========================================
# 4. DATA RESTRUCTURING FOR GT
# ==========================================

formatted_results <- all_results %>%
  mutate(Col_ID = paste(Sample, Model, sep = "_")) %>%
  mutate(
    Formatted_Cell = case_when(
      is.na(Estimate) ~ NA_character_,
      # If income, display without decimals, otherwise use 3 decimals
      DepVar %in% c("labor_income_95", "labor_income_dad_95") & Model == "2SLS_TwoBoys" ~ 
        sprintf("%.0f<br>(%.0f)<br>[%.3f]", Estimate, StdError, p_value_overid),
      DepVar %in% c("labor_income_95", "labor_income_dad_95") ~ 
        sprintf("%.0f<br>(%.0f)", Estimate, StdError),
      Model == "2SLS_TwoBoys" ~ 
        sprintf("%.3f<br>(%.3f)<br>[%.3f]", Estimate, StdError, p_value_overid),
      TRUE ~ 
        sprintf("%.3f<br>(%.3f)", Estimate, StdError)
    )
  ) %>%
  select(DepVar, Col_ID, Formatted_Cell) %>%
  pivot_wider(names_from = Col_ID, values_from = Formatted_Cell)

# Map internal variable names to publication-ready labels WITH HTML INDENTATION
dep_var_mapping <- c(
  "&nbsp;&nbsp;&nbsp;*Worked for pay*", 
  "&nbsp;&nbsp;&nbsp;*Weeks worked*", 
  "&nbsp;&nbsp;&nbsp;*Hours/week*", 
  "&nbsp;&nbsp;&nbsp;*Labor income*", 
  "&nbsp;&nbsp;&nbsp;*ln(Family income)*", 
  "&nbsp;&nbsp;&nbsp;*ln(Non-wife income)*"
)

# Extract horizontal blocks
res_w_all <- formatted_results %>% select(starts_with("All_Women")) %>% slice(1:6)
res_w_mar <- formatted_results %>% select(starts_with("Married_Women")) %>% slice(1:6)
res_h     <- formatted_results %>% select(starts_with("Husbands")) %>% slice(7:12)

# Bind them horizontally into the final structure
final_wide_data <- cbind(
  Variable = dep_var_mapping,
  res_w_all,
  res_w_mar,
  res_h
)

# Clear out the 'All Women' columns for 'ln(Non-wife income)' 
final_wide_data <- final_wide_data %>%
  mutate(
    All_Women_OLS = ifelse(Variable == "&nbsp;&nbsp;&nbsp;*ln(Non-wife income)*", NA_character_, All_Women_OLS),
    All_Women_2SLS_SameSex = ifelse(Variable == "&nbsp;&nbsp;&nbsp;*ln(Non-wife income)*", NA_character_, All_Women_2SLS_SameSex),
    All_Women_2SLS_TwoBoys = ifelse(Variable == "&nbsp;&nbsp;&nbsp;*ln(Non-wife income)*", NA_character_, All_Women_2SLS_TwoBoys)
  )

# Create the custom top rows for Methods, Instruments, and the Header
top_rows <- data.frame(
  Variable = c("Estimation method", "Instrument for *More than 2 children*", "Dependent variable:"),
  All_Women_OLS = c("OLS", "—", ""),
  All_Women_2SLS_SameSex = c("2SLS", "*Same sex*", ""),
  All_Women_2SLS_TwoBoys = c("2SLS", "*Two boys*,<br>*Two girls*", ""),
  Married_Women_OLS = c("OLS", "—", ""),
  Married_Women_2SLS_SameSex = c("2SLS", "*Same sex*", ""),
  Married_Women_2SLS_TwoBoys = c("2SLS", "*Two boys*,<br>*Two girls*", ""),
  Husbands_OLS = c("OLS", "—", ""),
  Husbands_2SLS_SameSex = c("2SLS", "*Same sex*", ""),
  Husbands_2SLS_TwoBoys = c("2SLS", "*Two boys*,<br>*Two girls*", ""),
  stringsAsFactors = FALSE
)

# Stack the header rows on top of the statistical data
final_table_data <- bind_rows(top_rows, final_wide_data)

# ==========================================
# 5. GT TABLE FORMATTING
# ==========================================

publication_table_8 <- final_table_data %>%
  gt() %>%
  tab_header(
    title = md("**Table 8 — OLS AND 2SLS ESTIMATES OF LABOR-SUPPLY MODELS USING 1990 CENSUS DATA**")
  ) %>%
  # Add exact column numbers
  cols_label(
    All_Women_OLS = "(1)",
    All_Women_2SLS_SameSex = "(2)",
    All_Women_2SLS_TwoBoys = "(3)",
    Married_Women_OLS = "(4)",
    Married_Women_2SLS_SameSex = "(5)",
    Married_Women_2SLS_TwoBoys = "(6)",
    Husbands_OLS = "(7)",
    Husbands_2SLS_SameSex = "(8)",
    Husbands_2SLS_TwoBoys = "(9)"
  ) %>%
  # Add the high-level spanners
  tab_spanner(label = "All women", columns = c("All_Women_OLS", "All_Women_2SLS_SameSex", "All_Women_2SLS_TwoBoys")) %>%
  tab_spanner(label = "Married women", columns = c("Married_Women_OLS", "Married_Women_2SLS_SameSex", "Married_Women_2SLS_TwoBoys")) %>%
  tab_spanner(label = "Husbands of married women", columns = c("Husbands_OLS", "Husbands_2SLS_SameSex", "Husbands_2SLS_TwoBoys")) %>%
  # Parse HTML tags for line breaks, italics, and non-breaking spaces
  fmt_markdown(columns = everything()) %>%
  # Centering and missing values
  cols_align(align = "center", columns = -Variable) %>%
  cols_align(align = "left", columns = Variable) %>%
  sub_missing(missing_text = "—") %>%
  # Add a thin border under the second row to separate headers from data
  tab_style(
    style = cell_borders(sides = "bottom", color = "black", weight = px(1)),
    locations = cells_body(rows = 2)
  ) %>%
  # Set thick borders for the whole table
  tab_options(
    table.border.top.color = "black",
    table.border.top.width = px(2),
    table.border.bottom.color = "black",
    table.border.bottom.width = px(2),
    table_body.border.bottom.width = px(0) 
  ) %>%
  
  tab_source_note(
    source_note = md("Notes: The table reports the coefficient on the *More than 2 children* variable in equations (4) and (6) in the text estimated with 1990 Census data. Other covariates in the models are *Age*, *Age at first birth*, plus indicators for *Boy 1st*, *Boy 2nd*, *Black*, *Hispanic*, and *Other race*. The variable *Boy 2nd* is excluded from equation (6). The *p*-value for the test of overidentifying restrictions associated with equation (6) is shown in brackets. Standard errors are reported in parentheses.")
  )

# View the table
publication_table_8

# To save as png
# gtsave(publication_table_8, "Table_8.png")

# ==========================================
# 6. ENVIRONMENT CLEANUP
# ==========================================

# Keep only the final datasets and the publication table
rm(list = setdiff(ls(), c(
  "clean_data_80", "clean_data_90", 
  "married_data_80", "married_data_90", 
  "publication_table_8"
)))



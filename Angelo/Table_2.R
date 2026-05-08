# ==========================================
# TABLE 2
# ==========================================

# ==============================================================================
# REPLICATION NOTE:
# Please run the cleaning script ("Cleaning.R") prior to 
# executing this file. This script relies on the cleaned data
# being present in your global environment.
# ==============================================================================

library(dplyr)
library(haven)
library(tidyr)
library(gt)

# ==========================================
# PART 1: UNIQUE FUNCTION TO PREPARE DATA
# ==========================================

prepare_table2_vars <- function(df, year) {
  if (year == 1980) {
    df %>% mutate(
      across(any_of(c("WEEKSM", "HOURSM", "INCOME1M", "INCOME2M", "FAMINC", 
                      "WEEKSD", "HOURSD", "INCOME1D", "INCOME2D", "AGEQ3RD", "AGED")), as.numeric),
      age_first_birth_dad = as.numeric(AGED) - as.numeric(AGEK)
    )
  } else {
    df %>% mutate(
      across(any_of(c("WEEK89M", "HOUR89M", "INCOMEM1", "INCOMEM2", "FAMINC", "MARITAL", 
                      "AGED", "WEEK89D", "HOUR89D", "INCOMED1", "INCOMED2", "MULTI2ND")), as.numeric),
      age_first_birth_dad = as.numeric(AGED) - as.numeric(AGEK)
    )
  }
}

# ==========================================
# APPLY FUNCTION TO THE DATASETS
# ==========================================

table2_all_80  <- prepare_table2_vars(clean_data_80, 1980)
table2_marr_80 <- prepare_table2_vars(married_data_80, 1980)

table2_all_90  <- prepare_table2_vars(clean_data_90, 1990)
table2_marr_90 <- prepare_table2_vars(married_data_90, 1990)


# ==========================================
# SUMMARIZE FUNCTIONS (1980)
# ==========================================
summarize_women_80 <- function(df, is_all_women = FALSE) {
  df %>%
    summarise(
      `Children ever born` = sprintf("%.2f (%.2f)", mean(real_kids, na.rm=T), sd(real_kids, na.rm=T)),
      `More than 2 children` = sprintf("%.3f (%.3f)", mean(more_than_2, na.rm=T), sd(more_than_2, na.rm=T)),
      `Boy 1st` = sprintf("%.3f (%.3f)", mean(boy1st, na.rm=T), sd(boy1st, na.rm=T)),
      `Boy 2nd` = sprintf("%.3f (%.3f)", mean(boy2nd, na.rm=T), sd(boy2nd, na.rm=T)),
      `Two boys` = sprintf("%.3f (%.3f)", mean(two_boys, na.rm=T), sd(two_boys, na.rm=T)),
      `Two girls` = sprintf("%.3f (%.3f)", mean(two_girls, na.rm=T), sd(two_girls, na.rm=T)),
      `Same sex` = sprintf("%.3f (%.3f)", mean(same_sex, na.rm=T), sd(same_sex, na.rm=T)),
      `Twins-2` = sprintf("%.4f (%.4f)", mean(twins_2, na.rm=T), sd(twins_2, na.rm=T)),
      `Age` = sprintf("%.1f (%.1f)", mean(AGEM, na.rm=T), sd(AGEM, na.rm=T)),
      `Age at first birth` = sprintf("%.1f (%.1f)", mean(mom_age_at_birth, na.rm=T), sd(mom_age_at_birth, na.rm=T)),
      `Worked for pay` = sprintf("%.3f (%.3f)", mean(worked_for_pay, na.rm=T), sd(worked_for_pay, na.rm=T)),
      `Weeks worked` = sprintf("%.1f (%.1f)", mean(WEEKSM, na.rm=T), sd(WEEKSM, na.rm=T)),
      `Hours/week` = sprintf("%.1f (%.1f)", mean(HOURSM, na.rm=T), sd(HOURSM, na.rm=T)),
      `Labor income ($1995)` = sprintf("%.0f (%.0f)", mean(labor_income_95, na.rm=T), sd(labor_income_95, na.rm=T)),
      `Family income ($1995)` = sprintf("%.0f (%.0f)", mean(family_income_95, na.rm=T), sd(family_income_95, na.rm=T)),
      `Non-wife income ($1995)` = if(is_all_women) "" else sprintf("%.0f (%.0f)", mean(non_wife_income_95, na.rm=T), sd(non_wife_income_95, na.rm=T))
    ) %>%
    pivot_longer(cols = everything(), names_to = "Variable", values_to = "Mean (SD)")
}

summarize_husbands_80 <- function(df) {
  df %>%
    summarise(
      `Children ever born` = "",
      `More than 2 children` = "",
      `Boy 1st` = "",
      `Boy 2nd` = "",
      `Two boys` = "",
      `Two girls` = "",
      `Same sex` = "",
      `Twins-2` = "",
      `Age` = sprintf("%.1f (%.1f)", mean(AGED, na.rm=T), sd(AGED, na.rm=T)),
      `Age at first birth` = sprintf("%.1f (%.1f)", mean(age_first_birth_dad, na.rm=T), sd(age_first_birth_dad, na.rm=T)),
      `Worked for pay` = sprintf("%.3f (%.3f)", mean(worked_for_pay_dad, na.rm=T), sd(worked_for_pay_dad, na.rm=T)),
      `Weeks worked` = sprintf("%.1f (%.1f)", mean(WEEKSD, na.rm=T), sd(WEEKSD, na.rm=T)),
      `Hours/week` = sprintf("%.1f (%.1f)", mean(HOURSD, na.rm=T), sd(HOURSD, na.rm=T)),
      `Labor income ($1995)` = sprintf("%.0f (%.0f)", mean(labor_income_dad_95, na.rm=T), sd(labor_income_dad_95, na.rm=T)),
      `Family income ($1995)` = "",
      `Non-wife income ($1995)` = ""
    ) %>%
    pivot_longer(cols = everything(), names_to = "Variable", values_to = "Mean (SD)")
}

# ==========================================
# SUMMARIZE FUNCTIONS (1990)
# ==========================================
summarize_women_90 <- function(df, is_all_women = FALSE) {
  df %>%
    summarise(
      `Children ever born` = sprintf("%.2f (%.2f)", mean(real_kids, na.rm=T), sd(real_kids, na.rm=T)),
      `More than 2 children` = sprintf("%.3f (%.3f)", mean(more_than_2, na.rm=T), sd(more_than_2, na.rm=T)),
      `Boy 1st` = sprintf("%.3f (%.3f)", mean(boy1st, na.rm=T), sd(boy1st, na.rm=T)),
      `Boy 2nd` = sprintf("%.3f (%.3f)", mean(boy2nd, na.rm=T), sd(boy2nd, na.rm=T)),
      `Two boys` = sprintf("%.3f (%.3f)", mean(two_boys, na.rm=T), sd(two_boys, na.rm=T)),
      `Two girls` = sprintf("%.3f (%.3f)", mean(two_girls, na.rm=T), sd(two_girls, na.rm=T)),
      `Same sex` = sprintf("%.3f (%.3f)", mean(same_sex, na.rm=T), sd(same_sex, na.rm=T)),
      `Twins-2` = sprintf("%.4f (%.4f)", mean(twins_2, na.rm=T), sd(twins_2, na.rm=T)),
      `Age` = sprintf("%.1f (%.1f)", mean(AGEM, na.rm=T), sd(AGEM, na.rm=T)),
      `Age at first birth` = sprintf("%.1f (%.1f)", mean(mom_age_at_birth, na.rm=T), sd(mom_age_at_birth, na.rm=T)),
      `Worked for pay` = sprintf("%.3f (%.3f)", mean(worked_for_pay, na.rm=T), sd(worked_for_pay, na.rm=T)),
      `Weeks worked` = sprintf("%.1f (%.1f)", mean(WEEK89M, na.rm=T), sd(WEEK89M, na.rm=T)),
      `Hours/week` = sprintf("%.1f (%.1f)", mean(HOUR89M, na.rm=T), sd(HOUR89M, na.rm=T)),
      `Labor income ($1995)` = sprintf("%.0f (%.0f)", mean(labor_income_95, na.rm=T), sd(labor_income_95, na.rm=T)),
      `Family income ($1995)` = sprintf("%.0f (%.0f)", mean(family_income_95, na.rm=T), sd(family_income_95, na.rm=T)),
      `Non-wife income ($1995)` = if(is_all_women) "" else sprintf("%.0f (%.0f)", mean(non_wife_income_95, na.rm=T), sd(non_wife_income_95, na.rm=T))
    ) %>%
    pivot_longer(cols = everything(), names_to = "Variable", values_to = "Mean (SD)")
}

summarize_husbands_90 <- function(df) {
  df %>%
    summarise(
      `Children ever born` = "",
      `More than 2 children` = "",
      `Boy 1st` = "",
      `Boy 2nd` = "",
      `Two boys` = "",
      `Two girls` = "",
      `Same sex` = "",
      `Twins-2` = "",
      `Age` = sprintf("%.1f (%.1f)", mean(AGED, na.rm=T), sd(AGED, na.rm=T)),
      `Age at first birth` = sprintf("%.1f (%.1f)", mean(age_first_birth_dad, na.rm=T), sd(age_first_birth_dad, na.rm=T)),
      `Worked for pay` = sprintf("%.3f (%.3f)", mean(worked_for_pay_dad, na.rm=T), sd(worked_for_pay_dad, na.rm=T)),
      `Weeks worked` = sprintf("%.1f (%.1f)", mean(WEEK89D, na.rm=T), sd(WEEK89D, na.rm=T)),
      `Hours/week` = sprintf("%.1f (%.1f)", mean(HOUR89D, na.rm=T), sd(HOUR89D, na.rm=T)),
      `Labor income ($1995)` = sprintf("%.0f (%.0f)", mean(labor_income_dad_95, na.rm=T), sd(labor_income_dad_95, na.rm=T)),
      `Family income ($1995)` = "",
      `Non-wife income ($1995)` = ""
    ) %>%
    pivot_longer(cols = everything(), names_to = "Variable", values_to = "Mean (SD)")
}



# ==========================================
# COLUMN CREATION (1980)
# ==========================================
col_all_women_80 <- summarize_women_80(table2_all_80, is_all_women = TRUE) %>% 
  rename(`All Women_1980` = `Mean (SD)`)

col_married_wives_80 <- summarize_women_80(table2_marr_80, is_all_women = FALSE) %>% 
  rename(`Married Wives_1980` = `Mean (SD)`)

col_husbands_80 <- summarize_husbands_80(table2_marr_80) %>% 
  rename(`Husbands_1980` = `Mean (SD)`)

final_table_80 <- col_all_women_80 %>%
  left_join(col_married_wives_80, by = "Variable") %>%
  left_join(col_husbands_80, by = "Variable")

# ==========================================
# COLUMN CREATION (1990)
# ==========================================
col_all_women_90 <- summarize_women_90(table2_all_90, is_all_women = TRUE) %>% 
  rename(`All Women_1990` = `Mean (SD)`)

col_married_wives_90 <- summarize_women_90(table2_marr_90, is_all_women = FALSE) %>% 
  rename(`Married Wives_1990` = `Mean (SD)`)

col_husbands_90 <- summarize_husbands_90(table2_marr_90) %>% 
  rename(`Husbands_1990` = `Mean (SD)`)

final_table_90 <- col_all_women_90 %>%
  left_join(col_married_wives_90, by = "Variable") %>%
  left_join(col_husbands_90, by = "Variable")

# ==========================================
# PART 2: FINAL MERGE AND EXPORT (TABLE 2)
# ==========================================

# Merge the clean 1980 and 1990 tables
merged_table_2 <- final_table_80 %>%
  left_join(final_table_90, by = "Variable")

# Create the final, publication-ready Table 2 using gt
publication_table_2 <- merged_table_2 %>%
  gt() %>%
  tab_header(
    title = md("**Table 2: Demographic and Labor Supply Characteristics**"),
    subtitle = "Replication of Angrist & Evans (1998) - 1980 and 1990 Census Data"
  ) %>%
  tab_spanner(
    label = md("**1980 Census**"),
    columns = c(`All Women_1980`, `Married Wives_1980`, `Husbands_1980`)
  ) %>%
  tab_spanner(
    label = md("**1990 Census**"),
    columns = c(`All Women_1990`, `Married Wives_1990`, `Husbands_1990`)
  ) %>%
  cols_label(
    `All Women_1980` = "All Women",
    `Married Wives_1980` = "Married Wives",
    `Husbands_1980` = "Husbands",
    `All Women_1990` = "All Women",
    `Married Wives_1990` = "Married Wives",
    `Husbands_1990` = "Husbands"
  ) %>%
  cols_align(align = "center", columns = -Variable) %>%
  cols_align(align = "left", columns = Variable) %>%
  opt_row_striping() %>%
  tab_source_note(
    source_note = "Notes: The samples include women aged 21-35 with two or more children except for women whose second child is less than a year old. In the 1980 PUMS, the married women sample refers to women who were married at the time of their first birth, married at the time of the survey, and married once. In the 1990 PUMS, the married women are those married at the time of the Census."
  )

# Display the final table
publication_table_2

# To save as png
# gtsave(publication_table_2, "Table_2.png")

# ==========================================
# PART 3: ENVIRONMENT CLEANUP
# ==========================================
# Keep the environment clean by removing intermediate datasets and functions
rm(list = setdiff(ls(), c('clean_data_80', 'clean_data_90', 'married_data_80', 'married_data_90', 'publication_table_2')))



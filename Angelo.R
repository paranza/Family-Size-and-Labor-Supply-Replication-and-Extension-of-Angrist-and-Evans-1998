# ANGELO 
library(dplyr)
library(haven)
library(tidyr)
library(gt)

data_80 = read_sas("dati/m_d_806.sas7bdat") # 1980
data_90 = read_sas("dati/m_d_903.sas7bdat") # 1990


##### CLEAN data_80

clean_data_80 <- data_80 %>%
  
  # 1. Convert math/count variables to numeric to avoid character comparison errors
  mutate(across(c(AGEM, FERT, KIDCOUNT, AGEK, AGEQ2ND, TWIN1ST), as.numeric)) %>%
  
  # 2. Decode the FERT variable (in Census/IPUMS data, FERT code is offset by 1)
  mutate(real_kids = FERT - 1) %>%
  
  # 3. Apply the Angrist & Evans sample selection filters
  filter(
    # Maternal age must be between 21 and 35
    AGEM >= 21 & AGEM <= 35,
    
    # Mother must have at least 2 children
    real_kids >= 2,
    
    # Children currently living in the household must match children ever born
    real_kids == KIDCOUNT,
    
    # Oldest child must be strictly younger than 18
    !is.na(AGEK) & AGEK < 18,
    
    # Second child must be at least 1 year old (4 quarters)
    !is.na(AGEQ2ND) & AGEQ2ND >= 4,
    
    # Exclude multiple births (twins/triplets) at first pregnancy
    TWIN1ST == 0,
    
    # Exclude census-allocated (imputed) data for age, sex, and quarter of birth
    AAGE == "0", 
    ASEX == "0",
    AAGE2ND == "0", 
    ASEX2ND == "0",
    AQTRBRTH == "0", 
    AQTR2ND == "0"
  ) %>%
  
  # 4. Create the final variables needed for the IV / 2SLS regressions
  mutate(
    # Endogenous variable (x): 1 if more than 2 children, 0 otherwise
    more_than_2 = ifelse(real_kids > 2, 1, 0),
    
    # Sex dummies: 1 = Boy, 0 = Girl (Assuming "0" is the raw string code for male)
    boy1st = ifelse(SEXK == "0", 1, 0),
    boy2nd = ifelse(SEX2ND == "0", 1, 0),
    
    # Instrumental variable (z): 1 if the first two children are the same sex
    same_sex = ifelse(boy1st == boy2nd, 1, 0)
  )

##### CLEAN data_90

# Using data_90 with the exact column names from your screenshot
clean_data_90 <- data_90 %>%
  
  # 1. Convert math/count variables to numeric
  mutate(across(c(AGEM, FERTIL, KIDCOUNT, AGEK, AGE2NDK, TWIN1ST), as.numeric)) %>%
  
  # 2. Decode the FERTIL variable (Assuming standard IPUMS +1 offset)
  mutate(real_kids = FERTIL - 1) %>%
  
  # 3. Apply the Angrist & Evans sample selection filters (1990 rules)
  filter(
    # Maternal age must be between 21 and 35
    AGEM >= 21 & AGEM <= 35,
    
    # Mother must have at least 2 children
    real_kids >= 2,
    
    # Children currently living in the household must match children ever born
    real_kids == KIDCOUNT,
    
    # Oldest child must be strictly younger than 18
    !is.na(AGEK) & AGEK < 18,
    
    # Second child must be at least 1 year old
    !is.na(AGE2NDK) & AGE2NDK >= 1,
    
    # Exclude multiple births (twins/triplets) at first pregnancy
    TWIN1ST == 0
    
  ) %>%
  
  # 4. Create the final variables needed for the IV / 2SLS regressions
  mutate(
    # Endogenous variable (x): 1 if more than 2 children, 0 otherwise
    more_than_2 = ifelse(real_kids > 2, 1, 0),
    
    # Sex dummies: 1 = Boy, 0 = Girl
    boy1st = ifelse(SEXK == "0", 1, 0),
    boy2nd = ifelse(SEX2NDK == "0", 1, 0),
    
    # Instrumental variable (z): 1 if the first two children are the same sex
    same_sex = ifelse(boy1st == boy2nd, 1, 0)
  )

######

# ==========================================
# PART 1: 1980 CENSUS DATA
# ==========================================

# 1. PROCESSING OF ALL VARIABLES (1980)
table2_data_80 <- clean_data_80 %>%
  mutate(
    # Convert the variables of the father and the 3rd child to numeric
    across(c(WEEKSM, HOURSM, INCOME1M, FAMINC, TIMESMAR, AGEMAR, MARITAL, 
             AGED, WEEKSD, HOURSD, INCOME1D, AGEQ3RD), as.numeric),
    
    # Sex and Twins Variables
    two_boys = ifelse(boy1st == 1 & boy2nd == 1, 1, 0),
    two_girls = ifelse(boy1st == 0 & boy2nd == 0, 1, 0),
    twins_2 = ifelse(!is.na(AGEQ2ND) & !is.na(AGEQ3RD) & AGEQ2ND == AGEQ3RD, 1, 0),
    
    # Mothers Variables
    age_first_birth = AGEM - AGEK,
    worked_for_pay = ifelse(WEEKSM > 0, 1, 0),
    labor_income_95 = INCOME1M * 2.27,
    
    # Fathers Variables
    age_first_birth_dad = AGED - AGEK,
    worked_for_pay_dad = ifelse(WEEKSD > 0, 1, 0),
    labor_income_dad_95 = INCOME1D * 2.27,
    
    # Family Incomes
    family_income_95 = FAMINC * 2.27,
    non_wife_income_95 = (FAMINC - INCOME1M) * 2.27
  )

# 2. SAMPLE OF MARRIED WOMEN AND THEIR HUSBANDS (1980)
married_data_80 <- table2_data_80 %>%
  filter(
    MARITAL <= 2,
    TIMESMAR == 1,
    AGEMAR <= age_first_birth
  )

# 3. FUNCTIONS FOR EXACT FORMATTING (1980)
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
      `Age at first birth` = sprintf("%.1f (%.1f)", mean(age_first_birth, na.rm=T), sd(age_first_birth, na.rm=T)),
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

# 4. MERGE COLUMNS (1980)
col_all_women_80 <- summarize_women_80(table2_data_80, is_all_women = TRUE) %>% 
  rename(`All Women_1980` = `Mean (SD)`)

col_married_wives_80 <- summarize_women_80(married_data_80, is_all_women = FALSE) %>% 
  rename(`Married Wives_1980` = `Mean (SD)`)

col_husbands_80 <- summarize_husbands_80(married_data_80) %>% 
  rename(`Husbands_1980` = `Mean (SD)`)

final_table_80 <- col_all_women_80 %>%
  left_join(col_married_wives_80, by = "Variable") %>%
  left_join(col_husbands_80, by = "Variable")


# ==========================================
# PART 2: 1990 CENSUS DATA
# ==========================================

# 1. PROCESSING OF ALL VARIABLES (1990)
table2_data_90 <- clean_data_90 %>%
  mutate(
    across(c(WEEK89M, HOUR89M, INCOMEM1, FAMINC, MARITAL, 
             AGED, WEEK89D, HOUR89D, INCOMED1, MULTI2ND), as.numeric),
    
    two_boys = ifelse(boy1st == 1 & boy2nd == 1, 1, 0),
    two_girls = ifelse(boy1st == 0 & boy2nd == 0, 1, 0),
    twins_2 = ifelse(MULTI2ND == 1, 1, 0),
    
    age_first_birth = AGEM - AGEK,
    worked_for_pay = ifelse(WEEK89M > 0, 1, 0),
    labor_income_95 = INCOMEM1 * 1.22,
    
    age_first_birth_dad = AGED - AGEK,
    worked_for_pay_dad = ifelse(WEEK89D > 0, 1, 0),
    labor_income_dad_95 = INCOMED1 * 1.22,
    
    family_income_95 = FAMINC * 1.22,
    non_wife_income_95 = (FAMINC - INCOMEM1) * 1.22
  )

# 2. SAMPLE OF MARRIED WOMEN AND THEIR HUSBANDS (1990)
married_data_90 <- table2_data_90 %>%
  filter(
    MARITAL <= 2
  )

# 3. FUNCTIONS FOR EXACT FORMATTING (1990)
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
      `Age at first birth` = sprintf("%.1f (%.1f)", mean(age_first_birth, na.rm=T), sd(age_first_birth, na.rm=T)),
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

# 4. MERGE COLUMNS (1990)
col_all_women_90 <- summarize_women_90(table2_data_90, is_all_women = TRUE) %>% 
  rename(`All Women_1990` = `Mean (SD)`)

col_married_wives_90 <- summarize_women_90(married_data_90, is_all_women = FALSE) %>% 
  rename(`Married Wives_1990` = `Mean (SD)`)

col_husbands_90 <- summarize_husbands_90(married_data_90) %>% 
  rename(`Husbands_1990` = `Mean (SD)`)

final_table_90 <- col_all_women_90 %>%
  left_join(col_married_wives_90, by = "Variable") %>%
  left_join(col_husbands_90, by = "Variable")


# ==========================================
# PART 3: FINAL MERGE AND EXPORT (TABLE 2)
# ==========================================
library(gt)

# Merge the clean 1980 and 1990 tables
merged_table_2 <- final_table_80 %>%
  left_join(final_table_90, by = "Variable")

# Create the final, publication-ready Table 2 using gt
publication_table <- merged_table_2 %>%
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
  opt_row_striping()

# Display the final table
publication_table

gtsave(publication_table, "Table_2.png")

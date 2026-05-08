# ==========================================
# CLEANING SCRIPT
# ==========================================

library(dplyr)
library(haven)
library(tidyr)
library(gt)

data_80 = read_sas("dati/m_d_806.sas7bdat") # 1980
data_90 = read_sas("dati/m_d_903.sas7bdat") # 1990


##### CLEAN data_80

clean_data_80 <- data_80 %>%
  
  # 1. Convert math/count variables to numeric to avoid character comparison errors
  mutate(across(c(AGEM, FERT, KIDCOUNT, AGEK, AGEQ2ND, AGEQ3RD, TWIN1ST, RACEM, SPANISHM, INCOME1M, INCOME2M, 
                  FAMINC, INCOME1D, INCOME2D, WEEKSM, HOURSM, WEEKSD, HOURSD, GRADEM), as.numeric)) %>%
  
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
    same_sex = ifelse(boy1st == boy2nd, 1, 0),
    
    # Sex mix instruments
    two_boys  = ifelse(boy1st == 1 & boy2nd == 1, 1, 0),
    two_girls = ifelse(boy1st == 0 & boy2nd == 0, 1, 0),
    mom_age_at_birth = AGEM - AGEK,
    
    # Create Twins at second birth instrument (1 if 2nd and 3rd child have same age/quarter)
    twins_2 = ifelse(!is.na(AGEQ2ND) & !is.na(AGEQ3RD) & AGEQ2ND == AGEQ3RD, 1, 0),
    
    # Standardize education variable name
    years_of_education = as.numeric(GRADEM),
    
    # Demographic dummies
    white = ifelse(RACEM == 1, 1, 0),
    black      = ifelse(RACEM == 2, 1, 0),
    hispanic   = ifelse(SPANISHM > 0, 1, 0),
    other_race = ifelse(RACEM > 2, 1, 0),
    
    # Labor variables (Deflated to 1995 dollars)
    worked_for_pay = ifelse(WEEKSM > 0, 1, 0),
    labor_income_95 = (INCOME1M + INCOME2M) * 2.27,
    family_income_95 = FAMINC * 2.27,
    non_wife_income_95 = (FAMINC - INCOME1M - INCOME2M) * 2.27,
    
    # Husband variables
    worked_for_pay_dad = ifelse(WEEKSD > 0, 1, 0),
    labor_income_dad_95 = (INCOME1D + INCOME2D) * 2.27,
    
    # Log Income variables
    ln_fam_inc = log(ifelse(family_income_95 <= 0, 1, family_income_95)),
    ln_nonwife_inc = log(ifelse(non_wife_income_95 <= 0, 1, non_wife_income_95))
  )

##### CREATE married_data_80

married_data_80 <- clean_data_80 %>%
  filter(
    MARITAL == "0",    # Spouse present
    TIMESMAR == "1",   # Mom married only once
    TIMEMARD == "1",   # Dad married only once
    AGED != "",        # Dad must be physically present in household
    AGEMAR != "",      # Marriage age data must exist
    AGEMARD != ""      # Dad marriage age data must exist
  ) %>%
  mutate(
    # Ensure numeric types for logical comparisons
    AGEM    = as.numeric(AGEM),
    AGED    = as.numeric(AGED),
    AGEK    = as.numeric(AGEK),
    AGEMAR  = as.numeric(AGEMAR),
    AGEMARD = as.numeric(AGEMARD),
    
    # Calculate age at first birth for both parents
    mom_age_at_birth = AGEM - AGEK,
    dad_age_at_birth = AGED - AGEK
  ) %>%
  filter(
    # Both parents married before or at the time of first birth
    AGEMAR <= mom_age_at_birth,
    AGEMARD <= dad_age_at_birth
  )


##### CLEAN data_90

# Using data_90
clean_data_90 <- data_90 %>%
  
  # 1. Convert math/count variables to numeric
  mutate(across(c(AGEM, FERTIL, KIDCOUNT, AGEK, AGE2NDK, TWIN1ST, HISPM, RACEM, WEEK89M, YEARSCHM,
                  WEEK89D, HOUR89M, HOUR89D, INCOMEM1, INCOMEM2, FAMINC, INCOMED1, INCOMED2), as.numeric)) %>%
  
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
    same_sex = ifelse(boy1st == boy2nd, 1, 0),
    
    # Standardize education variable name
    years_of_education = as.numeric(YEARSCHM),
    
    # Create Twins at second birth instrument
    twins_2 = ifelse(MULTI2ND == 1, 1, 0),
    
    # Sex mix instruments
    two_boys  = ifelse(boy1st == 1 & boy2nd == 1, 1, 0),
    two_girls = ifelse(boy1st == 0 & boy2nd == 0, 1, 0),
    mom_age_at_birth = AGEM - AGEK,
    
    # Demographic dummies
    white      = ifelse(RACEM == 1, 1, 0),
    black      = ifelse(RACEM == 2, 1, 0),
    hispanic   = ifelse(HISPM > 0, 1, 0),
    other_race = ifelse(RACEM > 2, 1, 0),
    
    # Labor variables (Deflated to 1995 dollars)
    worked_for_pay = ifelse(WEEK89M > 0, 1, 0),
    labor_income_95 = (INCOMEM1 + INCOMEM2) * 1.22,
    family_income_95 = FAMINC * 1.22,
    non_wife_income_95 = (FAMINC - INCOMEM1 - INCOMEM2) * 1.22,
    
    # Husband variables
    worked_for_pay_dad = ifelse(WEEK89D > 0, 1, 0),
    labor_income_dad_95 = (INCOMED1 + INCOMED2) * 1.22,
    
    # Log Income variables
    ln_fam_inc = log(ifelse(family_income_95 <= 0, 1, family_income_95)),
    ln_nonwife_inc = log(ifelse(non_wife_income_95 <= 0, 1, non_wife_income_95)),
  )

##### CREATE married_data_90

married_data_90 <- clean_data_90 %>%
  filter(
    MARITAL == "0",    # Spouse present
    !is.na(AGED)       # Dad must be physically present
  )


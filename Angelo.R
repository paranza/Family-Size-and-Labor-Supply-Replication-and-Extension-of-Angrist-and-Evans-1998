# ANGELO 
library(dplyr)
library(haven)
library(tidyr)

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

##### TABLE 2 (Only 80 data at the moment)

library(dplyr)
library(tidyr)

# 1. PREPARAZIONE DI TUTTE LE VARIABILI (Mogli, Mariti e Gemelli)
tabella2_data <- clean_data_80 %>%
  mutate(
    # Convertiamo in numerico anche le variabili del padre e del 3° figlio
    across(c(WEEKSM, HOURSM, INCOME1M, FAMINC, TIMESMAR, AGEMAR, MARITAL, 
             AGED, WEEKSD, HOURSD, INCOME1D, AGEQ3RD), as.numeric),
    
    # Variabili Sesso e Gemelli
    two_boys = ifelse(boy1st == 1 & boy2nd == 1, 1, 0),
    two_girls = ifelse(boy1st == 0 & boy2nd == 0, 1, 0),
    twins_2 = ifelse(!is.na(AGEQ2ND) & !is.na(AGEQ3RD) & AGEQ2ND == AGEQ3RD, 1, 0),
    
    # Variabili Madri
    age_first_birth = AGEM - AGEK,
    worked_for_pay = ifelse(WEEKSM > 0, 1, 0),
    labor_income_95 = INCOME1M * 2.27,
    
    # Variabili Padri
    age_first_birth_dad = AGED - AGEK,
    worked_for_pay_dad = ifelse(WEEKSD > 0, 1, 0),
    labor_income_dad_95 = INCOME1D * 2.27,
    
    # Redditi Familiari
    family_income_95 = FAMINC * 2.27,
    non_wife_income_95 = (FAMINC - INCOME1M) * 2.27
  )

# 2. CAMPIONE DELLE DONNE SPOSATE E DEI LORO MARITI
married_data <- tabella2_data %>%
  filter(
    MARITAL <= 2,                      
    TIMESMAR == 1,                     
    AGEMAR <= age_first_birth          
  )

# 3. FUNZIONI AGGIORNATE PER LA FORMATTAZIONE ESATTA
# Aggiungiamo un parametro logico (is_all_women) per gestire il reddito Non-wife
riassumi_donne <- function(df, is_all_women = FALSE) {
  df %>%
    summarise(
      `Children ever born` = sprintf("%.2f (%.2f)", mean(real_kids, na.rm=T), sd(real_kids, na.rm=T)),
      `More than 2 children` = sprintf("%.3f", mean(more_than_2, na.rm=T)),
      `Boy 1st` = sprintf("%.3f", mean(boy1st, na.rm=T)),
      `Boy 2nd` = sprintf("%.3f", mean(boy2nd, na.rm=T)),
      `Two boys` = sprintf("%.3f", mean(two_boys, na.rm=T)),
      `Two girls` = sprintf("%.3f", mean(two_girls, na.rm=T)),
      `Same sex` = sprintf("%.3f", mean(same_sex, na.rm=T)),
      `Twins-2` = sprintf("%.4f", mean(twins_2, na.rm=T)),
      `Age` = sprintf("%.1f (%.1f)", mean(AGEM, na.rm=T), sd(AGEM, na.rm=T)),
      `Age at first birth` = sprintf("%.1f (%.1f)", mean(age_first_birth, na.rm=T), sd(age_first_birth, na.rm=T)),
      `Worked for pay` = sprintf("%.3f (%.3f)", mean(worked_for_pay, na.rm=T), sd(worked_for_pay, na.rm=T)),
      `Weeks worked` = sprintf("%.1f (%.1f)", mean(WEEKSM, na.rm=T), sd(WEEKSM, na.rm=T)),
      `Hours/week` = sprintf("%.1f (%.1f)", mean(HOURSM, na.rm=T), sd(HOURSM, na.rm=T)),
      `Labor income ($1995)` = sprintf("%.0f (%.0f)", mean(labor_income_95, na.rm=T), sd(labor_income_95, na.rm=T)),
      `Family income ($1995)` = sprintf("%.0f (%.0f)", mean(family_income_95, na.rm=T), sd(family_income_95, na.rm=T)),
      # Condizione per lasciare vuota la riga in All Women
      `Non-wife income ($1995)` = if(is_all_women) "" else sprintf("%.0f (%.0f)", mean(non_wife_income_95, na.rm=T), sd(non_wife_income_95, na.rm=T))
    ) %>%
    pivot_longer(cols = everything(), names_to = "Variable", values_to = "Mean (SD)")
}

riassumi_mariti <- function(df) {
  df %>%
    summarise(
      # Mettiamo stringhe vuote per tutte le variabili della fertilità
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
      # Vuoto per Family Income e Non-wife income
      `Family income ($1995)` = "",
      `Non-wife income ($1995)` = ""
    ) %>%
    pivot_longer(cols = everything(), names_to = "Variable", values_to = "Mean (SD)")
}

# 4. UNIAMO LE TRE COLONNE (con il parametro is_all_women corretto)
col_all_women <- riassumi_donne(tabella2_data, is_all_women = TRUE) %>% 
  rename(`All Women 1980` = `Mean (SD)`)

col_married_wives <- riassumi_donne(married_data, is_all_women = FALSE) %>% 
  rename(`Married Wives` = `Mean (SD)`)

col_husbands <- riassumi_mariti(married_data) %>% 
  rename(`Husbands` = `Mean (SD)`)

# Tabella Finale unita
tabella_finale <- col_all_women %>%
  left_join(col_married_wives, by = "Variable") %>%
  left_join(col_husbands, by = "Variable")

# Rimuovi l'allineamento automatico dei numeri per vedere le stringhe vuote in modo pulito
print(as.data.frame(tabella_finale), right = FALSE)

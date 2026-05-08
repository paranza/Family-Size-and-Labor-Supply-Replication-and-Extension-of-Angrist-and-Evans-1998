# ==========================================
# TABLE 3
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

# 1. FUNCTION FOR EXACT FORMATTING
calculate_table3_stats <- function(df) {
  tot_n <- nrow(df)
  
  # Step A: Group by specific sibling combinations
  base_stats <- df %>%
    mutate(
      sibling_mix = case_when(
        boy1st == 1 & boy2nd == 0 ~ "one boy, one girl",
        boy1st == 0 & boy2nd == 1 ~ "one boy, one girl",
        boy1st == 0 & boy2nd == 0 ~ "two girls",
        boy1st == 1 & boy2nd == 1 ~ "two boys",
        TRUE ~ NA_character_
      )
    ) %>%
    group_by(sibling_mix) %>%
    summarise(
      n_obs = n(),
      frac_sample = n_obs / tot_n,
      prob_another = mean(more_than_2, na.rm = TRUE),
      se_another = sqrt(prob_another * (1 - prob_another) / n_obs),
      .groups = "drop"
    )
  
  # Step B: Aggregate for "both same sex"
  same_sex_stats <- df %>%
    filter(same_sex == 1) %>%
    summarise(
      sibling_mix = "(2) both same sex",
      n_obs = n(),
      frac_sample = n_obs / tot_n,
      prob_another = mean(more_than_2, na.rm = TRUE),
      se_another = sqrt(prob_another * (1 - prob_another) / n_obs)
    )
  
  # Step C: Calculate the difference (2) - (1)
  prob_mix <- base_stats$prob_another[base_stats$sibling_mix == "one boy, one girl"]
  se_mix   <- base_stats$se_another[base_stats$sibling_mix == "one boy, one girl"]
  
  prob_same <- same_sex_stats$prob_another
  se_same   <- same_sex_stats$se_another
  
  diff_stats <- tibble(
    sibling_mix = "difference (2) - (1)",
    n_obs = NA_integer_,
    frac_sample = NA_real_,
    prob_another = prob_same - prob_mix,
    se_another = sqrt(se_mix^2 + se_same^2)
  )
  
  # Step D: Assemble rows in the exact order of the paper
  ordered_results <- bind_rows(
    base_stats %>% filter(sibling_mix == "one boy, one girl"),
    base_stats %>% filter(sibling_mix == "two girls"),
    base_stats %>% filter(sibling_mix == "two boys"),
    base_stats %>% filter(sibling_mix == "one boy, one girl") %>% mutate(sibling_mix = "(1) one boy, one girl"),
    same_sex_stats,
    diff_stats
  ) %>%
    mutate(
      # Ensure order is preserved 
      Variable = factor(sibling_mix, levels = c(
        "one boy, one girl", "two girls", "two boys", 
        "(1) one boy, one girl", "(2) both same sex", "difference (2) - (1)"
      ))
    ) %>%
    arrange(Variable)
  
  # Step E: String formatting "Mean (SE)"
  ordered_results %>%
    mutate(
      `Fraction of sample` = ifelse(is.na(frac_sample), "—", sprintf("%.3f", frac_sample)),
      `Fraction that had another child` = sprintf("%.3f (%.3f)", prob_another, se_another)
    ) %>%
    select(Variable, `Fraction of sample`, `Fraction that had another child`)
}


# 2. CALCULATE AND RENAME COLUMNS FOR MERGING
t3_all_80 <- calculate_table3_stats(clean_data_80) %>%
  rename(`Frac_Sample_All_80` = `Fraction of sample`, `Had_Child_All_80` = `Fraction that had another child`)

t3_all_90 <- calculate_table3_stats(clean_data_90) %>%
  rename(`Frac_Sample_All_90` = `Fraction of sample`, `Had_Child_All_90` = `Fraction that had another child`)

t3_marr_80 <- calculate_table3_stats(married_data_80) %>%
  rename(`Frac_Sample_Marr_80` = `Fraction of sample`, `Had_Child_Marr_80` = `Fraction that had another child`)

t3_marr_90 <- calculate_table3_stats(married_data_90) %>%
  rename(`Frac_Sample_Marr_90` = `Fraction of sample`, `Had_Child_Marr_90` = `Fraction that had another child`)

# 3. FINAL MERGE
merged_table_3 <- t3_all_80 %>%
  left_join(t3_all_90, by = "Variable") %>%
  left_join(t3_marr_80, by = "Variable") %>%
  left_join(t3_marr_90, by = "Variable")

# 4. GT TABLE FORMATTING (Replicating paper's visual hierarchy)
publication_table_3 <- merged_table_3 %>%
  gt() %>%
  tab_header(
    title = md("**Table 3: Fraction of Families That Had Another Child by Parity and Sex of Children**"),
    subtitle = "Bottom Panel: Sex of first two children in families with two or more children"
  ) %>%
  # Top-level spanners (All Women vs Married Women)
  tab_spanner(
    label = md("**All women**"),
    id = "spanner_all_women",
    columns = c(`Frac_Sample_All_80`, `Had_Child_All_80`, `Frac_Sample_All_90`, `Had_Child_All_90`)
  ) %>%
  tab_spanner(
    label = md("**Married women**"),
    id = "spanner_married_women",
    columns = c(`Frac_Sample_Marr_80`, `Had_Child_Marr_80`, `Frac_Sample_Marr_90`, `Had_Child_Marr_90`)
  ) %>%
  # Sub-level spanners (1980 vs 1990)
  tab_spanner(
    label = "1980 PUMS",
    id = "1980_all",
    columns = c(`Frac_Sample_All_80`, `Had_Child_All_80`)
  ) %>%
  tab_spanner(
    label = "1990 PUMS",
    id = "1990_all",
    columns = c(`Frac_Sample_All_90`, `Had_Child_All_90`)
  ) %>%
  tab_spanner(
    label = "1980 PUMS",
    id = "1980_marr",
    columns = c(`Frac_Sample_Marr_80`, `Had_Child_Marr_80`)
  ) %>%
  tab_spanner(
    label = "1990 PUMS",
    id = "1990_marr",
    columns = c(`Frac_Sample_Marr_90`, `Had_Child_Marr_90`)
  ) %>%
  # Rename columns to match the repetitive headers in the paper
  cols_label(
    `Frac_Sample_All_80` = "Fraction of sample",
    `Had_Child_All_80` = "Fraction that had another child",
    `Frac_Sample_All_90` = "Fraction of sample",
    `Had_Child_All_90` = "Fraction that had another child",
    `Frac_Sample_Marr_80` = "Fraction of sample",
    `Had_Child_Marr_80` = "Fraction that had another child",
    `Frac_Sample_Marr_90` = "Fraction of sample",
    `Had_Child_Marr_90` = "Fraction that had another child",
    Variable = ""
  ) %>%
  cols_align(align = "center", columns = -Variable) %>%
  cols_align(align = "left", columns = Variable) %>%
  opt_row_striping() %>%
  tab_source_note(
    source_note = "Notes: The samples are the same as in Table 2. Standard errors are reported in parentheses"
  )

# Display the final table
publication_table_3

# To save as png
# gtsave(publication_table_3, "Table_3.png")


# 5. ENVIRONMENT CLEANUP

# Keep only the final married datasets and the raw clean ones
rm(list = setdiff(ls(), c("clean_data_80", "clean_data_90", 
                          "married_data_80", "married_data_90", "publication_table_3")))

###### TABLE 6

library(dplyr)
library(modelsummary)
library(gt)

# 1. Define covariates using exact names
covariates <- "AGEM + mom_age_at_birth + black + hispanic + other_race"

# 2. Prepare formulas
form2 <- as.formula(paste("more_than_2 ~ boy1st + boy2nd + same_sex +", covariates))
form3 <- as.formula(paste("more_than_2 ~ boy1st + two_boys + two_girls +", covariates))

# ==========================================
# RUN MODELS (1980 PUMS)
# ==========================================
mod1_80_all <- lm(more_than_2 ~ same_sex, data = clean_data_80)
mod2_80_all <- lm(form2, data = clean_data_80)
mod3_80_all <- lm(form3, data = clean_data_80)

mod4_80_mar <- lm(more_than_2 ~ same_sex, data = married_data_80)
mod5_80_mar <- lm(form2, data = married_data_80)
mod6_80_mar <- lm(form3, data = married_data_80)

models_1980 <- list(
  "(1)" = mod1_80_all, "(2)" = mod2_80_all, "(3)" = mod3_80_all,
  "(4)" = mod4_80_mar, "(5)" = mod5_80_mar, "(6)" = mod6_80_mar
)

# ==========================================
# RUN MODELS (1990 PUMS) - WITH SAMPLE WEIGHTS
# ==========================================

# All Women (Columns 1, 2, 3) using PWGTM1 as weights
mod1_90_all <- lm(more_than_2 ~ same_sex, data = clean_data_90, weights = PWGTM1)
mod2_90_all <- lm(form2, data = clean_data_90, weights = PWGTM1)
mod3_90_all <- lm(form3, data = clean_data_90, weights = PWGTM1)

# Married Women (Columns 4, 5, 6) using PWGTM1 as weights
mod4_90_mar <- lm(more_than_2 ~ same_sex, data = married_data_90, weights = PWGTM1)
mod5_90_mar <- lm(form2, data = married_data_90, weights = PWGTM1)
mod6_90_mar <- lm(form3, data = married_data_90, weights = PWGTM1)

# Group the weighted models into the list for modelsummary
models_1990 <- list(
  "(1)" = mod1_90_all, 
  "(2)" = mod2_90_all, 
  "(3)" = mod3_90_all,
  "(4)" = mod4_90_mar, 
  "(5)" = mod5_90_mar, 
  "(6)" = mod6_90_mar
)

# ==========================================
# FORMATTING & BINDING DATA FRAMES
# ==========================================

# Variable mapping
coef_map <- c(
  "boy1st"   = "Boy 1st",
  "boy2nd"   = "Boy 2nd",
  "same_sex" = "Same sex",
  "two_boys" = "Two boys",
  "two_girls"= "Two girls"
)

# Define exact row ordering required: Variables -> Covariates Row -> R-squared
exact_row_order <- c(unname(coef_map), "With other covariates", "R<sup>2</sup>")

# Configure GOF to keep only R-squared and format the superscript
gof_mapping <- data.frame(
  raw = "r.squared", 
  clean = "R<sup>2</sup>", 
  fmt = 3
)

# Custom row for covariates indication
custom_rows <- data.frame(
  term = "With other covariates",
  "(1)" = "no", "(2)" = "yes", "(3)" = "yes",
  "(4)" = "no", "(5)" = "yes", "(6)" = "yes",
  check.names = FALSE
)

# Generate 1980 dataframe
df_1980 <- modelsummary(
  models_1980,
  coef_map  = coef_map,
  vcov      = 'robust',
  fmt       = 4,                                      # Increase decimals to 4
  estimate  = "{estimate}<br>({std.error})",          # Put SE below estimate in same cell
  statistic = NULL,                                   # Remove dedicated SE row
  gof_map   = gof_mapping,
  add_rows  = custom_rows,
  output    = "data.frame"
) %>%
  mutate(
    term = factor(term, levels = exact_row_order),    # Force custom ordering
    Panel = "1980 PUMS"                               # Group identifier
  ) %>%
  arrange(term) %>%
  select(-part, -statistic)                           # Drop internal metadata

# Generate 1990 dataframe
df_1990 <- modelsummary(
  models_1990,
  coef_map  = coef_map,
  vcov      = "robust",
  fmt       = 4,
  estimate  = "{estimate}<br>({std.error})",
  statistic = NULL,
  gof_map   = gof_mapping,
  add_rows  = custom_rows,
  output    = "data.frame"
) %>%
  mutate(
    term = factor(term, levels = exact_row_order),
    Panel = "1990 PUMS"
  ) %>%
  arrange(term) %>%
  select(-part, -statistic)

# Bind vertically
df_combined <- bind_rows(df_1980, df_1990) %>%
  rename(Variable = term)

# ==========================================
# GT TABLE CREATION
# ==========================================

publication_table_6 <- df_combined %>%
  gt(groupname_col = "Panel") %>%
  tab_header(
    title = md("**Table 6 — OLS Estimates of *More than 2 Children* Equations**")
  ) %>%
  tab_spanner(label = md("**All women**"), columns = c("(1)", "(2)", "(3)")) %>%
  tab_spanner(label = md("**Married women**"), columns = c("(4)", "(5)", "(6)")) %>%
  fmt_markdown(columns = everything()) %>%               # Parses HTML <br> and <sup>
  cols_align(align = "center", columns = -Variable) %>%  # Center numbers
  cols_align(align = "left", columns = Variable) %>%     # Left align labels
  sub_missing(missing_text = "—") %>%                    # Replace NAs with em-dash
  opt_row_striping() %>%
  
  # Darken and thicken the borders above and below the panel headers
  tab_options(
    row_group.border.top.color = "black",
    row_group.border.top.width = px(2),
    row_group.border.bottom.color = "black",
    row_group.border.bottom.width = px(2)
  ) %>%
  
  tab_source_note(
    source_note = md("Notes: Other covariates in the models are indicators for *Age*, *Age at first birth*, *Black*, *Hispanic*, and *Other race*. The variable *Boy 2nd* is excluded from columns (3) and (6). Standard errors are reported in parentheses.")
  )

# Display the table
publication_table_6

# To save as png
# gtsave(publication_table_6, "Table_6.png")

# ==========================================
# ENVIRONMENT CLEANUP
# ==========================================

# Keep only the final datasets and the table object
rm(list = setdiff(ls(), c(
  "clean_data_80", "clean_data_90", 
  "married_data_80", "married_data_90")))

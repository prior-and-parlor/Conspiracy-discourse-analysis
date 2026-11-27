# =====================================================================
# LIWC-22: MONTHLY COMPARISON OF PRE-COVID AND COVID PERIODS
# =====================================================================
#
# This script compares monthly LIWC-22 scores across the pre-COVID
# (June–December 2019) and COVID (January–November 2020) periods.
#
# The analysis includes:
#   - Descriptive statistics for the 20 selected LIWC-22 variables
#   - Mann–Whitney U tests based on monthly mean scores
#   - Effect size estimates (r)
#   - Benjamini–Hochberg adjustment for multiple comparisons
#
# Tests are two-tailed and use a tie-corrected normal approximation
# with continuity correction.
#
# =====================================================================

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(lubridate)
  library(purrr)
  library(writexl)
})

# File paths
dir_cfa <- "....."

liwc_file <- file.path(
  dir_cfa,
  "LIWC-22Results_CleanedConspiracydata.xlsx"
)

output_file <- file.path(
  dir_cfa,
  "LIWC_monthly_statistical_results_REVISED.xlsx"
)

# Import data and retain the study period
liwc_raw <- read_excel(liwc_file) %>%
  select(-any_of("...1"))

liwc_data <- liwc_raw %>%
  mutate(Date = as.Date(Date)) %>%
  filter(
    !is.na(Date),
    Date >= as.Date("2019-06-01"),
    Date <= as.Date("2020-11-30")
  ) %>%
  mutate(
    month_date = floor_date(Date, "month"),
    phase = if_else(
      Date < as.Date("2020-01-01"),
      "Pre-COVID",
      "COVID"
    )
  )

cat("Rows retained:", nrow(liwc_data), "\n")

# LIWC variables included in the comparison
selected_variables <- c(
  "Physical", "health", "illness", "emo_anx", "mental",
  "risk", "focuspast", "death", "i", "Analytic",
  "relig", "they", "emo_pos", "focuspresent", "Clout",
  "Authentic", "power", "prosocial", "moral", "politic"
)

available_variables <- intersect(
  selected_variables,
  names(liwc_data)
)

missing_variables <- setdiff(
  selected_variables,
  names(liwc_data)
)

if (length(missing_variables) > 0) {
  warning(
    "Variables not found in the LIWC file: ",
    paste(missing_variables, collapse = ", ")
  )
}

if (length(available_variables) == 0) {
  stop("No selected LIWC variables were found.")
}

liwc_data <- liwc_data %>%
  mutate(
    across(
      all_of(available_variables),
      ~ suppressWarnings(as.numeric(.x))
    )
  )

# Calculate monthly means
monthly_data <- liwc_data %>%
  group_by(month_date, phase) %>%
  summarise(
    across(
      all_of(available_variables),
      ~ mean(.x, na.rm = TRUE)
    ),
    .groups = "drop"
  ) %>%
  arrange(month_date)

phase_counts <- monthly_data %>%
  count(phase, name = "monthly_observations")

print(phase_counts)

pre_count <- phase_counts %>%
  filter(phase == "Pre-COVID") %>%
  pull(monthly_observations)

covid_count <- phase_counts %>%
  filter(phase == "COVID") %>%
  pull(monthly_observations)

if (length(pre_count) == 0 || pre_count != 7) {
  warning("The Pre-COVID period does not contain seven months.")
}

if (length(covid_count) == 0 || covid_count != 11) {
  warning("The COVID period does not contain eleven months.")
}

# Descriptive statistics across monthly observations
monthly_descriptives <- monthly_data %>%
  pivot_longer(
    cols = all_of(available_variables),
    names_to = "variable",
    values_to = "monthly_mean"
  ) %>%
  group_by(variable, phase) %>%
  summarise(
    monthly_observations = sum(!is.na(monthly_mean)),
    mean = mean(monthly_mean, na.rm = TRUE),
    median = median(monthly_mean, na.rm = TRUE),
    sd = sd(monthly_mean, na.rm = TRUE),
    IQR = IQR(monthly_mean, na.rm = TRUE),
    .groups = "drop"
  )

# Tie-corrected Mann-Whitney U test
mann_whitney_tie_corrected <- function(pre, covid) {
  
  pre <- pre[!is.na(pre)]
  covid <- covid[!is.na(covid)]
  
  n1 <- length(pre)
  n2 <- length(covid)
  
  if (n1 == 0 || n2 == 0) {
    return(tibble(
      U = NA_real_,
      Z = NA_real_,
      p_value = NA_real_,
      effect_r = NA_real_
    ))
  }
  
  combined <- c(pre, covid)
  ranks <- rank(combined, ties.method = "average")
  
  rank_sum_pre <- sum(ranks[seq_len(n1)])
  
  U1 <- rank_sum_pre - n1 * (n1 + 1) / 2
  U2 <- n1 * n2 - U1
  U <- min(U1, U2)
  
  N <- n1 + n2
  
  tie_table <- table(combined)
  tie_term <- sum(tie_table^3 - tie_table)
  
  variance_U <- (n1 * n2 / 12) *
    (N + 1 - tie_term / (N * (N - 1)))
  
  if (variance_U <= 0) {
    return(tibble(
      U = U,
      Z = NA_real_,
      p_value = NA_real_,
      effect_r = NA_real_
    ))
  }
  
  mean_U <- n1 * n2 / 2
  
  Z <- if (U1 > mean_U) {
    (U1 - mean_U - 0.5) / sqrt(variance_U)
  } else {
    (U1 - mean_U + 0.5) / sqrt(variance_U)
  }
  
  p_value <- 2 * pnorm(-abs(Z))
  effect_r <- abs(Z) / sqrt(N)
  
  tibble(
    U = U,
    Z = Z,
    p_value = p_value,
    effect_r = effect_r
  )
}

# Run the tests for each variable
test_results <- map_dfr(
  available_variables,
  function(variable) {
    
    pre_values <- monthly_data %>%
      filter(phase == "Pre-COVID") %>%
      pull(all_of(variable))
    
    covid_values <- monthly_data %>%
      filter(phase == "COVID") %>%
      pull(all_of(variable))
    
    mann_whitney_tie_corrected(
      pre = pre_values,
      covid = covid_values
    ) %>%
      mutate(variable = variable, .before = 1)
  }
)

# Adjust p-values for multiple comparisons
test_results <- test_results %>%
  mutate(
    p_adjusted_BH = p.adjust(p_value, method = "BH"),
    significant_raw = p_value < 0.05,
    significant_BH = p_adjusted_BH < 0.05
  )

# Arrange descriptive statistics by phase
descriptives_wide <- monthly_descriptives %>%
  pivot_wider(
    names_from = phase,
    values_from = c(
      monthly_observations,
      mean,
      median,
      sd,
      IQR
    ),
    names_glue = "{.value}_{phase}"
  )

# Combine test results and descriptive statistics
final_results <- test_results %>%
  left_join(descriptives_wide, by = "variable") %>%
  select(
    variable,
    U,
    Z,
    p_value,
    p_adjusted_BH,
    effect_r,
    significant_raw,
    significant_BH,
    `monthly_observations_Pre-COVID`,
    monthly_observations_COVID,
    `mean_Pre-COVID`,
    mean_COVID,
    `median_Pre-COVID`,
    median_COVID,
    `sd_Pre-COVID`,
    sd_COVID,
    `IQR_Pre-COVID`,
    IQR_COVID
  ) %>%
  arrange(p_value)

print(final_results, n = Inf)

# Export results
writexl::write_xlsx(
  list(
    Final_Results = final_results,
    Monthly_Descriptives = monthly_descriptives,
    Phase_Counts = phase_counts,
    Monthly_Data = monthly_data
  ),
  path = output_file
)

cat("\nResults saved to:", output_file, "\n")


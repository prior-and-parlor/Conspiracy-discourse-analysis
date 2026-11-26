# Moral polarity analysis (vice - virtue)
# Monthly data from June 2019 to November 2020 (18 observations).
#
# The reduced model is the primary analysis. The interaction model is
# supplementary because the two election months have leverage of 1. Its
# coefficients are reported as OLS estimates only; event effects are
# summarised for the months observed in the data.
#
# The script also runs bootstrap, sensitivity, equivalence, Bayesian and
# effect-size analyses. The optional brms analysis is off by default.
#
# Run with: source("moral_polarity_revised_v2.R", echo = TRUE)

suppressPackageStartupMessages({
  library(readr); library(dplyr); library(tidyr); library(lubridate)
  library(sandwich); library(lmtest)
})
has_bf  <- requireNamespace("BayesFactor", quietly = TRUE)
has_brm <- requireNamespace("brms",        quietly = TRUE)
has_ms  <- requireNamespace("modelsummary", quietly = TRUE)
if (!has_bf) message("BayesFactor not installed: install.packages(\"BayesFactor\") for section 4.")

set.seed(2026)

# Settings

DATA_FILE      <- "all-vv-updated.csv"   # relative path
OUT_DIR        <- "outputs"
NW_LAG         <- 2        # Main HAC lag; lags 1-4 are checked below.
B_BOOT         <- 5000     # wild bootstrap draws
B_BAYES        <- 10000    # posterior draws per Bayesian model/prior specification
B_SIM          <- 5000     # simulation draws per grid point (lower for a test run)
SESOI_TOTAL_SD <- 0.5      # Equivalence bound: total change across the window, in SD units.
RUN_BRMS       <- FALSE    # Set TRUE only when the brms/Stan toolchain is available
MAKE_FIGURE    <- TRUE
WINDOW_START   <- as.Date("2019-06-01")
WINDOW_END     <- as.Date("2020-11-01")   # month-start date of the last month
BASELINE_N     <- 7        # Phase 1 = Jun-Dec 2019 
dir.create(OUT_DIR, showWarnings = FALSE)
write_csv_console <- function(x, filename) {
  out_file <- file.path(OUT_DIR, filename)
  
  utils::write.csv(x, out_file, row.names = FALSE)
  
  cat("\n", strrep("=", 80), "\n", sep = "")
  cat("CSV RESULTS: ", filename, "\n", sep = "")
  cat(strrep("=", 80), "\n")
  
  print(as.data.frame(x), row.names = FALSE, max = nrow(x))
  
  cat("\nSaved to: ", normalizePath(out_file), "\n\n", sep = "")
  
  invisible(out_file)
}

# Primary event windows. January 2020 is not coded as COVID-19, and May 2020
# is not coded as BLM; alternative windows are checked below.
COVID_MONTHS    <- as.Date(c("2020-02-01", "2020-03-01", "2020-04-01", "2020-05-01"))
BLM_MONTHS      <- as.Date(c("2020-06-01", "2020-07-01", "2020-08-01"))
ELECTION_MONTHS <- as.Date(c("2020-10-01", "2020-11-01"))

foundation_vars <- c(
  "care.vice", "fairness.vice", "loyalty.vice", "authority.vice", "sanctity.vice",
  "care.virtue", "fairness.virtue", "loyalty.virtue", "authority.virtue", "sanctity.virtue"
)

# Get Newey-West estimates with the small-sample adjustment.
nw_tab <- function(m, lag) {
  ct   <- coeftest(m, vcov = NeweyWest(m, lag = lag, prewhite = FALSE, adjust = TRUE))
  crit <- qt(.975, df.residual(m))
  data.frame(term = rownames(ct), est = ct[, 1], se = ct[, 2], t = ct[, 3], p = ct[, 4],
             lo = ct[, 1] - crit * ct[, 2], hi = ct[, 1] + crit * ct[, 2], row.names = NULL)
}

# 1. Preparing the monthly data

df <- read_csv(DATA_FILE, show_col_types = FALSE, name_repair = "unique_quiet",
               col_types = cols(date = col_character()))
cat("\n== Raw data ==\n")
cat("Rows read:", nrow(df), "\n")
df <- df %>% mutate(date = suppressWarnings(lubridate::dmy(date)))   # day-first, 2- or 4-digit year
n_bad <- sum(is.na(df$date))
cat("Rows with an unparseable or missing date:", n_bad, "\n")
if (n_bad > 0) stop("Dates failed to parse: inspect them before going any further.")
df <- df %>% mutate(month = floor_date(date, "month"))
outside <- df %>% filter(month < WINDOW_START | month > WINDOW_END)
cat("Rows outside", format(WINDOW_START, "%b %Y"), "-", format(WINDOW_END, "%b %Y"), ":", nrow(outside), "\n")
if (nrow(outside) > 0) {
  message("Rows outside the window are dropped. If this is not ~0, re-check the date parse.")
  df <- df %>% filter(month >= WINDOW_START, month <= WINDOW_END)
}

# Check the number of comments and calendar days represented in each month.
cover <- df %>%
  group_by(month) %>%
  summarise(n_comments = n(), n_days = n_distinct(date), .groups = "drop") %>%
  mutate(days_in_month = lubridate::days_in_month(month),
         pct_days = round(100 * n_days / days_in_month),
         # Flag months with low comment volume or incomplete day coverage.
         flag_thin = n_comments < 0.5 * median(n_comments) | pct_days < 60)
cat("\n== Comments per month ==\n"); print(as.data.frame(cover))
cat("Months with ANY uncovered day:", sum(cover$n_days < cover$days_in_month), "of", nrow(cover),
    "| range of day coverage:", min(cover$pct_days), "-", max(cover$pct_days), "%\n")
write.csv(cover, file.path(OUT_DIR, "comments_per_month.csv"), row.names = FALSE)
thin_months <- cover$month[cover$flag_thin]
if (length(thin_months))
  cat("Months flagged as thin/gappy:", format(thin_months, "%b %Y"), "\n")

df_month <- df %>%
  group_by(month) %>%
  summarise(across(all_of(foundation_vars), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  arrange(month)

long <- df_month %>%
  pivot_longer(-month, names_to = "raw", values_to = "score") %>%
  mutate(foundation = sub("\\.(vice|virtue)$", "", raw),
         system     = ifelse(grepl("\\.vice$", raw), "Vice", "Virtue")) %>%
  group_by(foundation, system) %>%
  mutate(z = as.numeric(scale(score))) %>%
  ungroup()

# Delta is the difference between the mean z-scores for vice and virtue.
# It tracks their relative movement, not an absolute level of vice dominance.
delta_df <- long %>%
  group_by(month, system) %>%
  summarise(z = mean(z, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = system, values_from = z) %>%
  arrange(month) %>%
  mutate(delta    = Vice - Virtue,
         time     = row_number(),
         covid    = as.integer(month %in% COVID_MONTHS),
         blm      = as.integer(month %in% BLM_MONTHS),
         election = as.integer(month %in% ELECTION_MONTHS)) %>%
  filter(is.finite(delta))
n <- nrow(delta_df)

stopifnot(n == 18)
stopifnot(all(delta_df$month == seq(min(delta_df$month), by = "month", length.out = n)))
cat("\n== Data checks ==\n")
cat("Months:", n, "| first:", format(min(delta_df$month)), "| last:", format(max(delta_df$month)), "\n")
cat("COVID   :", format(delta_df$month[delta_df$covid == 1], "%b %Y"), "\n")
cat("BLM     :", format(delta_df$month[delta_df$blm == 1], "%b %Y"), "\n")
cat("Election:", format(delta_df$month[delta_df$election == 1], "%b %Y"), "\n")
cat("Reference (no event) months:", format(delta_df$month[rowSums(delta_df[, c("covid", "blm", "election")]) == 0], "%b %Y"), "\n")
cat("SD of delta:", round(sd(delta_df$delta), 3), "\n")
write.csv(delta_df, file.path(OUT_DIR, "delta_monthly_series.csv"), row.names = FALSE)

# Primary regression model

f_red <- delta ~ time + covid + blm + election
m_red <- lm(f_red, data = delta_df)
cat("\n== PRIMARY: reduced model ==\n")
cat(sprintf("N = %d | R2 = %.3f | adjusted R2 = %.3f | residual df = %d\n",
            nobs(m_red), summary(m_red)$r.squared, summary(m_red)$adj.r.squared, df.residual(m_red)))
cat("\n-- OLS --\n");  print(summary(m_red)$coefficients)
cat(sprintf("\n-- Newey-West (lag = %d, small-sample adjusted; t on %d df) --\n", NW_LAG, df.residual(m_red)))
red_tab <- nw_tab(m_red, NW_LAG)
names(red_tab)[names(red_tab) %in% c("se", "p")] <- c("se_hac", "p_hac")   # t, lo, hi are HAC-based
red_tab$se_ols <- summary(m_red)$coefficients[, 2]
red_tab$p_ols  <- summary(m_red)$coefficients[, 4]
red_tab$p_hc3  <- coeftest(m_red, vcov = vcovHC(m_red, type = "HC3"))[, 4]
# With 18 observations, compare OLS, HC3 and HAC results rather than relying
# on a single standard-error estimate. Bootstrap intervals are reported below.
print(red_tab, digits = 4)
write.csv(red_tab, file.path(OUT_DIR, "Table2_reduced_HAC.csv"), row.names = FALSE)

# Check residual autocorrelation and whether results change with the HAC lag.
cat("\n-- Residual autocorrelation, reduced model --\n")
print(bgtest(m_red, order = 1)); print(dwtest(m_red))
cat("Lag-1 residual autocorrelation:", round(acf(resid(m_red), plot = FALSE, lag.max = 1)$acf[2], 3), "\n")

lag_tab <- do.call(rbind, lapply(1:4, function(L) cbind(lag = L, nw_tab(m_red, L))))
write.csv(lag_tab, file.path(OUT_DIR, "hac_lag_sensitivity.csv"), row.names = FALSE)
cat("\n-- HAC lag sensitivity (p-values, reduced model) --\n")
print(reshape(lag_tab[, c("lag", "term", "p")], idvar = "term", timevar = "lag", direction = "wide"), digits = 3)

# Save a Word version of Table 2 when modelsummary is available.
if (has_ms) {
  v_nw_red <- NeweyWest(m_red, lag = NW_LAG, prewhite = FALSE, adjust = TRUE)
  try(modelsummary::modelsummary(
        setNames(list(m_red, m_red), c("Reduced, OLS", paste0("Reduced, HAC (lag ", NW_LAG, ")"))),
        vcov = list(NULL, v_nw_red), statistic = "({std.error})", stars = TRUE, fmt = 3,
        gof_map = c("nobs", "r.squared", "adj.r.squared"),
        output = file.path(OUT_DIR, "Table2_reduced.docx")), silent = TRUE)
}

# Supplementary model with time-by-event interactions. Interpret cautiously.
f_full <- delta ~ time + covid + blm + election + time:covid + time:blm + time:election
m_full <- lm(f_full, data = delta_df)
cat("\n== SUPPLEMENT: full model (", length(coef(m_full)), "parameters,", df.residual(m_full), "residual df) ==\n")
cat(sprintf("R2 = %.3f | adjusted R2 = %.3f\n", summary(m_full)$r.squared, summary(m_full)$adj.r.squared))
lev <- hatvalues(m_full)
if (any(lev > 0.99)) {
  cat("Leverage ~1 (fitted exactly):", format(delta_df$month[lev > 0.99], "%b %Y"), "\n")
  cat("HC1/HAC/bootstrap SEs are degenerate here (election SEs equal the intercept's) and are not reported.\n")
}
print(summary(m_full)$coefficients)   # OLS only; coefficients for election terms are not interpretable
# Time = 0 is outside the observed period, so summarise event effects at
# the months represented in the data.
b <- coef(m_full)
ev_months <- list(covid = delta_df$time[delta_df$covid == 1],
                  blm = delta_df$time[delta_df$blm == 1],
                  election = delta_df$time[delta_df$election == 1])
me_full <- sapply(names(ev_months), function(e)
  mean(b[[e]] + b[[paste0("time:", e)]] * ev_months[[e]]))
cat("Mean event effect at the observed months (full model):\n"); print(round(me_full, 3))

# Wild bootstrap for the reduced model

wild_boot <- function(model, B = 5000) {
  X <- model.matrix(model); y <- model.response(model.frame(model))
  XtXi <- solve(crossprod(X)); beta <- as.vector(XtXi %*% crossprod(X, y))
  fit <- as.vector(X %*% beta); res <- y - fit
  h <- rowSums((X %*% XtXi) * X)
  res_adj <- numeric(length(res)); ok <- h < 0.999
  res_adj[ok] <- res[ok] / sqrt(1 - h[ok])
  w <- c(-sqrt(1.5), -1, -sqrt(0.5), sqrt(0.5), 1, sqrt(1.5))
  out <- matrix(NA_real_, B, ncol(X), dimnames = list(NULL, colnames(X)))
  for (i in seq_len(B)) out[i, ] <- XtXi %*% crossprod(X, fit + res_adj * sample(w, length(y), replace = TRUE))
  out
}
wb <- wild_boot(m_red, B_BOOT)
wb_tab <- data.frame(term = colnames(wb), estimate = coef(m_red),
                     ci_low = apply(wb, 2, quantile, 0.025), ci_high = apply(wb, 2, quantile, 0.975),
                     row.names = NULL)
wb_tab$ci_excludes_zero <- wb_tab$ci_low > 0 | wb_tab$ci_high < 0
cat("\n== Wild-bootstrap 95% percentile intervals (reduced model) ==\n"); print(wb_tab)
write.csv(wb_tab, file.path(OUT_DIR, "wild_bootstrap_ci_reduced.csv"), row.names = FALSE)

# Leave-one-month-out check

loo <- do.call(rbind, lapply(seq_len(n), function(i) {
  s <- summary(lm(f_red, data = delta_df[-i, ]))$coefficients
  data.frame(dropped = format(delta_df$month[i], "%b %Y"), term = rownames(s),
             est = s[, 1], p = s[, 4], row.names = NULL)
}))
loo_sum <- loo %>% group_by(term) %>%
  summarise(est_min = min(est), est_max = max(est),
            sign_changes = as.integer(min(est) < 0 & max(est) > 0),
            n_p_below_05 = sum(p < .05), n_fits = n(), .groups = "drop")
cat("\n== Leave-one-month-out (reduced model; OLS p) ==\n"); print(as.data.frame(loo_sum))
write.csv(loo, file.path(OUT_DIR, "leave_one_out_all.csv"), row.names = FALSE)
write.csv(loo_sum, file.path(OUT_DIR, "leave_one_out_summary.csv"), row.names = FALSE)

# Placebo breakpoints

break_stat <- function(cut) {
  d <- delta_df; d$post <- as.integer(d$month >= cut)
  s <- summary(lm(delta ~ time + post, data = d))$coefficients["post", ]
  c(est = s[[1]], t = s[[3]])
}
actual_cut <- as.Date("2020-01-01")
cand_cuts  <- delta_df$month[4:(n - 3)]
plc <- t(sapply(cand_cuts, break_stat))
plc <- data.frame(cut = cand_cuts, plc, abs_t = abs(plc[, "t"]))
t_actual <- abs(break_stat(actual_cut)[["t"]])
cat("\n== Placebo breakpoints ==\n")
cat(sprintf("|t| at 1 Jan 2020 = %.2f; share of candidate cuts with |t| >= this: %.2f (descriptive only)\n",
            t_actual, mean(plc$abs_t >= t_actual)))
write.csv(plc, file.path(OUT_DIR, "placebo_breakpoints.csv"), row.names = FALSE)

# Simulation-based sensitivity analysis
# Uses the full primary-model design and Newey-West inference.

# The simulations use all 18 months and the full primary model. Each
# coefficient is varied in turn while the other terms remain at their fitted
# values. Errors follow an AR(1) process estimated from the model residuals,
# and tests use the same HAC settings as the main analysis. These are
# conditional sensitivity estimates, not retrospective power calculations.

# Simulation settings

ALPHA_SIM <- 0.05
TARGET_POWER <- 0.80

# Effect grids use residual-SD units: per month for the trend and as a level
# shift for each event indicator.
SLOPE_MAX_RESID_SD <- 0.30
EVENT_MAX_RESID_SD <- 4.00

N_GRID <- 31

# Estimate the residual variance and lag-1 correlation

sigma_sim <- summary(m_red)$sigma

rho_sim <- as.numeric(
  acf(resid(m_red), plot = FALSE, lag.max = 1)$acf[2]
)

# Keep rho within a stationary range.
rho_sim <- max(min(rho_sim, 0.90), -0.50)

cat("\n== Design-matched simulation settings ==\n")
cat("Model: delta ~ time + covid + blm + election\n")
cat("Observations:", nobs(m_red), "\n")
cat("Residual degrees of freedom:", df.residual(m_red), "\n")
cat("Residual SD:", round(sigma_sim, 4), "\n")
cat("Estimated residual AR(1) rho:", round(rho_sim, 3), "\n")
cat("Newey-West lag:", NW_LAG, "\n")
cat("Simulations per effect size:", B_SIM, "\n")

# Generate AR(1) errors

sim_noise_primary <- function() {
  
  # Convert the marginal residual SD to the innovation SD required by arima.sim().
  innovation_sd <- sigma_sim * sqrt(1 - rho_sim^2)
  
  if (abs(rho_sim) < 1e-8) {
    return(rnorm(n, mean = 0, sd = sigma_sim))
  }
  
  as.numeric(
    arima.sim(
      model = list(ar = rho_sim),
      n = n,
      sd = innovation_sd
    )
  )
}

# Test each simulated coefficient

# Use the same Newey-West settings and residual degrees of freedom as the main model.

get_hac_p_sim <- function(y, target) {
  
  d <- delta_df
  d$y <- y
  
  fit <- lm(
    y ~ time + covid + blm + election,
    data = d
  )
  
  V <- NeweyWest(
    fit,
    lag = NW_LAG,
    prewhite = FALSE,
    adjust = TRUE
  )
  
  estimate <- coef(fit)[target]
  se <- sqrt(diag(V))[target]
  
  if (!is.finite(estimate) || !is.finite(se) || se <= 0) {
    return(NA_real_)
  }
  
  t_stat <- estimate / se
  
  2 * pt(
    abs(t_stat),
    df = df.residual(fit),
    lower.tail = FALSE
  )
}

# Simulate rejection rates for one coefficient

simulate_primary_power <- function(
    target,
    effects,
    B = B_SIM
) {
  
  stopifnot(
    target %in% c("time", "covid", "blm", "election")
  )
  
  # Keep the design fixed and hold the other coefficients at their fitted values.
  X <- model.matrix(m_red)
  
  beta_base <- coef(m_red)
  
  results <- lapply(effects, function(effect) {
    
    beta_sim <- beta_base
    
    # Change only the coefficient being tested.
    beta_sim[target] <- effect
    
    # Calculate the expected outcome for these coefficient values.
    mu <- as.numeric(X %*% beta_sim)
    
    p_values <- replicate(B, {
      
      y_sim <- mu + sim_noise_primary()
      
      get_hac_p_sim(
        y = y_sim,
        target = target
      )
    })
    
    data.frame(
      coefficient = target,
      effect = effect,
      power = mean(p_values < ALPHA_SIM, na.rm = TRUE),
      valid_simulations = sum(is.finite(p_values)),
      simulations = B
    )
  })
  
  do.call(rbind, results)
}

# Set up the effect-size grids

# Time runs from 1 to 18. Multiply the slope by 17 intervals to get the
# total fitted change across the study period.

slope_grid <- sigma_sim * seq(
  0,
  SLOPE_MAX_RESID_SD,
  length.out = N_GRID
)

# Event effects are conditional on the other terms in the model.
event_grid <- sigma_sim * seq(
  0,
  EVENT_MAX_RESID_SD,
  length.out = N_GRID
)

# Run the simulations for each model term

cat("\n== Running design-matched simulations ==\n")

pw_time <- simulate_primary_power(
  target = "time",
  effects = slope_grid
)

pw_covid <- simulate_primary_power(
  target = "covid",
  effects = event_grid
)

pw_blm <- simulate_primary_power(
  target = "blm",
  effects = event_grid
)

pw_election <- simulate_primary_power(
  target = "election",
  effects = event_grid
)

# Express the simulated effects in SD units

pw_time <- pw_time %>%
  mutate(
    effect_residual_SD_per_month = effect / sigma_sim,
    total_change = effect * (n - 1),
    total_change_SD = total_change / sd(delta_df$delta)
  )

pw_covid <- pw_covid %>%
  mutate(
    effect_SD = effect / sd(delta_df$delta),
    effect_residual_SD = effect / sigma_sim
  )

pw_blm <- pw_blm %>%
  mutate(
    effect_SD = effect / sd(delta_df$delta),
    effect_residual_SD = effect / sigma_sim
  )

pw_election <- pw_election %>%
  mutate(
    effect_SD = effect / sd(delta_df$delta),
    effect_residual_SD = effect / sigma_sim
  )

# Combine the simulation results

power_curves <- bind_rows(
  pw_time,
  pw_covid,
  pw_blm,
  pw_election
)

# Find the first effect size to reach the target rejection rate

# Report the first grid point to reach 80%; do not extrapolate beyond the grid.

extract_mde <- function(tab, coefficient, effect_column) {
  
  x <- tab %>%
    filter(
      coefficient == !!coefficient,
      valid_simulations > 0
    ) %>%
    arrange(effect)
  
  eligible <- x %>%
    filter(power >= TARGET_POWER)
  
  if (nrow(eligible) == 0) {
    
    return(data.frame(
      coefficient = coefficient,
      detectable_effect = NA_real_,
      detectable_effect_SD = NA_real_,
      achieved_power = NA_real_,
      reached_80_percent = FALSE
    ))
  }
  
  first <- eligible[1, ]
  
  data.frame(
    coefficient = coefficient,
    detectable_effect = first$effect,
    detectable_effect_SD = first[[effect_column]],
    achieved_power = first$power,
    reached_80_percent = TRUE
  )
}

mde_time <- extract_mde(
  pw_time,
  "time",
  "total_change_SD"
)

mde_covid <- extract_mde(
  pw_covid,
  "covid",
  "effect_SD"
)

mde_blm <- extract_mde(
  pw_blm,
  "blm",
  "effect_SD"
)

mde_election <- extract_mde(
  pw_election,
  "election",
  "effect_SD"
)

mde_primary <- bind_rows(
  mde_time,
  mde_covid,
  mde_blm,
  mde_election
)

cat("\n== Design-matched sensitivity: 80% power thresholds ==\n")
cat("All effects tested using the complete primary model.\n")
cat("Inference: Newey-West HAC, lag =", NW_LAG, "\n")
print(mde_primary, digits = 3, row.names = FALSE)

# Save the simulation results

write.csv(
  power_curves,
  file.path(OUT_DIR, "power_curves_primary_model_HAC.csv"),
  row.names = FALSE
)

write.csv(
  mde_primary,
  file.path(OUT_DIR, "power_thresholds_primary_model_HAC.csv"),
  row.names = FALSE
)

# Plot the simulated power

if (requireNamespace("ggplot2", quietly = TRUE)) {
  
  library(ggplot2)
  
  power_plot_data <- power_curves %>%
    mutate(
      effect_size = case_when(
        coefficient == "time" ~ total_change_SD,
        TRUE ~ effect_SD
      ),
      effect_label = case_when(
        coefficient == "time" ~ "Total trend change (SD)",
        TRUE ~ "Event-period shift (SD)"
      ),
      coefficient = factor(
        coefficient,
        levels = c("time", "covid", "blm", "election"),
        labels = c("Linear trend", "COVID-19", "BLM", "Election")
      )
    )
  
  p_power <- ggplot(
    power_plot_data,
    aes(x = effect_size, y = power)
  ) +
    geom_line(linewidth = 0.75) +
    geom_hline(
      yintercept = TARGET_POWER,
      linetype = "dashed",
      linewidth = 0.4
    ) +
    geom_hline(
      yintercept = ALPHA_SIM,
      linetype = "dotted",
      linewidth = 0.4
    ) +
    facet_wrap(~ coefficient, scales = "free_x") +
    scale_y_continuous(
      limits = c(0, 1),
      breaks = seq(0, 1, by = 0.2)
    ) +
    labs(
      x = "Effect size (SD units)",
      y = "Estimated rejection probability",
      title = "Design-matched sensitivity analysis",
      subtitle = paste0(
        "Complete primary model; N = ", n,
        "; Newey-West HAC lag = ", NW_LAG,
        "; AR(1) rho = ", round(rho_sim, 2)
      ),
      caption = paste0(
        "Conditional simulations retain the observed estimates of the ",
        "other model coefficients. The 80% line is a reference threshold."
      )
    ) +
    theme_minimal(base_size = 10) +
    theme(
      axis.text = element_text(size = 8),
      axis.title = element_text(size = 9),
      strip.text = element_text(face = "bold", size = 9),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(
        colour = "#D0D0D0",
        fill = NA,
        linewidth = 0.35
      )
    )
  
  print(p_power)
  
  ggsave(
    file.path(OUT_DIR, "power_curves_primary_model_HAC.png"),
    p_power,
    width = 7.2,
    height = 5,
    units = "in",
    dpi = 600,
    bg = "white"
  )
}

cat("\nDesign-matched simulation completed.\n")
cat("Results saved to:", normalizePath(OUT_DIR), "\n")

# TOST equivalence tests

tost_slope <- function(y, sesoi_sd = SESOI_TOTAL_SD) {
  t <- seq_along(y); m <- lm(y ~ t); nn <- length(y)
  bound <- sesoi_sd * sd(y) / (nn - 1)
  est <- unname(coef(m)["t"]); se <- unname(sqrt(diag(vcovHC(m, type = "HC3")))["t"]); dfr <- df.residual(m)
  p_lo <- pt((est + bound) / se, dfr, lower.tail = FALSE)
  p_hi <- pt((est - bound) / se, dfr, lower.tail = TRUE)
  ci90 <- est + c(-1, 1) * qt(.95, dfr) * se
  data.frame(slope = est, bound = bound, ci90_lo = ci90[1], ci90_hi = ci90[2],
             total_change_sd_lo = ci90[1] * (nn - 1) / sd(y), total_change_sd_hi = ci90[2] * (nn - 1) / sd(y),
             p_tost = max(p_lo, p_hi), equivalent = max(p_lo, p_hi) < .05)
}
tost_all <- rbind(cbind(series = "delta (z-scored)", tost_slope(delta_df$delta)),
                  do.call(rbind, lapply(foundation_vars, function(v) cbind(series = v, tost_slope(df_month[[v]])))))
cat("\n== TOST on the linear time slope (HC3 SE; bounds = +/-", SESOI_TOTAL_SD, "SD total change) ==\n")
print(tost_all, digits = 3)
write.csv(tost_all, file.path(OUT_DIR, "tost_slopes.csv"), row.names = FALSE)

# Sensitivity to event coding and incomplete months

fit_sens <- function(d, label) {
  m <- lm(f_red, data = d)
  r <- nw_tab(m, NW_LAG)
  
  cbind(
    analysis = label,
    n = nrow(d),
    r[r$term != "(Intercept)", c("term", "est", "se", "p")]
  )
}

# Alternative windows follow the event periods shown in the figure. A month
# is included if it overlaps an event period, so May 2020 is included in both
# the COVID-19 and BLM indicators.

alt <- delta_df

alt$covid <- as.integer(
  alt$month >= as.Date("2020-01-01") &
    alt$month <= as.Date("2020-05-01")
)

alt$blm <- as.integer(
  alt$month >= as.Date("2020-05-01") &
    alt$month <= as.Date("2020-08-01")
)

alt$election <- as.integer(
  alt$month >= as.Date("2020-10-01") &
    alt$month <= as.Date("2020-11-01")
)

# Compare the primary and alternative event coding.

sens <- rbind(
  fit_sens(delta_df, "Primary event coding"),
  fit_sens(
    alt,
    "Alternative coding: COVID Jan-May; BLM May-Aug; Election Oct-Nov"
  ),
  
  # Check whether the estimates change when March or September 2020 is removed.
  
  fit_sens(
    delta_df[delta_df$month != as.Date("2020-03-01"), ],
    "Leave out March 2020"
  ),
  
  fit_sens(
    delta_df[delta_df$month != as.Date("2020-09-01"), ],
    "Leave out September 2020"
  )
)

# Refit the model after excluding months with low volume or coverage.

if (length(thin_months) > 0 && length(thin_months) < n - 8) {
  sens <- rbind(
    sens,
    fit_sens(
      delta_df[!(delta_df$month %in% thin_months), ],
      paste(
        "Exclude flagged thin months:",
        paste(format(thin_months, "%b %Y"), collapse = ", ")
      )
    )
  )
}

cat("\n== Primary model: event-coding and thin-month sensitivity ==\n")
cat(
  "Note: month-deletion analyses leave gaps in the monthly series;",
  "Newey-West HAC treats retained observations as consecutive.\n"
)

print(sens, digits = 3, row.names = FALSE)

write.csv(
  sens,
  file.path(OUT_DIR, "sensitivity_coding_and_thin_months.csv"),
  row.names = FALSE
)

# Bayesian analyses

# Run the BayesFactor comparisons on the monthly series prepared above.
# These include predictor and model comparisons, posterior estimates for
# the four terms in the primary model, and a separate time-only slope. The
# time-only slope is not adjusted for the event indicators. BF01 values above
# 1 favour the simpler model; values below 1 favour the more complex model.

dm <- as.data.frame(
  delta_df[, c("delta", "time", "covid", "blm", "election")]
)

dm$t_covid <- dm$time * dm$covid
dm$t_blm   <- dm$time * dm$blm
dm$t_elec  <- dm$time * dm$election

BAYES_RSCales <- c(0.2, 0.354, 0.5, 0.707, 1)
BAYES_POSTERIOR_RSCales <- c(0.2, 0.354, 1)
BAYES_DEFAULT_Rscale <- 0.354

bayes_ok <- FALSE

bf_tab <- data.frame()
bf_predictor_tab <- data.frame()
posterior_tab <- data.frame()
posterior_time_only <- numeric(0)
posterior_time_only_summary <- data.frame()
brm_tab <- data.frame()

if (!has_bf) {
  
  message(
    "Bayesian analyses skipped: BayesFactor is not installed. ",
    "Install with install.packages(\"BayesFactor\")."
  )
  
} else {
  
  tryCatch({
    
    # Return BF10 for a linear model.

    bf10_of <- function(formula, rscale) {
      
      fit_bf <- BayesFactor::lmBF(
        formula,
        data = dm,
        rscaleCont = rscale,
        progress = FALSE
      )
      
      as.numeric(
        BayesFactor::extractBF(fit_bf)$bf[1]
      )
    }
    
    # Specify the models to compare.

    predictors <- c("time", "covid", "blm", "election")
    
    f_null <- delta ~ 1
    
    f_time <- delta ~ time
    
    f_events <- delta ~ covid + blm + election
    
    f_main <- delta ~ time + covid + blm + election
    
    f_interaction <- delta ~
      time + covid + blm + election +
      t_covid + t_blm + t_elec
    
    # Predictor-level Bayes factors
    # Compare the full model with a model omitting one predictor at a time.
    # BF01 above 1 favours the model without that predictor.
    
    cat("\n== A1. Predictor-level Bayes factors ==\n")
    
    bf_predictor_tab <- do.call(
      rbind,
      lapply(BAYES_RSCales, function(r) {
        
        bf_full <- bf10_of(f_main, r)
        
        do.call(
          rbind,
          lapply(predictors, function(pred) {
            
            remaining <- setdiff(predictors, pred)
            
            f_reduced <- reformulate(
              remaining,
              response = "delta"
            )
            
            bf_reduced <- bf10_of(f_reduced, r)
            
            data.frame(
              rscale = r,
              predictor = pred,
              BF10_full_model = bf_full,
              BF10_model_excluding_predictor = bf_reduced,
              BF01_excluding_vs_full = bf_reduced / bf_full
            )
          })
        )
      })
    )
    
    print(bf_predictor_tab, digits = 4, row.names = FALSE)
    
    write.csv(
      bf_predictor_tab,
      file.path(OUT_DIR, "bayesfactor_predictor_level.csv"),
      row.names = FALSE
    )
    
    # Model-level Bayes factors
    
    cat("\n== A2. Model-level Bayes factors ==\n")
    
    bf_tab <- do.call(
      rbind,
      lapply(BAYES_RSCales, function(r) {
        
        bf_time <- bf10_of(f_time, r)
        bf_events <- bf10_of(f_events, r)
        bf_main <- bf10_of(f_main, r)
        bf_interaction <- bf10_of(f_interaction, r)
        
        data.frame(
          rscale = r,
          
          BF01_time_vs_null =
            1 / bf_time,
          
          BF01_events_only_vs_null =
            1 / bf_events,
          
          BF01_events_given_time =
            bf_time / bf_main,
          
          BF01_interactions_given_main =
            bf_main / bf_interaction
        )
      })
    )
    
    print(bf_tab, digits = 4, row.names = FALSE)
    
    write.csv(
      bf_tab,
      file.path(OUT_DIR, "bayesfactor_model_comparisons.csv"),
      row.names = FALSE
    )
    
    # Posterior estimates for the four terms in the primary model
    
    cat("\n== A3. Posterior estimates for four predictors ==\n")
    
    posterior_tab <- do.call(
      rbind,
      lapply(BAYES_POSTERIOR_RSCales, function(r) {
        
        bf_main <- BayesFactor::lmBF(
          f_main,
          data = dm,
          rscaleCont = r,
          progress = FALSE
        )
        
        draws <- BayesFactor::posterior(
          bf_main,
          iterations = B_BAYES,
          progress = FALSE
        )
        
        draws <- as.data.frame(as.matrix(draws))
        
        missing_terms <- setdiff(
          predictors,
          names(draws)
        )
        
        if (length(missing_terms) > 0) {
          stop(
            "Posterior output is missing coefficient(s): ",
            paste(missing_terms, collapse = ", ")
          )
        }
        
        do.call(
          rbind,
          lapply(predictors, function(pred) {
            
            x <- draws[[pred]]
            
            data.frame(
              rscale = r,
              predictor = pred,
              posterior_median = median(x, na.rm = TRUE),
              CrI_95_lower = unname(
                quantile(x, 0.025, na.rm = TRUE)
              ),
              CrI_95_upper = unname(
                quantile(x, 0.975, na.rm = TRUE)
              ),
              probability_coefficient_positive =
                mean(x > 0, na.rm = TRUE),
              posterior_draws = sum(is.finite(x))
            )
          })
        )
      })
    )
    
    print(posterior_tab, digits = 4, row.names = FALSE)
    
    write.csv(
      posterior_tab,
      file.path(OUT_DIR, "bayesian_posterior_four_predictors.csv"),
      row.names = FALSE
    )
    
    # Time-only posterior slope
    # This is a separate model, not the adjusted time coefficient above.
    
    cat("\n== A3b. Time-only posterior slope ==\n")
    
    bf_time_only <- BayesFactor::lmBF(
      f_time,
      data = dm,
      rscaleCont = BAYES_DEFAULT_Rscale,
      progress = FALSE
    )
    
    time_draws <- BayesFactor::posterior(
      bf_time_only,
      iterations = B_BAYES,
      progress = FALSE
    )
    
    time_draws <- as.data.frame(as.matrix(time_draws))
    
    if (!"time" %in% names(time_draws)) {
      stop("Time-only posterior output does not contain a 'time' coefficient.")
    }
    
    posterior_time_only <- time_draws$time
    
    posterior_time_only_summary <- data.frame(
      model = "Time-only",
      rscale = BAYES_DEFAULT_Rscale,
      posterior_median = median(
        posterior_time_only,
        na.rm = TRUE
      ),
      CrI_95_lower = unname(
        quantile(posterior_time_only, 0.025, na.rm = TRUE)
      ),
      CrI_95_upper = unname(
        quantile(posterior_time_only, 0.975, na.rm = TRUE)
      ),
      probability_slope_positive = mean(
        posterior_time_only > 0,
        na.rm = TRUE
      ),
      OLS_slope = unname(
        coef(lm(delta ~ time, data = dm))["time"]
      ),
      posterior_draws = sum(
        is.finite(posterior_time_only)
      )
    )
    
    print(
      posterior_time_only_summary,
      digits = 4,
      row.names = FALSE
    )
    
    write.csv(
      posterior_time_only_summary,
      file.path(OUT_DIR, "bayesian_time_only_posterior.csv"),
      row.names = FALSE
    )
    
    # Record that the Bayesian analyses completed.

    bayes_ok <- TRUE
    
    cat("\nBayesian analyses completed successfully.\n")
    cat("Prior widths for model comparisons:",
        paste(BAYES_RSCales, collapse = ", "), "\n")
    cat("Prior widths for four-predictor posterior estimates:",
        paste(BAYES_POSTERIOR_RSCales, collapse = ", "), "\n")
    cat("Time-only posterior prior width:",
        BAYES_DEFAULT_Rscale, "\n")
    cat("Posterior draws per model:", B_BAYES, "\n")
    
    cat(
      "\nInterpretation reminder: the A3 time coefficient is conditional",
      "on the event indicators. The A3b time-only slope is estimated",
      "without those indicators. They are different model specifications.\n"
    )
    
    cat(
      "\nThe interaction model is supplementary. Its election-month",
      "leverage is extremely high in the frequentist analysis, so",
      "the interaction Bayes factor should be interpreted cautiously.\n"
    )
    
  }, error = function(e) {
    
    bayes_ok <<- FALSE
    
    message(
      "Bayesian analysis section failed: ",
      conditionMessage(e)
    )
    
  })
}

# Optional brms analysis (off by default)

if (has_brm && RUN_BRMS) {
  
  tryCatch({
    
    brm_tab <- do.call(
      rbind,
      lapply(c(0.1, 0.5, 1), function(s) {
        
        fit <- brms::brm(
          formula = f_red,
          data = dm,
          prior = c(
            brms::prior_string(
              paste0("normal(0, ", s, ")"),
              class = "b"
            ),
            brms::prior(
              normal(0, 1),
              class = "Intercept"
            ),
            brms::prior(
              exponential(1),
              class = "sigma"
            )
          ),
          chains = 4,
          iter = 4000,
          warmup = 1000,
          seed = 42,
          refresh = 0,
          sample_prior = "yes",
          control = list(adapt_delta = 0.99),
          file = file.path(
            OUT_DIR,
            paste0("brms_prior_", s)
          ),
          file_refit = "on_change"
        )
        
        ps <- brms::posterior_summary(
          fit,
          variable = "b_time"
        )
        
        data.frame(
          prior_sd = s,
          time_mean = ps[1, "Estimate"],
          time_l95 = ps[1, "Q2.5"],
          time_u95 = ps[1, "Q97.5"],
          BF01_time = brms::hypothesis(
            fit,
            "time = 0"
          )$hypothesis$Evid.Ratio,
          max_Rhat = max(
            brms::rhat(fit),
            na.rm = TRUE
          )
        )
      })
    )
    
    print(brm_tab, digits = 4, row.names = FALSE)
    
    write.csv(
      brm_tab,
      file.path(OUT_DIR, "brms_prior_sensitivity.csv"),
      row.names = FALSE
    )
    
  }, error = function(e) {
    
    message(
      "brms section skipped: ",
      conditionMessage(e)
    )
    
  })
  
}

# 5. Effect sizes and temporal trends

# Percentage change is relative to the mean for the first seven months (Phase 1).
# Kendall's test does not account for autocorrelation. The lag-1 ACF is
# reported to help assess this when interpreting the p-value.
effect_sizes <- function(y, time = seq_along(y), baseline_n = BASELINE_N, pct = TRUE) {
  nn <- length(y); span <- max(time) - min(time)
  b  <- unname(coef(lm(y ~ time))[2])
  pr <- combn(nn, 2)
  sen <- median((y[pr[2, ]] - y[pr[1, ]]) / (time[pr[2, ]] - time[pr[1, ]]))
  mk  <- suppressWarnings(cor.test(time, y, method = "kendall"))
  base <- mean(y[seq_len(baseline_n)])
  data.frame(n = nn, ols_slope = b, sen_slope = sen, total_change_sen = sen * span, baseline = base,
             pct_change_from_baseline = ifelse(!pct || abs(base) < 1e-8, NA, 100 * sen * span / base),
             std_slope_sd_per_month = sen / sd(y), total_change_in_sd = sen * span / sd(y),
             acf1 = acf(resid(lm(y ~ time)), plot = FALSE, lag.max = 1)$acf[2],
             mk_tau = unname(mk$estimate), mk_p = mk$p.value)
}
es_delta <- cbind(series = "delta (z-scored)", effect_sizes(delta_df$delta, pct = FALSE))
es_found <- do.call(rbind, lapply(foundation_vars, function(v) cbind(series = v, effect_sizes(df_month[[v]]))))
es_found$mk_p_fdr <- p.adjust(es_found$mk_p, method = "BH")
cat("\n== Effect sizes: moral polarity ==\n");  print(es_delta, digits = 3)
cat("\n== Effect sizes: ten foundation series (raw monthly means) ==\n"); print(es_found, digits = 3)
write.csv(es_delta, file.path(OUT_DIR, "effect_sizes_delta.csv"), row.names = FALSE)
write.csv(es_found, file.path(OUT_DIR, "effect_sizes_foundations.csv"), row.names = FALSE)
# This function can also be used with other monthly series, such as populism
# or liberalism, when the input is a numeric vector.

# 6. Plot the monthly series

if (MAKE_FIGURE && requireNamespace("ggplot2", quietly = TRUE) && requireNamespace("patchwork", quietly = TRUE)) {
  library(ggplot2); library(patchwork)
  
  win <- function(flag) { m <- delta_df$month[flag == 1]; c(as.character(min(m)), as.character(max(m) %m+% months(1))) }
  
  event_df <- data.frame(event = factor(c("COVID-19", "BLM", "Election"), levels = c("COVID-19", "BLM", "Election")),
                         rbind(win(delta_df$covid), win(delta_df$blm), win(delta_df$election)), stringsAsFactors = FALSE)
  names(event_df)[2:3] <- c("xmin", "xmax")
  event_df$xmin <- as.Date(event_df$xmin); event_df$xmax <- as.Date(event_df$xmax)
  
  # Colours used for the event windows and moral foundations.
  event_cols <- c("COVID-19" = "#003366", "BLM" = "#800000", "Election" = "#E69F00")
  col_map <- c("Vice" = "#A23B3B", "Virtue" = "#2C6E91"); dark_green <- "#1B5E20"
  
  my_x <- scale_x_date(limits = c(WINDOW_START, max(delta_df$month) %m+% months(1)),
                       date_labels = "%b\n%Y", date_breaks = "3 months", expand = c(0.01, 0.01))
  
  bands <- geom_rect(data = event_df, aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = event),
                     inherit.aes = FALSE, alpha = 0.10, colour = NA)
  
  th <- theme_minimal(base_size = 10) +
    theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5, size = 8, colour = "#333333"),
      axis.text.y = element_text(size = 8, colour = "#333333"),
      axis.title = element_text(size = 9),
      axis.title.y = element_text(margin = margin(r = 8)),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(colour = "#E5E5E5", linewidth = 0.35),
      panel.border = element_rect(colour = "#D0D0D0", fill = NA, linewidth = 0.35),
      panel.spacing = grid::unit(0.8, "lines"),
      strip.text = element_text(face = "bold", size = 10, colour = "#222222"),
      strip.background = element_blank(),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.text = element_text(size = 8),
      legend.title = element_text(size = 8),
      legend.key.size = grid::unit(0.4, "cm"),
      plot.caption = element_text(size = 8, hjust = 0, colour = "#444444",
                                  lineheight = 1.15, margin = margin(t = 8)),
      plot.tag = element_text(face = "bold", size = 11)
    )
  
  # Panel A: foundation-level trajectories

  p1 <- ggplot(long, aes(month, z, colour = system, group = system)) + bands +
    geom_line(linewidth = 0.65) +
    geom_point(size = 1.5) +
    facet_wrap(~foundation, ncol = 2) +
    scale_colour_manual(values = col_map) +
    scale_fill_manual(values = event_cols, name = "Event window") +
    labs(x = NULL, y = "z-score", tag = "A") +
    my_x + th +
    theme(
      plot.margin = margin(t = 5.5, r = 5.5, b = 5.5, l = 5.5),
      plot.tag.position = c(0, 1)
    )
  
  # Panel B: vice-virtue difference

  p2 <- ggplot(delta_df, aes(month, delta)) + bands +
    geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.4) +
    geom_line(colour = dark_green, linewidth = 0.75) +
    geom_point(colour = dark_green, size = 1.7) +
    scale_fill_manual(values = event_cols, name = "Event window") +
    labs(x = NULL, y = "Delta (vice - virtue)", tag = "B") +
    my_x + th +
    theme(
      legend.position = "none",
      plot.margin = margin(t = 5.5, r = 5.5, b = 5.5, l = 5.5),
      plot.tag.position = c(0, 1.1)
    )
  
  # Panel C: month-to-month change in delta

  delta_df$delta_change <- c(NA, diff(delta_df$delta))
  
  p3 <- ggplot(delta_df, aes(month, delta_change)) + bands +
    geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.4) +
    geom_line(colour = dark_green, linewidth = 0.75, na.rm = TRUE) +
    geom_point(colour = dark_green, size = 1.7, na.rm = TRUE) +
    scale_fill_manual(values = event_cols, name = "Event window") +
    labs(x = NULL, y = "Month-to-month change in delta", tag = "C") +
    my_x + th +
    theme(
      legend.position = "none",
      plot.margin = margin(t = 5.5, r = 5.5, b = 5.5, l = 5.5),
      plot.tag.position = c(0, 1.1)
    )
  
  # Combine the three panels

  final_plot <- (p1 / patchwork::plot_spacer() / p2 /
                   patchwork::plot_spacer() / p3) +
    plot_layout(
      heights = c(2.4, 0.18, 1, 0.18, 1),
      guides = "collect"
    ) +
    plot_annotation(
      caption = paste(strwrap(paste0(
        "N = ", n, " monthly observations. Shaded bands mark the months coded 1 in the event indicators. ",
        "Delta is the difference between z-scored vice and virtue means. ",
        "Monthly change represents the first difference in delta."),
        width = 110), collapse = "\n")
    ) &
    theme(
      legend.position = "bottom",
      plot.tag = element_text(face = "bold", size = 11)
    )
  
  # Display the figure

  print(final_plot)
  
  # Save the figure

  ggsave(file.path(OUT_DIR, "FINAL_FIGURE.png"), final_plot,
         width = 7.2, height = 10, units = "in", dpi = 600, bg = "white")
  
  ggsave(file.path(OUT_DIR, "FINAL_FIGURE.tiff"), final_plot,
         width = 7.2, height = 10, units = "in", dpi = 600,
         compression = "lzw", bg = "white")
  
  ggsave(file.path(OUT_DIR, "FINAL_FIGURE.pdf"), final_plot,
         width = 7.2, height = 10, units = "in", device = "pdf", bg = "white")
  
}

cat("\nDone. Tables and figures written to:", normalizePath(OUT_DIR), "\n")

# Results report
# Print the results calculated above; no models are refitted here.

# Formatting helpers

report_heading <- function(title) {
  cat("\n\n")
  cat(strrep("=", 90), "\n")
  cat(toupper(title), "\n")
  cat(strrep("=", 90), "\n")
}

report_subheading <- function(title) {
  cat("\n", strrep("-", 75), "\n", sep = "")
  cat(title, "\n")
  cat(strrep("-", 75), "\n", sep = "")
}

report_table <- function(x, digits = 3) {
  if (is.null(x) || !NROW(x)) {
    cat("No results available.\n")
  } else {
    print(
      as.data.frame(x),
      digits = digits,
      row.names = FALSE,
      right = FALSE
    )
  }
}

report_value <- function(label, value) {
  cat(sprintf("%-42s %s\n", paste0(label, ":"), value))
}

# Widen the console for the tables below.
old_width <- getOption("width")
options(width = 160)

# Data description and monthly coverage

report_heading("1. DATA DESCRIPTION AND MONTHLY COVERAGE")

report_value("Raw rows read", format(nrow(df), big.mark = ","))
report_value("Unparseable dates", n_bad)
report_value("Rows outside analysis window", nrow(outside))
report_value("Monthly observations", n)
report_value("First month", format(min(delta_df$month), "%B %Y"))
report_value("Last month", format(max(delta_df$month), "%B %Y"))
report_value("Total comments in analysis window", format(sum(cover$n_comments), big.mark = ","))
report_value("Delta standard deviation", round(sd(delta_df$delta), 4))

report_subheading("Monthly comment counts and date coverage")

coverage_report <- cover %>%
  mutate(
    month = format(month, "%b %Y"),
    n_comments = format(n_comments, big.mark = ","),
    coverage = paste0(n_days, "/", days_in_month),
    pct_days = paste0(pct_days, "%"),
    flag_thin = ifelse(flag_thin, "Yes", "No")
  ) %>%
  select(month, n_comments, coverage, pct_days, flag_thin)

report_table(coverage_report)

report_value(
  "Months with at least one uncovered day",
  paste0(sum(cover$n_days < cover$days_in_month), " of ", nrow(cover))
)

report_value(
  "Range of days covered",
  paste0(min(cover$pct_days), "% to ", max(cover$pct_days), "%")
)

report_value(
  "Months flagged as thin/gappy",
  if (length(thin_months)) {
    paste(format(thin_months, "%b %Y"), collapse = ", ")
  } else {
    "None"
  }
)

report_subheading("Event-period coding")

event_coding_report <- data.frame(
  Event = c("COVID-19", "Black Lives Matter", "US election"),
  Months = c(
    paste(format(COVID_MONTHS, "%b %Y"), collapse = ", "),
    paste(format(BLM_MONTHS, "%b %Y"), collapse = ", "),
    paste(format(ELECTION_MONTHS, "%b %Y"), collapse = ", ")
  )
)

report_table(event_coding_report)

report_value(
  "Reference months",
  paste(
    format(
      delta_df$month[
        rowSums(delta_df[, c("covid", "blm", "election")]) == 0
      ],
      "%b %Y"
    ),
    collapse = ", "
  )
)

# Primary regression model

report_heading("2. PRIMARY REDUCED REGRESSION MODEL")

report_value("Model", "delta ~ time + covid + blm + election")
report_value("Observations", nobs(m_red))
report_value("Residual degrees of freedom", df.residual(m_red))
report_value("R-squared", round(summary(m_red)$r.squared, 3))
report_value("Adjusted R-squared", round(summary(m_red)$adj.r.squared, 3))
report_value("Newey-West lag", NW_LAG)

report_subheading("Primary model: OLS, Newey-West HAC and HC3")

primary_report <- red_tab %>%
  select(
    term,
    est,
    se_ols,
    p_ols,
    se_hac,
    p_hac,
    p_hc3,
    lo,
    hi
  ) %>%
  rename(
    Coefficient = term,
    Estimate = est,
    OLS_SE = se_ols,
    OLS_p = p_ols,
    HAC_SE = se_hac,
    HAC_p = p_hac,
    HC3_p = p_hc3,
    HAC_CI_lower = lo,
    HAC_CI_upper = hi
  )

report_table(primary_report, digits = 4)

cat(
  "\nNote: HAC confidence intervals use Newey-West standard errors",
  "with a small-sample adjustment and a t reference distribution",
  "with", df.residual(m_red), "residual degrees of freedom.\n"
)

report_subheading("Residual autocorrelation diagnostics")

cat("\nBreusch-Godfrey test:\n")
print(bgtest(m_red, order = 1))

cat("\nDurbin-Watson test:\n")
print(dwtest(m_red))

report_value(
  "Lag-1 residual autocorrelation",
  round(acf(resid(m_red), plot = FALSE, lag.max = 1)$acf[2], 3)
)

report_subheading("Newey-West lag sensitivity")

lag_report <- reshape(
  lag_tab[, c("lag", "term", "p")],
  idvar = "term",
  timevar = "lag",
  direction = "wide"
)

names(lag_report) <- sub("^p\\.", "HAC_p_lag_", names(lag_report))

report_table(lag_report, digits = 4)

# Supplementary interaction model

report_heading("3. SUPPLEMENTARY FULL MODEL WITH INTERACTIONS")

report_value("Model", "delta ~ time + covid + blm + election + time:event interactions")
report_value("Observations", nobs(m_full))
report_value("Number of parameters", length(coef(m_full)))
report_value("Residual degrees of freedom", df.residual(m_full))
report_value("R-squared", round(summary(m_full)$r.squared, 3))
report_value("Adjusted R-squared", round(summary(m_full)$adj.r.squared, 3))

report_subheading("Full-model OLS coefficients")

full_report <- as.data.frame(summary(m_full)$coefficients)
full_report$Term <- rownames(full_report)
rownames(full_report) <- NULL
full_report <- full_report %>%
  select(Term, everything())

report_table(full_report, digits = 4)

report_subheading("Mean event effects at observed event months")

mean_event_report <- data.frame(
  Event = names(me_full),
  Mean_event_effect = as.numeric(me_full)
)

report_table(mean_event_report, digits = 4)

report_value(
  "Maximum leverage",
  round(max(lev), 4)
)

report_value(
  "Months with leverage above .99",
  if (any(lev > .99)) {
    paste(format(delta_df$month[lev > .99], "%b %Y"), collapse = ", ")
  } else {
    "None"
  }
)

cat(
  "\nInterpretation note: the full interaction model is supplementary.",
  "Election-month leverage is approximately 1, so its election-related",
  "coefficients should not be interpreted as reliable inferential estimates.\n"
)

# Wild bootstrap intervals

report_heading("4. WILD BOOTSTRAP CONFIDENCE INTERVALS")

report_value("Bootstrap draws", B_BOOT)
report_value("Bootstrap method", "Webb weights with leverage adjustment")

report_subheading("95% percentile confidence intervals")

bootstrap_report <- wb_tab %>%
  mutate(
    CI_excludes_zero = ifelse(ci_excludes_zero, "Yes", "No")
  ) %>%
  select(
    Term = term,
    Estimate = estimate,
    CI_lower = ci_low,
    CI_upper = ci_high,
    CI_excludes_zero
  )

report_table(bootstrap_report, digits = 4)

# Leave-one-month-out analysis

report_heading("5. LEAVE-ONE-MONTH-OUT SENSITIVITY ANALYSIS")

report_value("Refitted models", nrow(loo) / length(unique(loo$term)))

report_subheading("Coefficient ranges across leave-one-month-out models")

loo_report <- loo_sum %>%
  rename(
    Coefficient = term,
    Minimum_estimate = est_min,
    Maximum_estimate = est_max,
    Sign_changes = sign_changes,
    Fits_with_OLS_p_below_.05 = n_p_below_05,
    Total_fits = n_fits
  )

report_table(loo_report, digits = 4)

# Placebo breakpoint analysis

report_heading("6. PLACEBO BREAKPOINT ANALYSIS")

report_value("Actual breakpoint", format(actual_cut, "%d %B %Y"))
report_value("Absolute t-statistic at actual breakpoint", round(t_actual, 3))
report_value(
  "Proportion of candidate cuts with equal or larger absolute t",
  round(mean(plc$abs_t >= t_actual), 3)
)

report_subheading("Candidate placebo breakpoints")

placebo_report <- plc %>%
  mutate(cut = format(cut, "%b %Y")) %>%
  rename(
    Breakpoint = cut,
    Estimate = est,
    T_statistic = t,
    Absolute_T = abs_t
  )

report_table(placebo_report, digits = 3)

cat(
  "\nNote: the proportion is descriptive and should not be interpreted",
  "as a formal permutation p-value.\n"
)

# Simulation sensitivity analysis

report_heading("7. DESIGN-MATCHED SIMULATION SENSITIVITY ANALYSIS")

report_value("Primary model", "delta ~ time + covid + blm + election")
report_value("Observations", n)
report_value("Residual SD used in simulation", round(sigma_sim, 4))
report_value("Estimated AR(1) rho", round(rho_sim, 3))
report_value("Newey-West lag", NW_LAG)
report_value("Simulations per effect size", B_SIM)
report_value("Alpha", ALPHA_SIM)
report_value("Target power", TARGET_POWER)

report_subheading("Approximate 80% detectable effect thresholds")

simulation_report <- mde_primary %>%
  mutate(
    reached_80_percent = ifelse(
      reached_80_percent,
      "Yes",
      "No"
    )
  ) %>%
  rename(
    Coefficient = coefficient,
    Detectable_effect = detectable_effect,
    Detectable_effect_SD = detectable_effect_SD,
    Achieved_power = achieved_power,
    Reached_80_percent = reached_80_percent
  )

report_table(simulation_report, digits = 4)

report_subheading("Simulation grid: power estimates")

power_summary <- power_curves %>%
  group_by(coefficient) %>%
  summarise(
    Minimum_effect = min(effect),
    Maximum_effect = max(effect),
    Minimum_power = min(power, na.rm = TRUE),
    Maximum_power = max(power, na.rm = TRUE),
    Valid_simulations = min(valid_simulations),
    .groups = "drop"
  )

report_table(power_summary, digits = 4)

cat(
  "\nInterpretation note: these are conditional simulation-based",
  "sensitivity estimates. Other coefficients are held at their",
  "observed estimates. They are not retrospective or definitive",
  "power estimates for the study.\n"
)

# TOST equivalence tests

report_heading("8. TOST EQUIVALENCE TESTS")

report_value("Smallest effect of interest", paste0("+/- ", SESOI_TOTAL_SD, " SD total change"))
report_value("Standard error estimator", "HC3")
report_value("Number of series tested", nrow(tost_all))

report_subheading("Equivalence tests for delta and foundation series")

tost_report <- tost_all %>%
  mutate(
    Equivalent = ifelse(equivalent, "Yes", "No")
  ) %>%
  rename(
    Series = series,
    Slope = slope,
    Equivalence_bound = bound,
    CI90_lower = ci90_lo,
    CI90_upper = ci90_hi,
    Total_change_SD_lower = total_change_sd_lo,
    Total_change_SD_upper = total_change_sd_hi,
    TOST_p = p_tost
  )

report_table(tost_report, digits = 4)

cat(
  "\nInterpretation note: failure to establish equivalence does not",
  "demonstrate either stability or meaningful change. These tests",
  "assess linear temporal slopes, not event-period contrasts.\n"
)

# Event coding and month-coverage sensitivity

report_heading("9. EVENT-CODING AND THIN-MONTH SENSITIVITY")

report_subheading("Newey-West sensitivity estimates")

sensitivity_report <- sens %>%
  rename(
    Analysis = analysis,
    N = n,
    Coefficient = term,
    Estimate = est,
    HAC_SE = se,
    HAC_p = p
  )

report_table(sensitivity_report, digits = 4)

cat(
  "\nNote: where months are omitted, HAC treats the remaining",
  "observations as consecutive. Those estimates are approximate.\n"
)

# Bayesian analyses

report_heading("10. INTEGRATED BAYESIAN ANALYSES")

if (isTRUE(bayes_ok)) {
  
  report_value("BayesFactor analyses completed", "Yes")
  report_value("Posterior draws per model", B_BAYES)
  
  # Predictor-level Bayes factors

  report_subheading(
    "A1. Predictor-level Bayes factors conditional on other predictors"
  )
  
  report_table(
    bf_predictor_tab,
    digits = 4
  )
  
  cat(
    "\nInterpretation: BF01_excluding_vs_full compares the model",
    "excluding a predictor with the complete primary model.",
    "Values above 1 favour excluding the predictor; values below",
    "1 favour retaining it, conditional on the other predictors.\n"
  )
  
  # Model-level Bayes factors

  report_subheading(
    "A2. Model-level Bayes factors across prior widths"
  )
  
  report_table(
    bf_tab,
    digits = 4
  )
  
  cat(
    "\nInterpretation: BF01 values above 1 favour the simpler model;",
    "values below 1 favour the more complex model.",
    "The events-given-time comparison evaluates the addition of",
    "the three event indicators to the time-only model.",
    "The interaction comparison evaluates the addition of the",
    "three time-by-event interactions to the main-effects model.\n"
  )
  
  # Four-predictor posterior estimates

  report_subheading(
    "A3. Posterior estimates for all four primary-model predictors"
  )
  
  report_table(
    posterior_tab,
    digits = 4
  )
  
  cat(
    "\nInterpretation: these posterior estimates are conditional",
    "coefficients from the complete reduced model.",
    "The probability column reports the posterior probability",
    "that the relevant coefficient is greater than zero.\n"
  )
  
  # Time-only posterior slope

  report_subheading(
    "A3b. Time-only posterior slope"
  )
  
  report_table(
    posterior_time_only_summary,
    digits = 4
  )
  
  cat(
    "\nInterpretation: this is the time slope from a separate",
    "time-only model. It is not interchangeable with the",
    "conditional time coefficient reported in A3.\n"
  )
  
} else {
  
  cat(
    "The integrated BayesFactor analyses did not complete.",
    "Check the messages printed during Section 4 and confirm",
    "that the BayesFactor package is installed.\n"
  )
  
}

# Optional brms results

report_subheading("Optional brms prior-sensitivity analysis")

if (isTRUE(RUN_BRMS) &&
    is.data.frame(brm_tab) &&
    nrow(brm_tab) > 0) {
  
  report_table(
    brm_tab,
    digits = 4
  )
  
} else {
  
  cat(
    "brms was not run. RUN_BRMS is currently FALSE,",
    "or the optional analysis did not complete.\n"
  )
  
}

# Effect sizes and temporal trends

report_heading("11. EFFECT SIZES AND TEMPORAL TRENDS")

report_subheading("Overall moral polarity (delta)")

delta_effect_report <- es_delta %>%
  select(
    series,
    n,
    ols_slope,
    sen_slope,
    total_change_sen,
    std_slope_sd_per_month,
    total_change_in_sd,
    acf1,
    mk_tau,
    mk_p
  )

report_table(delta_effect_report, digits = 4)

report_subheading("Ten foundation-level series")

foundation_effect_report <- es_found %>%
  select(
    series,
    n,
    ols_slope,
    sen_slope,
    total_change_sen,
    baseline,
    pct_change_from_baseline,
    std_slope_sd_per_month,
    total_change_in_sd,
    acf1,
    mk_tau,
    mk_p,
    mk_p_fdr
  )

report_table(foundation_effect_report, digits = 4)

cat(
  "\nInterpretation note: percentage changes are relative to the",
  BASELINE_N, "month baseline mean. Kendall p-values are unadjusted",
  "for autocorrelation; foundation-level p-values are also shown",
  "with Benjamini-Hochberg FDR adjustment.\n"
)

# Output files

report_heading("12. OUTPUT FILES")

output_files <- list.files(
  OUT_DIR,
  full.names = FALSE
)

cat("Output directory:", normalizePath(OUT_DIR), "\n\n")
cat("Files generated:\n")
cat(paste0(" - ", output_files, collapse = "\n"), "\n")

# End of results report

options(width = old_width)

cat("\n\n")
cat(strrep("=", 90), "\n")
cat("END OF CONSOLIDATED RESULTS REPORT\n")
cat(strrep("=", 90), "\n")

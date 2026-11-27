# =====================================================================
# RUN: (A) Shannon entropy and segmented regression (Table 3, Figure 6)
#         (B) Mann-Kendall and Sen's slope (Figure 2, Table S8)
# =====================================================================

suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(tidyr); library(ggplot2)
})

# ---------------------------------------------------------------------
# 0. FILE PATHS
# ---------------------------------------------------------------------
dir_cfa <- "C:/Users/abiga/OneDrive/Desktop/CFA"
lg_file <- file.path(dir_cfa, "output_lavergarry.lexicon.xlsx")
mz_file <- file.path(dir_cfa, "output_maerz.lexicon.xlsx")
pc_file <- file.path(dir_cfa, "output_policy_agendas_dict.xlsx")
out_dir <- dir_cfa

mz_vars <- c("illiberalism", "democracy", "maintenance_of_power", "reforms")
policy_vars <- c("macroeconomics","civil_rights","healthcare","agriculture","forestry",
                 "labour","immigration","education","environment","energy","fisheries","transportation",
                 "crime","social_welfare","housing","finance","defence","sstc","foreign_trade",
                 "intl_affairs","government_ops","land-water-management","culture","prov_local",
                 "intergovernmental","constitutional_natl_unity","aboriginal","religion")

policy_map <- tibble(category = policy_vars,
                     macro_category = case_when(
                       category %in% c("macroeconomics","finance") ~ "Economy",
                       category %in% c("civil_rights","social_welfare","labour") ~ "Social",
                       category %in% c("healthcare","education") ~ "Public Services",
                       category %in% c("crime","defence") ~ "Security",
                       category %in% c("environment","energy","forestry","fisheries","land-water-management") ~ "Environment",
                       category %in% c("foreign_trade","intl_affairs","sstc") ~ "External Affairs",
                       TRUE ~ "Governance"))

# ---------------------------------------------------------------------
# 1. DATA PREPARATION
# ---------------------------------------------------------------------

parse_month_year <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXct")) return(as.Date(x))
  
  x <- trimws(as.character(x))
  out <- rep(as.Date(NA), length(x))
  
  # Correctly parse YYYY-MM values
  ym <- !is.na(x) & grepl("^\\d{4}-\\d{2}$", x)
  out[ym] <- as.Date(paste0(x[ym], "-01"))
  
  # Parse the other supported date formats
  tries <- list(
    list("%Y-%m-%d", function(s) s),
    list("%d-%b-%Y", function(s) paste0("01-", s)),
    list("%d %b %Y", function(s) paste0("01 ", s)),
    list("%d-%B-%Y", function(s) paste0("01-", s)),
    list("%d %B %Y", function(s) paste0("01 ", s)),
    list("%d/%m/%Y", function(s) paste0("01/", s)),
    list("%d-%m-%Y", function(s) paste0("01-", s))
  )
  
  for (tr in tries) {
    idx <- is.na(out) & !is.na(x)
    if (!any(idx)) break
    
    out[idx] <- suppressWarnings(
      as.Date(tr[[2]](x[idx]), format = tr[[1]])
    )
  }
  
  out
}

prep_monthly <- function(file, vars = NULL, collapse_map = NULL) {
  
  df <- read_excel(file)
  names(df) <- trimws(names(df))
  
  if (!"MonthYear" %in% names(df)) {
    stop("MonthYear column not found in ", basename(file))
  }
  
  if (is.null(vars)) {
    vars <- setdiff(names(df), c("Date", "MonthYear"))
  }
  
  # Stop rather than silently dropping expected dictionary columns
  missing_vars <- setdiff(vars, names(df))
  
  if (length(missing_vars) > 0) {
    stop(
      "Missing expected columns in ", basename(file), ": ",
      paste(missing_vars, collapse = ", ")
    )
  }
  
  df$month_date <- parse_month_year(df$MonthYear)
  
  if (all(is.na(df$month_date))) {
    stop("Cannot parse MonthYear in ", file)
  }
  
  long <- df %>%
    mutate(across(all_of(vars), ~ suppressWarnings(as.numeric(.x)))) %>%
    select(month_date, all_of(vars)) %>%
    pivot_longer(
      all_of(vars),
      names_to = "category",
      values_to = "value"
    )
  
  if (!is.null(collapse_map)) {
    long <- long %>%
      left_join(collapse_map, by = "category") %>%
      mutate(category = macro_category) %>%
      select(-macro_category)
  }
  
  long %>%
    group_by(month_date, category) %>%
    summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    group_by(month_date) %>%
    mutate(
      total = sum(value),
      share = ifelse(total > 0, value / total, 0)
    ) %>%
    ungroup() %>%
    arrange(month_date)
}

shannon <- function(p) {
  p <- p[p > 0]
  p <- p / sum(p)
  -sum(p * log(p))
}   # natural log


# =====================================================================
# PART A. ENTROPY AND SEGMENTED REGRESSION
# =====================================================================

# Prepare the three dictionaries
lg <- prep_monthly(lg_file)
mz <- prep_monthly(mz_file, mz_vars)
pc <- prep_monthly(pc_file, policy_vars, policy_map)

# 7 macro-categories (maximum entropy = ln(7) = 1.946)

# Calculate monthly Shannon entropy
ent <- bind_rows(
  lg %>%
    group_by(month_date) %>%
    summarise(entropy = shannon(share), .groups = "drop") %>%
    mutate(dataset = "Laver-Garry"),
  
  mz %>%
    group_by(month_date) %>%
    summarise(entropy = shannon(share), .groups = "drop") %>%
    mutate(dataset = "Maerz"),
  
  pc %>%
    group_by(month_date) %>%
    summarise(entropy = shannon(share), .groups = "drop") %>%
    mutate(dataset = "Policy Agendas")
) %>%
  group_by(dataset) %>%
  arrange(month_date) %>%
  mutate(
    time = row_number() - 1,  # 0..17, as in manuscript Table 3
    post = as.integer(month_date >= as.Date("2020-01-01"))
  ) %>%
  ungroup() %>%
  mutate(
    dataset = factor(
      dataset,
      levels = c("Laver-Garry", "Maerz", "Policy Agendas")
    )
  )

stopifnot(all(table(ent$dataset) == 18))

cat("\nMonthly entropy (compare with Supplementary Table S9 / old S9):\n")

print(
  ent %>%
    select(dataset, month_date, entropy) %>%
    pivot_wider(names_from = dataset, values_from = entropy),
  n = 18
)

# Fit the segmented model, using Laver-Garry as the reference category
fit <- lm(entropy ~ dataset * (time + post), data = ent)

nm_map <- c(
  "(Intercept)" = "Intercept (Laver-Garry baseline)",
  "datasetMaerz" = "Maerz (vs Laver-Garry)",
  "datasetPolicy Agendas" = "Policy Agendas (vs Laver-Garry)",
  "time" = "Time (Laver-Garry slope)",
  "post" = "Post (Laver-Garry level change)",
  "datasetMaerz:time" = "Maerz x Time",
  "datasetPolicy Agendas:time" = "Policy Agendas x Time",
  "datasetMaerz:post" = "Maerz x Post",
  "datasetPolicy Agendas:post" = "Policy Agendas x Post"
)

# Calculate OLS or HAC inference, depending on the covariance estimator
make_table <- function(fit, vcov_mat, label, inference = "t") {
  
  b <- coef(fit)
  se <- sqrt(diag(vcov_mat))
  
  if (inference == "t") {
    
    # Conventional OLS: Student's t-distribution
    dfr <- df.residual(fit)
    statistic <- b / se
    p <- 2 * pt(abs(statistic), dfr, lower.tail = FALSE)
    q <- qt(.975, dfr)
    
    t_value <- statistic
    z_value <- rep(NA_real_, length(b))
    
  } else if (inference == "z") {
    
    # HAC: asymptotic normal (z) inference
    statistic <- b / se
    p <- 2 * pnorm(abs(statistic), lower.tail = FALSE)
    q <- qnorm(.975)
    
    t_value <- rep(NA_real_, length(b))
    z_value <- statistic
    
  } else {
    stop("inference must be either 't' or 'z'")
  }
  
  data.frame(
    variant = label,
    term = unname(nm_map[names(b)]),
    b = b,
    SE = se,
    t = t_value,
    z = z_value,
    p = p,
    p_FDR = p.adjust(p, "BH"),
    CI_low = b - q * se,
    CI_high = b + q * se,
    row.names = NULL
  )
}

# Conventional OLS standard errors and t-based inference
tab_ols <- make_table(
  fit,
  vcov(fit),
  "OLS (conventional SE)",
  inference = "t"
)

# Newey-West / Bartlett HAC standard errors, using lags 1-3
# Calculated within each dictionary's time series
tabs_hac <- list()

if (
  requireNamespace("sandwich", quietly = TRUE) &&
  packageVersion("sandwich") >= "3.0.0"
) {
  
  ent <- ent %>% arrange(dataset, time)
  
  fit <- lm(entropy ~ dataset * (time + post), data = ent)
  
  for (L in 1:3) {
    
    V <- sandwich::vcovPL(
      fit,
      cluster = ent$dataset,
      order.by = ent$time,
      lag = L,
      adjust = TRUE
    )
    
    tabs_hac[[L]] <- make_table(
      fit,
      V,
      paste0("Newey-West panel HAC, lag ", L),
      inference = "z"
    )
  }
  
} else {
  message("Install/upgrade 'sandwich' (>= 3.0-0) for the HAC variants.")
}

# Combine the OLS and HAC results
tab_all <- bind_rows(tab_ols, tabs_hac)

options(width = 200)

print(
  as_tibble(
    tab_all %>%
      mutate(across(where(is.numeric), ~ round(.x, 3)))
  ),
  n = Inf,
  width = Inf
)

write.csv(
  tab_all,
  file.path(out_dir, "Table3_entropy_segmented_variants.csv"),
  row.names = FALSE
)

# Calculate the pre-COVID means and the overall entropy ranges
base <- ent %>%
  filter(month_date < as.Date("2020-01-01")) %>%
  group_by(dataset) %>%
  summarise(pre_COVID_mean = round(mean(entropy), 3), .groups = "drop")

rng <- ent %>%
  group_by(dataset) %>%
  summarise(
    min = round(min(entropy), 3),
    max = round(max(entropy), 3),
    .groups = "drop"
  )

cat("\nPre-COVID (Jun-Dec 2019) mean entropy:\n")
print(base)

cat("\nRange across the 18 months:\n")
print(rng)

# Plot monthly entropy for the three dictionaries
fig6 <- ggplot(ent, aes(month_date, entropy, colour = dataset)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.8) +
  geom_vline(
    xintercept = as.Date("2020-01-01"),
    linetype = "dashed"
  ) +
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%b %Y"
  ) +
  labs(
    x = NULL,
    y = "Shannon entropy (nats)",
    colour = NULL,
    title = "Shannon entropy for three political dictionaries"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_text(
      angle = 90,
      vjust = .5,
      hjust = 1
    )
  )

ggsave(
  file.path(out_dir, "Figure6_entropy.png"),
  fig6,
  width = 8,
  height = 4.5,
  dpi = 300
)


# =====================================================================
# PART B. MANN-KENDALL AND SEN'S SLOPE
# =====================================================================

# Calculate the Mann-Kendall statistic using base R
mann_kendall <- function(y) {
  
  n <- length(y)
  S <- 0
  
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      S <- S + sign(y[j] - y[i])
    }
  }
  
  tie <- table(y)
  tie <- tie[tie > 1]
  
  v <- (
    n * (n - 1) * (2 * n + 5) -
      sum(tie * (tie - 1) * (2 * tie + 5))
  ) / 18
  
  z <- ifelse(
    S > 0,
    (S - 1) / sqrt(v),
    ifelse(S < 0, (S + 1) / sqrt(v), 0)
  )
  
  p <- 2 * pnorm(-abs(z))
  
  # # Calculate Kendall's tau-b 
  tau <- cor(seq_len(n), y, method = "kendall")
  
  c(S = S, tau = tau, z = z, p = p)
}

# Calculate Sen's slope and its confidence interval
sen_slope <- function(y, alpha = .05) {
  
  n <- length(y)
  sl <- c()
  
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      sl <- c(sl, (y[j] - y[i]) / (j - i))
    }
  }
  
  sl <- sort(sl)
  N <- length(sl)
  
  tie <- table(y)
  tie <- tie[tie > 1]
  
  v <- (
    n * (n - 1) * (2 * n + 5) -
      sum(tie * (tie - 1) * (2 * tie + 5))
  ) / 18
  
  C <- qnorm(1 - alpha / 2) * sqrt(v)
  
  lo <- sl[max(1, floor((N - C) / 2))]
  hi <- sl[min(N, ceiling((N + C) / 2) + 1)]
  
  c(
    sen = median(sl),
    ci_low = lo,
    ci_high = hi
  )
}

# Run both tests for each category in the three dictionaries
# using monthly shares from Part A
mk_one <- function(monthly, label) {
  
  cats <- sort(unique(monthly$category))
  
  out <- lapply(cats, function(cat) {
    
    y <- monthly %>%
      filter(category == cat) %>%
      arrange(month_date) %>%
      pull(share)
    
    if (length(y) != 18 || length(unique(y)) < 3) {
      return(NULL)
    }
    
    data.frame(
      dictionary = label,
      category = cat,
      t(mann_kendall(y)),
      t(sen_slope(y)),
      row.names = NULL
    )
  })
  
  tab <- do.call(rbind, out)
  tab$p_FDR <- p.adjust(tab$p, "BH")
  
  tab
}

# Combine results and apply the FDR correction
mk_tab <- bind_rows(
  mk_one(lg, "Laver-Garry"),
  mk_one(mz, "Maerz"),
  mk_one(pc, "Policy Agendas")
)

mk_tab <- mk_tab %>% arrange(dictionary, p)

print(
  as_tibble(
    mk_tab %>%
      mutate(across(where(is.numeric), ~ signif(.x, 3)))
  ),
  n = Inf,
  width = Inf
)

write.csv(
  mk_tab,
  file.path(out_dir, "MannKendall_Sen_all_categories.csv"),
  row.names = FALSE
)

# Mann-Kendall depends only on ranks; Sen's slope is per month on the share scale.
# FDR is BH within each dictionary (change to pooled if the manuscript pooled).


# ---- Populism / liberalism (Rooduijn & Pauwels) -------------------------------
# Check the three workbooks for populism and liberalism variables.
# Set pl_file if these variables are stored in a separate workbook.

pl_file <- NULL  # e.g. file.path(dir_cfa, "output_populism_liberalism.xlsx")

scan_files <- c(
  lg_file,
  mz_file,
  pc_file,
  if (!is.null(pl_file)) pl_file
)

series <- list()

for (f in scan_files) {
  
  d0 <- read_excel(f)
  names(d0) <- trimws(names(d0))
  
  cat(
    "\n",
    basename(f),
    ": ",
    paste(names(d0), collapse = ", "),
    "\n",
    sep = ""
  )
  
  hits <- grep(
    "^populism|^populist|^liberalism$|^liberal$",
    names(d0),
    ignore.case = TRUE,
    value = TRUE
  )
  
  if (length(hits) > 0 && "MonthYear" %in% names(d0)) {
    
    d0$month_date <- parse_month_year(d0$MonthYear)
    
    for (h in hits) {
      
      sm <- d0 %>%
        group_by(month_date) %>%
        summarise(
          v = sum(as.numeric(.data[[h]]), na.rm = TRUE),
          .groups = "drop"
        ) %>%
        arrange(month_date)
      
      series[[paste0(basename(f), ":", h)]] <- sm$v
    }
  }
}

if (length(series) > 0) {
  
  pl_tab <- do.call(
    rbind,
    lapply(names(series), function(nm) {
      
      y <- series[[nm]]
      stopifnot(length(y) == 18)
      
      data.frame(
        series = nm,
        t(mann_kendall(y)),
        t(sen_slope(y)),
        row.names = NULL
      )
    })
  )
  
  print(
    pl_tab %>%
      mutate(across(where(is.numeric), ~ signif(.x, 4)))
  )
  
  write.csv(
    pl_tab,
    file.path(out_dir, "MannKendall_Sen_populism_liberalism.csv"),
    row.names = FALSE
  )
  
} else {
  message(
    "\nNo populism/liberalism columns found; ",
    "set pl_file to the right workbook if you have one."
  )
}


# =====================================================================
# PART C. REFERENCES TO WOMEN AND COVID-TOPIC SHARE
# =====================================================================

# COVID-topic shares from the response letter (section 2)
# These values are based on the 1,016,069-comment topic file.

covid <- data.frame(
  month_key = format(
    seq(as.Date("2019-06-01"), by = "month", length.out = 18),
    "%Y-%m"
  ),
  
  covid_deaths = c(
    0.115,0.075,0.096,0.098,0.093,0.109,0.088,0.371,0.460,
    1.282,2.099,1.300,0.634,0.777,0.858,1.113,0.996,1.004
  ),
  
  death_rate = c(
    0.351,0.258,0.329,0.285,0.276,0.296,0.286,0.395,0.513,
    0.694,0.894,0.562,0.482,0.441,0.442,0.427,0.476,0.588
  ),
  
  covid_severe = c(
    0.108,0.150,0.107,0.145,0.142,0.124,0.075,0.182,0.257,
    0.650,1.213,0.570,0.354,0.400,0.301,0.273,0.392,0.403
  ),
  
  covid_combined = c(
    0.573,0.483,0.532,0.528,0.511,0.529,0.449,0.949,1.230,
    2.626,4.205,2.433,1.471,1.617,1.601,1.813,1.864,1.996
  )
)

# Refit the monthly trends with COVID-topic share included as a control
adjusted_trends <- function(monthly, label, control = "covid_combined") {
  
  monthly <- monthly %>%
    mutate(month_key = format(month_date, "%Y-%m")) %>%
    left_join(covid, by = "month_key") %>%
    group_by(category) %>%
    arrange(month_date, .by_group = TRUE) %>%
    mutate(time = row_number()) %>%
    ungroup()
  
  stopifnot(!any(is.na(monthly[[control]])))
  
  cats <- sort(unique(monthly$category))
  
  out <- lapply(cats, function(cat) {
    
    d <- monthly %>% filter(category == cat)
    
    if (nrow(d) != 18 || sd(d$share) == 0) {
      return(NULL)
    }
    
    f0 <- lm(share ~ time, data = d)
    
    f1 <- lm(
      reformulate(c("time", control), response = "share"),
      data = d
    )
    
    s0 <- summary(f0)$coefficients["time", ]
    s1 <- summary(f1)$coefficients["time", ]
    
    data.frame(
      dictionary = label,
      category = cat,
      b_unadj = s0[1],
      p_unadj = s0[4],
      b_adj = s1[1],
      se_adj = s1[2],
      p_adj = s1[4],
      b_covid = summary(f1)$coefficients[control, 1],
      p_covid = summary(f1)$coefficients[control, 4],
      cor_time_covid = cor(d$time, d[[control]]),
      row.names = NULL
    )
  })
  
  tab <- do.call(rbind, out)
  
  tab$p_unadj_FDR <- p.adjust(tab$p_unadj, "BH")
  tab$p_adj_FDR <- p.adjust(tab$p_adj, "BH")
  
  tab
}

# Run the adjusted models for all three dictionaries
adj_all <- bind_rows(
  adjusted_trends(lg, "Laver-Garry"),
  adjusted_trends(mz, "Maerz"),
  adjusted_trends(pc, "Policy Agendas")
)

print(
  as_tibble(
    adj_all %>%
      mutate(across(where(is.numeric), ~ signif(.x, 3)))
  ),
  n = Inf,
  width = Inf
)

write.csv(
  adj_all,
  file.path(out_dir, "trends_controlling_COVID_share.csv"),
  row.names = FALSE
)

# Check the women-related categories in Laver-Garry
women_rx <- "wom|female|gender|fem"

cat(
  "\nLaver-Garry categories matching '",
  women_rx,
  "':\n",
  sep = ""
)

print(
  grep(
    women_rx,
    unique(lg$category),
    ignore.case = TRUE,
    value = TRUE
  )
)

cat(
  "\nReferences to women, unadjusted vs adjusted for COVID-topic share (Combined):\n"
)

print(
  adj_all %>%
    filter(
      dictionary == "Laver-Garry",
      grepl(women_rx, category, ignore.case = TRUE)
    ) %>%
    select(
      category,
      b_unadj,
      p_unadj,
      p_unadj_FDR,
      b_adj,
      se_adj,
      p_adj,
      p_adj_FDR,
      cor_time_covid
    ) %>%
    mutate(across(where(is.numeric), ~ signif(.x, 3)))
)

# Sensitivity check using the three COVID topics separately
sens <- function(monthly, cat_rx) {
  
  monthly <- monthly %>%
    mutate(month_key = format(month_date, "%Y-%m")) %>%
    left_join(covid, by = "month_key")
  
  cats <- grep(
    cat_rx,
    unique(monthly$category),
    ignore.case = TRUE,
    value = TRUE
  )
  
  do.call(
    rbind,
    lapply(cats, function(cat) {
      
      d <- monthly %>%
        filter(category == cat) %>%
        arrange(month_date) %>%
        mutate(time = row_number())
      
      f <- lm(
        share ~ time + covid_deaths + death_rate + covid_severe,
        data = d
      )
      
      s <- summary(f)$coefficients["time", ]
      
      data.frame(
        category = cat,
        b_time = s[1],
        se = s[2],
        p = s[4],
        row.names = NULL
      )
    })
  )
}

cat(
  "\nWomen category, controlling for the three COVID topics separately:\n"
)

print(sens(lg, women_rx))

# A negative time slope after adjustment suggests that the decline is not
# fully explained by the changing COVID-topic share. If the slope moves
# towards zero or loses significance, topic composition may explain part
# of the decline.


# =====================================================================
# NOTES FROM THE ANALYSIS
# =====================================================================

# 1. The 18 monthly entropy values in Supplementary Table S9 reproduce
#    the manuscript's Table 3 coefficients (time = 0..17; January 2020
#    as the breakpoint). The reported SEs, t statistics, p values and CIs
#    match conventional OLS, not Newey-West. With panel HAC standard
#    errors (lags 1-3), none of the nine terms changes conclusion.
#    Maerz x Time becomes p = .13-.16 (OLS p = .032, FDR .072).
#
#    HAC p-values and confidence intervals in this script use
#    asymptotic normal (z) inference. OLS retains t-based inference.
#
# 2. The Mann-Kendall statistics reported in the manuscript (S = -59 and
#    -71, z = -2.197 and -2.651, p = .028 and .008) are reproduced using
#    the standard n = 18 calculation without an autocorrelation
#    correction. This does not match the Methods description of a
#    Newey-West (HAC) correction.
#
# 3. The maximum possible entropy is ln(7) = 1.946 for Policy Agendas
#    and ln(4) = 1.386 for Maerz. The reported values are within these
#    limits.


# =====================================================================
# FIGURE 6. SHANNON ENTROPY BY DICTIONARY
# Requires `ent` from Part A and `out_dir` from the main script.
# =====================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(scales)
})

stopifnot(exists("ent"), exists("out_dir"))

# ---- 1. SETTINGS -----------------------------------------------------

y_span     <- 0.30
fig_width  <- 170
fig_height <- 150
base_pt    <- 9
breakpoint <- as.Date("2020-01-01")

windows <- tibble(
  period = c("COVID-19\nonset", "BLM\nprotests", "U.S.\nelection"),
  start = as.Date(c("2020-01-01", "2020-05-25", "2020-10-01")),
  end = as.Date(c("2020-05-24", "2020-09-30", "2020-11-30")),
  fill = c("#E6E6E6", "#CFCFCF", "#B8B8B8")
)

cols <- c(
  "Laver\u2013Garry" = "#0B2D6B",
  "Maerz" = "#5A5A5A",
  "Policy Agendas" = "#B8860B"
)

shapes <- c(
  "Laver\u2013Garry" = 16,
  "Maerz" = 15,
  "Policy Agendas" = 17
)

# ---- 2. DATA ---------------------------------------------------------

dat <- ent %>%
  mutate(
    dataset = factor(
      as.character(dataset),
      levels = c("Laver-Garry", "Maerz", "Policy Agendas"),
      labels = names(cols)
    )
  ) %>%
  select(month_date, entropy, dataset)

# Calculate the pre-COVID baseline for each dictionary
base <- dat %>%
  filter(month_date < breakpoint) %>%
  group_by(dataset) %>%
  summarise(baseline = mean(entropy), .groups = "drop")

# Set the same y-axis span for each panel
ranges <- dat %>%
  group_by(dataset) %>%
  summarise(
    lo_obs = min(entropy),
    hi_obs = max(entropy),
    .groups = "drop"
  )

stopifnot(all(ranges$hi_obs - ranges$lo_obs <= y_span))

lims <- ranges %>%
  mutate(
    mid = (lo_obs + hi_obs) / 2,
    lo = mid - y_span / 2,
    hi = mid + y_span / 2
  ) %>%
  select(dataset, lo, hi) %>%
  pivot_longer(
    c(lo, hi),
    values_to = "entropy"
  ) %>%
  mutate(month_date = min(dat$month_date)) %>%
  select(dataset, month_date, entropy)

# Add period labels to the top panel
lab <- windows %>%
  mutate(
    x = start + as.numeric(end - start) / 2,
    dataset = factor(
      "Laver\u2013Garry",
      levels = names(cols)
    )
  ) %>%
  select(period, x, dataset)

# ---- 3. FIGURE -------------------------------------------------------

fig6 <- ggplot(dat, aes(x = month_date, y = entropy)) +
  
  geom_rect(
    data = windows,
    aes(
      xmin = start,
      xmax = end,
      ymin = -Inf,
      ymax = Inf,
      fill = fill
    ),
    alpha = 0.8,
    inherit.aes = FALSE
  ) +
  
  geom_blank(data = lims) +
  
  geom_hline(
    data = base,
    aes(yintercept = baseline),
    linetype = "dotted",
    colour = "black",
    linewidth = 0.4
  ) +
  
  geom_vline(
    xintercept = breakpoint,
    linetype = "dashed",
    colour = "black",
    linewidth = 0.4
  ) +
  
  geom_line(
    aes(colour = dataset),
    linewidth = 0.7
  ) +
  
  geom_point(
    aes(colour = dataset, shape = dataset),
    size = 1.6
  ) +
  
  geom_text(
    data = lab,
    aes(x = x, y = Inf, label = period),
    vjust = 1.2,
    size = base_pt * 0.28,
    lineheight = 0.9,
    inherit.aes = FALSE
  ) +
  
  facet_wrap(
    ~ dataset,
    ncol = 1,
    scales = "free_y"
  ) +
  
  scale_colour_manual(
    values = cols,
    guide = "none"
  ) +
  
  scale_shape_manual(
    values = shapes,
    guide = "none"
  ) +
  
  scale_fill_identity() +
  
  scale_x_date(
    breaks = seq(
      as.Date("2019-07-01"),
      as.Date("2020-11-01"),
      by = "2 months"
    ),
    date_labels = "%b %Y",
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  
  scale_y_continuous(
    breaks = pretty_breaks(4),
    labels = number_format(accuracy = 0.01)
  ) +
  
  labs(
    x = "Month",
    y = "Shannon entropy (nats)"
  ) +
  
  theme_minimal(
    base_size = base_pt,
    base_family = "sans"
  ) +
  
  theme(
    strip.text = element_text(
      size = 12,
      face = "bold",
      hjust = 0
    ),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(
      size = 10,
      angle = 45,
      hjust = 1
    ),
    axis.text.y = element_text(size = 10),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(
      colour = "grey90",
      linewidth = 0.3
    ),
    panel.spacing.y = unit(0.9, "lines"),
    plot.margin = margin(4, 6, 4, 4)
  )

print(fig6)

# ---- 4. EXPORT -------------------------------------------------------

pdf_dev <- if (capabilities("cairo")) {
  grDevices::cairo_pdf
} else {
  grDevices::pdf
}

ggsave(
  file.path(out_dir, "Figure6_entropy.pdf"),
  fig6,
  width = fig_width,
  height = fig_height,
  units = "mm",
  device = pdf_dev
)

ggsave(
  file.path(out_dir, "Figure6_entropy.tiff"),
  fig6,
  width = fig_width,
  height = fig_height,
  units = "mm",
  dpi = 600,
  compression = "lzw",
  bg = "white"
)

ggsave(
  file.path(out_dir, "Figure6_entropy.png"),
  fig6,
  width = fig_width,
  height = fig_height,
  units = "mm",
  dpi = 600,
  bg = "white"
)

cat(
  "Saved Figure6_entropy (.pdf, .tiff, .png) to",
  out_dir,
  "\n"
)



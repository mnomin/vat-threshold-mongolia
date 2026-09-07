# =============================================================================
# DIGITAL ENFORCEMENT AND TAX FRICTIONS
# Evidence from the VAT Registration Threshold in Mongolia
#
# IDENTIFICATION RATIONALE:
#
#   B2B (Wholesale, Manufacturing):
#     Buyer claims the same transaction as VAT input cost -> built-in cross-
#     verification even before Ebarimt. Less room to underreport revenue.
#
#   B2C (Retail, Food service):
#     Final consumers do NOT report purchases. No cross-checking -> more scope
#     to bunch revenue just below the VAT threshold pre-Ebarimt.
#
#   Ebarimt (2016):
#     Lottery receipt system incentivises consumers to demand receipts for
#     B2C purchases. Replicates for B2C what VAT invoicing already provided
#     for B2B. Prediction: B2C bunching converges toward B2B post-2016.
#
#   Threshold change (2016): 10M -> 50M MNT
#     Threshold shift is common to ALL sectors -> does not confound the
#     B2C vs B2B comparison. DiD isolates the Ebarimt effect.
#
#   DiD = (Δb_B2C) - (Δb_B2B)
#   H1: b_B2C > b_B2B  pre-2016 (weaker enforcement for B2C)
#   H2: Parallel pre-trends hold (only 2 pre-years; discuss as limitation)
#   H3: DiD < 0 (Ebarimt reduces B2C bunching relative to B2B)
# =============================================================================


# =============================================================================
# STEP 0 -- Packages
# =============================================================================
packages <- c("tidyverse", "readxl", "ggplot2", "patchwork",
              "lmtest", "sandwich", "broom", "scales")
lapply(packages, library, character.only = TRUE)


# =============================================================================
# Wild cluster bootstrap -- Webb 6-point weights + restricted bootstrap
#
# WHY WEBB WEIGHTS (not Rademacher):
#   With G = 4 clusters, Rademacher weights yield only 2^4 = 16 distinct
#   weight combinations, imposing a minimum two-sided p-value of 0.125 that
#   makes conventional 5% testing uninformative (Cameron & Miller 2015;
#   MacKinnon & Webb 2023). Webb (2023) 6-point weights have 6^4 = 1,296
#   distinct combinations and provide far finer p-value resolution at G = 4.
#
# Webb 6-point mass points (each with prob 1/6):
#   { -sqrt(3/2), -1, -sqrt(1/2), sqrt(1/2), 1, sqrt(3/2) }
#
# WHY RESTRICTED BOOTSTRAP:
#   The restricted bootstrap imposes H0 (β_{coef_name} = 0) when generating
#   bootstrap samples by using residuals from the null-restricted model.
#   This gives better size control than centring the unrestricted distribution
#   (MacKinnon & Webb 2023).
#
# Returns: coef, SE, two-sided p-value, 95% CI, and diagnostics.
# =============================================================================
wild_cluster_boot <- function(model, data, cluster_var, coef_name,
                              B = 9999, seed = 42) {
  set.seed(seed)
  
  # Webb 6-point weights
  webb_pts <- c(-sqrt(3/2), -1, -sqrt(1/2), sqrt(1/2), 1, sqrt(3/2))
  
  clusters <- unique(data[[cluster_var]])
  K        <- length(clusters)
  b_obs    <- coef(model)[coef_name]
  
  # ── Restricted model: drop the coefficient of interest (impose H0: β = 0) ──
  null_formula <- update(formula(model),
                         paste(". ~ . -", coef_name))
  model_r   <- lm(null_formula, data = data)
  fitted_r  <- fitted(model_r)
  resids_r  <- residuals(model_r)
  
  b_boot <- vapply(seq_len(B), function(i) {
    # One Webb weight per cluster
    w_cl        <- sample(webb_pts, K, replace = TRUE)
    names(w_cl) <- as.character(clusters)
    w           <- w_cl[as.character(data[[cluster_var]])]
    
    # Bootstrap outcome generated under H0 (restricted fitted values + weighted residuals)
    y_star                <- fitted_r + resids_r * w
    boot_data             <- data
    boot_data[["y_star__"]] <- y_star
    
    fit_b <- lm(update(formula(model), y_star__ ~ .),
                data = boot_data)
    coef(fit_b)[coef_name]
  }, numeric(1L))
  
  # p-value: compare observed |t| against bootstrap distribution centred at 0
  # (bootstrap distribution is already centred under H0 by construction)
  p_val   <- mean(abs(b_boot) >= abs(b_obs), na.rm = TRUE)
  se_boot <- sd(b_boot, na.rm = TRUE)
  ci      <- quantile(b_boot, c(0.025, 0.975), na.rm = TRUE)
  
  list(coef = b_obs, se = se_boot, p_value = p_val,
       ci_lo = ci[[1]], ci_hi = ci[[2]],
       n_clusters = K, B = B)
}

# Pretty-printer for wild_cluster_boot output
print_wcb <- function(wb, coef_name) {
  cat(sprintf(
    "    %-12s  coef = %7.4f  SE = %6.4f  p = %.4f  95%% CI [%7.4f, %7.4f]  (K=%d, B=%d)\n",
    coef_name, wb$coef, wb$se, wb$p_value,
    wb$ci_lo, wb$ci_hi, wb$n_clusters, wb$B
  ))
}


# =============================================================================
# STEP 1 -- Parameters
# =============================================================================

# Input data path
data_path  <- "/Users/nomin-erdenemunkhjargal/Documents/GraSPP/Thesis/Data/MAIN_DATA.xlsx"

# Output directory -- all CSVs and PDFs are saved here
# Change this one line to redirect all outputs to a different folder
output_dir <- "/Users/nomin-erdenemunkhjargal/Documents/GraSPP/Thesis/Data/Rcode/output"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# Sector metadata: must match EXACTLY the sector names in the Excel file
sector_meta <- tibble(
  sector  = c("Retail", "Food service", "Wholesale", "Manufacturing"),
  type    = c("B2C",    "B2C",          "B2B",       "B2B"),
  label   = c("Retail (B2C)", "Food service (B2C)",
              "Wholesale (B2B)", "Manufacturing (B2B)")
)

# Thresholds in MILLIONS MNT
threshold_pre  <- 10    # 2014-2015
threshold_post <- 50    # 2016-2023

# Polynomial degree for counterfactual
poly_degree <- 7

# Excluded-region parameters -- threshold-specific (millions MNT)
#
# Two edges define the excluded region used for the counterfactual fit:
#   excl_below : lower edge, set at the point where bunching begins, following
#                Harju et al. (2019, y_L) and Liu et al. (2021, s*_-). Fixed.
#   excl_above : UPPER SEARCH CEILING for the iterated bound zU (Step 2). The
#                upper edge is determined endogenously by the Kleven & Waseem
#                (2013) mass-conservation rule (missing mass = bunching mass),
#                iterated upward from the threshold; excl_above caps that search.
#
# Why a finite ceiling is required here. In the dense micro-data of Harju et al.
# and Liu et al. the missing-mass condition self-terminates within a short
# distance of the notch because the hole above it closes quickly. With 1M-MNT
# binned data and a heavy, slowly declining upper tail, the fitted counterfactual
# sits below observed counts across most of the tail, so missing mass never
# overtakes bunching mass and an UNCAPPED iteration walks zU to the data edge,
# swallowing the distribution and producing degenerate b-hats. The ceiling is
# therefore the binned-data analogue of the endogenous stopping point that the
# micro-data iteration reaches on its own. Cells that reach the ceiling without
# satisfying the condition are flagged (converged = FALSE), not silently used.
# Robustness to the window choice is reported in the excl_lo sweep (Step 5b).
excl_below_pre  <- 2    # lower edge: bunching window [8M, 10M)   <- main spec
excl_above_pre  <- 10   # zU search ceiling pre-reform:  threshold + 10M
poly_degree_pre <- 5    # lower degree for sparser pre-reform data

excl_below_post  <- 2   # lower edge: bunching window [48M, 50M)  <- main spec
excl_above_post  <- 30  # zU search ceiling post-reform: threshold + 30M
poly_degree_post <- 7   # standard degree for denser post-reform data

# Bootstrap replications for standard errors
# Set to 500 for final run; use 50 for a quick test (~2 min vs ~30 min)
n_boot <- 500

# Analysis years
years <- 2014:2023

# Analysis window around threshold (keep only bins within ±window_m of threshold)
# Using NULL means use all bins (0.5M to 399.5M)
window_m_pre  <- 80    # ±80M around 10M threshold -> bins 0.5 to ~90M
window_m_post <- 120   # ±120M around 50M threshold -> bins 0.5 to ~170M

# Reform year (first year of new threshold + Ebarimt)
reform_year <- 2016


# =============================================================================
# STEP 2 -- Load and process real data
# =============================================================================

raw <- read_excel(data_path, sheet = 1)

# Rename columns to safe names
names(raw)[1:5] <- c("sector", "year", "lower", "upper", "count")

df <- raw |>
  # Drop header-like rows and the "X filing" rows (revenue = 0 or non-numeric)
  filter(
    sector %in% sector_meta$sector,
    !is.na(lower),
    is.numeric(lower) | is.character(lower)
  ) |>
  # Convert lower bound to numeric (some may be read as character)
  mutate(
    lower = suppressWarnings(as.numeric(lower)),
    count = suppressWarnings(as.numeric(count)),
    year  = as.integer(year)
  ) |>
  # Null counts in valid bins = 0 firms (sparse upper-tail bins)
  mutate(count = replace_na(count, 0)) |>
  filter(!is.na(lower), year %in% years) |>
  mutate(
    bin_mid = lower / 1e6 + 0.5   # midpoint of 1M-wide bin, in millions MNT
  ) |>
  select(sector, year, bin_mid, count) |>
  left_join(sector_meta, by = "sector") |>
  mutate(
    threshold = if_else(year < reform_year, threshold_pre, threshold_post),
    period    = if_else(year < reform_year,
                        "Pre-reform (2014-15)",
                        "Post-reform (2016-23)"),
    Post = as.integer(year >= reform_year),
    B2C  = as.integer(type == "B2C")
  )

cat(sprintf("Loaded %d bin-observations across %d sector × year cells.\n",
            nrow(df), n_distinct(paste(df$sector, df$year))))
cat("Sectors:", paste(unique(df$sector), collapse = ", "), "\n")
cat("Years:  ", paste(sort(unique(df$year)), collapse = ", "), "\n")

# Quick sanity check: count at threshold bin for each sector-year
df |>
  filter(abs(bin_mid - threshold - 0.5) < 1) |>
  arrange(sector, year) |>
  print()


# =============================================================================
# STEP 2b -- Table 3.2: Descriptive Statistics by Sector Group and Policy Regime
#            + Figure: Raw Revenue Distributions Around the Active Threshold
# =============================================================================

# ── Table 3.2 ─────────────────────────────────────────────────────────────────
desc_stats <- df |>
  filter(bin_mid > 1) |>
  mutate(era = if_else(year < reform_year,
                       "Pre-Reform (2014-2015)",
                       "Post-Reform (2016-2023)")) |>
  group_by(sector, era, year) |>
  summarise(
    annual_total    = sum(count, na.rm = TRUE),
    mean_per_bin    = mean(count[count > 0], na.rm = TRUE),
    .groups = "drop"
  ) |>
  group_by(sector, era) |>
  summarise(
    mean_firms_bin  = round(mean(mean_per_bin,  na.rm = TRUE), 1),
    total_active    = round(mean(annual_total,  na.rm = TRUE), 0),
    .groups = "drop"
  ) |>
  left_join(sector_meta |> select(sector, type), by = "sector") |>
  mutate(
    group = if_else(type == "B2C",
                    "B2C (Treatment Group)",
                    "B2B (Control Group)"),
    era   = factor(era, levels = c("Pre-Reform (2014-2015)",
                                   "Post-Reform (2016-2023)"))
  ) |>
  arrange(type, sector, era)

desc_wide <- desc_stats |>
  pivot_wider(
    id_cols   = c(group, sector),
    names_from  = era,
    values_from = c(mean_firms_bin, total_active),
    names_glue  = "{era}_{.value}"
  ) |>
  select(
    group, sector,
    `Pre-Reform (2014-2015)_mean_firms_bin`,
    `Pre-Reform (2014-2015)_total_active`,
    `Post-Reform (2016-2023)_mean_firms_bin`,
    `Post-Reform (2016-2023)_total_active`
  )

names(desc_wide) <- c(
  "Group", "Sector",
  "Pre Mean Firms/Bin", "Pre Total Active Firms",
  "Post Mean Firms/Bin", "Post Total Active Firms"
)

cat("\n══════════════════════════════════════════════════════════════════\n")
cat("TABLE 3.2: Descriptive Statistics by Sector Group and Policy Regime\n")
cat("══════════════════════════════════════════════════════════════════\n")
cat("(Excludes 0-1M catch-all bin; Total Active Firms = mean annual total)\n\n")
print(desc_wide, n = Inf)

write_csv(desc_wide, file.path(output_dir, "table3_2_descriptive_stats.csv"))
cat("Saved table3_2_descriptive_stats.csv\n")


# ── Figure: Raw Revenue Distributions Around the Active Threshold ─────────────
# X-axis is threshold-normalised: revenue / active VAT threshold. The kink sits
# at 1.0 in every panel and all panels span the SAME range (0 to norm_max),
# so the pre-reform (10M) and post-reform (50M) distributions are directly
# comparable despite the threshold change.
norm_max     <- 3                       # plot out to 3x the active threshold
norm_breaks  <- seq(0, norm_max, by = 0.5)

period_avg <- df |>
  filter(bin_mid > 1) |>
  mutate(era = if_else(year < reform_year,
                       "Pre-Reform (2014-2015)\nThreshold = 10M MNT",
                       "Post-Reform (2016-2023)\nThreshold = 50M MNT"),
         thresh_era = if_else(year < reform_year, threshold_pre, threshold_post),
         rev_norm   = bin_mid / thresh_era) |>
  filter(rev_norm <= norm_max) |>
  group_by(sector, label, type, era, thresh_era, bin_mid, rev_norm) |>
  summarise(avg_count = mean(count, na.rm = TRUE), .groups = "drop") |>
  mutate(
    group = if_else(type == "B2C",
                    "B2C (Treatment)",
                    "B2B (Control)"),
    label = factor(label, levels = c("Retail (B2C)", "Food service (B2C)",
                                     "Wholesale (B2B)", "Manufacturing (B2B)")),
    # exact bin edges in normalised units (1M-wide bins -> width varies by era)
    xmin = (bin_mid - 0.5) / thresh_era,
    xmax = (bin_mid + 0.5) / thresh_era
  )

p_desc <- ggplot(period_avg) +
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = avg_count,
                fill = group), alpha = 0.75) +
  geom_vline(xintercept = 1,
             color = "red", linetype = "dashed", linewidth = 0.9) +
  annotate("text", x = 1, y = Inf, label = "VAT\nthreshold",
           vjust = 1.4, hjust = -0.12, color = "red", size = 2.6) +
  scale_fill_manual(
    values = c("B2C (Treatment)" = "#E53935",
               "B2B (Control)"   = "#1E88E5")
  ) +
  scale_x_continuous(limits = c(0, norm_max), breaks = norm_breaks) +
  facet_grid(label ~ era, scales = "free_y") +
  labs(
    title    = "Raw Revenue Distributions Around the Active VAT Threshold",
    subtitle = "Average annual number of firms per 1M MNT bin (0-1M catch-all bin excluded)",
    x        = "Revenue / active VAT threshold  (1.0 = threshold; 10M pre-2016, 50M from 2016)",
    y        = "Average number of firms per bin",
    fill     = NULL,
    caption  = paste0(
      "Each panel shows the mean annual firm count per 1M revenue bin, pooled within each policy era.\n",
      "Red dashed line = VAT registration threshold (10M pre-2016; 50M from 2016).\n",
      "Visible spike just below the threshold indicates bunching (tax-minimising behaviour)."
    )
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position   = "top",
    panel.grid.minor  = element_blank(),
    strip.text.x      = element_text(face = "bold", size = 9),
    strip.text.y      = element_text(face = "bold", size = 9),
    axis.text.x       = element_text(angle = 45, hjust = 1),
    plot.caption      = element_text(size = 7.5, color = "gray50")
  )

print(p_desc)
ggsave(file.path(output_dir, "fig_desc_distributions.pdf"), p_desc, width = 12, height = 13)
cat("Saved fig_desc_distributions.pdf\n")


# =============================================================================
# STEP 3 -- Visualise raw distributions (spot-check bunching visually)
# =============================================================================

# X-axis is threshold-normalised (revenue / active threshold); every panel spans
# 0 to norm_max with the kink at 1.0, so panels are comparable across the 2016
# threshold change. norm_max / norm_breaks are defined in Step 2b.
plot_dist <- function(sector_name, year_val, df, x_max = norm_max) {
  thresh <- if (year_val < reform_year) threshold_pre else threshold_post
  d <- df |> filter(sector == sector_name, year == year_val,
                    bin_mid > 1,
                    bin_mid / thresh <= x_max) |>
    mutate(rev_norm = bin_mid / thresh)
  ggplot(d, aes(x = rev_norm, y = count)) +
    geom_col(fill = "steelblue", alpha = 0.7, width = 0.9 / thresh) +
    geom_vline(xintercept = 1, color = "red", linewidth = 1,
               linetype = "dashed") +
    annotate("text", x = 1.03, y = max(d$count, na.rm = TRUE) * 0.9,
             label = paste0("Threshold\n", thresh, "M"),
             color = "red", size = 3, hjust = 0) +
    scale_x_continuous(limits = c(0, x_max), breaks = norm_breaks) +
    labs(title = paste(sector_name, year_val),
         x = "Revenue / VAT threshold", y = "Number of firms") +
    theme_minimal(base_size = 10)
}

check_years <- c(2014, 2017)

for (yr in check_years) {

  plots_yr <- map(sector_meta$sector, ~plot_dist(.x, yr, df))
  
  p_yr <- wrap_plots(plots_yr, ncol = 2) +
    plot_annotation(
      title = paste("Raw revenue distributions --", yr),
      subtitle = "Red dashed line = VAT registration threshold"
    )
  
  ggsave(
    file.path(output_dir, paste0("fig0_raw_distributions_", yr, ".pdf")),
    p_yr,
    width = 10,
    height = 8
  )
  
  cat(sprintf("Saved fig0_raw_distributions_%d.pdf\n", yr))
}


# =============================================================================
# STEP 4 -- Bunching estimation functions
# =============================================================================

estimate_bunching <- function(bin_counts, bin_mids, threshold,
                              excl_lo = excl_below_post, excl_hi = excl_above_post,
                              poly_deg = poly_degree,
                              bin_w = 1, zU_fixed = NULL) {
  
  # Fitting range. The lower edge of the excluded region (threshold - excl_lo)
  # follows Harju et al. (2019) and Liu et al. (2021). The upper edge is the
  # iterated bound zU determined by mass conservation (Kleven & Waseem 2013);
  # excl_hi is the search ceiling for that iteration, not a fixed window edge.
  window <- max(0.5, threshold - 3 * excl_lo)
  max_m  <- threshold + 5 * excl_hi
  keep   <- bin_mids >= window & bin_mids <= max_m & !is.na(bin_counts)
  
  bm <- bin_mids[keep]
  bc <- bin_counts[keep]
  
  # Fit the counterfactual for a GIVEN upper bound zU: exclude [threshold - excl_lo, zU].
  fit_cf <- function(zU) {
    excl_range <- bm >= (threshold - excl_lo) & bm <= zU
    excl_idx   <- which(excl_range)
    if (length(excl_idx) == 0) return(NULL)
    
    excl_dummies <- map_dfc(excl_idx, function(i)
      tibble(!!paste0("d", i) := as.integer(seq_along(bm) == i)))
    
    reg_data    <- tibble(count = bc, bin_mid = bm) |> bind_cols(excl_dummies)
    poly_terms  <- paste0("I(bin_mid^", 1:poly_deg, ")", collapse = " + ")
    dummy_terms <- paste(names(excl_dummies), collapse = " + ")
    formula_str <- paste("count ~", poly_terms, "+", dummy_terms)
    
    fit  <- lm(as.formula(formula_str), data = reg_data)
    pred <- reg_data |> mutate(across(starts_with("d"), ~0))
    list(cf = pmax(predict(fit, newdata = pred), 0), fit = fit, excl_range = excl_range)
  }
  
  bunch_win <- bm >= (threshold - excl_lo) & bm < threshold
  
  # --- Determine zU (Kleven-Waseem mass conservation, capped search) ---
  zU_cap <- threshold + excl_hi   # search ceiling: binned-data analogue of the
  # endogenous stopping point micro-data reaches
  converged <- FALSE
  
  if (!is.null(zU_fixed)) {
    # Bootstrap path: hold zU at the point-estimate value.
    zU <- min(zU_fixed, zU_cap)
    cf_obj <- fit_cf(zU)
    if (is.null(cf_obj)) return(NULL)
    converged <- NA   # not re-determined in bootstrap
  } else {
    # Iterate zU upward in bin_w steps until missing mass >= bunching mass (Kleven-Waseem).
    zU      <- threshold
    cf_obj  <- fit_cf(zU)
    if (is.null(cf_obj)) return(NULL)
    repeat {
      cf_try  <- fit_cf(zU)
      if (is.null(cf_try)) break
      cf      <- cf_try$cf
      miss_win<- bm >= threshold & bm <= zU
      B_tmp   <- sum(bc[bunch_win] - cf[bunch_win], na.rm = TRUE)
      M_tmp   <- sum(cf[miss_win]  - bc[miss_win],  na.rm = TRUE)
      if (M_tmp >= B_tmp) { cf_obj <- cf_try; converged <- TRUE; break }
      if (zU >= zU_cap)   { cf_obj <- cf_try; converged <- FALSE; break }
      zU <- zU + bin_w
    }
  }
  
  counterfactual <- cf_obj$cf
  miss_win <- bm >= threshold & bm <= zU
  
  B_hat <- sum(bc[bunch_win] - counterfactual[bunch_win], na.rm = TRUE)
  b_hat <- if (mean(counterfactual[bunch_win]) > 0)
    B_hat / mean(counterfactual[bunch_win]) else NA_real_
  M_hat <- sum(counterfactual[miss_win] - bc[miss_win], na.rm = TRUE)
  
  list(B = B_hat, b = b_hat, M = M_hat, zU = zU, converged = converged,
       cf = counterfactual, bm = bm, bc = bc,
       fit = cf_obj$fit, excl_range = cf_obj$excl_range,
       bunch_win = bunch_win, miss_win = miss_win)
}


bootstrap_se <- function(bin_counts, bin_mids, threshold,
                         excl_lo = excl_below_post, excl_hi = excl_above_post,
                         poly_deg = poly_degree_post, n_reps = n_boot, bin_w = 1) {
  base <- estimate_bunching(bin_counts, bin_mids, threshold,
                            excl_lo, excl_hi, poly_deg, bin_w = bin_w)
  if (is.null(base)) return(list(b_se = NA, b_ci = c(NA, NA)))
  
  fitted_v <- fitted(base$fit)
  resids   <- residuals(base$fit)
  zU_star  <- base$zU   # hold the iterated bound fixed in the bootstrap
  
  b_boot <- replicate(n_reps, {
    rc  <- pmax(round(fitted_v + sample(resids, length(resids), replace = TRUE)), 0)
    est <- estimate_bunching(rc, base$bm, threshold, excl_lo, excl_hi, poly_deg,
                             bin_w = bin_w, zU_fixed = zU_star)
    if (is.null(est)) NA_real_ else est$b
  })
  b_boot <- b_boot[!is.na(b_boot)]
  list(b_se = sd(b_boot),
       b_ci = quantile(b_boot, c(0.025, 0.975), na.rm = TRUE))
}


# =============================================================================
# STEP 5 -- Run estimation for all sector × year cells
#
# The inner loop is factored into run_bunching_estimation() so the robustness
# sweep (Step 5b) can call it with different excl_lo values without repeating
# the bootstrap or any downstream code.
#
# Arguments
#   df_in        : the binned data frame (output of Step 2)
#   excl_lo_pre  : bunching-window half-width for pre-reform cells  (default = excl_below_pre)
#   excl_lo_post : bunching-window half-width for post-reform cells (default = excl_below_post)
#   excl_hi_pre  : missing-mass cap for pre-reform cells            (default = excl_above_pre)
#   excl_hi_post : missing-mass cap for post-reform cells           (default = excl_above_post)
#   verbose      : print per-cell progress lines
# =============================================================================

run_bunching_estimation <- function(df_in,
                                    excl_lo_pre  = excl_below_pre,
                                    excl_lo_post = excl_below_post,
                                    excl_hi_pre  = excl_above_pre,
                                    excl_hi_post = excl_above_post,
                                    verbose      = TRUE) {
  map_dfr(sector_meta$sector, function(s) {
    map_dfr(years, function(yr) {
      thresh   <- if (yr < reform_year) threshold_pre    else threshold_post
      excl_lo  <- if (yr < reform_year) excl_lo_pre      else excl_lo_post
      excl_hi  <- if (yr < reform_year) excl_hi_pre      else excl_hi_post
      poly_deg <- if (yr < reform_year) poly_degree_pre  else poly_degree_post
      d        <- df_in |> filter(sector == s, year == yr) |> arrange(bin_mid)
      
      if (nrow(d) < poly_deg + 5) {
        if (verbose)
          cat(sprintf("  SKIP %-20s %d -- too few bins\n", s, yr))
        return(NULL)
      }
      
      if (verbose)
        cat(sprintf("  %-20s %d | threshold = %dM | excl [-%d,+%d] | deg %d\n",
                    s, yr, thresh, excl_lo, excl_hi, poly_deg))
      
      est  <- estimate_bunching(d$count, d$bin_mid, thresh, excl_lo, excl_hi, poly_deg)
      boot <- bootstrap_se(d$count, d$bin_mid, thresh, excl_lo, excl_hi, poly_deg)
      
      tibble(sector = s, year = yr, threshold = thresh,
             b       = est$b,         b_se     = boot$b_se,
             b_ci_lo = boot$b_ci[1],  b_ci_hi  = boot$b_ci[2],
             B = est$B, M = est$M, zU = est$zU, converged = est$converged)
    })
  }) |>
    filter(!is.na(b)) |>
    left_join(sector_meta, by = "sector") |>
    mutate(
      period   = if_else(year < reform_year, "Pre-reform (2014-15)", "Post-reform (2016-23)"),
      Post     = as.integer(year >= reform_year),
      B2C      = as.integer(type == "B2C"),
      B2C_Post = B2C * Post
    )
}

# ── Main run (baseline excl_lo values from Step 1) ───────────────────────────
cat("\nEstimating bunching for all sector × year cells...\n")
results <- run_bunching_estimation(df)
cat(sprintf("\nCompleted estimation for %d sector × year cells.\n", nrow(results)))
print(results |> select(sector, year, b, b_se, b_ci_lo, b_ci_hi))


# =============================================================================
# STEP 5b -- Robustness sweep over excl_lo ∈ {2, 3, 4, 5}
#
# Runs run_bunching_estimation() four times (no bootstrap re-use issue --
# each call is fully independent). Summarises pre/post gaps and DiD for
# each excl_lo value so you can confirm the DiD sign is stable.
#
# NOTE: bootstrap SEs are re-estimated in each sweep call; set n_boot to a
#       lower value while prototyping if speed is a concern.
# =============================================================================

cat("\n══════════════════════════════════════════════════════════\n")
cat("STEP 5b -- excl_lo robustness sweep (values: 2, 3, 4, 5)\n")
cat("══════════════════════════════════════════════════════════\n")

sweep_lo_vals <- c(2, 3, 4, 5)

sweep <- bind_rows(lapply(sweep_lo_vals, function(v) {
  cat(sprintf("\n  -> excl_lo = %d ...\n", v))
  res      <- run_bunching_estimation(df, excl_lo_pre = v, excl_lo_post = v,
                                      verbose = FALSE)
  res$excl_lo <- v
  res
}))

# ── DiD summary table across excl_lo values ───────────────────────────────────
sweep_did <- sweep |>
  filter(year != 2020) |>                          # mirror main-spec exclusion
  group_by(excl_lo, type, Post) |>
  summarise(mean_b = mean(b, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(names_from = c(type, Post),
              values_from = mean_b,
              names_glue  = "{type}_Post{Post}") |>
  mutate(
    gap_pre  = B2C_Post0 - B2B_Post0,
    gap_post = B2C_Post1 - B2B_Post1,
    DiD      = (B2C_Post1 - B2C_Post0) - (B2B_Post1 - B2B_Post0),
    pct_narrowing = if_else(gap_pre != 0,
                            round(100 * (gap_pre - gap_post) / gap_pre, 1),
                            NA_real_)
  ) |>
  select(excl_lo, gap_pre, gap_post, DiD, pct_narrowing,
         B2C_pre = B2C_Post0, B2C_post = B2C_Post1,
         B2B_pre = B2B_Post0, B2B_post = B2B_Post1)

cat("\n  excl_lo sweep -- DiD summary (main spec: 2020 excluded)\n\n")
print(sweep_did, n = Inf)

write_csv(sweep,     file.path(output_dir, "sweep_excl_lo_full.csv"))
write_csv(sweep_did, file.path(output_dir, "sweep_excl_lo_did_summary.csv"))
cat("\nSaved sweep_excl_lo_full.csv and sweep_excl_lo_did_summary.csv\n")


# =============================================================================
# STEP 6 -- Hypothesis 1: Pre-reform B2C bunches MORE than B2B
# =============================================================================

pre  <- results |> filter(year < reform_year)
post <- results |> filter(year >= reform_year)

pre_means <- pre |>
  group_by(type) |>
  summarise(mean_b = mean(b, na.rm = TRUE),
            se_b   = sqrt(sum(b_se^2, na.rm = TRUE)) / n(),
            .groups = "drop")

b_pre_B2C <- pre_means |> filter(type == "B2C") |> pull(mean_b)
b_pre_B2B <- pre_means |> filter(type == "B2B") |> pull(mean_b)
gap_pre   <- b_pre_B2C - b_pre_B2B

cat("\n==============================================================\n")
cat("HYPOTHESIS 1: Pre-reform B2C > B2B bunching\n")
cat("==============================================================\n")
cat(sprintf("  Pre-reform mean b -- B2C: %.3f | B2B: %.3f | Gap: %.3f\n",
            b_pre_B2C, b_pre_B2B, gap_pre))
cat(sprintf("  Prediction: GAP > 0 (B2C has weaker pre-Ebarimt enforcement)\n"))

if (nrow(pre |> filter(type == "B2C")) >= 2 &&
    nrow(pre |> filter(type == "B2B")) >= 2) {
  t_pre <- t.test(
    pre |> filter(type == "B2C") |> pull(b),
    pre |> filter(type == "B2B") |> pull(b),
    alternative = "greater"
  )
  cat(sprintf("  t-test (B2C > B2B): t = %.3f, p = %.4f\n",
              t_pre$statistic, t_pre$p.value))
} else {
  cat("  NOTE: Only 2 pre-reform years -- t-test has very limited power.\n")
  cat("  Treat H1 as descriptive; DiD (H3) is the main test.\n")
}


# =============================================================================
# STEP 7 -- Hypothesis 2: Parallel pre-trends
# =============================================================================

cat("\n==============================================================\n")
cat("HYPOTHESIS 2: Parallel pre-trends\n")
cat("==============================================================\n")
cat("  Only 2 pre-reform years (2014, 2015) -- formal pre-trend test\n")
cat("  is underpowered. We assess plausibility descriptively.\n\n")

pre_sector_b <- pre |>
  select(sector, year, b, type) |>
  pivot_wider(names_from = year, values_from = b, names_prefix = "b_")

cat("  Pre-reform bunching by sector:\n")
print(pre_sector_b)

pre_change <- pre |>
  arrange(sector, year) |>
  group_by(sector, type) |>
  summarise(delta_pre = diff(b), .groups = "drop") |>
  group_by(type) |>
  summarise(mean_delta = mean(delta_pre), .groups = "drop")

cat("\n  Mean year-on-year change in b (2014->2015):\n")
print(pre_change)
cat("  If both types move similarly, parallel trends are plausible.\n")


# =============================================================================
# STEP 8 -- Hypothesis 3: DiD -- Ebarimt reduces B2C bunching
#
# THREE SAMPLES:
#   results_main          -- 2020 excluded (main spec; COVID hit B2C disproportionately)
#   results_excl_2020_21  -- 2020 and 2021 excluded (robustness; also removes recovery year)
#   results               -- all years (robustness / full sample)
#
# THREE SE APPROACHES per regression:
#   (a) Wild cluster bootstrap  -- preferred; valid with as few as 4 clusters
#   (b) HC2 heteroskedasticity-robust -- no clustering assumption
#   (c) Cluster-robust (vcovCL) -- reported for comparability; unreliable at K=4
# =============================================================================

results_main         <- results |> filter(year != 2020)
results_excl_2020_21 <- results |> filter(!year %in% c(2020, 2021))

cat(sprintf(
  "\nMain sample (excl. 2020): %d obs | Excl. 2020+2021: %d obs | Full sample: %d obs\n",
  nrow(results_main), nrow(results_excl_2020_21), nrow(results)
))

# ── Helper: compute summary stats and run all three DiD specs ─────────────────
run_did_specs <- function(dat, sample_label) {
  
  post_m <- dat |> filter(year >= reform_year) |>
    group_by(type) |>
    summarise(mean_b = mean(b, na.rm = TRUE), .groups = "drop")
  pre_m  <- dat |> filter(year  < reform_year) |>
    group_by(type) |>
    summarise(mean_b = mean(b, na.rm = TRUE), .groups = "drop")
  
  b_post_B2C_ <- post_m |> filter(type == "B2C") |> pull(mean_b)
  b_post_B2B_ <- post_m |> filter(type == "B2B") |> pull(mean_b)
  b_pre_B2C_  <- pre_m  |> filter(type == "B2C") |> pull(mean_b)
  b_pre_B2B_  <- pre_m  |> filter(type == "B2B") |> pull(mean_b)
  
  gap_pre_  <- b_pre_B2C_  - b_pre_B2B_
  gap_post_ <- b_post_B2C_ - b_post_B2B_
  DiD_      <- (b_post_B2C_ - b_pre_B2C_) - (b_post_B2B_ - b_pre_B2B_)
  
  cat(sprintf("\n══════════════════════════════════════════════════════════\n"))
  cat(sprintf("HYPOTHESIS 3 -- %s\n", sample_label))
  cat(sprintf("══════════════════════════════════════════════════════════\n"))
  cat(sprintf("  B2C: Pre = %.3f -> Post = %.3f  |  Δ = %+.3f\n",
              b_pre_B2C_, b_post_B2C_, b_post_B2C_ - b_pre_B2C_))
  cat(sprintf("  B2B: Pre = %.3f -> Post = %.3f  |  Δ = %+.3f\n",
              b_pre_B2B_, b_post_B2B_, b_post_B2B_ - b_pre_B2B_))
  cat(sprintf("  DiD = %+.3f  (prediction: < 0)\n", DiD_))
  cat(sprintf("  Enforcement gap: pre = %.3f -> post = %.3f", gap_pre_, gap_post_))
  if (gap_pre_ != 0)
    cat(sprintf("  (%.0f%% narrowing)", 100 * (gap_pre_ - gap_post_) / gap_pre_))
  cat("\n")
  
  # ── Spec 1: Basic DiD (no FE) ─────────────────────────────────────────────
  cat("\n  [Spec 1] Basic DiD: b ~ B2C + Post + B2C×Post\n")
  cat("  β₁ > 0 (H1: B2C level), β₃ < 0 (H3: Ebarimt)\n\n")
  m1 <- lm(b ~ B2C + Post + B2C_Post, data = dat)
  
  cat("  (a) Wild cluster bootstrap [PREFERRED -- Webb 6-pt, restricted, K=4, B=9999]:\n")
  wb1 <- wild_cluster_boot(m1, dat, "sector", "B2C_Post")
  print_wcb(wb1, "B2C_Post")
  
  cat("\n  (b) HC2 heteroskedasticity-robust SEs:\n")
  print(coeftest(m1, vcov = vcovHC(m1, type = "HC2")))
  
  cat("\n  (c) Cluster-robust SEs (K=4; for reference only):\n")
  print(coeftest(m1, vcov = vcovCL(m1, cluster = ~sector)))
  
  # ── Spec 2: Sector FE ─────────────────────────────────────────────────────
  cat("\n  [Spec 2] Sector FE: b ~ sector FE + Post + B2C×Post\n")
  cat("  (B2C absorbed into sector FE; β_B2C×Post = Ebarimt effect)\n\n")
  m2 <- lm(b ~ factor(sector) + Post + B2C_Post, data = dat)
  
  cat("  (a) Wild cluster bootstrap:\n")
  wb2 <- wild_cluster_boot(m2, dat, "sector", "B2C_Post")
  print_wcb(wb2, "B2C_Post")
  
  cat("\n  (b) HC2:\n")
  print(coeftest(m2, vcov = vcovHC(m2, type = "HC2")))
  
  cat("\n  (c) Cluster-robust (reference):\n")
  print(coeftest(m2, vcov = vcovCL(m2, cluster = ~sector)))
  
  # ── Spec 3: Sector + Year FE ──────────────────────────────────────────────
  cat("\n  [Spec 3] Sector + Year FE: b ~ sector FE + year FE + B2C×Post\n\n")
  m3 <- lm(b ~ factor(sector) + factor(year) + B2C_Post, data = dat)
  
  cat("  (a) Wild cluster bootstrap:\n")
  wb3 <- wild_cluster_boot(m3, dat, "sector", "B2C_Post")
  print_wcb(wb3, "B2C_Post")
  
  cat("\n  (b) HC2:\n")
  print(coeftest(m3, vcov = vcovHC(m3, type = "HC2")))
  
  cat("\n  (c) Cluster-robust (reference):\n")
  print(coeftest(m3, vcov = vcovCL(m3, cluster = ~sector)))
  
  invisible(list(m1 = m1, m2 = m2, m3 = m3,
                 wb1 = wb1, wb2 = wb2, wb3 = wb3,
                 DiD = DiD_, gap_pre = gap_pre_, gap_post = gap_post_,
                 b_pre_B2C = b_pre_B2C_, b_post_B2C = b_post_B2C_,
                 b_pre_B2B = b_pre_B2B_, b_post_B2B = b_post_B2B_))
}

# ── MAIN specification (2020 excluded) ───────────────────────────────────────
did_main <- run_did_specs(results_main,
                          "MAIN SPECIFICATION (2020 excluded -- COVID confound)")

# ── Robustness: 2020 and 2021 excluded ───────────────────────────────────────
did_excl_2020_21 <- run_did_specs(
  results_excl_2020_21,
  "ROBUSTNESS: 2020 and 2021 excluded (COVID + recovery year)"
)

# ── Robustness: full sample ───────────────────────────────────────────────────
did_full <- run_did_specs(results,
                          "ROBUSTNESS: Full sample (2020 included)")

# ── COVID impact check: how different is 2020 and 2021? ──────────────────────
cat("\n══════════════════════════════════════════════════════════\n")
cat("COVID CHECK: Mean b in 2019-2022 by group\n")
cat("══════════════════════════════════════════════════════════\n")
results |>
  filter(year %in% c(2019, 2020, 2021, 2022)) |>
  group_by(type, year) |>
  summarise(mean_b = mean(b, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(names_from = year, values_from = mean_b,
              names_prefix = "b_") |>
  print()
cat("If B2C drops more than B2B in 2020-2021, COVID is a confound.\n")

# Use main-spec values for downstream figures
DiD      <- did_main$DiD
gap_pre  <- did_main$gap_pre
gap_post <- did_main$gap_post


# =============================================================================
# STEP 9 -- Event study (B2C - B2B relative to 2015)
#
# Reference year: 2015 (last pre-treatment period -- standard convention).
#
# Main:                    results_main         (2020 excluded)
# Robustness A:            results_excl_2020_21 (2020 and 2021 excluded)
# Robustness B (full):     results              (all years)
# =============================================================================

run_event_study <- function(dat, label) {
  cat(sprintf("\n══════════════════════════════════════════════════════════\n"))
  cat(sprintf("EVENT STUDY -- %s\n", label))
  cat(sprintf("══════════════════════════════════════════════════════════\n"))
  # NOTE ON INFERENCE: The event-study SEs and p-values below are conventional
  # OLS standard errors from the sector-year panel (~36-40 obs). They are NOT
  # wild cluster bootstrap SEs. The wild cluster bootstrap (Webb weights,
  # restricted, K=4) is applied only to the headline 2×2 DiD in Step 8.
  # This is disclosed in the thesis: the event study is used for visualising
  # the dynamic pattern (parallel pre-trends + post-2016 divergence) rather
  # than for formal inference, which rests on the DiD bootstrap p-value.
  cat("  NOTE: SEs are conventional OLS (sector-year panel). Formal inference\n")
  cat("  for the headline DiD uses the Webb wild cluster bootstrap (see Step 8).\n\n")
  
  ref_yr      <- 2015
  non_ref_yrs <- sort(setdiff(unique(dat$year), ref_yr))
  dummy_names <- paste0("B2Cx", non_ref_yrs)
  
  ed <- dat
  for (i in seq_along(non_ref_yrs)) {
    ed[[dummy_names[i]]] <- as.integer(ed$B2C == 1 & ed$year == non_ref_yrs[i])
  }
  
  formula_str <- paste0(
    "b ~ factor(sector) + factor(year) + ",
    paste(dummy_names, collapse = " + ")
  )
  mod <- lm(as.formula(formula_str), data = ed)
  
  coefs <- tidy(mod) |>
    filter(str_detect(term, "^B2Cx\\d{4}$")) |>
    mutate(
      year  = as.integer(str_extract(term, "\\d{4}")),
      ci_lo = estimate - 1.96 * std.error,
      ci_hi = estimate + 1.96 * std.error,
      sig   = p.value < 0.05,
      se_type = "conventional OLS (not wild-bootstrap)"   # explicit flag in CSV
    ) |>
    bind_rows(tibble(year = ref_yr, estimate = 0, std.error = 0,
                     p.value = 1, ci_lo = 0, ci_hi = 0, sig = FALSE,
                     se_type = "reference year (normalised to 0)")) |>
    arrange(year)
  
  cat("  Pre-reform ~= 0 -> parallel trends; post-reform < 0 -> Ebarimt effect.\n\n")
  print(coefs |> select(year, estimate, std.error, p.value, ci_lo, ci_hi, sig, se_type))
  invisible(coefs)
}

event_coefs          <- run_event_study(results_main,         "MAIN (2020 excluded)")
event_coefs_excl2021 <- run_event_study(results_excl_2020_21, "ROBUSTNESS (2020 and 2021 excluded)")
event_coefs_full     <- run_event_study(results,              "ROBUSTNESS (full sample, 2020 included)")


# =============================================================================
# STEP 10 -- Figures
# =============================================================================

cols_type <- c(
  "B2C (Retail + Food service)"     = "#E53935",
  "B2B (Wholesale + Manufacturing)" = "#1E88E5"
)

# Figures use the FULL time series for visual context; main-spec stats for labels
type_means <- results |>
  group_by(type, year) |>
  summarise(
    mean_b = mean(b, na.rm = TRUE),
    se_b   = sqrt(sum(b_se^2, na.rm = TRUE)) / n(),
    .groups = "drop"
  ) |>
  mutate(
    ci_lo  = mean_b - 1.96 * se_b,
    ci_hi  = mean_b + 1.96 * se_b,
    covid  = (year %in% c(2020, 2021)),
    group  = if_else(type == "B2C",
                     "B2C (Retail + Food service)",
                     "B2B (Wholesale + Manufacturing)")
  )

# ── Figure 1: Bunching over time with enforcement gap annotations ─────────────

yr_pre_ann  <- min(years)
yr_post_ann <- max(years)

b_B2C_pre_ann  <- type_means |> filter(type == "B2C",  year == yr_pre_ann)  |> pull(mean_b)
b_B2B_pre_ann  <- type_means |> filter(type == "B2B",  year == yr_pre_ann)  |> pull(mean_b)
b_B2C_post_ann <- type_means |> filter(type == "B2C",  year == yr_post_ann) |> pull(mean_b)
b_B2B_post_ann <- type_means |> filter(type == "B2B",  year == yr_post_ann) |> pull(mean_b)

p_gap <- ggplot(type_means, aes(x = year, y = mean_b,
                                color = group, fill = group)) +
  geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi), alpha = 0.12, color = NA) +
  geom_line(linewidth = 1.4) +
  geom_point(data = filter(type_means, !covid), size = 3) +
  geom_point(data = filter(type_means,  covid), size = 3.5,
             shape = 2, stroke = 1.2) +
  annotate("rect", xmin = 2019.5, xmax = 2021.5,
           ymin = -Inf, ymax = Inf, fill = "gray80", alpha = 0.4) +
  annotate("text", x = 2020.5, y = Inf, vjust = 1.4, hjust = 0.5,
           size = 2.6, color = "gray40", label = "COVID\n(excl. main/rob.A)") +
  geom_vline(xintercept = reform_year - 0.5, linetype = "longdash",
             color = "black", linewidth = 0.8) +
  annotate("text",
           x = reform_year - 0.3, y = Inf,
           vjust = 1.5, hjust = 0, size = 3, fontface = "italic",
           label = "2016: Threshold 10->50M\n+ Ebarimt launched") +
  annotate("segment",
           x = yr_pre_ann - 0.1, xend = yr_pre_ann - 0.1,
           y = min(b_B2C_pre_ann, b_B2B_pre_ann),
           yend = max(b_B2C_pre_ann, b_B2B_pre_ann),
           arrow = arrow(ends = "both", length = unit(0.06, "in")),
           color = "gray40", linewidth = 0.7) +
  annotate("text",
           x = yr_pre_ann - 0.2,
           y = (b_B2C_pre_ann + b_B2B_pre_ann) / 2,
           label = sprintf("Pre-gap\n%.2f", gap_pre),
           hjust = 1, size = 2.8, color = "gray40") +
  annotate("segment",
           x = yr_post_ann + 0.1, xend = yr_post_ann + 0.1,
           y = min(b_B2C_post_ann, b_B2B_post_ann),
           yend = max(b_B2C_post_ann, b_B2B_post_ann),
           arrow = arrow(ends = "both", length = unit(0.06, "in")),
           color = "gray40", linewidth = 0.7) +
  annotate("text",
           x = yr_post_ann + 0.2,
           y = (b_B2C_post_ann + b_B2B_post_ann) / 2,
           label = sprintf("Post-gap\n%.2f", gap_post),
           hjust = 0, size = 2.8, color = "gray40") +
  scale_color_manual(values = cols_type) +
  scale_fill_manual(values  = cols_type) +
  scale_x_continuous(breaks = years) +
  labs(
    title    = "Bunching at VAT Threshold: B2C vs B2B over Time",
    subtitle = sprintf(
      "DiD = %.3f | Pre-gap = %.2f -> Post-gap = %.2f (%.0f%% narrowing)",
      DiD, gap_pre, gap_post,
      if (gap_pre != 0) 100 * (gap_pre - gap_post) / gap_pre else 0),
    x = "Year", y = "Normalised excess mass (b)",
    color = NULL, fill = NULL,
    caption = paste0(
      "B2C (Retail, Food service) vs B2B (Wholesale, Manufacturing). ",
      "b = excess mass at threshold / mean counterfactual density.\n",
      "Shading = 95% CI from bootstrap (", n_boot, " replications). ",
      "Triangles = 2020-2021 (COVID; excluded from main spec and robustness A)."
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top",
        panel.grid.minor  = element_blank(),
        plot.caption      = element_text(size = 7.5, color = "gray50"),
        axis.text.x       = element_text(angle = 45, hjust = 1))

print(p_gap)
ggsave(file.path(output_dir, "fig1_enforcement_gap_convergence.pdf"), p_gap, width = 11, height = 6.5)
cat("Saved fig1_enforcement_gap_convergence.pdf\n")


# ── Figure 2: Individual sector bunching over time ────────────────────────────

sector_ts <- results |>
  mutate(ci_lo = b - 1.96 * b_se,
         ci_hi = b + 1.96 * b_se,
         covid = (year %in% c(2020, 2021)),
         group = if_else(type == "B2C",
                         "B2C (Retail + Food service)",
                         "B2B (Wholesale + Manufacturing)"))

p_sector <- ggplot(sector_ts, aes(x = year, y = b, color = group, fill = group)) +
  geom_rect(data = tibble(xmin = 2019.5, xmax = 2021.5,
                          ymin = -Inf,   ymax = Inf),
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            inherit.aes = FALSE, fill = "gray80", alpha = 0.5) +
  geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi), alpha = 0.12, color = NA) +
  geom_line(linewidth = 1) +
  geom_point(data = filter(sector_ts, !covid), size = 2.5) +
  geom_point(data = filter(sector_ts,  covid), size = 3,
             shape = 2, stroke = 1.2) +
  geom_vline(xintercept = reform_year - 0.5, linetype = "longdash",
             color = "black", linewidth = 0.6) +
  facet_wrap(~label, ncol = 2, scales = "free_y") +
  scale_color_manual(values = cols_type) +
  scale_fill_manual(values  = cols_type) +
  scale_x_continuous(breaks = years) +
  labs(
    title    = "Bunching Estimates by Sector",
    subtitle = "Vertical line = 2016 reform | Triangles = 2020-2021 (COVID)",
    x = "Year", y = "Normalised excess mass (b)",
    color = NULL, fill = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position  = "top",
        panel.grid.minor = element_blank(),
        strip.text       = element_text(face = "bold"),
        axis.text.x      = element_text(angle = 45, hjust = 1))

print(p_sector)
ggsave(file.path(output_dir, "fig2_sector_bunching.pdf"), p_sector, width = 12, height = 8)
cat("Saved fig2_sector_bunching.pdf\n")


# ── Figure 3: Event study -- three samples overlaid ───────────────────────────
# Main (solid red): 2020 excluded
# Robustness A (dashed orange): 2020 and 2021 excluded
# Robustness B (dashed gray): full sample

event_combined <- bind_rows(
  event_coefs          |> mutate(sample = "Main (2020 excl.)"),
  event_coefs_excl2021 |> mutate(sample = "Rob. A (2020+2021 excl.)"),
  event_coefs_full     |> mutate(sample = "Rob. B (full sample)")
) |>
  mutate(covid_yr = (year %in% c(2020, 2021)))

p_event <- ggplot(event_combined,
                  aes(x = year, y = estimate,
                      color = sample, fill = sample,
                      linetype = sample)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = reform_year - 0.5, linetype = "longdash",
             color = "black", linewidth = 0.8) +
  annotate("rect", xmin = 2019.5, xmax = 2021.5,
           ymin = -Inf, ymax = Inf, fill = "gray80", alpha = 0.4) +
  annotate("text", x = 2020.5, y = Inf, vjust = 1.4, hjust = 0.5,
           size = 2.6, color = "gray40", label = "COVID\n2020-21") +
  geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi), alpha = 0.08, color = NA) +
  geom_line(linewidth = 1.1) +
  geom_point(data = filter(event_combined, !covid_yr),
             aes(shape = as.character(sig)), size = 2.8) +
  geom_point(data = filter(event_combined,  covid_yr),
             size = 3.5, shape = 2, stroke = 1.2) +
  geom_point(data = filter(event_combined, year == 2015),
             color = "black", size = 4, shape = 21, fill = "white") +
  scale_color_manual(values = c(
    "Main (2020 excl.)"       = "#E53935",
    "Rob. A (2020+2021 excl.)" = "#FB8C00",
    "Rob. B (full sample)"    = "#888888"
  )) +
  scale_fill_manual(values = c(
    "Main (2020 excl.)"       = "#E53935",
    "Rob. A (2020+2021 excl.)" = "#FB8C00",
    "Rob. B (full sample)"    = "#888888"
  )) +
  scale_linetype_manual(values = c(
    "Main (2020 excl.)"       = "solid",
    "Rob. A (2020+2021 excl.)" = "dashed",
    "Rob. B (full sample)"    = "dotted"
  )) +
  scale_shape_manual(values = c("FALSE" = 1, "TRUE" = 19),
                     labels = c("p >= 0.05", "p < 0.05"), name = NULL) +
  scale_x_continuous(breaks = years) +
  annotate("text", x = reform_year - 0.3, y = Inf,
           vjust = 1.5, hjust = 0, size = 3, fontface = "italic",
           label = "Ebarimt + threshold shift ->") +
  labs(
    title    = "Event Study: B2C Bunching Relative to B2B",
    subtitle = "Reference year: 2015 | Sector + year FEs | Triangle = 2020-2021 (COVID)",
    x        = "Year",
    y        = "B2C×year coefficient (relative to 2015)",
    color    = "Sample", fill = "Sample", linetype = "Sample",
    caption  = paste0(
      "Main spec excludes 2020; Rob. A excludes 2020 and 2021 (COVID + recovery confound); ",
      "Rob. B is the full sample.\n",
      "Pre-2016 coefficients ~= 0 -> parallel trends; post-2016 < 0 -> Ebarimt effect. ",
      "Shading = 95% CI."
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        axis.text.x      = element_text(angle = 45, hjust = 1),
        plot.caption     = element_text(size = 7.5, color = "gray50"),
        legend.position  = "top")

print(p_event)
ggsave(file.path(output_dir, "fig3_event_study.pdf"), p_event, width = 11, height = 6.5)
cat("Saved fig3_event_study.pdf\n")


# ── Figure 4: 2×2 DiD bar chart ──────────────────────────────────────────────

avg_b <- results_main |>
  group_by(type, period) |>
  summarise(mean_b = mean(b, na.rm = TRUE),
            se_b   = sqrt(sum(b_se^2, na.rm = TRUE)) / n(),
            .groups = "drop") |>
  mutate(
    ci_lo  = mean_b - 1.96 * se_b,
    ci_hi  = mean_b + 1.96 * se_b,
    Group  = if_else(type == "B2C",
                     "B2C\n(Retail + Food service)",
                     "B2B\n(Wholesale + Manufacturing)"),
    period = factor(period,
                    levels = c("Pre-reform (2014-15)", "Post-reform (2016-23)"))
  )

p_did <- ggplot(avg_b, aes(x = Group, y = mean_b, fill = period)) +
  geom_col(position = "dodge", alpha = 0.85, width = 0.55) +
  geom_errorbar(aes(ymin = ci_lo, ymax = ci_hi),
                position = position_dodge(0.55), width = 0.25) +
  scale_fill_manual(
    values = c("Pre-reform (2014-15)"  = "#90A4AE",
               "Post-reform (2016-23)" = "#37474F")
  ) +
  labs(
    title   = "2×2 DiD: Enforcement Gap Before and After Ebarimt",
    subtitle = sprintf("DiD = %.3f | Pre-gap = %.2f -> Post-gap = %.2f",
                       DiD, gap_pre, gap_post),
    x = NULL, y = "Mean normalised bunching (b)",
    fill    = NULL,
    caption = paste0(
      "Error bars = 95% CI. DiD = (B2C post - B2C pre) - (B2B post - B2B pre).\n",
      "Negative DiD means B2C bunching fell more than B2B -> Ebarimt effective."
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top",
        panel.grid.minor = element_blank(),
        plot.caption     = element_text(size = 7.5, color = "gray50"))

print(p_did)
ggsave(file.path(output_dir, "fig4_did_bar.pdf"), p_did, width = 8, height = 6)
cat("Saved fig4_did_bar.pdf\n")


# ── Figure 5: Bunching density plots (counterfactual overlay) ─────────────────

# X-axis is threshold-normalised (revenue / active threshold); every panel spans
# 0 to x_max with the kink at 1.0, so panels are comparable across the 2016
# threshold change. norm_max / norm_breaks are defined in Step 2b.
plot_cf <- function(sector_name, yr, df, x_max = norm_max) {
  thresh   <- if (yr < reform_year) threshold_pre   else threshold_post
  excl_lo  <- if (yr < reform_year) excl_below_pre  else excl_below_post
  excl_hi  <- if (yr < reform_year) excl_above_pre  else excl_above_post
  d      <- df |> filter(sector == sector_name, year == yr) |> arrange(bin_mid)
  poly_d <- if (yr < reform_year) poly_degree_pre else poly_degree_post
  est    <- estimate_bunching(d$count, d$bin_mid, thresh, excl_lo, excl_hi, poly_d)
  if (is.null(est)) return(NULL)

  plot_df <- tibble(bin_mid = est$bm, actual = est$bc, cf = est$cf) |>
    mutate(rev_norm = bin_mid / thresh) |>
    filter(bin_mid > 1, rev_norm <= x_max)

  ggplot(plot_df, aes(x = rev_norm)) +
    geom_col(aes(y = actual), fill = "steelblue", alpha = 0.6, width = 0.9 / thresh) +
    geom_line(aes(y = cf), color = "red", linewidth = 1) +
    geom_vline(xintercept = 1, linetype = "dashed",
               color = "darkred", linewidth = 0.8) +
    geom_rect(
      data = tibble(
        xmin = (thresh - excl_lo) / thresh, xmax = 1,
        ymin = -Inf, ymax = Inf
      ),
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      fill = "gold", alpha = 0.2, inherit.aes = FALSE
    ) +
    annotate("text", x = 1.03,
             y = max(plot_df$actual, na.rm = TRUE) * 0.85,
             label = sprintf("b = %.2f", est$b),
             color = "darkred", size = 3.5, hjust = 0) +
    labs(title = paste(sector_name, yr),
         x = "Revenue / VAT threshold", y = "Number of firms") +
    scale_x_continuous(limits = c(0, x_max), breaks = norm_breaks) +
    theme_minimal(base_size = 10)
}

cf_years <- c(2015, 2022)

for (yr in cf_years) {

  plots_yr <- map(sector_meta$sector, ~plot_cf(.x, yr, df)) |> compact()
  
  if (length(plots_yr) == 0) next
  
  p_cf <- wrap_plots(plots_yr, ncol = 2) +
    plot_annotation(
      title = paste("Actual vs Counterfactual --", yr),
      subtitle = "Red = counterfactual | Gold = bunching window | b = excess mass"
    )
  
  ggsave(
    file.path(output_dir, paste0("fig5_counterfactual_", yr, ".pdf")),
    p_cf,
    width = 10,
    height = 8
  )
  
  cat(sprintf("Saved fig5_counterfactual_%d.pdf\n", yr))
}


# =============================================================================
# STEP 11 -- Summary table and CSV export
# =============================================================================

# ── Webb bootstrap p-values for the headline DiD (Spec 1, 2, 3) ──────────────
# These are the values that determine what the Results chapter can claim.
# Print them prominently so they are not buried in the run_did_specs output.
cat("\n══════════════════════════════════════════════════════════\n")
cat("WEBB WILD BOOTSTRAP p-VALUES FOR HEADLINE DiD (B2C_Post)\n")
cat("  Inference basis for the Results chapter\n")
cat("  K=4, B=9999, Webb 6-pt weights, restricted bootstrap\n")
cat("══════════════════════════════════════════════════════════\n")
cat("MAIN SPEC (2020 excluded):\n")
cat(sprintf("  Spec 1 (no FE):         p = %.4f\n", did_main$wb1$p_value))
cat(sprintf("  Spec 2 (sector FE):     p = %.4f\n", did_main$wb2$p_value))
cat(sprintf("  Spec 3 (sector+yr FE):  p = %.4f\n", did_main$wb3$p_value))
cat("ROB A (2020+2021 excluded):\n")
cat(sprintf("  Spec 1:  p = %.4f  |  Spec 2:  p = %.4f  |  Spec 3:  p = %.4f\n",
            did_excl_2020_21$wb1$p_value,
            did_excl_2020_21$wb2$p_value,
            did_excl_2020_21$wb3$p_value))
cat("ROB B (full sample):\n")
cat(sprintf("  Spec 1:  p = %.4f  |  Spec 2:  p = %.4f  |  Spec 3:  p = %.4f\n",
            did_full$wb1$p_value,
            did_full$wb2$p_value,
            did_full$wb3$p_value))
cat("══════════════════════════════════════════════════════════\n")
cat("NOTE: With K=4 clusters Webb weights yield 6^4=1296 combinations.\n")
cat("Minimum achievable two-sided p = 1/1296 ~= 0.001.\n")
cat("Do NOT write p<0.001 -- report the exact value above.\n\n")

result_table <- tibble(
  Hypothesis = c(
    "H1: B2C bunches more pre-reform",
    "H2: Parallel pre-trends",
    "H3: DiD < 0 (Ebarimt effect)"
  ),
  Prediction = c(
    "b_B2C > b_B2B, 2014-15",
    "No differential trend 2014-15",
    "β₃ < 0 in DiD regression"
  ),
  Finding_main = c(
    sprintf("b_B2C = %.3f, b_B2B = %.3f -> gap = %.3f (%s)",
            did_main$b_pre_B2C, did_main$b_pre_B2B, did_main$gap_pre,
            if (did_main$gap_pre > 0) "CONSISTENT" else "NOT consistent"),
    "Only 2 pre-years -- assess descriptively (see pre_sector_b output)",
    sprintf("DiD = %.3f, Webb-p(Spec1) = %.4f, Webb-p(Spec2) = %.4f, Webb-p(Spec3) = %.4f (%s)",
            did_main$DiD,
            did_main$wb1$p_value, did_main$wb2$p_value, did_main$wb3$p_value,
            if (did_main$DiD < 0) "CONSISTENT -- B2C fell more" else
              "NOT consistent -- gap did not narrow")
  ),
  Finding_robA = c(
    sprintf("gap = %.3f", did_excl_2020_21$gap_pre),
    "--",
    sprintf("DiD = %.3f, Webb-p(Spec1) = %.4f",
            did_excl_2020_21$DiD, did_excl_2020_21$wb1$p_value)
  ),
  Finding_robB_full = c(
    sprintf("gap = %.3f", did_full$gap_pre),
    "--",
    sprintf("DiD = %.3f, Webb-p(Spec1) = %.4f",
            did_full$DiD, did_full$wb1$p_value)
  )
)

names(result_table)[4] <- "Finding_robA (excl 2020+2021)"
names(result_table)[5] <- "Finding_robB (full sample)"

cat("\n══════════════════════════════════════════════════════════\n")
cat("SUMMARY OF HYPOTHESES\n")
cat("  Main spec = 2020 excluded\n")
cat("  Rob. A    = 2020 and 2021 excluded\n")
cat("  Rob. B    = full sample\n")
cat("══════════════════════════════════════════════════════════\n")
print(result_table, width = Inf)

write_csv(results,              file.path(output_dir, "results_all_years.csv"))
write_csv(results_main,         file.path(output_dir, "results_main_excl2020.csv"))
write_csv(results_excl_2020_21, file.path(output_dir, "results_robA_excl2020_2021.csv"))
write_csv(result_table,         file.path(output_dir, "results_hypothesis_summary.csv"))
write_csv(event_coefs,          file.path(output_dir, "results_event_study_main.csv"))
write_csv(event_coefs_excl2021, file.path(output_dir, "results_event_study_robA.csv"))
write_csv(event_coefs_full,     file.path(output_dir, "results_event_study_full.csv"))

cat("\n✓ Output files written:\n")
cat("  results_all_years.csv               -- full sample bunching estimates\n")
cat("  results_main_excl2020.csv           -- main spec (2020 excluded)\n")
cat("  results_robA_excl2020_2021.csv      -- robustness A (2020+2021 excluded)\n")
cat("  results_hypothesis_summary.csv\n")
cat("  results_event_study_main.csv\n")
cat("  results_event_study_robA.csv\n")
cat("  results_event_study_full.csv\n")
cat("  fig0_raw_distributions.pdf\n")
cat("  fig_desc_distributions.pdf\n")
cat("  fig1_enforcement_gap_convergence.pdf  (COVID band = 2020-2021)\n")
cat("  fig2_sector_bunching.pdf              (COVID band = 2020-2021)\n")
cat("  fig3_event_study.pdf                  (main + rob.A + rob.B overlay)\n")
cat("  fig4_did_bar.pdf\n")
cat("  fig5_counterfactual_overlay.pdf\n")


# =============================================================================
# STEP 12 -- Auto-generate LaTeX table files
# =============================================================================
# Tables generated (saved as .tex in output_dir):
#
#   tab_descriptive_stats.tex       Table 3.1  -- firm counts by group/regime
#   tab_bunching_means.tex          Table 4.1  -- b-hat means + enforcement gap
#   tab_did.tex                     Table 4.2  -- DiD (Webb p-values, 3 samples)
#   tab_event_study.tex             Table 4.3  -- event study, all 3 samples
#   tab_app_bunching_main.tex       Table A.3  -- full b-hat by sector x year
#   tab_app_density_ratios.tex      Table A.2  -- density discontinuity ratios
#   tab_app_covid.tex               Table A.4  -- 2020 + 2021 side-by-side
#   tab_app_robust_mfg.tex          Table B.1  -- Manufacturing-only DiD
#   tab_app_robust_polynomial.tex   Table C.1  -- polynomial degree sensitivity
#   tab_app_excl_lo.tex             Table C.2  -- excl_lo sweep DiD (NEW)
#
# Decisions embedded here:
#   - threshold_zones (old A.1) CUT; density_ratios kept
#   - event study main-spec-only table CUT; all-samples version kept
#   - 2020 and 2021 COVID tables MERGED into one
#   - converged flag added to full b-hat table
#   - excl_lo sweep replaces window-width table
#   - ALL numbers from locked spec (excl_lo=2, Webb bootstrap)
# =============================================================================

cat("\n\n")
cat("==============================================================\n")
cat("STEP 12 -- Generating LaTeX table files\n")
cat("==============================================================\n")

# ── Formatting helpers ────────────────────────────────────────────────────────

wb_stars <- function(p) {
  if (is.na(p) || p >= 0.10) return("")
  if (p < 0.01) return("^{***}")
  if (p < 0.05) return("^{**}")
  return("^{*}")
}
fmtf <- function(x, d = 3) formatC(round(x, d), digits = d, format = "f")
fmtp <- function(p) {
  if (is.na(p)) return("---")
  if (p < 0.001) return("$<$0.001")
  sprintf("%.3f", p)
}
pm_fmt <- function(x, d = 3) {
  if (is.na(x)) return("---")
  if (x >= 0) sprintf("$+$%s", fmtf(x, d)) else sprintf("$-$%s", fmtf(abs(x), d))
}
wtex <- function(fname, lines) {
  writeLines(lines, con = file.path(output_dir, fname))
  cat(sprintf("  Saved %s\n", fname))
}

# ── Table 3.1: Descriptive Statistics ────────────────────────────────────────

{
  d <- desc_wide
  lines <- c(
    "% Auto-generated by Step 12 -- do not edit manually",
    "\\begin{table}[H]",
    "\\centering",
    "\\caption{\\textbf{Descriptive Statistics: Aggregate Firm Distribution",
    "         by Sector Group and Policy Regime}}",
    "\\label{tab:descriptive_stats}",
    "\\begin{threeparttable}",
    "\\setlength{\\tabcolsep}{0pt}",
    "\\small",
    "\\begin{tabular*}{\\linewidth}{l@{\\extracolsep{\\fill}}rrrr}",
    "\\toprule",
    "& \\multicolumn{2}{c}{\\textit{Pre-Reform (2014--2015)}}",
    "& \\multicolumn{2}{c}{\\textit{Post-Reform (2016--2023)}} \\\\",
    "\\cmidrule(lr){2-3}\\cmidrule(lr){4-5}",
    "Sector & \\multicolumn{1}{c}{Mean Firms/Bin\\tnote{a}}",
    "& \\multicolumn{1}{c}{Annual Total\\tnote{b}}",
    "& \\multicolumn{1}{c}{Mean Firms/Bin\\tnote{a}}",
    "& \\multicolumn{1}{c}{Annual Total\\tnote{b}} \\\\",
    "\\midrule",
    "\\multicolumn{5}{l}{\\textit{B2C Treatment Group}} \\\\[2pt]"
  )
  for (i in which(d$Group == "B2C (Treatment Group)")) {
    lines <- c(lines, sprintf(
      "\\quad %-20s & %s & %s & %s & %s \\\\",
      d$Sector[i],
      fmtf(d$`Pre Mean Firms/Bin`[i], 1),
      formatC(d$`Pre Total Active Firms`[i], format="d", big.mark=","),
      fmtf(d$`Post Mean Firms/Bin`[i], 1),
      formatC(d$`Post Total Active Firms`[i], format="d", big.mark=",")
    ))
  }
  lines <- c(lines, "[6pt]", "\\multicolumn{5}{l}{\\textit{B2B Control Group}} \\\\[2pt]")
  for (i in which(d$Group == "B2B (Control Group)")) {
    lines <- c(lines, sprintf(
      "\\quad %-20s & %s & %s & %s & %s \\\\",
      d$Sector[i],
      fmtf(d$`Pre Mean Firms/Bin`[i], 1),
      formatC(d$`Pre Total Active Firms`[i], format="d", big.mark=","),
      fmtf(d$`Post Mean Firms/Bin`[i], 1),
      formatC(d$`Post Total Active Firms`[i], format="d", big.mark=",")
    ))
  }
  lines <- c(lines,
             "\\bottomrule",
             "\\end{tabular*}",
             "\\begin{tablenotes}[para,flushleft]\\footnotesize",
             "\\small\\textit{Notes:}",
             "\\item[a] Average annual firms per active revenue bin across 400 bins (0--400M MNT).",
             "\\item[b] Mean annual firm count excluding X-filers.",
             "\\textit{Source:} MTA CIT registry.",
             "\\end{tablenotes}",
             "\\end{threeparttable}",
             "\\end{table}"
  )
  wtex("tab_descriptive_stats.tex", lines)
}

# ── Table 4.1: Bunching means + enforcement gap ───────────────────────────────

{
  yrs <- sort(unique(results_main$year))  # 2020 excluded in main
  
  b2c_means <- sapply(yrs, function(y)
    mean(results_main$b[results_main$type == "B2C" & results_main$year == y], na.rm = TRUE))
  b2b_means <- sapply(yrs, function(y)
    mean(results_main$b[results_main$type == "B2B" & results_main$year == y], na.rm = TRUE))
  gaps <- b2c_means - b2b_means
  
  yr_cols <- paste(yrs, collapse = " & ")
  
  fmt_row <- function(vals) paste(sapply(vals, fmtf), collapse = " & ")
  fmt_gap <- function(vals) paste(sapply(vals, pm_fmt), collapse = " & ")
  
  lines <- c(
    "% Auto-generated by Step 12 -- do not edit manually",
    "\\begin{table}[H]",
    "\\centering",
    "\\caption{\\textbf{Normalised Bunching Estimates: Group Means and",
    "         Enforcement Gap (Main Sample, 2020 Excluded)}}",
    "\\label{tab:bunching}",
    "\\begin{threeparttable}",
    "\\setlength{\\tabcolsep}{0pt}",
    "\\small",
    sprintf("\\begin{tabular*}{\\linewidth}{l@{\\extracolsep{\\fill}}%s}",
            paste(rep("c", length(yrs)), collapse="")),
    "\\toprule",
    sprintf("Structural Metric & %s \\\\", yr_cols),
    "\\midrule",
    sprintf("B2C mean & %s \\\\", fmt_row(b2c_means)),
    sprintf("B2B mean & %s \\\\", fmt_row(b2b_means)),
    "\\midrule",
    sprintf("Gap (B2C $-$ B2B) & %s \\\\", fmt_gap(gaps)),
    "\\bottomrule",
    "\\end{tabular*}",
    "\\begin{tablenotes}[para,flushleft]\\footnotesize",
    sprintf(paste0("\\item \\textit{Notes:} Group means of $\\hat{b}_{st}$ estimated via a",
                   " %d-degree polynomial counterfactual (excl.\\ window $[z^*_t - %dM,\\, z^*_t + %dM)$",
                   " post-reform). B2C = Retail Trade + Food Services; B2B = Wholesale + Manufacturing.",
                   " 2020 excluded. \\textit{Source:} Author's calculations based on MTA CIT registry data."),
            poly_degree_post, excl_below_post, excl_above_post),
    "\\end{tablenotes}",
    "\\end{threeparttable}",
    "\\end{table}"
  )
  wtex("tab_bunching_means.tex", lines)
}

# ── Table 4.2: DiD results ────────────────────────────────────────────────────
# Spec 3 (sector + year FE) presented; CR1 SEs; stars from Webb bootstrap.

{
  cr1_se <- function(mod)
    sqrt(diag(vcovCL(mod, cluster = ~sector)))["B2C_Post"]
  
  # Point estimates (same across specs due to K=4 cluster structure)
  b_m  <- coef(did_main$m3)["B2C_Post"]
  b_a  <- coef(did_excl_2020_21$m3)["B2C_Post"]
  b_b  <- coef(did_full$m3)["B2C_Post"]
  
  se_m <- cr1_se(did_main$m3)
  se_a <- cr1_se(did_excl_2020_21$m3)
  se_b <- cr1_se(did_full$m3)
  
  p_m  <- did_main$wb3$p_value
  p_a  <- did_excl_2020_21$wb3$p_value
  p_b  <- did_full$wb3$p_value
  
  # Gap compression = |beta| / gap_pre * 100
  gc_m <- abs(b_m) / did_main$gap_pre   * 100
  gc_a <- abs(b_a) / did_excl_2020_21$gap_pre * 100
  gc_b <- abs(b_b) / did_full$gap_pre   * 100
  
  lines <- c(
    "% Auto-generated by Step 12 -- do not edit manually",
    "\\begin{table}[H]",
    "\\centering",
    "\\caption{\\textbf{Difference-in-Differences Estimation Results: Three Samples}}",
    "\\label{tab:did}",
    "\\begin{threeparttable}",
    "\\begin{tabular}{lccc}",
    "\\toprule",
    " & (1) Main & (2) Rob.\\ A & (3) Full Sample \\\\",
    " & (2020 excl.) & (2020+21 excl.) & (all years) \\\\",
    "\\midrule",
    "$\\hat{\\beta}$: Post $\\times$ Treated",
    sprintf("  & $%s%s$ & $%s%s$ & $%s%s$ \\\\",
            fmtf(b_m), wb_stars(p_m),
            fmtf(b_a), wb_stars(p_a),
            fmtf(b_b), wb_stars(p_b)),
    sprintf("  & $(%s)$ & $(%s)$ & $(%s)$ \\\\",
            fmtf(se_m), fmtf(se_a), fmtf(se_b)),
    sprintf("Webb $p$-value & %.3f & %.3f & %.3f \\\\", p_m, p_a, p_b),
    "\\addlinespace",
    sprintf("Pre-reform B2C mean $(\\bar{\\hat{b}})$ & $%.3f$ & $%.3f$ & $%.3f$ \\\\",
            did_main$b_pre_B2C, did_excl_2020_21$b_pre_B2C, did_full$b_pre_B2C),
    sprintf("Post-reform B2C mean $(\\bar{\\hat{b}})$ & $%.3f$ & $%.3f$ & $%.3f$ \\\\",
            did_main$b_post_B2C, did_excl_2020_21$b_post_B2C, did_full$b_post_B2C),
    sprintf("Pre-reform B2B mean $(\\bar{\\hat{b}})$ & $%.3f$ & $%.3f$ & $%.3f$ \\\\",
            did_main$b_pre_B2B, did_excl_2020_21$b_pre_B2B, did_full$b_pre_B2B),
    sprintf("Post-reform B2B mean $(\\bar{\\hat{b}})$ & $%.3f$ & $%.3f$ & $%.3f$ \\\\",
            did_main$b_post_B2B, did_excl_2020_21$b_post_B2B, did_full$b_post_B2B),
    "\\addlinespace",
    sprintf("Pre-reform enforcement gap & $%s$ & $%s$ & $%s$ \\\\",
            pm_fmt(did_main$gap_pre), pm_fmt(did_excl_2020_21$gap_pre), pm_fmt(did_full$gap_pre)),
    sprintf("Post-reform enforcement gap & $%s$ & $%s$ & $%s$ \\\\",
            pm_fmt(did_main$gap_post), pm_fmt(did_excl_2020_21$gap_post), pm_fmt(did_full$gap_post)),
    sprintf("Gap compression & %.0f\\%% & %.0f\\%% & %.0f\\%% \\\\",
            gc_m, gc_a, gc_b),
    "\\midrule",
    "Sector FE     & Yes & Yes & Yes \\\\",
    "Year FE       & Yes & Yes & Yes \\\\",
    "2020 excluded & Yes & Yes & No  \\\\",
    "2021 excluded & No  & Yes & No  \\\\",
    sprintf("$N$ (cells)   & %d  & %d  & %d  \\\\",
            nrow(results_main), nrow(results_excl_2020_21), nrow(results)),
    "\\bottomrule",
    "\\end{tabular}",
    "\\begin{tablenotes}[para,flushleft]\\footnotesize",
    "\\item \\textit{Notes:} Dependent variable: normalised bunching ratio $\\hat{b}_{st}$.",
    "CR1 cluster-robust standard errors (clustered by sector, $G = 4$) in parentheses.",
    "Significance stars based on Webb wild cluster bootstrap",
    "\\parencite{webb2023reworking} 6-point weights ($B = 9{,}999$ replications,",
    "restricted bootstrap); $^{*}p < 0.10$, $^{**}p < 0.05$, $^{***}p < 0.01$.",
    "With $G = 4$ clusters the minimum achievable two-sided $p$-value is",
    "$2/6^4 \\approx 0.002$; the bootstrap $p$-value falls below 0.10 but not 0.05,",
    "consistent with the coarseness of inference at $G = 4$ rather than a weak effect.",
    "Cross-sample stability of $\\hat{\\beta} \\in [-1.532,\\,-1.602]$ and the",
    "monotone event-study pattern are the primary evidence.",
    sprintf(paste0("Exclusion window: $[z^*_t - %dM,\\, z^*_t + %dM)$ post-reform.",
                   " \\textit{Source:} Author's calculations based on MTA CIT registry data."),
            excl_below_post, excl_above_post),
    "\\end{tablenotes}",
    "\\end{threeparttable}",
    "\\end{table}"
  )
  wtex("tab_did.tex", lines)
}

# ── Table 4.3: Event study -- all three samples ───────────────────────────────
# Conventional OLS SEs (explicitly disclosed in notes).
# Replaces both the main-spec-only table and the separate robustness version.

{
  all_yrs <- sort(unique(event_coefs$year))
  
  # Align all three sets on the same year grid
  ev <- list(
    main  = event_coefs          |> select(year, estimate, p.value),
    robA  = event_coefs_excl2021 |> select(year, estimate, p.value),
    full  = event_coefs_full     |> select(year, estimate, p.value)
  )
  
  lines <- c(
    "% Auto-generated by Step 12 -- do not edit manually",
    "\\begin{table}[H]",
    "\\centering",
    "\\caption{\\textbf{Event Study Coefficients: B2C Differential Bunching",
    "         Relative to the 2015 Reference Baseline}}",
    "\\label{tab:event_study}",
    "\\begin{threeparttable}",
    "\\setlength{\\tabcolsep}{0pt}",
    "\\small",
    "\\begin{tabular*}{\\linewidth}{l@{\\extracolsep{\\fill}}cccccc}",
    "\\toprule",
    " & \\multicolumn{2}{c}{Main Specification}",
    " & \\multicolumn{2}{c}{Robustness A}",
    " & \\multicolumn{2}{c}{Full Sample} \\\\",
    " & \\multicolumn{2}{c}{(2020 Omitted)}",
    " & \\multicolumn{2}{c}{(2020 \\& 2021 Omitted)}",
    " & \\multicolumn{2}{c}{(All Years)} \\\\",
    "\\cmidrule(lr){2-3} \\cmidrule(lr){4-5} \\cmidrule(lr){6-7}",
    "Year & $\\hat{\\beta}_t$ & $p$-value & $\\hat{\\beta}_t$ & $p$-value & $\\hat{\\beta}_t$ & $p$-value \\\\",
    "\\midrule",
    "\\multicolumn{7}{l}{\\textit{Pre-Reform Horizon}} \\\\[2pt]"
  )
  
  for (y in all_yrs[all_yrs < reform_year]) {
    get_cell <- function(ev_df) {
      row <- ev_df[ev_df$year == y, ]
      if (nrow(row) == 0) return(c("---", "---"))
      if (row$estimate == 0 && row$p.value == 1)
        return(c("\\multicolumn{2}{c}{0.000\\; (Baseline)}", NA))
      est_str <- if (row$estimate >= 0) sprintf("$+$%.3f", row$estimate) else
        sprintf("$-$%.3f", abs(row$estimate))
      c(est_str, fmtp(row$p.value))
    }
    cells <- lapply(ev, get_cell)
    if (is.na(cells$main[2])) {
      lines <- c(lines, sprintf(
        "%d & %s & %s & %s \\\\",
        y, cells$main[1], cells$robA[1], cells$full[1]))
    } else {
      lines <- c(lines, sprintf(
        "%d & %s & %s & %s & %s & %s & %s \\\\",
        y,
        cells$main[1], cells$main[2],
        cells$robA[1], cells$robA[2],
        cells$full[1], cells$full[2]))
    }
  }
  # Reference year
  lines <- c(lines,
             paste0("2015 & \\multicolumn{2}{c}{0.000\\; (Baseline)}",
                    " & \\multicolumn{2}{c}{0.000\\; (Baseline)}",
                    " & \\multicolumn{2}{c}{0.000\\; (Baseline)} \\\\"),
             "\\midrule",
             "\\multicolumn{7}{l}{\\textit{Post-Reform Horizon}} \\\\[2pt]"
  )
  
  for (y in all_yrs[all_yrs > reform_year - 1]) {
    if (y == reform_year - 1) next  # already handled 2015
    get_cell <- function(ev_df) {
      row <- ev_df[ev_df$year == y, ]
      if (nrow(row) == 0) return(c("\\multicolumn{2}{c}{---}", NA))
      if (row$estimate == 0 && row$p.value == 1)
        return(c("\\multicolumn{2}{c}{0.000\\; (Baseline)}", NA))
      p_val  <- row$p.value
      stars  <- if (p_val < 0.01) "^{***}" else if (p_val < 0.05) "^{**}" else
        if (p_val < 0.10) "^{*}" else ""
      est_str <- if (row$estimate >= 0) sprintf("$+$%.3f%s", row$estimate, stars) else
        sprintf("$-$%.3f%s", abs(row$estimate), stars)
      c(est_str, fmtp(p_val))
    }
    cells <- lapply(ev, get_cell)
    # Check if any cell is a multicolumn (---) marker
    mc_main <- grepl("multicolumn", cells$main[1])
    mc_robA <- grepl("multicolumn", cells$robA[1])
    if (mc_main || mc_robA) {
      lines <- c(lines, sprintf(
        "%d & %s & %s & %s \\\\",
        y,
        if (mc_main) cells$main[1] else paste(cells$main, collapse=" & "),
        if (mc_robA) cells$robA[1] else paste(cells$robA, collapse=" & "),
        if (grepl("multicolumn", cells$full[1])) cells$full[1] else
          paste(cells$full, collapse=" & ")
      ))
    } else {
      lines <- c(lines, sprintf(
        "%d & %s & %s & %s & %s & %s & %s \\\\",
        y,
        cells$main[1], cells$main[2],
        cells$robA[1], cells$robA[2],
        cells$full[1], cells$full[2]))
    }
  }
  
  lines <- c(lines,
             "\\midrule",
             "Conventional SE & \\multicolumn{2}{c}{(see note)}",
             "& \\multicolumn{2}{c}{(see note)} & \\multicolumn{2}{c}{(see note)} \\\\",
             "\\bottomrule",
             "\\end{tabular*}",
             "\\begin{tablenotes}[para,flushleft]\\footnotesize",
             "\\item \\textit{Notes:} Point estimates are $\\hat{\\beta}_t$ from a dynamic",
             "event study with sector and year fixed effects; 2015 is the omitted baseline.",
             "Standard errors and $p$-values are conventional OLS (not wild cluster bootstrap);",
             "the event study is used to visualise the dynamic pattern of parallel pre-trends",
             "and post-reform divergence, while formal inference on the treatment effect",
             "rests on the DiD Webb bootstrap reported in Table~\\ref{tab:did}.",
             "$^{*}p < 0.10$, $^{**}p < 0.05$, $^{***}p < 0.01$.",
             "\\textit{Source:} Author's calculations based on MTA CIT registry data.",
             "\\end{tablenotes}",
             "\\end{threeparttable}",
             "\\end{table}"
  )
  wtex("tab_event_study.tex", lines)
}

# ── Table A.3: Full b-hat by sector x year (with converged flag) ──────────────

{
  sectors_ordered <- c("Retail", "Food service", "Wholesale", "Manufacturing")
  labels_ordered  <- c("Retail", "Food Service", "Wholesale", "Manufacturing")
  groups          <- c("B2C","B2C","B2B","B2B")
  yrs <- sort(unique(results_main$year))
  
  # Wide format: one row per sector, columns = years
  wide <- results_main |>
    select(sector, year, b, b_se, converged) |>
    arrange(sector, year)
  
  # Flag any sector-year that did not converge (M < B at cap)
  nc_cells <- wide |> filter(!is.na(converged) & converged == FALSE) |>
    mutate(flag = paste0(sector, " ", year))
  
  yr_header <- paste(yrs, collapse = " & ")
  
  lines <- c(
    "% Auto-generated by Step 12 -- do not edit manually",
    "\\renewcommand{\\arraystretch}{0.9}",
    "\\begin{table}[H]",
    "\\centering",
    "\\caption{\\textbf{Normalised Bunching Estimates $\\hat{b}_{st}$",
    "         by Sector and Year (Main Sample)}}",
    "\\label{tab:app_bunching_main}",
    "\\begin{threeparttable}",
    "\\setlength{\\tabcolsep}{0pt}",
    "\\small",
    sprintf("\\begin{tabular*}{\\linewidth}{l@{\\extracolsep{\\fill}}%s}",
            paste(rep("c", length(yrs)), collapse="")),
    "\\toprule",
    sprintf("Sector & %s \\\\", yr_header),
    "\\midrule",
    "\\multicolumn{10}{l}{\\textit{B2C Treatment Group}} \\\\[2pt]"
  )
  
  for (si in 1:4) {
    s <- sectors_ordered[si]
    lbl <- labels_ordered[si]
    if (si == 3) lines <- c(lines, "[6pt]",
                            "\\multicolumn{10}{l}{\\textit{B2B Control Group}} \\\\[2pt]")
    
    b_vals  <- sapply(yrs, function(y) {
      row <- wide[wide$sector == s & wide$year == y, ]
      if (nrow(row) == 0) return("---")
      conv_flag <- if (!is.na(row$converged) && row$converged == FALSE) "\\dag" else ""
      sprintf("%.3f%s", row$b, conv_flag)
    })
    se_vals <- sapply(yrs, function(y) {
      row <- wide[wide$sector == s & wide$year == y, ]
      if (nrow(row) == 0 || is.na(row$b_se)) return("")
      sprintf("(%.3f)", row$b_se)
    })
    lines <- c(lines,
               sprintf("\\quad %-16s & %s \\\\", lbl, paste(b_vals,  collapse=" & ")),
               sprintf("               & %s \\\\[4pt]", paste(se_vals, collapse=" & "))
    )
  }
  
  # Group means
  b2c_m <- sapply(yrs, function(y)
    mean(results_main$b[results_main$type=="B2C" & results_main$year==y], na.rm=TRUE))
  b2b_m <- sapply(yrs, function(y)
    mean(results_main$b[results_main$type=="B2B" & results_main$year==y], na.rm=TRUE))
  gaps  <- b2c_m - b2b_m
  
  lines <- c(lines,
             "\\midrule",
             sprintf("\\textit{B2C Mean} & %s \\\\",
                     paste(sapply(b2c_m, function(x) sprintf("%.3f", x)), collapse=" & ")),
             sprintf("\\textit{B2B Mean} & %s \\\\",
                     paste(sapply(b2b_m, function(x) sprintf("%.3f", x)), collapse=" & ")),
             "\\midrule",
             sprintf("\\textit{Gap (B2C $-$ B2B)} & %s \\\\",
                     paste(sapply(gaps, pm_fmt), collapse=" & ")),
             "\\bottomrule",
             "\\end{tabular*}",
             "\\begin{tablenotes}[para,flushleft]\\footnotesize",
             sprintf(paste0("\\item \\textit{Notes:} $\\hat{b}_{st}$ from a %d-degree polynomial",
                            " counterfactual. Exclusion window $[z^*_t - %dM,\\, z^*_t + %dM)$ post-reform,",
                            " $[z^*_t - %dM,\\, z^*_t + %dM)$ pre-reform.",
                            " Bootstrap standard errors (%d replications) in parentheses."),
                     poly_degree_post, excl_below_post, excl_above_post,
                     excl_below_pre,  excl_above_pre, n_boot),
             "Negative values indicate deficits relative to the counterfactual.",
             "2020 excluded from this table."
  )
  if (nrow(nc_cells) > 0) {
    lines <- c(lines,
               sprintf("\\item[$\\dag$] Missing mass did not reach bunching mass within the cap",
                       "($z^U_t = z^*_t + %dM$); upper bound set to cap.", excl_above_post),
               sprintf("Capped cells: %s.", paste(nc_cells$flag, collapse="; "))
    )
  }
  lines <- c(lines,
             "\\textit{Source:} Author's calculations based on MTA CIT registry data.",
             "\\end{tablenotes}",
             "\\end{threeparttable}",
             "\\end{table}"
  )
  wtex("tab_app_bunching_main.tex", lines)
}

# ── Table A.2: Density discontinuity ratios ───────────────────────────────────
# N_j- = count in 1M bin immediately below threshold
# N_j+ = count in 1M bin immediately above threshold
# Ratio = N_j- / N_j+
#
# FIX: build tbl with explicit lapply + do.call(bind_rows) to avoid the
# bind_rows(list, list) ambiguity that dropped the label column.
# Use a plain lookup list instead of named-vector iteration to avoid
# the names(s_label) == length-0 bug.

{
  make_dr_year <- function(yr, thresh) {
    df |>
      filter(year == yr) |>
      mutate(side = case_when(
        bin_mid == thresh - 0.5 ~ "below",
        bin_mid == thresh + 0.5 ~ "above",
        TRUE                    ~ NA_character_
      )) |>
      filter(!is.na(side)) |>
      select(sector, side, count) |>
      pivot_wider(names_from = side, values_from = count) |>
      left_join(sector_meta, by = "sector") |>       # adds type, label
      mutate(year = yr,
             ratio = round(below / pmax(above, 1L), 1))
  }
  
  density_ratio_tbl <- do.call(bind_rows, list(
    make_dr_year(2014, threshold_pre),
    make_dr_year(2017, threshold_post)
  ))
  
  # Sector display order: explicit list of (sector key, display label) pairs
  sector_rows <- list(
    list(key = "Retail",        disp = "Retail Trade",    group = "B2C"),
    list(key = "Food service",  disp = "Food Services",   group = "B2C"),
    list(key = "Wholesale",     disp = "Wholesale Trade", group = "B2B"),
    list(key = "Manufacturing", disp = "Manufacturing",   group = "B2B")
  )
  
  lines <- c(
    "% Auto-generated by Step 12 -- do not edit manually",
    "\\begin{table}[H]",
    "\\centering",
    "\\caption{\\textbf{Revenue Density Discontinuities at the Active",
    "         Registration Threshold}}",
    "\\label{tab:app_density_ratios}",
    "\\begin{threeparttable}",
    "\\setlength{\\tabcolsep}{0pt}",
    "\\small",
    "\\begin{tabular*}{\\linewidth}{l@{\\extracolsep{\\fill}}cccccc}",
    "\\toprule",
    "& \\multicolumn{3}{c}{\\textit{Pre-Reform: $z^*_1 = 10$M MNT (2014)}}",
    "& \\multicolumn{3}{c}{\\textit{Post-Reform: $z^*_2 = 50$M MNT (2017)}} \\\\",
    "\\cmidrule(lr){2-4}\\cmidrule(lr){5-7}",
    "Sector & $N_{j^-}$\\tnote{a} & $N_{j^+}$\\tnote{b} & Ratio\\tnote{c}",
    "       & $N_{j^-}$\\tnote{a} & $N_{j^+}$\\tnote{b} & Ratio\\tnote{c} \\\\",
    "\\midrule",
    "\\multicolumn{7}{l}{\\textit{B2C Treatment Group}} \\\\[2pt]"
  )
  
  prev_group <- "B2C"
  for (sr in sector_rows) {
    if (sr$group == "B2B" && prev_group == "B2C") {
      lines <- c(lines, "[6pt]",
                 "\\multicolumn{7}{l}{\\textit{B2B Control Group}} \\\\[2pt]")
    }
    prev_group <- sr$group
    
    r14  <- density_ratio_tbl[density_ratio_tbl$sector == sr$key &
                                density_ratio_tbl$year   == 2014, ]
    r17  <- density_ratio_tbl[density_ratio_tbl$sector == sr$key &
                                density_ratio_tbl$year   == 2017, ]
    if (nrow(r14) == 0 || nrow(r17) == 0) next
    
    lines <- c(lines, sprintf(
      "\\quad %-16s & %s & %s & %.1f & %s & %s & %.1f \\\\",
      sr$disp,
      formatC(r14$below,  format = "d", big.mark = ","),
      formatC(r14$above,  format = "d", big.mark = ","),
      r14$ratio,
      formatC(r17$below,  format = "d", big.mark = ","),
      formatC(r17$above,  format = "d", big.mark = ","),
      r17$ratio
    ))
  }
  
  lines <- c(lines,
             "\\bottomrule",
             "\\end{tabular*}",
             "\\begin{tablenotes}[para,flushleft]\\footnotesize",
             "\\item \\textit{Source:} MTA CIT registry data.",
             "\\item[a] Firm count in the 1M MNT bin immediately below the threshold",
             "  ($[z^*_t - 1\\text{M},\\, z^*_t)$).",
             "\\item[b] Firm count in the 1M MNT bin immediately above the threshold",
             "  ($[z^*_t,\\, z^*_t + 1\\text{M})$).",
             "\\item[c] Density ratio $N_{j^-} / N_{j^+}$; values near 1 indicate",
             "  no discontinuity.",
             "\\end{tablenotes}",
             "\\end{threeparttable}",
             "\\end{table}"
  )
  wtex("tab_app_density_ratios.tex", lines)
}

# ── Table A.4: 2020 + 2021 COVID estimates (merged) ───────────────────────────

{
  covid_dat <- results |>
    filter(year %in% c(2020, 2021)) |>
    select(sector, type, year, b, b_se, b_ci_lo, b_ci_hi) |>
    arrange(type, sector, year)
  
  sector_order <- c("Retail","Food service","Wholesale","Manufacturing")
  sector_labs  <- c("Retail (B2C)","Food Service (B2C)",
                    "Wholesale (B2B)","Manufacturing (B2B)")
  
  lines <- c(
    "% Auto-generated by Step 12 -- do not edit manually",
    "\\begin{table}[H]",
    "\\centering",
    "\\caption{\\textbf{Bunching Estimates in Pandemic Years (2020 and 2021)}}",
    "\\label{tab:app_covid}",
    "\\begin{threeparttable}",
    "\\setlength{\\tabcolsep}{10pt}",
    "\\small",
    "\\begin{tabular}{lrrrrrr}",
    "\\toprule",
    " & \\multicolumn{3}{c}{2020} & \\multicolumn{3}{c}{2021} \\\\",
    "\\cmidrule(lr){2-4}\\cmidrule(lr){5-7}",
    "Sector & $\\hat{b}$ & SE & 95\\% CI & $\\hat{b}$ & SE & 95\\% CI \\\\",
    "\\midrule"
  )
  
  for (si in seq_along(sector_order)) {
    s   <- sector_order[si]
    lbl <- sector_labs[si]
    r20 <- covid_dat |> filter(sector == s, year == 2020)
    r21 <- covid_dat |> filter(sector == s, year == 2021)
    fmt_row_covid <- function(r) {
      if (nrow(r) == 0) return("--- & --- & ---")
      sprintf("%.3f & %.3f & [%.3f,\\; %.3f]", r$b, r$b_se, r$b_ci_lo, r$b_ci_hi)
    }
    lines <- c(lines,
               sprintf("%-20s & %s & %s \\\\", lbl, fmt_row_covid(r20), fmt_row_covid(r21)))
  }
  
  # Group means
  for (yr in c(2020, 2021)) {
    rd <- covid_dat |> filter(year == yr)
    b2c_m <- mean(rd$b[rd$type == "B2C"], na.rm = TRUE)
    b2b_m <- mean(rd$b[rd$type == "B2B"], na.rm = TRUE)
    if (yr == 2020) {
      lines <- c(lines, "\\midrule",
                 sprintf("\\textit{B2C mean} & %.3f & & & %.3f & & \\\\",
                         b2c_m,
                         mean(covid_dat$b[covid_dat$type=="B2C" & covid_dat$year==2021],
                              na.rm=TRUE)),
                 sprintf("\\textit{B2B mean} & %.3f & & & %.3f & & \\\\",
                         b2b_m,
                         mean(covid_dat$b[covid_dat$type=="B2B" & covid_dat$year==2021],
                              na.rm=TRUE)),
                 sprintf("\\textit{Gap (B2C $-$ B2B)} & %s & & & %s & & \\\\",
                         pm_fmt(b2c_m - b2b_m),
                         pm_fmt(
                           mean(covid_dat$b[covid_dat$type=="B2C" & covid_dat$year==2021],
                                na.rm=TRUE) -
                             mean(covid_dat$b[covid_dat$type=="B2B" & covid_dat$year==2021],
                                  na.rm=TRUE)
                         ))
      )
    }
  }
  lines <- c(lines,
             "\\bottomrule",
             "\\end{tabular}",
             "\\begin{tablenotes}[para,flushleft]\\footnotesize",
             "\\item \\textit{Notes:} 2020 observations are excluded from the Main and",
             "Robustness A samples but retained in the Full sample.",
             "2021 observations are included in Main but excluded from Robustness A.",
             sprintf("Bootstrap standard errors based on %d replications.", n_boot),
             "Pandemic conditions likely inflate retail bunching and suppress food service;",
             "estimates should not be interpreted as steady-state behaviour.",
             "\\textit{Source:} Author's calculations based on MTA CIT registry data.",
             "\\end{tablenotes}",
             "\\end{threeparttable}",
             "\\end{table}"
  )
  wtex("tab_app_covid.tex", lines)
}

# ── Table B.1: Manufacturing-only DiD ────────────────────────────────────────
# Control group = Manufacturing only (drops Wholesale).
# HC2 robust SEs (not wild cluster: G drops to 3 when Wholesale excluded).

{
  res_mfg_main <- results_main |> filter(sector != "Wholesale")
  res_mfg_robA <- results_excl_2020_21 |> filter(sector != "Wholesale")
  res_mfg_full <- results |> filter(sector != "Wholesale")
  
  fit_mfg <- function(dat) {
    m  <- lm(b ~ factor(sector) + factor(year) + B2C_Post, data = dat)
    b  <- coef(m)["B2C_Post"]
    se <- sqrt(diag(vcovHC(m, type = "HC2")))["B2C_Post"]
    p  <- coeftest(m, vcov = vcovHC(m, type = "HC2"))["B2C_Post", "Pr(>|t|)"]
    stars <- if (p < 0.01) "^{***}" else if (p < 0.05) "^{**}" else
      if (p < 0.10) "^{*}" else ""
    list(b = b, se = se, p = p, stars = stars, n = nrow(dat))
  }
  
  r_m <- fit_mfg(res_mfg_main)
  r_a <- fit_mfg(res_mfg_robA)
  r_b <- fit_mfg(res_mfg_full)
  
  lines <- c(
    "% Auto-generated by Step 12 -- do not edit manually",
    "\\begin{table}[H]",
    "\\centering",
    "\\caption{\\textbf{Difference-in-Differences Sensitivities:",
    "         Manufacturing-Only Control Group}}",
    "\\label{tab:app_robust_mfg}",
    "\\begin{threeparttable}",
    "\\setlength{\\tabcolsep}{12pt}",
    "\\small",
    "\\begin{tabular}{lccc}",
    "\\toprule",
    " & (1) Main & (2) Robustness A & (3) Full Sample \\\\",
    " & (2020 excl.) & (2020+21 excl.) & (all years) \\\\",
    "\\midrule",
    "$\\hat{\\beta}$: $\\text{Post} \\times \\text{Treated}$",
    sprintf("  & $%s%s$ & $%s%s$ & $%s%s$ \\\\",
            fmtf(r_m$b), r_m$stars, fmtf(r_a$b), r_a$stars, fmtf(r_b$b), r_b$stars),
    sprintf("  & $(%s)$ & $(%s)$ & $(%s)$ \\\\",
            fmtf(r_m$se), fmtf(r_a$se), fmtf(r_b$se)),
    "\\midrule",
    "Sector + Year FE & Yes & Yes & Yes \\\\",
    sprintf("$N$ (cells) & %d & %d & %d \\\\",
            r_m$n, r_a$n, r_b$n),
    "\\bottomrule",
    "\\end{tabular}",
    "\\begin{tablenotes}[para,flushleft]\\footnotesize",
    "\\item \\textit{Notes:} HC2 heteroskedasticity-robust standard errors in",
    "parentheses (wild cluster bootstrap not applied here as $G = 3$).",
    "$^{*}p < 0.10$, $^{**}p < 0.05$, $^{***}p < 0.01$.",
    "Treatment group: B2C (Retail, Food Services).",
    "Control group: Manufacturing only (Wholesale excluded).",
    "\\textit{Source:} Author's calculations based on MTA CIT registry data.",
    "\\end{tablenotes}",
    "\\end{threeparttable}",
    "\\end{table}"
  )
  wtex("tab_app_robust_mfg.tex", lines)
}

# ── Table C.1: Polynomial degree sensitivity ─────────────────────────────────
# Re-run estimate_bunching for selected cells at degrees 5, 6, 7, 8.

{
  poly_cells <- list(
    list(sector = "Retail",        year = 2015, thresh = threshold_pre,
         elo = excl_below_pre, ehi = excl_above_pre),
    list(sector = "Food service",  year = 2015, thresh = threshold_pre,
         elo = excl_below_pre, ehi = excl_above_pre),
    list(sector = "Wholesale",     year = 2015, thresh = threshold_pre,
         elo = excl_below_pre, ehi = excl_above_pre),
    list(sector = "Manufacturing", year = 2015, thresh = threshold_pre,
         elo = excl_below_pre, ehi = excl_above_pre),
    list(sector = "Retail",        year = 2022, thresh = threshold_post,
         elo = excl_below_post, ehi = excl_above_post),
    list(sector = "Food service",  year = 2022, thresh = threshold_post,
         elo = excl_below_post, ehi = excl_above_post)
  )
  
  poly_results <- lapply(poly_cells, function(pc) {
    d <- df |> filter(sector == pc$sector, year == pc$year) |> arrange(bin_mid)
    b_vals <- sapply(5:8, function(deg) {
      est <- tryCatch(
        estimate_bunching(d$count, d$bin_mid, pc$thresh, pc$elo, pc$ehi, deg),
        error = function(e) NULL)
      if (is.null(est) || is.na(est$b)) NA_real_ else round(est$b, 3)
    })
    c(label = paste(pc$sector, pc$year), b_vals)
  })
  
  lines <- c(
    "% Auto-generated by Step 12 -- do not edit manually",
    "\\begin{table}[H]",
    "\\centering",
    "\\caption{\\textbf{Sensitivity of Bunching Estimates to Polynomial Degree:",
    "         Selected Sector-Year Cells}}",
    "\\label{tab:app_robust_polynomial}",
    "\\begin{threeparttable}",
    "\\setlength{\\tabcolsep}{12pt}",
    "\\small",
    "\\begin{tabular}{lcccc}",
    "\\toprule",
    " & Degree 5 & Degree 6 & Degree 7 & Degree 8 \\\\",
    sprintf("Sector-Year & & & (Baseline, $p=%d$) & \\\\", poly_degree_post),
    "\\midrule",
    sprintf("\\multicolumn{5}{l}{\\textit{Panel A: Pre-Reform (2015, $z^* = %dM$ MNT)}} \\\\[2pt]",
            threshold_pre)
  )
  for (i in 1:4) {
    r <- poly_results[[i]]
    lines <- c(lines, sprintf(
      "\\quad %-20s & %s & %s & %s & %s \\\\",
      r["label"],
      ifelse(is.na(as.numeric(r[2])), "---", fmtf(as.numeric(r[2]))),
      ifelse(is.na(as.numeric(r[3])), "---", fmtf(as.numeric(r[3]))),
      ifelse(is.na(as.numeric(r[4])), "---", fmtf(as.numeric(r[4]))),
      ifelse(is.na(as.numeric(r[5])), "---", fmtf(as.numeric(r[5])))
    ))
  }
  lines <- c(lines,
             "[6pt]",
             sprintf("\\multicolumn{5}{l}{\\textit{Panel B: Post-Reform (2022, $z^* = %dM$ MNT)}} \\\\[2pt]",
                     threshold_post))
  for (i in 5:6) {
    r <- poly_results[[i]]
    lines <- c(lines, sprintf(
      "\\quad %-20s & %s & %s & %s & %s \\\\",
      r["label"],
      ifelse(is.na(as.numeric(r[2])), "---", fmtf(as.numeric(r[2]))),
      ifelse(is.na(as.numeric(r[3])), "---", fmtf(as.numeric(r[3]))),
      ifelse(is.na(as.numeric(r[4])), "---", fmtf(as.numeric(r[4]))),
      ifelse(is.na(as.numeric(r[5])), "---", fmtf(as.numeric(r[5])))
    ))
  }
  lines <- c(lines,
             "\\bottomrule",
             "\\end{tabular}",
             "\\begin{tablenotes}[para,flushleft]\\footnotesize",
             sprintf(paste0("\\item \\textit{Notes:} Exclusion window",
                            " $[z^*_t - %dM,\\, z^*_t + %dM)$ post-reform,",
                            " $[z^*_t - %dM,\\, z^*_t + %dM)$ pre-reform."),
                     excl_below_post, excl_above_post, excl_below_pre, excl_above_pre),
             sprintf("Degree %d is the baseline specification.", poly_degree_post),
             "Estimates are generally stable across degrees 5--8.",
             "\\textit{Source:} Author's calculations based on MTA CIT registry data.",
             "\\end{tablenotes}",
             "\\end{threeparttable}",
             "\\end{table}"
  )
  wtex("tab_app_robust_polynomial.tex", lines)
}

# ── Table C.2: excl_lo sweep DiD ─────────────────────────────────────────────
# Replaces the old window-width table.
# Shows DiD and enforcement gap for excl_lo in {2,3,4,5} (locked spec = 2).
# excl_lo=4,5 extend the window into the ascending pre-notch slope,
# picking up non-bunching mass and inflating b-hat -- shown to break here.

{
  s <- sweep_did  # from Step 5b
  
  lines <- c(
    "% Auto-generated by Step 12 -- do not edit manually",
    "\\begin{table}[H]",
    "\\centering",
    "\\caption{\\textbf{Sensitivity of DiD Estimate to Bunching Window Width",
    "         ($\\texttt{excl\\_lo}$ Sweep)}}",
    "\\label{tab:app_excl_lo}",
    "\\begin{threeparttable}",
    "\\setlength{\\tabcolsep}{8pt}",
    "\\small",
    "\\begin{tabular}{ccccccc}",
    "\\toprule",
    "$\\delta$ (bins below $z^*$)\\tnote{a} & B2C pre & B2C post & B2B pre & B2B post",
    "  & Gap pre & Gap post & DiD & \\% narrow \\\\",
    "\\midrule"
  )
  for (i in seq_len(nrow(s))) {
    r <- s[i, ]
    marker <- if (r$excl_lo == excl_below_post) "\\dag" else ""
    lines <- c(lines, sprintf(
      "$%d%s$ & %.3f & %.3f & %.3f & %.3f & %s & %s & $%s$ & %.0f\\%% \\\\",
      r$excl_lo, marker,
      r$B2C_pre, r$B2C_post, r$B2B_pre, r$B2B_post,
      pm_fmt(r$gap_pre), pm_fmt(r$gap_post),
      fmtf(r$DiD), r$pct_narrowing
    ))
  }
  lines <- c(lines,
             "\\bottomrule",
             "\\end{tabular}",
             "\\begin{tablenotes}[para,flushleft]\\footnotesize",
             sprintf(paste0("\\item \\textit{Notes:} Each row re-estimates all sector-year",
                            " cells with a bunching window of $[z^*_t - \\delta\\text{M},\\, z^*_t)$.",
                            " DiD = $(\\Delta\\bar{b}_{B2C}) - (\\Delta\\bar{b}_{B2B})$, main sample",
                            " (2020 excluded). \\%% narrowing $= (\\text{gap}_{\\text{pre}} -",
                            " \\text{gap}_{\\text{post}}) / \\text{gap}_{\\text{pre}} \\times 100$.")),
             sprintf("\\item[$\\dag$] Locked specification ($\\delta = %d$).",
                     excl_below_post),
             "\\item[a] Window width in 1M MNT bins below the threshold.",
             "DiD sign is stable at $\\delta \\in \\{2,3\\}$; larger windows",
             "extend into the ascending pre-notch slope and are not reported as admissible.",
             "\\textit{Source:} Author's calculations based on MTA CIT registry data.",
             "\\end{tablenotes}",
             "\\end{threeparttable}",
             "\\end{table}"
  )
  wtex("tab_app_excl_lo.tex", lines)
}

cat("\n✓ LaTeX tables written to output_dir:\n")
cat("  tab_descriptive_stats.tex     -- Table 3.1\n")
cat("  tab_bunching_means.tex        -- Table 4.1\n")
cat("  tab_did.tex                   -- Table 4.2 (Webb p-values embedded)\n")
cat("  tab_event_study.tex           -- Table 4.3/B.2 combined (conventional SEs flagged)\n")
cat("  tab_app_bunching_main.tex     -- Table A.3 (converged flag added)\n")
cat("  tab_app_density_ratios.tex    -- Table A.2 (threshold_zones cut)\n")
cat("  tab_app_covid.tex             -- Tables A.4+A.5 merged\n")
cat("  tab_app_robust_mfg.tex        -- Table B.1\n")
cat("  tab_app_robust_polynomial.tex -- Table C.1\n")
cat("  tab_app_excl_lo.tex           -- Table C.2 (excl_lo sweep, replaces window table)\n")
cat("\nIn your LaTeX document use: \\input{output/tab_xxx.tex}\n")
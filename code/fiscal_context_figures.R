# =============================================================================
# VAT AND THE FISCAL CONTEXT
# Descriptive macro-fiscal series for the Institutional Background chapter
# Evidence from the VAT Registration Threshold in Mongolia
#
# PURPOSE
#   Produce the aggregate fiscal series that frame the 2016 reform:
#     (1) Tax revenue / GDP          -- overall revenue effort
#     (2) VAT revenue / GDP          -- size of the VAT base collected
#     (3) VAT revenue / tax revenue  -- VAT's share of the tax system
#     (4) Real VAT revenue           -- CPI-deflated VAT collections
#
#   Each series is plotted over time with a vertical marker on the reform
#   year (January 2016: threshold 10M -> 50M MNT + ebarimt launch), so the
#   pre- and post-reform periods read off at a glance. 
#
# INPUTS  (data/raw/)
#   nso_budget_revenue_clean.csv  -- General Government Budget revenue, NSO
#                                    (indicator, year, amount in million MNT;
#                                     converted from the NSO "МОНГОЛ УЛСЫН
#                                     НЭГДСЭН ТӨСВИЙН ОРЛОГО" workbook)
#   GDP.csv                       -- NSO GDP, current prices, quarterly,
#                                    million MNT (xlsx payload; read_excel)
#   CPI.csv                       -- NSO CPI, monthly index, multiple base
#                                    years (xlsx payload; read_excel)
#
# OUTPUT  (output/)
#   fiscal_context_series.csv                -- tidy annual series behind the figures
#   [if WRITE_FIGURES] fig_vat_tax_share.pdf       -- MAIN TEXT
#   [if WRITE_FIGURES] fig_fiscal_ratios_panel.pdf -- APPENDIX (2x2)
#   [if WRITE_FIGURES] fig_tax_to_gdp.pdf / fig_vat_to_gdp.pdf /
#                      fig_vat_to_tax.pdf / fig_real_vat.pdf  -- single panels
# =============================================================================


# =============================================================================
# STEP 0 -- Packages
# =============================================================================
packages <- c("tidyverse", "readxl", "scales", "patchwork")
invisible(lapply(packages, library, character.only = TRUE))


# =============================================================================
# STEP 1 -- Parameters
# =============================================================================

project_dir <- "~/Documents/GraSPP/vat-threshold-mongolia"

raw_dir    <- file.path(project_dir, "data/raw")
output_dir <- file.path(project_dir, "output")
fig_dir    <- file.path(project_dir, "paper/Figures")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

nso_path <- file.path(raw_dir, "nso_budget_revenue_clean.csv")
gdp_path <- file.path(raw_dir, "GDP.csv")
cpi_path <- file.path(raw_dir, "CPI.csv")

# Reform year: January 2016 -- VAT threshold 10M -> 50M MNT + ebarimt launch.
# The vertical marker is drawn at reform_year - 0.5 so it sits on the
# boundary between the last pre-reform year and the first post-reform year,
# exactly as in bunching_enforcement_gap.R.
reform_year <- 2016

# Plot window. Ratios go back to 2005; the real-VAT series is limited by the
# CPI splice and effectively starts in 2006. 2025 is preliminary (partial-year
# execution) and is dropped by default.
year_min <- 2005
year_max <- 2024

# Base year for the real (CPI-deflated) VAT series: constant <base_year> MNT.
cpi_base_year <- 2015

WRITE_FIGURES <- TRUE   # set TRUE to render the PDFs

# ── Shared visual language (identical to the bunching figures) ────────────────
col_series    <- "#1E88E5"   # primary line  (matches B2B blue)
col_series_2  <- "#E53935"   # secondary line (matches B2C red)
col_reform    <- "black"
reform_label  <- "2016: threshold 10->50M MNT\n+ ebarimt launched"

theme_fiscal <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      legend.position  = "top",
      panel.grid.minor = element_blank(),
      plot.caption     = element_text(size = 7.5, color = "gray50"),
      axis.text.x      = element_text(angle = 45, hjust = 1),
      strip.text       = element_text(face = "bold")
    )
}

# Reusable reform marker: vertical longdash line + italic annotation.
reform_layer <- function(label = TRUE, y = Inf, vjust = 1.5, hjust = 0,
                         size = 3) {
  layers <- list(
    geom_vline(xintercept = reform_year - 0.5, linetype = "longdash",
               color = col_reform, linewidth = 0.8)
  )
  if (label) {
    layers <- c(layers, list(
      annotate("text", x = reform_year - 0.3, y = y,
               vjust = vjust, hjust = hjust, size = size,
               fontface = "italic", color = "gray30",
               label = reform_label)
    ))
  }
  layers
}


# =============================================================================
# STEP 2 -- Load and process the raw series
# =============================================================================

# ── 2a. Budget revenue (NSO) -- million MNT, current prices ───────────────────
# indicator strings come from the cleaned NSO workbook.
nso_raw <- read_csv(nso_path, show_col_types = FALSE) |>
  mutate(
    year   = suppressWarnings(as.integer(year)),
    amount = suppressWarnings(as.numeric(amount_mln_mnt))
  ) |>
  filter(!is.na(year))

pick_nso <- function(name) {
  nso_raw |>
    filter(indicator == name) |>
    select(year, !!name := amount)
}

budget <- reduce(
  list(
    pick_nso("Total revenue and aid amount"),
    pick_nso("Tax revenue"),
    pick_nso("Value added tax")
  ),
  full_join, by = "year"
) |>
  rename(
    total_revenue = `Total revenue and aid amount`,
    tax_revenue   = `Tax revenue`,
    vat_revenue   = `Value added tax`
  ) |>
  arrange(year)


# ── 2b. GDP (NSO) -- current prices, quarterly -> annual, million MNT ─────────
# Column layout: 1 = statistic label, 2 = sector, 3 = "YYYY-Q", 4 = value.
# Keep only the current-price series (drop the constant-price "зэрэгцүүлэх" rows).
gdp_raw <- read_excel(gdp_path, sheet = 1)
names(gdp_raw)[1:4] <- c("stat", "sector", "period", "value")

gdp_annual <- gdp_raw |>
  filter(!str_detect(stat, "зэрэгцүүлэх")) |>  # "зэрэгцүүлэх" = constant prices
  mutate(
    year    = suppressWarnings(as.integer(str_sub(period, 1, 4))),
    quarter = suppressWarnings(as.integer(str_extract(period, "(?<=-)\\d+$"))),
    value   = suppressWarnings(as.numeric(value))
  ) |>
  filter(!is.na(year), !is.na(quarter), !is.na(value)) |>
  group_by(year) |>
  summarise(n_q = n_distinct(quarter),
            gdp = sum(value), .groups = "drop") |>
  filter(n_q == 4) |>                      # complete years only
  select(year, gdp)


# ── 2c. CPI (NSO) -- monthly index -> annual average, spliced across bases ────
# Column layout: 1 = base year ("2015=100" / "2020=100" / ...),
#                2 = group, 3 = "YYYY-MM", 4 = index value.
cpi_raw <- read_excel(cpi_path, sheet = 1)
names(cpi_raw)[1:4] <- c("base", "group", "month", "value")

cpi_annual_by_base <- cpi_raw |>
  mutate(
    year  = suppressWarnings(as.integer(str_sub(month, 1, 4))),
    value = suppressWarnings(as.numeric(value))
  ) |>
  filter(!is.na(year), !is.na(value), base %in% c("2015=100", "2020=100")) |>
  group_by(base, year) |>
  summarise(n_m = n(), cpi = mean(value), .groups = "drop") |>
  filter(n_m == 12) |>                     # complete years only
  select(base, year, cpi)

# Splice: use the 2015=100 series where available, then chain forward with the
# year-on-year growth of the 2020=100 series for the more recent years.
cpi_2015 <- cpi_annual_by_base |> filter(base == "2015=100") |>
  select(year, cpi15 = cpi)
cpi_2020 <- cpi_annual_by_base |> filter(base == "2020=100") |>
  select(year, cpi20 = cpi)

cpi_spliced <- full_join(cpi_2015, cpi_2020, by = "year") |> arrange(year)

cpi_spliced$cpi_link <- NA_real_
for (i in seq_len(nrow(cpi_spliced))) {
  if (!is.na(cpi_spliced$cpi15[i])) {
    cpi_spliced$cpi_link[i] <- cpi_spliced$cpi15[i]        # anchor on 2015 base
  } else if (i > 1 && !is.na(cpi_spliced$cpi_link[i - 1]) &&
             !is.na(cpi_spliced$cpi20[i]) && !is.na(cpi_spliced$cpi20[i - 1])) {
    growth <- cpi_spliced$cpi20[i] / cpi_spliced$cpi20[i - 1]
    cpi_spliced$cpi_link[i] <- cpi_spliced$cpi_link[i - 1] * growth
  }
}

base_val <- cpi_spliced$cpi_link[cpi_spliced$year == cpi_base_year]
stopifnot(length(base_val) == 1, !is.na(base_val))

cpi_index <- cpi_spliced |>
  filter(!is.na(cpi_link)) |>
  transmute(year, cpi_index = 100 * cpi_link / base_val)   # <base_year> = 100


# =============================================================================
# STEP 3 -- Assemble the annual analysis frame
# =============================================================================

fiscal <- budget |>
  left_join(gdp_annual, by = "year") |>
  left_join(cpi_index,  by = "year") |>
  filter(year >= year_min, year <= year_max) |>
  mutate(
    tax_to_gdp   = 100 * tax_revenue / gdp,                 # %
    vat_to_gdp   = 100 * vat_revenue / gdp,                 # %
    vat_to_tax   = 100 * vat_revenue / tax_revenue,         # %
    real_vat_bn  = (vat_revenue / (cpi_index / 100)) / 1e3, # billion constant MNT
    nom_vat_bn   = vat_revenue / 1e3                        # billion current MNT
  ) |>
  arrange(year)

# Long form for the faceted appendix panel.
metric_levels <- c(
  "Tax revenue / GDP (%)",
  "VAT revenue / GDP (%)",
  "VAT revenue / tax revenue (%)",
  sprintf("Real VAT revenue (bn %d MNT)", cpi_base_year)
)

fiscal_long <- fiscal |>
  select(year,
         `Tax revenue / GDP (%)`            = tax_to_gdp,
         `VAT revenue / GDP (%)`            = vat_to_gdp,
         `VAT revenue / tax revenue (%)`    = vat_to_tax,
         !!metric_levels[4]                 := real_vat_bn) |>
  pivot_longer(-year, names_to = "metric", values_to = "value") |>
  mutate(metric = factor(metric, levels = metric_levels)) |>
  filter(!is.na(value))

write_csv(
  fiscal |> select(year, total_revenue, tax_revenue, vat_revenue, gdp,
                   cpi_index, tax_to_gdp, vat_to_gdp, vat_to_tax,
                   real_vat_bn, nom_vat_bn),
  file.path(output_dir, "fiscal_context_series.csv")
)
cat("Saved fiscal_context_series.csv\n")
print(fiscal |> select(year, tax_to_gdp, vat_to_gdp, vat_to_tax, real_vat_bn))


# =============================================================================
# STEP 4 -- Figure builders
# =============================================================================

x_breaks <- seq(year_min, year_max, by = 2)

# ── Generic single-series time-series panel (bunching-figure styling) ─────────
line_panel <- function(df, yvar, y_lab, title, subtitle = NULL,
                       caption = NULL, pct = TRUE, label_reform = TRUE) {
  p <- ggplot(df, aes(x = year, y = .data[[yvar]])) +
    geom_line(color = col_series, linewidth = 1.4) +
    geom_point(color = col_series, size = 2.6) +
    reform_layer(label = label_reform) +
    scale_x_continuous(breaks = x_breaks) +
    labs(title = title, subtitle = subtitle, x = "Year", y = y_lab,
         caption = caption) +
    theme_fiscal()
  if (pct) p <- p + scale_y_continuous(labels = label_number(suffix = "%"))
  p
}

src_note <- paste0(
  "Source: NSO General Government Budget revenue and GDP (current prices); ",
  "NSO CPI for the deflator. Vertical line = January 2016 reform."
)

# ── Figure A (MAIN TEXT): VAT as a share of total tax revenue ────────────────
fig_vat_tax_share <- line_panel(
  fiscal, "vat_to_tax",
  y_lab    = "VAT revenue / total tax revenue",
  title    = "VAT's Share of Mongolian Tax Revenue, 2005-2024",
  subtitle = "Domestic + import VAT, net of refunds, as a percentage of total tax revenue",
  caption  = paste0(src_note,
                    "\nThe pre-reform threshold (10M MNT, fixed 2001-2015) lost real value to ",
                    "inflation, drawing more small firms into VAT before the 2016 increase.")
)

# ── Figure B (APPENDIX): 2x2 panel of all four fiscal series ─────────────────
fig_fiscal_panel <- ggplot(fiscal_long, aes(x = year, y = value)) +
  geom_line(color = col_series, linewidth = 1.1) +
  geom_point(color = col_series, size = 1.9) +
  reform_layer(label = FALSE) +
  facet_wrap(~ metric, scales = "free_y", ncol = 2) +
  scale_x_continuous(breaks = x_breaks) +
  labs(
    title    = "Mongolia: VAT and the Fiscal Aggregates Around the 2016 Reform",
    subtitle = "Annual series; vertical line marks the January 2016 VAT reform (threshold increase + ebarimt)",
    x = "Year", y = NULL,
    caption = src_note
  ) +
  theme_fiscal(base_size = 11)

# ── Single-panel versions (for flexible placement) ──────────────────────────
fig_tax_to_gdp <- line_panel(
  fiscal, "tax_to_gdp", "Tax revenue / GDP",
  "Mongolia: Tax Revenue as a Share of GDP, 2005-2024",
  caption = src_note)

fig_vat_to_gdp <- line_panel(
  fiscal, "vat_to_gdp", "VAT revenue / GDP",
  "Mongolia: VAT Revenue as a Share of GDP, 2005-2024",
  caption = src_note)

fig_vat_to_tax <- fig_vat_tax_share

fig_real_vat <- ggplot(fiscal, aes(x = year)) +
  geom_line(aes(y = real_vat_bn, color = "Real (constant prices)"), linewidth = 1.4) +
  geom_point(aes(y = real_vat_bn, color = "Real (constant prices)"), size = 2.6) +
  geom_line(aes(y = nom_vat_bn, color = "Nominal (current prices)"),
            linewidth = 0.9, linetype = "dashed") +
  reform_layer(label = TRUE) +
  scale_color_manual(values = c("Real (constant prices)"    = col_series,
                                "Nominal (current prices)"  = col_series_2),
                     name = NULL) +
  scale_x_continuous(breaks = x_breaks) +
  scale_y_continuous(labels = label_number(big.mark = ",")) +
  labs(
    title    = sprintf("Mongolia: Real VAT Revenue, 2006-2024 (constant %d MNT)", cpi_base_year),
    subtitle = "VAT collections deflated by the CPI; nominal series shown for reference",
    x = "Year", y = sprintf("VAT revenue (bn %d MNT)", cpi_base_year),
    caption = paste0(src_note, "  CPI spliced across the 2015=100 and 2020=100 base years.")
  ) +
  theme_fiscal()


# =============================================================================
# STEP 5 -- Render (only when WRITE_FIGURES = TRUE)
# =============================================================================

if (WRITE_FIGURES) {
  save_fig <- function(plot, file, width, height) {
    for (d in unique(c(output_dir, fig_dir))) {
      if (dir.exists(d))
        ggsave(file.path(d, file), plot, width = width, height = height)
    }
    cat("Saved", file, "\n")
  }
  save_fig(fig_vat_tax_share,   "fig_vat_tax_share.pdf",       11, 6.5)  # main text
  save_fig(fig_fiscal_panel,    "fig_fiscal_ratios_panel.pdf", 12, 8.5)  # appendix
  save_fig(fig_tax_to_gdp,      "fig_tax_to_gdp.pdf",          11, 6.5)
  save_fig(fig_vat_to_gdp,      "fig_vat_to_gdp.pdf",          11, 6.5)
  save_fig(fig_vat_to_tax,      "fig_vat_to_tax.pdf",          11, 6.5)
  save_fig(fig_real_vat,        "fig_real_vat.pdf",            11, 6.5)
} else {
  cat("\nWRITE_FIGURES = FALSE -- no PDFs written. Objects available:\n",
      "  fig_vat_tax_share, fig_fiscal_panel, fig_tax_to_gdp,\n",
      "  fig_vat_to_gdp, fig_vat_to_tax, fig_real_vat\n")
}

# =============================================================================
# STEP 6 -- Quick reference values for the write-up
# =============================================================================
cat("\n--- Values for the Institutional Background text ---\n")
fiscal |>
  filter(year %in% c(2005, 2010, 2013, 2014, 2015, 2016, 2019, 2023, 2024)) |>
  transmute(year,
            tax_to_gdp = round(tax_to_gdp, 1),
            vat_to_gdp = round(vat_to_gdp, 1),
            vat_to_tax = round(vat_to_tax, 1),
            real_vat_bn = round(real_vat_bn, 0)) |>
  print()

cat(sprintf("\nVAT / tax revenue: min %.1f%% (%d), max %.1f%% (%d) over %d-%d\n",
            min(fiscal$vat_to_tax, na.rm = TRUE),
            fiscal$year[which.min(fiscal$vat_to_tax)],
            max(fiscal$vat_to_tax, na.rm = TRUE),
            fiscal$year[which.max(fiscal$vat_to_tax)],
            year_min, year_max))

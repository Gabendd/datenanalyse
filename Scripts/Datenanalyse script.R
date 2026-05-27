setwd(
  "/Users/vfp/kDrive/Uni Bern/Einführung in die Datenanalyse mit R/Positron/Datenanalyse"
)
# ============================================================
# CHAPTER 1 — PROJECT SETUP
# ============================================================
# This chapter prepares the folders, packages, and helper files
# needed for the Swiss migration analysis.

paths <- c("data/raw", "data/processed", "plots", "tables")
required_packages <- c(
  "WDI",
  "dplyr",
  "tidyr",
  "ggplot2",
  "scales",
  "huxtable",
  "readr",
  "stringr"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

invisible(lapply(required_packages, library, character.only = TRUE))

source("Themes/Theme.R")

invisible(lapply(
  paths,
  function(path) {
    if (!dir.exists(path)) {
      dir.create(path, recursive = TRUE)
    }
  }
))

# Keep only the World Bank indicators needed for this study.
indicator_list_df <- tibble::tibble(
  Code = c(
    "NY.GDP.MKTP.KD.ZG",
    "SL.EMP.TOTL.SP.ZS",
    "SL.UEM.TOTL.ZS"
  ),
  Description = c(
    "GDP growth (annual %)",
    "Employment to population ratio, 15+, total (modeled ILO estimate)",
    "Unemployment, total (% of total labor force) (modeled ILO estimate)"
  )
)

utils::write.csv(
  indicator_list_df,
  file = "tables/indicator_list.csv",
  row.names = FALSE
)

indicators <- indicator_list_df$Code

# ============================================================
# CHAPTER 2 — SWISS IMMIGRATION DATA
# ============================================================
# This chapter loads the Swiss immigration file and creates a
# yearly Swiss net-migration series.

swiss_immigration_data <- readr::read_csv(
  "data/processed/swiss_immigration_countries_year.csv",
  show_col_types = FALSE
) |>
  dplyr::mutate(
    year = as.integer(year),
    net_migration = as.numeric(net_migration)
  ) |>
  dplyr::filter(
    !origin_en %in%
      c(
        "Afrique",
        "Amérique",
        "Asie",
        "Océanie",
        "EFTA countries",
        "Yugoslavia",
        "Serbia and Montenegro"
      ),
    !stringr::str_detect(origin_en, "Renseignements|Source|© OFS")
  ) |>
  dplyr::select(origin_en, year, net_migration) |>
  dplyr::arrange(origin_en, year)

swiss_row_count <- nrow(swiss_immigration_data)
swiss_origins <- sort(unique(swiss_immigration_data$origin_en))
swiss_year_range <- range(swiss_immigration_data$year, na.rm = TRUE)
swiss_missingness <- swiss_immigration_data |>
  dplyr::summarize(dplyr::across(everything(), ~ sum(is.na(.x))))
swiss_counts_by_origin <- swiss_immigration_data |>
  dplyr::count(origin_en, sort = TRUE)

swiss_immigration_yearly_data <- swiss_immigration_data |>
  dplyr::group_by(year) |>
  dplyr::summarize(
    net_migration = sum(net_migration, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::arrange(year)

swiss_yearly_row_count <- nrow(swiss_immigration_yearly_data)
swiss_yearly_year_range <- range(
  swiss_immigration_yearly_data$year,
  na.rm = TRUE
)
swiss_yearly_missingness <- swiss_immigration_yearly_data |>
  dplyr::summarize(dplyr::across(everything(), ~ sum(is.na(.x))))

# ============================================================
# CHAPTER 3 — WORLD BANK DATA FOR SWITZERLAND
# ============================================================
# This chapter downloads and keeps only the Swiss indicators needed
# for GDP growth, employment, and unemployment.

if (!file.exists("data/raw/world_bank_raw.rds")) {
  world_bank_raw_data <- WDI::WDI(
    country = "CHE",
    indicator = indicators,
    start = min(swiss_immigration_yearly_data$year, na.rm = TRUE),
    end = max(swiss_immigration_yearly_data$year, na.rm = TRUE),
    extra = TRUE
  )
  saveRDS(world_bank_raw_data, "data/raw/world_bank_raw.rds")
} else {
  temp_check <- readRDS("data/raw/world_bank_raw.rds")
  missing_in_file <- setdiff(indicators, names(temp_check))
  if (length(missing_in_file) > 0) {
    world_bank_raw_data <- WDI::WDI(
      country = "CHE",
      indicator = indicators,
      start = min(swiss_immigration_yearly_data$year, na.rm = TRUE),
      end = max(swiss_immigration_yearly_data$year, na.rm = TRUE),
      extra = TRUE
    )
    saveRDS(world_bank_raw_data, "data/raw/world_bank_raw.rds")
  } else {
    world_bank_raw_data <- temp_check
  }
  rm(temp_check, missing_in_file)
}

swiss_world_bank_data <- world_bank_raw_data |>
  dplyr::filter(iso3c == "CHE") |>
  dplyr::select(
    country,
    iso3c,
    year,
    dplyr::all_of(indicators)
  ) |>
  dplyr::rename(
    gdp_growth = NY.GDP.MKTP.KD.ZG,
    employment_ratio = SL.EMP.TOTL.SP.ZS,
    unemployment_total = SL.UEM.TOTL.ZS
  ) |>
  dplyr::arrange(year)

# ============================================================
# CHAPTER 4 — MERGED ANALYSIS DATA
# ============================================================
# This chapter combines Swiss migration data with the World Bank
# indicators so the regression can use one clean analysis table.

swiss_analysis_data <- swiss_immigration_yearly_data |>
  dplyr::left_join(
    swiss_world_bank_data |>
      dplyr::select(year, gdp_growth, employment_ratio, unemployment_total),
    by = "year"
  ) |>
  dplyr::arrange(year)

swiss_analysis_row_count <- nrow(swiss_analysis_data)
swiss_analysis_year_range <- range(swiss_analysis_data$year, na.rm = TRUE)
swiss_analysis_missingness <- swiss_analysis_data |>
  dplyr::summarize(dplyr::across(everything(), ~ sum(is.na(.x))))

# ============================================================
# CHAPTER 5 — VISUALIZATION
# ============================================================
# This chapter plots the main series used in the analysis.

swiss_analysis_plot_data <- swiss_analysis_data |>
  tidyr::pivot_longer(
    cols = c(net_migration, gdp_growth, employment_ratio, unemployment_total),
    names_to = "series",
    values_to = "value"
  ) |>
  dplyr::mutate(
    series = dplyr::recode(
      series,
      net_migration = "Net migration",
      gdp_growth = "GDP growth",
      employment_ratio = "Employment ratio",
      unemployment_total = "Unemployment total"
    )
  )

swiss_analysis_plot <- ggplot2::ggplot(
  swiss_analysis_plot_data,
  ggplot2::aes(x = year, y = value)
) +
  ggplot2::geom_line(color = "#2C7FB8", linewidth = 0.6) +
  ggplot2::geom_point(color = "#2C7FB8", size = 1) +
  ggplot2::facet_wrap(~series, scales = "free_y", ncol = 1) +
  ggplot2::labs(
    title = "Swiss migration and macroeconomic indicators",
    subtitle = "Yearly series for migration, GDP growth, employment, and unemployment.",
    x = "Year",
    y = NULL,
    caption = "Sources: Swiss immigration file and World Bank data"
  ) +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold"),
    plot.subtitle = ggplot2::element_text(color = "gray40"),
    strip.text = ggplot2::element_text(face = "bold")
  )

print(swiss_analysis_plot)

# ============================================================
# CHAPTER 6 — REGRESSION MODELS
# ============================================================
# This chapter estimates the relationship between Swiss net migration
# and GDP growth, with unemployment added as a control.

swiss_regression_data <- swiss_analysis_data |>
  dplyr::filter(
    !is.na(net_migration),
    !is.na(gdp_growth),
    !is.na(unemployment_total)
  )

swiss_migration_model <- stats::lm(
  net_migration ~ gdp_growth,
  data = swiss_regression_data
)

swiss_migration_model_controlled <- stats::lm(
  net_migration ~ gdp_growth + unemployment_total,
  data = swiss_regression_data
)

summary(swiss_migration_model)
summary(swiss_migration_model_controlled)


# ============================================================
# REGRESSION INTERPRETATION
# ============================================================

# MODEL 1: net_migration ~ gdp_growth
# ------------------------------------------------------------
# Coefficient for gdp_growth: 1163 (p = 0.591)
# Interpretation: No statistically significant relationship. For each 1% increase in GDP growth, net migration increases by 1163, but this effect is not distinguishable from zero.
# Model fit: R-squared = 0.009 (0.9% of variance explained) - very poor fit

# MODEL 2: net_migration ~ gdp_growth + unemployment_total
# ------------------------------------------------------------
# Coefficient for gdp_growth: -186.9 (p = 0.926) - Not statistically significant
# Coefficient for unemployment_total: 11709.4 (p = 0.00785) - Statistically significant at 1% level
# Interpretation: Holding GDP growth constant, each 1 percentage point increase in unemployment is associated with 11,709 additional net migrants. The GDP growth coefficient is not significant.
# Model fit: R-squared = 0.214 (21.4% of variance explained)
# Overall model significance: p = 0.02394 (significant at 5% level)

# ============================================================
# CHAPTER 7 — TABLE EXPORT (PDF)
# ============================================================
# Build a readable regression table and export as a PDF with borders
# and simple styling (uses huxtable::quick_pdf).

make_regression_table <- function(model, model_name) {
  results <- as.data.frame(coef(summary(model)))
  results$term <- rownames(results)
  rownames(results) <- NULL

  results |>
    dplyr::mutate(model = model_name, .before = term) |>
    dplyr::select(
      model,
      term,
      estimate = Estimate,
      std_error = `Std. Error`,
      statistic = `t value`,
      p_value = `Pr(>|t|)`
    )
}

swiss_regression_results <- dplyr::bind_rows(
  make_regression_table(swiss_migration_model, "GDP growth only"),
  make_regression_table(
    swiss_migration_model_controlled,
    "GDP growth + unemployment"
  )
)

swiss_regression_fit <- tibble::tibble(
  model = c("GDP growth only", "GDP growth + unemployment"),
  n = c(
    stats::nobs(swiss_migration_model),
    stats::nobs(swiss_migration_model_controlled)
  ),
  r_squared = c(
    summary(swiss_migration_model)$r.squared,
    summary(swiss_migration_model_controlled)$r.squared
  ),
  adjusted_r_squared = c(
    summary(swiss_migration_model)$adj.r.squared,
    summary(swiss_migration_model_controlled)$adj.r.squared
  )
)

# ============================================================
# KAPITEL 1 — SETUP: PAKETE, PFADE, INDIKATOREN
# ============================================================

required_packages <- c(
  "readr",
  "dplyr",
  "stringr",
  "tibble",
  "WDI",
  "ggplot2",
  "scales",
  "huxtable",
  "countrycode",
  "rnaturalearth",
  "rnaturalearthdata",
  "sf"
)

missing_packages <- setdiff(required_packages, rownames(installed.packages()))
if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

invisible(lapply(required_packages, library, character.only = TRUE))

paths <- c("data/raw", "data/processed", "tables", "figures")

invisible(lapply(
  paths,
  function(path) {
    if (!dir.exists(path)) {
      dir.create(path, recursive = TRUE)
    }
  }
))

# Nur die World-Bank-Indikatoren behalten, die für diese Studie nötig sind.
indicator_list_df <- tibble::tibble(
  Code = c(
    "NY.GDP.MKTP.KD.ZG",
    "SL.EMP.TOTL.SP.ZS",
    "SL.UEM.TOTL.ZS"
  ),
  Description = c(
    "BIP-Wachstum (jährlich in %)",
    "Beschäftigungsquote, 15+, insgesamt (modellierte ILO-Schätzung)",
    "Arbeitslosigkeit, insgesamt (% der Erwerbsbevölkerung) (modellierte ILO-Schätzung)"
  )
)

utils::write.csv(
  indicator_list_df,
  file = "tables/indicator_list.csv",
  row.names = FALSE
)

indicators <- indicator_list_df$Code

# ============================================================
# KAPITEL 2 — SCHWEIZER ZUWANDERUNGSDATEN
# ============================================================

swiss_immigration_data <- readr::read_csv(
  "data/processed/swiss_immigration_countries_year.csv",
  show_col_types = FALSE
) |>
  dplyr::mutate(
    year          = as.integer(year),
    net_migration = as.numeric(net_migration)
  ) |>
  dplyr::filter(
    !origin_en %in% c(
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

swiss_row_count       <- nrow(swiss_immigration_data)
swiss_origins         <- sort(unique(swiss_immigration_data$origin_en))
swiss_year_range      <- range(swiss_immigration_data$year, na.rm = TRUE)
swiss_missingness     <- swiss_immigration_data |>
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

swiss_yearly_row_count   <- nrow(swiss_immigration_yearly_data)
swiss_yearly_year_range  <- range(swiss_immigration_yearly_data$year, na.rm = TRUE)
swiss_yearly_missingness <- swiss_immigration_yearly_data |>
  dplyr::summarize(dplyr::across(everything(), ~ sum(is.na(.x))))

# ============================================================
# KAPITEL 3 — WORLD BANK DATEN FÜR DIE SCHWEIZ
# ============================================================

if (!file.exists("data/raw/world_bank_raw.rds")) {
  world_bank_raw_data <- WDI::WDI(
    country   = "CHE",
    indicator = indicators,
    start     = min(swiss_immigration_yearly_data$year, na.rm = TRUE),
    end       = max(swiss_immigration_yearly_data$year, na.rm = TRUE),
    extra     = TRUE
  )
  saveRDS(world_bank_raw_data, "data/raw/world_bank_raw.rds")
} else {
  temp_check       <- readRDS("data/raw/world_bank_raw.rds")
  missing_in_file  <- setdiff(indicators, names(temp_check))
  if (length(missing_in_file) > 0) {
    world_bank_raw_data <- WDI::WDI(
      country   = "CHE",
      indicator = indicators,
      start     = min(swiss_immigration_yearly_data$year, na.rm = TRUE),
      end       = max(swiss_immigration_yearly_data$year, na.rm = TRUE),
      extra     = TRUE
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
    gdp_growth         = NY.GDP.MKTP.KD.ZG,
    employment_ratio   = SL.EMP.TOTL.SP.ZS,
    unemployment_total = SL.UEM.TOTL.ZS
  ) |>
  dplyr::arrange(year)

# ============================================================
# KAPITEL 4 — ZUSAMMENGEFÜHRTE ANALYSEDATEN
# ============================================================

swiss_analysis_data <- swiss_immigration_yearly_data |>
  dplyr::left_join(
    swiss_world_bank_data |>
      dplyr::select(year, gdp_growth, employment_ratio, unemployment_total),
    by = "year"
  ) |>
  dplyr::arrange(year)

swiss_analysis_row_count   <- nrow(swiss_analysis_data)
swiss_analysis_year_range  <- range(swiss_analysis_data$year, na.rm = TRUE)
swiss_analysis_missingness <- swiss_analysis_data |>
  dplyr::summarize(dplyr::across(everything(), ~ sum(is.na(.x))))

# ============================================================
# KAPITEL 6 — REGRESSIONSMODELLE
# ============================================================

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

# ============================================================
# KAPITEL 7 — TABELLEN-EXPORT (HTML)
# ============================================================

make_regression_table <- function(models, model_names) {
  model_list <- stats::setNames(models, model_names)
  do.call(
    huxtable::huxreg,
    c(
      model_list,
      list(
        number_format = "%.3f",
        stars         = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
        statistics    = c(
          "N"                = "nobs",
          "R-Quadrat"        = "r.squared",
          "Angep. R-Quadrat" = "adj.r.squared",
          "Resid. SE"        = "sigma"
        )
      )
    )
  )
}

swiss_regression_table <- make_regression_table(
  models      = list(swiss_migration_model, swiss_migration_model_controlled),
  model_names = c("Nur BIP-Wachstum", "BIP-Wachstum + Arbeitslosigkeit")
)

swiss_regression_table

html_file <- normalizePath(
  "tables/swiss_regression_results.html",
  winslash  = "/",
  mustWork  = FALSE
)

invisible(
  huxtable::quick_html(
    swiss_regression_table,
    file = html_file,
    open = FALSE
  )
)

# ============================================================
# KAPITEL 8 — VISUALISIERUNGEN
# ============================================================

# --- Liniendiagramm: Nettozuwanderung über die Jahre ---
swiss_migration_over_time <- ggplot(
  swiss_immigration_yearly_data,
  aes(x = year, y = net_migration)
) +
  geom_line(color = "#1f77b4", linewidth = 1) +
  geom_point(color = "#1f77b4", size = 2) +
  labs(
    title   = "Nettozuwanderung der Schweiz über die Jahre",
    x       = "Jahr",
    y       = "Nettozuwanderung",
    caption = paste(
      "Datenbereich:",
      min(swiss_immigration_yearly_data$year),
      "bis",
      max(swiss_immigration_yearly_data$year)
    )
  ) +
  scale_y_continuous(
    labels = scales::comma_format(big.mark = ".", decimal.mark = ",")
  ) +
  theme_minimal() +
  theme(
    plot.title  = element_text(face = "bold", size = 14),
    axis.title  = element_text(size = 11),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank()
  )

swiss_migration_over_time

# --- Balkendiagramm: Nettozuwanderung nach Herkunftsland ---
latest_year <- max(swiss_immigration_data$year, na.rm = TRUE)

swiss_migration_by_country <- swiss_immigration_data |>
  dplyr::filter(year == latest_year) |>
  dplyr::arrange(desc(net_migration)) |>
  ggplot(aes(x = reorder(origin_en, net_migration), y = net_migration)) +
  geom_col(fill = "#2ca02c", alpha = 0.9) +
  geom_text(
    aes(label = scales::comma(net_migration, big.mark = ".", decimal.mark = ",")),
    hjust = -0.1,
    size  = 3.2
  ) +
  coord_flip() +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.08)),
    labels = scales::comma_format(big.mark = ".", decimal.mark = ",")
  ) +
  labs(
    title = paste("Nettozuwanderung nach Herkunftsland im Jahr", latest_year),
    x     = "Herkunftsland",
    y     = "Nettozuwanderung"
  ) +
  theme_minimal() +
  theme(
    plot.title  = element_text(face = "bold", size = 12),
    axis.title  = element_text(size = 11),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank()
  )

swiss_migration_by_country

# --- Kreisdiagramm: Anteil an gesamter Nettozuwanderung ---
totals_by_country <- swiss_immigration_data |>
  dplyr::filter(!is.na(origin_en)) |>
  dplyr::group_by(origin_en) |>
  dplyr::summarize(
    total_net = sum(net_migration, na.rm = TRUE),
    .groups   = "drop"
  ) |>
  dplyr::arrange(desc(total_net)) |>
  dplyr::filter(total_net > 0)

years_range <- range(swiss_immigration_data$year, na.rm = TRUE)

pie_total_net <- ggplot(
  totals_by_country,
  aes(x = "", y = total_net, fill = reorder(origin_en, total_net))
) +
  geom_col(color = "white", width = 1) +
  coord_polar(theta = "y") +
  labs(
    title    = "Anteil an der gesamten Nettozuwanderung nach Herkunftsland",
    subtitle = paste0("Zeitraum: ", years_range[1], "–", years_range[2]),
    fill     = "Herkunftsland",
    caption  = "Daten: BFS / OFS (bereinigt)"
  ) +
  theme_void() +
  theme(
    plot.title    = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10),
    legend.position = "right"
  )

pie_total_net
# ============================================================
# KAPITEL 9 — EUROPAKARTE DER NETTOZUWANDERUNG
# ============================================================
# Dunkleres Blau = höhere Nettozuwanderung in die Schweiz.
# Fokus auf Europa; Kosovo wird manuell mit XKX gemappt.

swiss_migration_map_data <- swiss_immigration_data |>
  dplyr::filter(year == latest_year, !is.na(origin_en)) |>
  dplyr::group_by(origin_en) |>
  dplyr::summarize(
    net_migration = sum(net_migration, na.rm = TRUE),
    .groups       = "drop"
  ) |>
  dplyr::mutate(
    iso_a3 = countrycode::countrycode(
      origin_en,
      origin       = "country.name",
      destination  = "iso3c",
      custom_match = c("Kosovo" = "XKX")
    )
  ) |>
  dplyr::filter(!is.na(iso_a3), net_migration > 0)

swiss_world_map_data <- rnaturalearth::ne_countries(
  scale       = "medium",
  returnclass = "sf"
) |>
  dplyr::select(iso_a3_eh, name_long, continent, geometry) |>
  dplyr::rename(iso_a3 = iso_a3_eh) |>
  # Nur europäische Länder behalten
  dplyr::filter(continent == "Europe") |>
  dplyr::left_join(swiss_migration_map_data, by = "iso_a3")

swiss_migration_world_map <- ggplot(swiss_world_map_data) +
  geom_sf(aes(fill = net_migration), color = "white", linewidth = 0.2) +
  scale_fill_gradient(
    low      = "#deebf7",
    high     = "#08519c",
    na.value = "grey90",
    name     = paste("Nettozuwanderung\n", latest_year),
    labels   = scales::comma_format(big.mark = ".", decimal.mark = ",")
  ) +
  labs(
    title   = paste("Nettozuwanderung nach Herkunftsland in Europa im Jahr", latest_year),
    caption = "Daten: BFS / OFS (bereinigt)"
  ) +
  theme_void() +
  theme(
    plot.title      = element_text(face = "bold", size = 14),
    legend.position = "right"
  )

swiss_migration_world_map

# ============================================================
# KAPITEL 9 — EUROPAKARTE DER NETTOZUWANDERUNG
# ============================================================
# Dunkleres Blau = höhere Nettozuwanderung in die Schweiz.
# Fokus auf Europa; Kosovo manuell gemappt; Russland ausgeschlossen.

swiss_migration_map_data <- swiss_immigration_data |>
  dplyr::filter(year == latest_year, !is.na(origin_en)) |>
  dplyr::group_by(origin_en) |>
  dplyr::summarize(
    net_migration = sum(net_migration, na.rm = TRUE),
    .groups       = "drop"
  ) |>
  dplyr::mutate(
    iso_a3 = countrycode::countrycode(
      origin_en,
      origin       = "country.name",
      destination  = "iso3c",
      custom_match = c("Kosovo" = "XKX")
    )
  ) |>
  dplyr::filter(!is.na(iso_a3), net_migration > 0)

swiss_world_map_data <- rnaturalearth::ne_countries(
  scale       = "medium",
  returnclass = "sf"
) |>
  dplyr::select(iso_a3_eh, name_long, continent, geometry) |>
  dplyr::rename(iso_a3 = iso_a3_eh) |>
  dplyr::filter(continent == "Europe", iso_a3 != "RUS") |>
  dplyr::left_join(swiss_migration_map_data, by = "iso_a3")

swiss_migration_world_map <- ggplot(swiss_world_map_data) +
  geom_sf(aes(fill = net_migration), color = "white", linewidth = 0.2) +
  scale_fill_gradient(
    low      = "#deebf7",
    high     = "#08519c",
    na.value = "grey90",
    name     = paste("Nettozuwanderung\n", latest_year),
    labels   = scales::comma_format(big.mark = ".", decimal.mark = ",")
  ) +
  labs(
    title   = paste("Nettozuwanderung nach Herkunftsland in Europa im Jahr", latest_year),
    caption = "Daten: BFS / OFS (bereinigt)"
  ) +
  theme_void() +
  theme(
    plot.title      = element_text(face = "bold", size = 14),
    legend.position = "right"
  )

swiss_migration_world_map
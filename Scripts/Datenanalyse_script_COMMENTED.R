# ============================================================
# DATENANALYSE: BIP-WACHSTUM VS. NETTOZUWANDERUNG IN DER SCHWEIZ
# Vollstaendig kommentiertes Analyseskript fuer externe Leser
# ============================================================

# ==== SECTION 1: PAKET-INITIALISIERUNG ====
# Alle erforderlichen R-Pakete laden: dplyr fuer Datenmanipulation,
# ggplot2 fuer Visualisierung, WDI fuer World-Bank-Daten, BFS fuer
# Schweizer Statistikdaten, und weitere Utilities.

required_packages <- c(
  "readr",
  "dplyr",
  "stringr",
  "ggplot2",
  "WDI",
  "BFS",
  "rnaturalearth",
  "countrycode",
  "huxtable",
  "scales",
  "tidyr",
  "data.table",
  "foreign",
  "boot",
  "sf",
  "tibble",
  "tmap",
  "mapview",
  "leaflet",
  "gridExtra",
  "cowplot"
)

# Installiere fehlende Pakete automatisch
missing_packages <- setdiff(required_packages, rownames(installed.packages()))
if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

# Lade alle Pakete in den aktuellen R-Workspace
invisible(lapply(required_packages, library, character.only = TRUE))

# Erstelle Arbeitsverzeichnisse fuer organisierte Datenspeicherung
paths <- c("data/raw", "data/processed", "tables", "figures")
invisible(lapply(paths, function(path) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
  }
}))

# ==== SECTION 2: INDIKATOREN DEFINIEREN ====
# Aus der World Bank API benote: BIP-Wachstum, Beschaeftigungsquote, Arbeitslosenquote
indicator_list_df <- tibble::tibble(
  Code = c("NY.GDP.MKTP.KD.ZG", "SL.EMP.TOTL.SP.ZS", "SL.UEM.TOTL.ZS"),
  Description = c(
    "BIP-Wachstum (jaehrlich in %)",
    "Beschaeftigungsquote, 15+, insgesamt",
    "Arbeitslosigkeit, insgesamt (%)"
  )
)

utils::write.csv(
  indicator_list_df,
  file = "tables/indicator_list.csv",
  row.names = FALSE
)
indicators <- indicator_list_df$Code

# ==== SECTION 3: SCHWEIZER ZUWANDERUNGSDATEN ====
# Lade jaehrliche Nettozuwanderungs-Daten nach Herkunftsland.
# Filtere Metadaten und aggregierte Regionen.

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
        "Amerique",
        "Asie",
        "Oceanie",
        "EFTA countries",
        "Yugoslavia",
        "Serbia and Montenegro"
      ),
    !stringr::str_detect(origin_en, "Renseignements|Source|© OFS")
  ) |>
  dplyr::select(origin_en, year, net_migration) |>
  dplyr::arrange(origin_en, year)

# Berechne Basis-Statistiken und aggregiere nach Jahr
swiss_row_count <- nrow(swiss_immigration_data)
swiss_origins <- sort(unique(swiss_immigration_data$origin_en))
swiss_year_range <- range(swiss_immigration_data$year, na.rm = TRUE)
swiss_missingness <- swiss_immigration_data |>
  dplyr::summarize(dplyr::across(everything(), ~ sum(is.na(.x))))
swiss_counts_by_origin <- swiss_immigration_data |>
  dplyr::count(origin_en, sort = TRUE)

# Aggregiere Zuwanderung nach Jahr (Summe ueber alle Laender)
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

# Berechne Mittelwert und Standardabweichung pro Jahr fuer Variabilitaet
mean_by_year <- swiss_immigration_data |>
  dplyr::group_by(year) |>
  dplyr::summarize(
    mean_net_migration = mean(net_migration, na.rm = TRUE),
    sd_net_migration = sd(net_migration, na.rm = TRUE),
    n_origins = dplyr::n(),
    .groups = "drop"
  ) |>
  dplyr::arrange(year)

# ==== SECTION 4: WORLD-BANK-DATEN ====
# Hole BIP-Wachstum, Beschaeftigungs- und Arbeitslosenquoten fuer die Schweiz.
# Cache wird verwendet, um API-Quotas zu sparen.

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
  rm(list = intersect(c("temp_check", "missing_in_file"), ls()))
}

# Formatiere und benenne Spalten fuer Klarheit
swiss_world_bank_data <- world_bank_raw_data |>
  dplyr::filter(iso3c == "CHE") |>
  dplyr::select(country, iso3c, year, dplyr::all_of(indicators)) |>
  dplyr::rename(
    gdp_growth = NY.GDP.MKTP.KD.ZG,
    employment_ratio = SL.EMP.TOTL.SP.ZS,
    unemployment_total = SL.UEM.TOTL.ZS
  ) |>
  dplyr::arrange(year)

# ==== SECTION 5: KOMBINIERTE ANALYSEDATEN ====
# Verbinde Zuwanderungs- und Wirtschaftsdaten nach Jahr.
# Dies ist der zentrale Datensatz fuer alle Regressionen und Tests.

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

message("Datenvorbereitung abgeschlossen. Bitte Skript weiterfuehren.")

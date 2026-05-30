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
  "sf",
  "rvest",
  "BFS",
  "tidyr",
  "tinytex"
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


mean_by_year <- swiss_immigration_data |>
  dplyr::group_by(year) |>
  dplyr::summarize(
    mean_net_migration = mean(net_migration, na.rm = TRUE),
    sd_net_migration = sd(net_migration, na.rm = TRUE),
    n_origins = dplyr::n(),
    .groups = "drop"
  ) |>
  dplyr::arrange(year)

# show full tibble in console
print(mean_by_year, n = Inf)

#Wir sehen, dass die durchschnittliche Nettozuwanderung pro Herkunftsland im Zeitverlauf stark schwankt, 
# mit einigen Jahren deutlich über 1000 und anderen Jahren unter 500. Zwischen 1996 und 1999 sogat negativ. 


# ============================================================
# KAPITEL 3 — WORLD BANK DATEN FÜR DIE SCHWEIZ
# ============================================================

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
# KAPITEL 4 — ZUSAMMENGEFÜHRTE ANALYSEDATEN
# ============================================================

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

# ── 5B: BFS API ──────────────────────────────────────────────────────────────
# Datensatz px-x-0103020200_102: Permanent resident population by Year and Citizenship
bfs_rds <- "data/raw/bfs_raw.rds"

if (file.exists(bfs_rds)) {
  bfs_raw <- readRDS(bfs_rds)
  message("Loaded BFS data from ", bfs_rds)
} else {
  bfs_raw <- tryCatch(
    {
      BFS::bfs_get_data(
        number_bfs = "px-x-0103020200_102",
        language = "en",
        valueFilter = list(
          Jahr = meta2 |>
            dplyr::filter(code == "Jahr") |>
            dplyr::pull(values) |>
            unlist(),
          Staatsangehörigkeit = meta2 |>
            dplyr::filter(code == "Staatsangehörigkeit") |>
            dplyr::pull(values) |>
            unlist()
        )
      )
    },
    error = function(e) {
      message("BFS API call failed: ", conditionMessage(e))
      NULL
    }
  )
  if (!is.null(bfs_raw)) {
    saveRDS(bfs_raw, bfs_rds)
    message("Saved BFS API result to ", bfs_rds)
  }
}

# ── 5C: CLEAN ────────────────────────────────────────────────────────────────
if (!is.null(bfs_raw)) {
  message("Column names: ", paste(names(bfs_raw), collapse = " | "))

  value_col <- names(bfs_raw)[length(names(bfs_raw))] # last col = value

  bfs_clean <- bfs_raw |>
    dplyr::rename_with(stringr::str_to_lower) |>
    dplyr::transmute(
      nationality = stringr::str_trim(.data[["citizenship"]]),
      year = as.integer(.data[["year"]]),
      pop_bfs = as.numeric(.data[[stringr::str_to_lower(value_col)]])
    ) |>
    dplyr::filter(!is.na(pop_bfs), !is.na(nationality), nationality != "")

  message(
    "BFS API returned ",
    nrow(bfs_clean),
    " rows (",
    dplyr::n_distinct(bfs_clean$nationality),
    " nationalities, years ",
    min(bfs_clean$year, na.rm = TRUE),
    "–",
    max(bfs_clean$year, na.rm = TRUE),
    ")"
  )
}

# ── 5D: MERGE ────────────────────────────────────────────────────────────────
if (!is.null(bfs_raw) && exists("bfs_clean")) {
  swiss_immigration_enhanced <- swiss_immigration_data |>
    dplyr::left_join(
      bfs_clean |>
        dplyr::select(
          origin_en = nationality,
          year,
          resident_population = pop_bfs
        ),
      by = c("origin_en", "year")
    )
}
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
        stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
        statistics = c(
          "N" = "nobs",
          "R-Quadrat" = "r.squared",
          "Angep. R-Quadrat" = "adj.r.squared",
          "Resid. SE" = "sigma"
        )
      )
    )
  )
}

swiss_regression_table <- make_regression_table(
  models = list(swiss_migration_model, swiss_migration_model_controlled),
  model_names = c("Nur BIP-Wachstum", "BIP-Wachstum + Arbeitslosigkeit")
)

swiss_regression_table

html_file <- normalizePath(
  "tables/swiss_regression_results.html",
  winslash = "/",
  mustWork = FALSE
)

# A) huxtable -> PDF (requires LaTeX; install tinytex::install_tinytex() once if needed)
huxtable::quick_pdf(
  swiss_regression_table,
  file = "tables/swiss_regression_results.pdf",
  open = FALSE
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
    title = "Nettozuwanderung der Schweiz über die Jahre",
    x = "Jahr",
    y = "Nettozuwanderung",
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
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(size = 11),
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
    aes(
      label = scales::comma(net_migration, big.mark = ".", decimal.mark = ",")
    ),
    hjust = -0.1,
    size = 3.2
  ) +
  coord_flip() +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.08)),
    labels = scales::comma_format(big.mark = ".", decimal.mark = ",")
  ) +
  labs(
    title = paste("Nettozuwanderung nach Herkunftsland im Jahr", latest_year),
    x = "Herkunftsland",
    y = "Nettozuwanderung"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    axis.title = element_text(size = 11),
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
    .groups = "drop"
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
    title = "Anteil an der gesamten Nettozuwanderung nach Herkunftsland",
    subtitle = paste0("Zeitraum: ", years_range[1], "–", years_range[2]),
    fill = "Herkunftsland",
    caption = "Daten: BFS / OFS (bereinigt)"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10),
    legend.position = "right"
  )

pie_total_net


# =================================================

swiss_migration_map_data <- swiss_immigration_data |>
  dplyr::filter(year == latest_year) |>
  dplyr::group_by(origin_en) |>
  dplyr::summarize(
    net_migration = sum(net_migration, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    iso_a3 = dplyr::case_when(
      origin_en == "Kosovo" ~ "XKX",
      TRUE ~ countrycode::countrycode(origin_en, "country.name", "iso3c")
    )
  )

swiss_world_map_all <- rnaturalearth::ne_countries(
  scale = "medium",
  returnclass = "sf"
) |>
  dplyr::select(iso_a3_eh, name_long, continent, geometry) |>
  dplyr::rename(iso_a3 = iso_a3_eh)


# Example: pattern to exclude island/overseas territory names (adjust list as needed)
territory_names <- c(
  "Azores", "Canary", "Madeira", "Faroe", "Svalbard", "Greenland",
  "Reunion", "Réunion", "Martinique", "Guadeloupe", "Mayotte",
  "Ceuta", "Melilla", "Saint", "Isle", "Islands?", "Guiana", "French Guiana"
)

swiss_world_map_data <- swiss_world_map_all |>
  dplyr::filter(
    continent == "Europe",
    !iso_a3 %in% c("RUS"),
    !stringr::str_detect(name_long, territory_pattern)
  ) |>
  dplyr::left_join(swiss_migration_map_data, by = "iso_a3")

swiss_migration_world_map <- ggplot(swiss_world_map_data) +
  geom_sf(aes(fill = net_migration), color = "white", linewidth = 0.2) +
  scale_fill_gradient(
    low = "#deebf7",
    high = "#08519c",
    na.value = "grey90",
    name = paste("Nettozuwanderung\n", latest_year),
    labels = scales::comma_format(big.mark = ".", decimal.mark = ",")
  ) +
  labs(
    title = paste(
      "Nettozuwanderung nach Herkunftsland für die Schweiz in Europa im Jahr",
      latest_year
    ),
    caption = paste0(
      "Daten: BFS / OFS (bereinigt)\n",
      "Ausgeschlossen: Russland sowie nicht-kontinentale europäische Territorien und Inselterritorien. ",
      "Grund: Die Karte zeigt nur das europäische Festland."
    )
  ) +
  theme_void() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "right",
    plot.caption = element_text(size = 9, hjust = 0)
  )

swiss_migration_world_map

# ============================================================
# ORGANISATION DER OBJEKTE UND DEUTSCHE KOMMENTARE
# ============================================================

# Erstelle ein Dataframe mit allen Herkunftsländern, ISO3-Codes und
# der kumulierten Nettozuwanderung (über alle Jahre oder spezifisches Jahr).
countries_df <- swiss_immigration_data |>
  dplyr::group_by(origin_en) |>
  dplyr::summarize(
    total_net = sum(net_migration, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    iso3 = dplyr::case_when(
      origin_en == "Kosovo" ~ "XKX",
      TRUE ~ countrycode::countrycode(origin_en, "country.name", "iso3c")
    )
  ) |>
  dplyr::arrange(dplyr::desc(total_net))

# Hilfsfunktion: gebe ein Objekt nur zurück, wenn es existiert (sonst NULL)
get_if_exists <- function(name) {
  if (exists(name, inherits = FALSE)) get(name) else NULL
}

# Liste mit allen importierten Datensätzen (sinnvolle Gruppierung für die Analyse)
imported_data_list <- list(
  swiss_immigration_data = get_if_exists("swiss_immigration_data"),
  swiss_immigration_yearly_data = get_if_exists(
    "swiss_immigration_yearly_data"
  ),
  swiss_world_bank_data = get_if_exists("swiss_world_bank_data"),
  swiss_analysis_data = get_if_exists("swiss_analysis_data"),
  swiss_immigration_enhanced = get_if_exists("swiss_immigration_enhanced"),
  bfs_raw = get_if_exists("bfs_raw"),
  world_bank_raw_data = get_if_exists("world_bank_raw_data")
)
# Entferne NULL-Einträge, damit die Liste sauber ist
imported_data_list <- imported_data_list[
  !vapply(imported_data_list, is.null, logical(1))
]

# Liste mit Plot-Objekten (ggplot2-Objekte)
plots_list <- list(
  swiss_migration_over_time = get_if_exists("swiss_migration_over_time"),
  swiss_migration_by_country = get_if_exists("swiss_migration_by_country"),
  pie_total_net = get_if_exists("pie_total_net"),
  swiss_migration_world_map = get_if_exists("swiss_migration_world_map")
)
plots_list <- plots_list[!vapply(plots_list, is.null, logical(1))]








# Liste mit Regressions-Modellen
model_list <- list(
  swiss_migration_model = get_if_exists("swiss_migration_model"),
  swiss_migration_model_controlled = get_if_exists(
    "swiss_migration_model_controlled"
  )
)
model_list <- model_list[!vapply(model_list, is.null, logical(1))]

# Liste mit Tabellen/zusätzlichen Dataframes
table_list <- list(
  swiss_regression_table = get_if_exists("swiss_regression_table"),
  countries_df = countries_df
)
table_list <- table_list[!vapply(table_list, is.null, logical(1))]

# Gesamtübersicht: alle gruppierten Objekte in einer Struktur sammeln
session_objects <- list(
  imported_data = imported_data_list,
  plots = plots_list,
  models = model_list,
  tables = table_list
)

# Speichere die Struktur für spätere Nutzung (z.B. in einem anderen Skript)
saveRDS(session_objects, file = "data/processed/session_objects.rds")

# Kurze Benachrichtigung auf Deutsch
message(
  "Objekte wurden in Listen zusammengefasst und nach data/processed/session_objects.rds gespeichert."
)

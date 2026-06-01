# ============================================================
# KAPITEL 1 — SETUP UND PAKETE
# ============================================================

# Benötigte Pakete laden
required_packages <- c(
  "readr",
  "dplyr",
  "stringr",
  "stringi",
  "ggplot2",
  "WDI",
  "BFS",
  "rnaturalearth",
  "countrycode",
  "huxtable",
  "scales",
  "data.table",
  "sf",
  "tibble"
)

missing_packages <- setdiff(required_packages, rownames(installed.packages()))
if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

invisible(lapply(required_packages, library, character.only = TRUE))

# Arbeitsverzeichnisse definieren
paths <- c("data", "tables", "figures")
invisible(lapply(paths, function(path) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
  }
}))

# ============================================================
# KAPITEL 2 — Daten Importieren und Aufbereiten
# ============================================================

# ============================================================
# KAPITEL 2A —  Schweizerische Einwanderungsdaten importieren und aufbereiten.
# ============================================================
#Bei der Swiss Immigration Dataien handelt es sich um auf die BFS Webseite manuell heruntergeladene CSV Datei.

#Hier wird das Immigration Data importiert, die Spalten werden in die richtigen Formate umgewandelt, und es werden nur die relevanten Herkunftsländer und Zeilen behalten.
# Danach wird die Daten nach Herkunftsland und Jahr sortiert. Nur die relevanten Variablen (origin_en, year, net_migration) werden behalten.
#Auch werden die Zeilen mit aggregierten Regionen (z.B. "Afrique", "Amérique", "Asie", "Océanie") sowie die Zeilen mit nicht-informativem Text
# (z.B. "Renseignements|Source|© OFS") herausgefiltert.

# Immigrationsdaten einlesen, bereinigen und filtern
RAW_swiss_immigration <- readr::read_csv(
  "data/swiss_immigration_countries_year.csv",
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

# Jetzt werden die Daten auf Jahresbasis aggregiert, um die Nettozuwanderung pro Jahr zu erhalten.
# Zusätzlich werden die durchschnittliche Nettozuwanderung und die Anzahl der Herkunftsländer pro Jahr berechnet.
YEARLY_swiss_immigration <- RAW_swiss_immigration |>
  dplyr::group_by(year) |>
  dplyr::summarize(
    net_migration = sum(net_migration, na.rm = TRUE),
    mean_net_migration = mean(net_migration, na.rm = TRUE),
    n_origins = dplyr::n(),
    .groups = "drop"
  ) |>
  dplyr::arrange(year)

summary(YEARLY_swiss_immigration$net_migration)
#Summary zeigt uns, dass wir teilweise Auswanderungen haben. Das Minimum von -10589
# zeigt, dass es Jahre gab, in denen die Schweiz insgesamt
# mehr Auswanderungen als Einwanderungen verzeichnete (negative Nettozuwanderung).
# Der Median (26290) liegt deutlich unter dem Mittelwert (27759), was auf
# einzelne Jahre mit hoher Zuwanderung hindeutet.

# Die deskriptive Analyse umfasst 15 europäische Herkunftsländer.
# Für die Regressionsanalyse werden die Daten auf Jahresbasis aggregiert.

# ============================================================
# KAPITEL 2B —  World Bank Indikatoren importieren und aufbereiten.
# ============================================================

# Hier importiere ich die Indikatoren der World Bank, die ich für die Analyse verwenden möchte. Mit meiner Masterarbeit arbeite
#ich so, indem ich zuerst die Indikator definiere dich ich brauch, und danach den API call sende.
# Bei World Bank Indikatoren handelt es sich um unsere Unabhängige Variable (BIP-Wachstum) und eine Kontrollvariable (Arbeitslosenquote).
# Die Abhänige Variable ist die Nettozuwanderung, die wir bereits vorher importiert und aufbereitet haben.
indicator_list_df <- tibble::tibble(
  Code = c(
    "NY.GDP.MKTP.KD.ZG",
    "SL.UEM.TOTL.ZS"
  ),

  #Und hier was für Konkrete Indikatoren es sind.
  Description = c(
    "BIP-Wachstum (jährlich in %)",
    "Arbeitslosigkeit, insgesamt (% der Erwerbsbevölkerung) (modellierte ILO-Schätzung)"
  )
)

#Die Indikatoren als Referenz Liste speichern.
indicators <- indicator_list_df$Code

#Jetzt die World Bank Indikatoren Mithilfe APi importieren.
# Das Skript prüft zuerst, ob die Datei bereits existiert. Wenn ja, wird sie geladen. Wenn nein, wird der API-Aufruf durchgeführt.
# Zusätzlich prüft das Skript, ob alle benötigten Indikatoren in der vorhandenen Datei enthalten sind.
# Wenn nicht, wird die Datei erneut mit den fehlenden Indikatoren aktualisiert.

# Definiere den Dateipfad
world_bank_file <- "data/world_bank_raw.rds"

# Definiere den Zeitrahmen (min/max Jahr der Schweizer Einwanderungsdaten). Somit werden nur die Jahre abgefragt, für die wir auch Einwanderungsdaten haben.
# Das spart Zeit und API-Aufrufe.
start_year <- min(YEARLY_swiss_immigration$year, na.rm = TRUE)
end_year <- max(YEARLY_swiss_immigration$year, na.rm = TRUE)

# Funktion zum Laden der Daten via API
fetch_world_bank_data <- function() {
  WDI::WDI(
    country = "CHE",
    indicator = indicators,
    start = start_year,
    end = end_year,
    extra = TRUE
  )
}

# Lade oder aktualisiere die Daten
if (!file.exists(world_bank_file)) {
  # Datei existiert nicht → API-Aufruf
  world_bank_raw_data <- fetch_world_bank_data()
  saveRDS(world_bank_raw_data, world_bank_file)
} else {
  # Datei existiert → Prüfe auf fehlende Indikatoren
  temp_check <- readRDS(world_bank_file)
  missing_in_file <- setdiff(indicators, names(temp_check))

  if (length(missing_in_file) > 0) {
    # Fehlende Indikatoren → API-Aufruf und Überschreiben
    world_bank_raw_data <- fetch_world_bank_data()
    saveRDS(world_bank_raw_data, world_bank_file)
  } else {
    # Alle Indikatoren vorhanden → Verwende die Datei
    world_bank_raw_data <- temp_check
  }
  # Bereinige temporäre Variablen
  rm(temp_check, missing_in_file)
}

names(world_bank_raw_data)

# Für die Analyse benötige ich nur die Spalten year, gdp_growth, employment_ratio und unemployment_total. Alle anderen Spalten werden entfernt.
# - NY.GDP.MKTP.KD.ZG (gdp_growth): Unabhängige Variable
# - SL.UEM.TOTL.ZS (unemployment_total): Kontrollvariable 1

# --- Jetzt verarbeite ich die World Bank Daten, um sie für die Analyse vorzubereiten. ---
# 1. Wähle Spalten: country, iso3c, year + alle Indikatoren
# 3. Benenne Indikatoren um:
#    - NY.GDP.MKTP.KD.ZG → gdp_growth (BIP-Wachstum)
#    - SL.UEM.TOTL.ZS → unemployment_total (Arbeitslosenquote)
# 4. Sortiere nach Jahr
swiss_world_bank_data <- world_bank_raw_data |>
  dplyr::select(year, dplyr::all_of(indicators)) |>
  dplyr::rename(
    bip_wachstum = NY.GDP.MKTP.KD.ZG,
    arbeitslosenquote = SL.UEM.TOTL.ZS
  ) |>
  dplyr::arrange(year)

# ============================================================
# KAPITEL 2C — ZUSAMMENGEFÜHRTE ANALYSEDATEN
# ============================================================

#Jetzt werden die aufbereiteten Einwanderungsdaten mit den World Bank Indikatoren zusammengeführt,
# um einen Datensatz zu erstellen, der alle benötigten Variablen für die Analyse enthält.

ANALYSIS_swiss <- YEARLY_swiss_immigration |>
  dplyr::left_join(
    swiss_world_bank_data,
    by = "year"
  ) |>
  dplyr::arrange(year)

cat("NA counts per variable:\n")
print(colSums(is.na(ANALYSIS_swiss)))

#Wir haben keine NA, was eine gute Nachricht ist.
# ============================================================
# KAPITEL 3 — BFS-DATEN AUFBEREITEN
# ============================================================

# Das Kapitel importiert die BFS Einwanderungsdaten auf Länderebene,
# um sie mit den Schweizer Einwanderungsdaten zusammenzuführen.

# Konfiguration: Hier wird den Datensatz definiert.
bfs_dataset_id <- "px-x-0103020200_102"
bfs_pop_rds <- "data/bfs_population_immigration.rds"


# Jetzt wir das BFS API gesendet. Zuerst wird geprüft, ob die Daten bereits lokal als RDS-Datei vorliegen. Wenn ja, werden sie geladen.
# Wenn nein, wird der API-Aufruf durchgeführt, um die Daten zu holen, und danach werden sie als RDS-Datei gespeichert, um zukünftige Aufrufe zu vermeiden.
if (file.exists(bfs_pop_rds)) {
  bfs_raw <- readRDS(bfs_pop_rds)
} else {
  bfs_raw <- BFS::bfs_get_data(
    number_bfs = bfs_dataset_id,
    language = "en",
    clean_names = TRUE
  )
  saveRDS(bfs_raw, bfs_pop_rds)
}

#Wie sieht die Struktur des BFS API aus ?
str(bfs_raw)


# Hier werden die BFS-Daten bereinigt und in ein einheitliches Format gebracht,
# damit sie später mit den Einwanderungsdaten der Schweiz zusammengeführt werden können.
#Zuerst wird das bfs clean erstelelt, es beinhaltet die Spalten citizenship, year und pop_bfs
#und nur Ländernamen, die bereits in den Schweizer Einwanderungsdaten enthalten sind.
bfs_clean <- bfs_raw |>
  filter(!is.na(citizenship), citizenship != 'Citizenship - total') |>
  group_by(citizenship, year) |>
  summarise(
    pop_bfs = sum(
      as.numeric(immigration_of_the_permanent_resident_population),
      na.rm = TRUE
    ),
    .groups = "drop"
  ) |>
  transmute(
    nationality = stringr::str_trim(citizenship),
    year = as.integer(year),
    pop_bfs = pop_bfs
  ) |>
  filter(!is.na(nationality), !is.na(year), !is.na(pop_bfs)) |>

  # Nur die Länder behalten, die bereits in den Schweizer Einwanderungsdaten enthalten sind
  # Mit dem package stringi werden die Ländernamen (z.B. "Türkiye" -> "Turkiye") für den Vergleich normalisert.
  mutate(
    nationality_normalized = stringi::stri_trans_general(
      nationality,
      "latin-ascii"
    )
  ) |>
  filter(
    nationality_normalized %in%
      stringi::stri_trans_general(
        RAW_swiss_immigration$origin_en,
        "latin-ascii"
      )
  ) |>
  select(-nationality_normalized)


#Hier werden die bereinigten BFS-Daten mit den Schweizer Einwanderungsdaten zusammengeführt, um einen Datensatz zu erstellen,
# der die Nettozuwanderung pro Herkunftsland und Jahr enthält.
MERGED_swiss_immigration <- RAW_swiss_immigration |>
  dplyr::mutate(
    year = as.integer(year),
    origin_en = stringr::str_trim(origin_en)
  ) |>
  dplyr::left_join(
    bfs_clean |>
      dplyr::rename(
        origin_en = nationality
      ) |>
      dplyr::select(-pop_bfs),
    by = c("origin_en", "year")
  )


# ============================================================
# KAPITEL 4 — BFS-DATEN Visualisieren
# ============================================================

# --- 2) BFS: Kantonsdaten für Karte laden und bereinigen
# Lade lokalen Cache oder frage API gezielt für latest_year_canton; bereinige mit dplyr.
if (file.exists(bfs_canton_rds)) {
  bfs_canton_raw <- readRDS(bfs_canton_rds)
} else {
  meta_canton <- tryCatch(
    BFS::bfs_get_metadata(number_bfs = bfs_dataset_id, language = "de"),
    error = function(e) NULL
  )

  canton_values <- NULL
  nationality_values <- NULL
  if (!is.null(meta_canton) && !is.null(meta_canton$values)) {
    if (length(meta_canton$values) >= 2) {
      canton_values <- meta_canton$values[[2]][-1] %||% meta_canton$values[[2]]
    }
    if (length(meta_canton$values) >= 3) {
      nationality_values <- meta_canton$values[[3]][-1] %||%
        meta_canton$values[[3]]
    }
  }

  bfs_canton_raw <- tryCatch(
    BFS::bfs_get_data(
      number_bfs = bfs_dataset_id,
      language = "de",
      query = list(
        Jahr = as.character(latest_year_canton),
        Kanton = if (!is.null(canton_values)) canton_values else NULL,
        Staatsangehörigkeit = if (!is.null(nationality_values)) {
          nationality_values
        } else {
          NULL
        },
        Geschlecht = "0",
        Altersklasse = "0"
      ),
      clean_names = TRUE
    ),
    error = function(e) {
      message("BFS-API-Kanton fehlgeschlagen: ", conditionMessage(e))
      NULL
    }
  )
  if (!is.null(bfs_canton_raw)) saveRDS(bfs_canton_raw, bfs_canton_rds)
}

if (!is.null(bfs_canton_raw) && nrow(bfs_canton_raw) > 0) {
  canton_cols <- c("kanton", "canton", "kanton_name")
  nat_cols2 <- c(
    "staatsangehoerigkeit",
    "staatsangehörigkeit",
    "citizenship",
    "nationality"
  )
  year_cols2 <- c("jahr", "year")
  val_cols2 <- c(
    "einwanderung_der_standigen_wohnbevolkerung",
    "immigration_from_abroad",
    "value"
  )

  present2 <- names(bfs_canton_raw)
  use_canton <- intersect(present2, canton_cols)
  use_nat2 <- intersect(present2, nat_cols2)
  use_year2 <- intersect(present2, year_cols2)
  use_val2 <- intersect(present2, val_cols2)

  bfs_canton_clean <- as.data.frame(bfs_canton_raw) %>%
    dplyr::mutate(
      canton = dplyr::coalesce(!!!rlang::syms(use_canton)),
      nationality = dplyr::coalesce(!!!rlang::syms(use_nat2)),
      year = as.integer(dplyr::coalesce(!!!rlang::syms(use_year2))),
      immigration_from_abroad = as.numeric(dplyr::coalesce(
        !!!rlang::syms(use_val2)
      ))
    ) %>%
    dplyr::filter(
      !is.na(canton),
      canton != "Schweiz",
      !is.na(nationality),
      nationality != "Schweiz",
      !is.na(year)
    ) %>%
    dplyr::mutate(
      canton = stringr::str_trim(as.character(canton)),
      nationality = stringr::str_trim(as.character(nationality))
    )

  bfs_canton_map_data <- bfs_canton_clean %>%
    dplyr::filter(year == latest_year_canton) %>%
    dplyr::group_by(canton) %>%
    dplyr::summarize(
      immigration_from_abroad = sum(immigration_from_abroad, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      canton_name = dplyr::case_when(
        canton == "Bern / Berne" ~ "Bern",
        canton == "Fribourg / Freiburg" ~ "Fribourg",
        canton == "Graubünden / Grigioni / Grischun" ~ "Graubünden",
        canton == "Valais / Wallis" ~ "Valais",
        TRUE ~ canton
      )
    )

  # Basis-Shape der Kantone laden und für spätere Visualisierung joinen (keine ggplot-Erzeugung hier)
  swiss_canton_map_data <- BFS::bfs_get_base_maps(
    geom = "kant",
    return_sf = TRUE
  ) %>%
    dplyr::left_join(
      bfs_canton_map_data %>%
        dplyr::select(canton_name, immigration_from_abroad),
      by = c("name" = "canton_name")
    )
} else {
  bfs_canton_clean <- NULL
  bfs_canton_map_data <- NULL
  swiss_canton_map_data <- NULL
}

# --- Ergebnisobjekte dieses Kapitels (für weitere Kapitel):
# bfs_clean, swiss_immigration_merged, bfs_canton_clean, bfs_canton_map_data, swiss_canton_map_data

# ============================================================
# KAPITEL 6 — REGRESSIONSMODELLE UND ROBUSTHEITSPRÜFUNGEN
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

correlation_test <- stats::cor.test(
  swiss_regression_data$gdp_growth,
  swiss_regression_data$net_migration,
  method = "pearson"
)
saveRDS(correlation_test, file = "data/processed/correlation_test_results.rds")

median_unemployment <- stats::median(
  swiss_regression_data$unemployment_total,
  na.rm = TRUE
)
mean_unemployment <- mean(
  swiss_regression_data$unemployment_total,
  na.rm = TRUE
)
quartile_unemployment <- stats::quantile(
  swiss_regression_data$unemployment_total,
  probs = c(0.25, 0.75),
  na.rm = TRUE,
  names = FALSE
)

run_split_t_test <- function(
  data,
  split_name,
  split_var,
  low_label,
  high_label
) {
  test_data <- data |>
    dplyr::mutate(.split_group = split_var) |>
    dplyr::filter(!is.na(.split_group), !is.na(net_migration))

  test_result <- stats::t.test(net_migration ~ .split_group, data = test_data)

  tibble::tibble(
    split = split_name,
    low_group = low_label,
    high_group = high_label,
    low_mean = mean(
      test_data$net_migration[!test_data$.split_group],
      na.rm = TRUE
    ),
    high_mean = mean(
      test_data$net_migration[test_data$.split_group],
      na.rm = TRUE
    ),
    statistic = unname(test_result$statistic),
    p_value = test_result$p.value,
    conf_low = test_result$conf.int[1],
    conf_high = test_result$conf.int[2]
  ) |>
    dplyr::mutate(test_object = list(test_result))
}

t_test_median_result <- run_split_t_test(
  swiss_regression_data,
  split_name = "Median-Split",
  split_var = swiss_regression_data$unemployment_total >= median_unemployment,
  low_label = "unter Median",
  high_label = "ab Median"
)

t_test_mean_result <- run_split_t_test(
  swiss_regression_data,
  split_name = "Mittelwert-Split",
  split_var = swiss_regression_data$unemployment_total >= mean_unemployment,
  low_label = "unter Mittelwert",
  high_label = "ab Mittelwert"
)

quartile_split_data <- swiss_regression_data |>
  dplyr::filter(
    unemployment_total <= quartile_unemployment[1] |
      unemployment_total >= quartile_unemployment[2]
  )

t_test_quartile_result <- run_split_t_test(
  quartile_split_data,
  split_name = "Quartils-Split (Q1 vs. Q4)",
  split_var = quartile_split_data$unemployment_total >=
    quartile_unemployment[2],
  low_label = "Q1 (niedrig)",
  high_label = "Q4 (hoch)"
)

t_test_summary_table <- dplyr::bind_rows(
  t_test_median_result,
  t_test_mean_result,
  t_test_quartile_result
) |>
  dplyr::select(-test_object)

split_test_objects <- list(
  median = t_test_median_result$test_object[[1]],
  mean = t_test_mean_result$test_object[[1]],
  quartiles = t_test_quartile_result$test_object[[1]],
  summary = t_test_summary_table
)

saveRDS(
  split_test_objects,
  file = "data/processed/t_test_unemployment_results_all_splits.rds"
)

utils::write.csv(
  t_test_summary_table,
  file = "tables/t_test_unemployment_results_all_splits.csv",
  row.names = FALSE
)

rolling_window_size <- 10L
rolling_years <- sort(unique(swiss_regression_data$year))
rolling_window_starts <- rolling_years[
  rolling_years <= max(rolling_years, na.rm = TRUE) - rolling_window_size + 1L
]

extract_model_rows <- function(model, window_start, window_end, model_name) {
  coef_table <- coef(summary(model))
  tibble::tibble(
    window_start = window_start,
    window_end = window_end,
    model_name = model_name,
    term = rownames(coef_table),
    estimate = coef_table[, "Estimate"],
    std_error = coef_table[, "Std. Error"],
    statistic = coef_table[, "t value"],
    p_value = coef_table[, "Pr(>|t|)"],
    r_squared = summary(model)$r.squared,
    adj_r_squared = summary(model)$adj.r.squared,
    n_obs = stats::nobs(model)
  )
}

rolling_model_store <- list()
rolling_regression_rows <- list()

for (window_start in rolling_window_starts) {
  window_end <- window_start + rolling_window_size - 1L
  window_data <- swiss_regression_data |>
    dplyr::filter(year >= window_start, year <= window_end)

  if (nrow(window_data) < 6) {
    next
  }

  simple_model <- stats::lm(net_migration ~ gdp_growth, data = window_data)
  controlled_model <- stats::lm(
    net_migration ~ gdp_growth + unemployment_total,
    data = window_data
  )

  rolling_model_store[[as.character(window_start)]] <- list(
    simple = simple_model,
    controlled = controlled_model
  )

  rolling_regression_rows[[
    length(rolling_regression_rows) + 1L
  ]] <- dplyr::bind_rows(
    extract_model_rows(
      simple_model,
      window_start,
      window_end,
      "Nur BIP-Wachstum"
    ),
    extract_model_rows(
      controlled_model,
      window_start,
      window_end,
      "BIP-Wachstum + Arbeitslosigkeit"
    )
  )
}

rolling_regression_results <- dplyr::bind_rows(rolling_regression_rows)
saveRDS(
  rolling_regression_results,
  file = "data/processed/rolling_regression_results.rds"
)
utils::write.csv(
  rolling_regression_results,
  file = "tables/rolling_regression_results.csv",
  row.names = FALSE
)

swiss_rolling_regression_models <- rolling_model_store

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

huxtable::quick_html(
  swiss_regression_table,
  file = "tables/swiss_regression_results.html",
  open = FALSE
)

# ============================================================
# KAPITEL 8 — VISUALISIERUNGEN
# ============================================================

swiss_migration_over_time <- ggplot2::ggplot(
  swiss_immigration_yearly_data,
  ggplot2::aes(x = year, y = net_migration)
) +
  ggplot2::geom_line(color = "#1f77b4", linewidth = 1) +
  ggplot2::geom_point(color = "#1f77b4", size = 2) +
  ggplot2::labs(
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
  ggplot2::scale_y_continuous(
    labels = scales::comma_format(big.mark = ".", decimal.mark = ",")
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold", size = 14),
    axis.title = ggplot2::element_text(size = 11),
    panel.grid.major = ggplot2::element_line(color = "gray90"),
    panel.grid.minor = ggplot2::element_blank()
  )

latest_year <- max(swiss_immigration_data$year, na.rm = TRUE)

swiss_migration_by_country <- swiss_immigration_data |>
  dplyr::filter(year == latest_year) |>
  dplyr::arrange(desc(net_migration)) |>
  ggplot2::ggplot(ggplot2::aes(
    x = reorder(origin_en, net_migration),
    y = net_migration
  )) +
  ggplot2::geom_col(fill = "#2ca02c", alpha = 0.9) +
  ggplot2::geom_text(
    ggplot2::aes(
      label = scales::comma(net_migration, big.mark = ".", decimal.mark = ",")
    ),
    hjust = -0.1,
    size = 3.2
  ) +
  ggplot2::coord_flip() +
  ggplot2::scale_y_continuous(
    expand = ggplot2::expansion(mult = c(0, 0.08)),
    labels = scales::comma_format(big.mark = ".", decimal.mark = ",")
  ) +
  ggplot2::labs(
    title = paste("Nettozuwanderung nach Herkunftsland im Jahr", latest_year),
    x = "Herkunftsland",
    y = "Nettozuwanderung"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold", size = 12),
    axis.title = ggplot2::element_text(size = 11),
    panel.grid.major = ggplot2::element_line(color = "gray90"),
    panel.grid.minor = ggplot2::element_blank()
  )

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

pie_total_net <- ggplot2::ggplot(
  totals_by_country,
  ggplot2::aes(x = "", y = total_net, fill = reorder(origin_en, total_net))
) +
  ggplot2::geom_col(color = "white", width = 1) +
  ggplot2::coord_polar(theta = "y") +
  ggplot2::labs(
    title = "Anteil an der gesamten Nettozuwanderung nach Herkunftsland",
    subtitle = paste0("Zeitraum: ", years_range[1], "–", years_range[2]),
    fill = "Herkunftsland",
    caption = "Daten: BFS / OFS (bereinigt)"
  ) +
  ggplot2::theme_void() +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold", size = 14),
    plot.subtitle = ggplot2::element_text(size = 10),
    legend.position = "right"
  )

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

territory_names <- c(
  "Azores",
  "Canary",
  "Madeira",
  "Faroe",
  "Svalbard",
  "Greenland",
  "Reunion",
  "Réunion",
  "Martinique",
  "Guadeloupe",
  "Mayotte",
  "Ceuta",
  "Melilla",
  "Saint",
  "Isle",
  "Islands?",
  "Guiana",
  "French Guiana"
)

swiss_world_map_data <- swiss_world_map_all |>
  dplyr::filter(
    continent == "Europe",
    !iso_a3 %in% c("RUS"),
    !stringr::str_detect(name_long, paste(territory_names, collapse = "|"))
  ) |>
  dplyr::left_join(swiss_migration_map_data, by = "iso_a3")

swiss_migration_world_map <- ggplot2::ggplot(swiss_world_map_data) +
  ggplot2::geom_sf(
    ggplot2::aes(fill = net_migration),
    color = "white",
    linewidth = 0.2
  ) +
  ggplot2::scale_fill_gradient(
    low = "#deebf7",
    high = "#08519c",
    na.value = "grey90",
    name = paste("Nettozuwanderung\n", latest_year),
    labels = scales::comma_format(big.mark = ".", decimal.mark = ",")
  ) +
  ggplot2::labs(
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
  ggplot2::theme_void() +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold", size = 14),
    legend.position = "right",
    plot.caption = ggplot2::element_text(size = 9, hjust = 0)
  )

scatter_gdp_migration <- ggplot2::ggplot(swiss_regression_data) +
  ggplot2::aes(x = gdp_growth, y = net_migration, color = unemployment_total) +
  ggplot2::geom_point(size = 3, alpha = 0.7) +
  ggplot2::geom_smooth(method = "loess", se = TRUE, color = "black") +
  ggplot2::scale_color_gradient(
    low = "blue",
    high = "red",
    name = "Arbeitslosigkeit (%)"
  ) +
  ggplot2::labs(
    title = "BIP-Wachstum vs. Nettozuwanderung, gefärbt nach Arbeitslosigkeit",
    subtitle = "LOESS-Glätter zur Visualisierung des nicht-linearen Zusammenhangs",
    x = "BIP-Wachstum (jährlich %)",
    y = "Nettozuwanderung"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold", size = 14),
    plot.subtitle = ggplot2::element_text(size = 10),
    axis.title = ggplot2::element_text(size = 11)
  )

hist_net_migration <- ggplot2::ggplot(swiss_regression_data) +
  ggplot2::aes(x = net_migration) +
  ggplot2::geom_histogram(
    ggplot2::aes(y = after_stat(density)),
    bins = 30,
    fill = "steelblue",
    color = "white",
    alpha = 0.8
  ) +
  ggplot2::geom_density(color = "red", linewidth = 1, alpha = 0.5) +
  ggplot2::geom_vline(
    ggplot2::aes(xintercept = mean(net_migration, na.rm = TRUE)),
    color = "darkred",
    linetype = "dashed",
    linewidth = 1
  ) +
  ggplot2::labs(
    title = "Verteilung der Nettozuwanderung",
    subtitle = "Mit überlagerter Dichtekurve und Mittelwertslinie",
    x = "Nettozuwanderung",
    y = "Dichte"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold", size = 14),
    plot.subtitle = ggplot2::element_text(size = 10)
  )

swiss_regression_data_quartiles <- swiss_regression_data |>
  dplyr::mutate(unemployment_quartile = dplyr::ntile(unemployment_total, 4)) |>
  dplyr::mutate(
    unemployment_quartile = factor(
      unemployment_quartile,
      levels = 1:4,
      labels = c("Q1 (niedrig)", "Q2", "Q3", "Q4 (hoch)")
    )
  )

boxplot_net_migration_quartiles <- ggplot2::ggplot(
  swiss_regression_data_quartiles,
  ggplot2::aes(
    x = unemployment_quartile,
    y = net_migration,
    fill = unemployment_quartile
  )
) +
  ggplot2::geom_boxplot(alpha = 0.8) +
  ggplot2::geom_jitter(width = 0.2, size = 2, alpha = 0.5, color = "black") +
  ggplot2::scale_fill_brewer(
    palette = "Blues",
    name = "Arbeitslosigkeits-Quartil"
  ) +
  ggplot2::labs(
    title = "Nettozuwanderung nach Quartilen der Arbeitslosigkeit",
    x = "Arbeitslosigkeits-Quartil",
    y = "Nettozuwanderung"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold", size = 14),
    axis.title = ggplot2::element_text(size = 11)
  )

# ============================================================
# KAPITEL 8B — EXPORT DER PLOTS (PNG)
# ============================================================

# Globale Plot-Export-Defaults
plot_export_defaults <- list(
  width = 10,
  height = 6,
  dpi = 300,
  bg = "white"
)

# Export-Hilfsfunktion
export_png <- function(plot, file, width = NULL, height = NULL, dpi = NULL) {
  ggsave(
    filename = file,
    plot = plot,
    width = width %||% plot_export_defaults$width,
    height = height %||% plot_export_defaults$height,
    dpi = dpi %||% plot_export_defaults$dpi,
    bg = plot_export_defaults$bg
  )
}

# Liste aller zu exportierenden Plots mit custom Dimensionen
plot_exports <- list(
  list(
    file = "figures/01_nettozuwanderung_over_time.png",
    plot = swiss_migration_over_time,
    width = 12,
    height = 6
  ),
  list(
    file = "figures/02_nettozuwanderung_by_country.png",
    plot = swiss_migration_by_country,
    width = 12,
    height = 8
  ),
  list(
    file = "figures/03_pie_total_net_migration.png",
    plot = pie_total_net,
    width = 10,
    height = 8
  ),
  list(
    file = "figures/04_swiss_migration_world_map.png",
    plot = swiss_migration_world_map,
    width = 14,
    height = 10
  ),
  list(
    file = "figures/06_scatter_gdp_migration_loess.png",
    plot = scatter_gdp_migration,
    width = 10,
    height = 8
  ),
  list(
    file = "figures/07_histogram_net_migration.png",
    plot = hist_net_migration,
    width = 10,
    height = 6
  ),
  list(
    file = "figures/08_boxplot_net_migration_quartiles.png",
    plot = boxplot_net_migration_quartiles,
    width = 10,
    height = 6
  )
)

# Kantons-Map hinzufügen, falls vorhanden
if (exists("swiss_canton_immigration_map")) {
  plot_exports <- c(
    plot_exports,
    list(list(
      file = "figures/05_swiss_canton_immigration_map.png",
      plot = swiss_canton_immigration_map,
      width = 12,
      height = 10
    ))
  )
}

# Alle Plots in einer Schleife exportieren
invisible(lapply(plot_exports, function(item) {
  do.call(export_png, item)
}))

# R

# ============================================================
# KAPITEL 1 — PROJEKTSETUP
# ============================================================
# Dieses Kapitel bereitet Verzeichnisse, benötigte Pakete und
# Hilfsdateien für die Schweizer Migrationsanalyse vor.

# 'here' zuerst installieren und laden für relative Pfade
# Prüfe, ob 'here' installiert ist; falls nicht, installiere es.
if (!requireNamespace("here", quietly = TRUE)) {
  install.packages("here")
}
library(here)

# Definiert die Projekt-Root relativ zur Position dieser Script-Datei.
here::i_am("Scripts/Datenanalyse script.R")

# Liste der Verzeichnisse, die das Projekt benötigt.
paths <- c("data/raw", "data/processed", "plots", "tables")

# Benötigte Pakete (ohne 'here', da oben separat installiert/geladen).
required_packages <- c(
  "WDI",
  "dplyr",
  "tidyr",
  "ggplot2",
  "scales",
  "huxtable",
  "readr",
  "stringr",
  "tinytex",
  "broom"
)

# Installiere fehlende Pakete aus 'required_packages'.
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

# Lade alle benötigten Pakete (sichtbar im Namespace).
invisible(lapply(required_packages, library, character.only = TRUE))

# Lade ein lokales Theme oder Hilfsfunktionen (Theme.R muss vorhanden sein).
source("Themes/Theme.R")

# Erstelle die benötigten Verzeichnisse, falls sie noch nicht existieren.
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

# Schreibe die Indikatorliste als CSV in den tables-Ordner (für Dokumentation).
utils::write.csv(
  indicator_list_df,
  file = "tables/indicator_list.csv",
  row.names = FALSE
)

# Extrahiere die Indikatorcodes für WDI-Abfrage.
indicators <- indicator_list_df$Code

# ============================================================
# KAPITEL 2 — SCHWEIZER ZUWANDERUNGSDATEN
# ============================================================
# Lade die bereits verarbeiteten Schweizer Migrationsdaten ein und
# bereite Datentypen sowie Filter vor.

swiss_immigration_data <- readr::read_csv(
  "data/processed/swiss_immigration_countries_year.csv",
  show_col_types = FALSE
) |>
  # Stelle sicher, dass Jahr numerisch und Nettozuwanderung numerisch ist.
  dplyr::mutate(
    year = as.integer(year),
    net_migration = as.numeric(net_migration)
  ) |>
  # Filter: entferne aggregierte Regionen, veraltete Länderbezeichnungen
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
    # Entferne Metadaten/Zeilen mit Hinweisen aus der OFS-Tabelle
    !stringr::str_detect(origin_en, "Renseignements|Source|© OFS")
  ) |>
  # Retain only the columns we need and sort
  dplyr::select(origin_en, year, net_migration) |>
  dplyr::arrange(origin_en, year)

# Hilfsvariablen: Zeilenanzahl, Herkunftsländer, Jahresbereich etc.
swiss_row_count <- nrow(swiss_immigration_data)
swiss_origins <- sort(unique(swiss_immigration_data$origin_en))
swiss_year_range <- range(swiss_immigration_data$year, na.rm = TRUE)

# Prüfe auf fehlende Werte in den Spalten.
swiss_missingness <- swiss_immigration_data |>
  dplyr::summarize(dplyr::across(everything(), ~ sum(is.na(.x))))

# Zähle Einträge pro Herkunftsland (für Übersicht / QA).
swiss_counts_by_origin <- swiss_immigration_data |>
  dplyr::count(origin_en, sort = TRUE)

# Erstelle eine jährliche Serie der gesamten Nettozuwanderung (aggregiert über Länder).
swiss_immigration_yearly_data <- swiss_immigration_data |>
  dplyr::group_by(year) |>
  dplyr::summarize(
    net_migration = sum(net_migration, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::arrange(year)

# Weitere Hilfsgrössen für die Jahresdaten.
swiss_yearly_row_count <- nrow(swiss_immigration_yearly_data)
swiss_yearly_year_range <- range(
  swiss_immigration_yearly_data$year,
  na.rm = TRUE
)
swiss_yearly_missingness <- swiss_immigration_yearly_data |>
  dplyr::summarize(dplyr::across(everything(), ~ sum(is.na(.x))))

# ============================================================
# KAPITEL 3 — WORLD BANK DATEN FÜR DIE SCHWEIZ
# ============================================================
# Lade World Bank Indikatoren für die Schweiz (nur falls noch nicht gespeichert).

if (!file.exists("data/raw/world_bank_raw.rds")) {
  # Lade Indikatoren von WDI für den relevanten Jahresbereich
  world_bank_raw_data <- WDI::WDI(
    country = "CHE",
    indicator = indicators,
    start = min(swiss_immigration_yearly_data$year, na.rm = TRUE),
    end = max(swiss_immigration_yearly_data$year, na.rm = TRUE),
    extra = TRUE
  )
  saveRDS(world_bank_raw_data, "data/raw/world_bank_raw.rds")
} else {
  # Wenn Datei existiert, prüfe ob alle Indikatoren vorhanden sind,
  # ansonsten lade erneut (Sicherheit für inkrementelle Updates).
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

# Filtere die World-Bank-Daten auf die Schweiz und wähle relevante Spalten aus.
swiss_world_bank_data <- world_bank_raw_data |>
  dplyr::filter(iso3c == "CHE") |>
  dplyr::select(
    country,
    iso3c,
    year,
    dplyr::all_of(indicators)
  ) |>
  # Benenne die Indikatoren um für einfachere Verwendung im Code.
  dplyr::rename(
    gdp_growth = NY.GDP.MKTP.KD.ZG,
    employment_ratio = SL.EMP.TOTL.SP.ZS,
    unemployment_total = SL.UEM.TOTL.ZS
  ) |>
  dplyr::arrange(year)

# ============================================================
# KAPITEL 4 — ZUSAMMENGEFÜHRTE ANALYSEDATEN
# ============================================================
# Kombiniere die jährliche Zuwanderungsserie mit den World-Bank-Indikatoren.
swiss_analysis_data <- swiss_immigration_yearly_data |>
  dplyr::left_join(
    swiss_world_bank_data |>
      dplyr::select(year, gdp_growth, employment_ratio, unemployment_total),
    by = "year"
  ) |>
  dplyr::arrange(year)

# Hilfsgrössen für die Analyse-Tabelle.
swiss_analysis_row_count <- nrow(swiss_analysis_data)
swiss_analysis_year_range <- range(swiss_analysis_data$year, na.rm = TRUE)
swiss_analysis_missingness <- swiss_analysis_data |>
  dplyr::summarize(dplyr::across(everything(), ~ sum(is.na(.x))))

# ============================================================
# KAPITEL 6 — REGRESSIONSMODELLE
# ============================================================
# Schätze einfache lineare Modelle: Nettozuwanderung ~ BIP-Wachstum
# und zusätzlich mit Arbeitslosigkeit als Kontrollvariable.

# Filtere Beobachtungen ohne fehlende Werte in benötigten Variablen.
swiss_regression_data <- swiss_analysis_data |>
  dplyr::filter(
    !is.na(net_migration),
    !is.na(gdp_growth),
    !is.na(unemployment_total)
  )

# Einfaches Modell nur mit BIP-Wachstum.
swiss_migration_model <- stats::lm(
  net_migration ~ gdp_growth,
  data = swiss_regression_data
)

# Modell mit zusätzlicher Kontrollvariablen Arbeitslosigkeit.
swiss_migration_model_controlled <- stats::lm(
  net_migration ~ gdp_growth + unemployment_total,
  data = swiss_regression_data
)

# ============================================================
# KAPITEL 7 — TABELLEN-EXPORT (HTML) + VIEWER-TABELLEN
# ============================================================
# Erzeuge eine veröffentlichungsfähige Regressionstabelle aus Modellobjekten.

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

# Erzeuge die Tabelle und speichere als HTML-Datei im tables-Ordner.
swiss_regression_table <- make_regression_table(
  models = list(
    swiss_migration_model,
    swiss_migration_model_controlled
  ),
  model_names = c("Nur BIP-Wachstum", "BIP-Wachstum + Arbeitslosigkeit")
)

swiss_regression_table

html_file <- normalizePath(
  "tables/swiss_regression_results.html",
  winslash = "/",
  mustWork = FALSE
)

invisible(
  huxtable::quick_html(
    swiss_regression_table,
    file = html_file,
    open = FALSE
  )
)

# ============================================================
# KAPITEL 8 — VISUALISIERUNG 
# ============================================================
# Erstelle Visualisierungen: Liniendiagramm, Balkendiagramm und Kreisdiagramm.

# Liniendiagramm: Nettozuwanderung der Schweiz über die Jahre
swiss_migration_over_time <- ggplot(
  swiss_immigration_yearly_data,
  aes(x = year, y = net_migration)
) +
  # Linie zeigt den Trend; Punkte markieren die Jahreswerte.
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
  # Deutsche Zahlenformatierung mit Punkt als Tausendertrennzeichen
  scale_y_continuous(labels = scales::comma_format(big.mark = ".", decimal.mark = ",")) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(size = 11),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank()
  )

# Anzeige des Liniendiagramms (in der interaktiven Sitzung).
swiss_migration_over_time

# Balkendiagramm: Nettozuwanderung nach Herkunftsland im festen Jahr 2024
# (falls Sie lieber das jeweils letzte Jahr möchten: latest_year <- max(...))
latest_year <- 2024

swiss_migration_by_country <- swiss_immigration_data |>
  # Filter auf das gewünschte Jahr
  dplyr::filter(year == latest_year) |>
  # Sortiere absteigend nach Nettozuwanderung für ansprechende Beschriftung
  dplyr::arrange(desc(net_migration)) |>
  ggplot(aes(x = reorder(origin_en, net_migration), y = net_migration)) +
  # Balken zeichnen
  geom_col(fill = "#2ca02c", alpha = 0.9) +
  # Zahlen am rechten Ende der Balken hinzufügen (deutsches Format)
  geom_text(
    aes(label = scales::comma(net_migration, big.mark = ".", decimal.mark = ",")),
    hjust = -0.1,
    size = 3.2
  ) +
  # Drehung, damit Länder auf der y-Achse stehen
  coord_flip() +
  # Sorge dafür, dass rechts genügend Platz für die Beschriftungen ist
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

# Anzeige des Balkendiagramms
swiss_migration_by_country

# =# ============================================================
# KREISDIAGRAMM — ANTEIL DER GESAMTEN NETTOZUWANDERUNG
# ============================================================
# Summiere die Nettozuwanderung über den gesamten Beobachtungszeitraum
# pro Herkunftsland und zeichne nur die Länder (Farben/Legende), ohne Werte im Diagramm.

totals_by_country <- swiss_immigration_data |>
  dplyr::filter(!is.na(origin_en)) |>
  dplyr::group_by(origin_en) |>
  dplyr::summarize(total_net = sum(net_migration, na.rm = TRUE), .groups = "drop") |>
  dplyr::arrange(desc(total_net)) |>
  dplyr::filter(total_net > 0)

# Bestimme den betrachteten Zeitraum für das Subtitle-Label
years_range <- range(swiss_immigration_data$year, na.rm = TRUE)

# Erstelle das Kreisdiagramm (ggplot + coord_polar) — keine Beschriftungen in den Segmenten,
# nur Legende mit Ländern und Farben.
pie_total_net <- ggplot(totals_by_country, aes(x = "", y = total_net, fill = reorder(origin_en, total_net))) +
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

# Anzeige des Kreisdiagramms
pie_total_net
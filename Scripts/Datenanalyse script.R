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
# KAPITEL 3 — BFS-DATEN
# ============================================================

# BFS-Datensatz zur Einwanderungsstatistik der Schweiz.
# Dieser Datensatz enthält sowohl nationale als auch kantonale Informationen.
bfs_dataset_id <- "px-x-0103020200_102"

# Lokale Speicherpfade (Caching, damit API nicht jedes Mal aufgerufen wird)
bfs_pop_rds <- "data/bfs_population_immigration.rds"
bfs_canton_rds <- "data/bfs_canton_immigration.rds"


# ------------------------------------------------------------
# 2) NATIONALE BFS-DATEN LADEN
# ------------------------------------------------------------

# Ziel: Einwanderung nach Staatsangehörigkeit und Jahr

# Prüfen ob Daten bereits lokal gespeichert sind
# → wenn ja: direkt laden (spart API Zeit und verhindert Fehler)
if (file.exists(bfs_pop_rds)) {
  bfs_raw <- readRDS(bfs_pop_rds)
} else {
  # Falls keine lokale Version existiert:
  # → Daten direkt von BFS API abrufen
  bfs_raw <- BFS::bfs_get_data(
    number_bfs = bfs_dataset_id,
    language = "en",
    clean_names = TRUE
  )

  # Danach speichern wir die Rohdaten lokal
  saveRDS(bfs_raw, bfs_pop_rds)
}

# Struktur prüfen, um zu verstehen welche Variablen vorhanden sind
str(bfs_raw)


# ------------------------------------------------------------
# 3) NATIONALE DATEN BEREINIGEN
# ------------------------------------------------------------

# Ziel:
# - nur echte Länder (keine Gesamtzeilen)
# - nur Jahr + Herkunft + Einwanderung
# - Daten auf ein einheitliches Format bringen

bfs_clean <- bfs_raw |>

  # Entferne aggregierte Gesamtzeilen
  # (diese enthalten keine einzelnen Nationalitäten)
  filter(
    !is.na(citizenship),
    citizenship != "Citizenship - total"
  ) |>

  # Gruppierung notwendig, da BFS Daten teilweise mehrfach vorkommen
  group_by(citizenship, year) |>

  # Falls mehrere Teilwerte existieren → zusammenführen
  summarise(
    immigration_from_abroad = sum(
      as.numeric(
        immigration_of_the_permanent_resident_population
      ),
      na.rm = TRUE
    ),
    .groups = "drop"
  ) |>

  # Vereinheitlichung der Struktur für spätere Analysen
  transmute(
    nationality = stringr::str_trim(citizenship),
    year = as.integer(year),
    immigration_from_abroad
  )


# ------------------------------------------------------------
# 4) KANTONALE BFS-DATEN LADEN
# ------------------------------------------------------------

# Ziel: Einwanderung nach Kanton (für spätere Karten)

# Gleiche Logik wie bei nationalen Daten:
# zuerst prüfen ob Cache existiert
if (file.exists(bfs_canton_rds)) {
  bfs_canton_raw <- readRDS(bfs_canton_rds)
} else {
  # API call für kantonale Daten
  bfs_canton_raw <- BFS::bfs_get_data(
    number_bfs = bfs_dataset_id,
    language = "de",
    clean_names = TRUE
  )

  # lokal speichern für zukünftige Nutzung
  saveRDS(bfs_canton_raw, bfs_canton_rds)
}


# ------------------------------------------------------------
# 5) KANTONALE DATEN BEREINIGEN
# ------------------------------------------------------------

# Ziel:
# Jede Zeile = Kanton × Nationalität × Jahr

bfs_canton_clean <- bfs_canton_raw |>

  # Auswahl und Standardisierung der Variablen
  transmute(
    canton = kanton,
    nationality = staatsangehoerigkeit,
    year = as.integer(jahr),

    # Hauptvariable: Einwanderung aus dem Ausland
    immigration_from_abroad = as.numeric(
      einwanderung_der_standigen_wohnbevolkerung
    )
  ) |>

  # Entferne ungültige Werte und aggregierte Kategorien
  filter(
    !is.na(canton),
    canton != "Schweiz",
    !is.na(nationality),
    nationality != "Schweiz"
  )


# ------------------------------------------------------------
# 6) DATEN FÜR KANTONSKARTE AUFBEREITEN
# ------------------------------------------------------------

# Für Visualisierung wird nur das aktuellste Jahr verwendet,
# damit die Karte nicht mehrere Jahre gleichzeitig zeigt.

latest_year_canton <- max(
  bfs_canton_clean$year,
  na.rm = TRUE
)

# Aggregation:
# alle Nationalitäten werden pro Kanton zusammengezählt
# → Ergebnis: Gesamt-Einwanderung pro Kanton
bfs_canton_map_data <- bfs_canton_clean |>

  filter(year == latest_year_canton) |>

  group_by(canton) |>

  summarise(
    immigration_from_abroad = sum(
      immigration_from_abroad,
      na.rm = TRUE
    ),
    .groups = "drop"
  )


# ------------------------------------------------------------
# 7) KANTONSKARTE ERSTELLEN
# ------------------------------------------------------------

# Hier werden die geografischen Kantonsgrenzen geladen
# und mit den BFS Daten verbunden

swiss_canton_map_data <- BFS::bfs_get_base_maps(
  geom = "kant",
  return_sf = TRUE
) |>

  # Verknüpfung der Geometrie mit den Einwanderungsdaten
  left_join(
    bfs_canton_map_data,
    by = c("name" = "canton")
  )

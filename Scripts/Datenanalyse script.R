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
  "tibble",
  "rnaturalearthdata",
  "sf"
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
# KAPITEL 3 — WELTKARTE DER NETTOZUWANDERUNG
# ============================================================

# Dieses Kapitel visualisiert die Nettozuwanderung in die Schweiz
# nach Herkunftsland auf einer geografischen Weltkarte.
# Fokus: ausschließlich die 15 im Datensatz enthaltenen Herkunftsländer.

#Zuerst müssen wir die Weltkarte laden, um die geometrischen Daten für die Länder zu erhalten.
# Wir verwenden Natural Earth Daten als geometrische Basis für die Karte.
world <- rnaturalearth::ne_countries(
  scale = "medium",
  returnclass = "sf"
)

#Die Karte wird nur die 15 Herkunftsländer zeigen, die in unserem Datensatz enthalten sind.
# Die Nettozuwanderung wird über den gesamten Zeitraum (1991–2024) pro Herkunftsland aufsummiert.
map_data <- RAW_swiss_immigration |>
  group_by(origin_en) |>
  summarise(
    net_migration = sum(net_migration, na.rm = TRUE),
    .groups = "drop"
  ) |>

  # Umwandlung in ISO3-Code für sauberes Matching mit der Weltkarte.
  mutate(
    iso_a3 = countrycode(
      origin_en,
      origin = "country.name",
      destination = "iso3c"
    )
  ) |>

  # Sicherheitsfilter:
  # Nur Länder behalten, die erfolgreich in ISO3 umgewandelt wurden
  filter(!is.na(iso_a3))


# ------------------------------------------------------------
# 3) Weltkarte mit Migrationsdaten verbinden
# ------------------------------------------------------------
# Hier werden Geodaten (world) mit den Migrationsdaten verknüpft.
# Nur Länder mit Matching ISO-Code erhalten Werte.
# Wir verwenden iso_a3_eh statt iso_a3, da einige Länder (z.B. Frankreich) in iso_a3 den Wert "-99" haben.
world_map <- world |>
  dplyr::select(-iso_a3) |>
  dplyr::rename(iso_a3 = iso_a3_eh) |>
  left_join(
    map_data,
    by = "iso_a3"
  )

#Jetzt können wir die Karte visualisieren.
# Zeichnet die Ländergrenzen und füllt jedes Land mit der entsprechenden Farbe basierend auf net_migration
world_map_plot <- ggplot(
  world_map |>
    dplyr::filter(continent == "Europe", iso_a3 != "RUS")
) +
  geom_sf(
    aes(fill = net_migration),
    color = "grey80", # Farbe der Ländergrenzen
    linewidth = 0.2 # Dicke der Ländergrenzen
  ) +
  # Definiert die Farbskala: hellblau für niedrige Werte, dunkelblau für hohe Werte
  # Länder ohne Daten werden grau dargestellt
  # scales::comma_format verhindert wissenschaftliche Notation in der Legende
  scale_fill_gradient(
    low = "#deebf7",
    high = "#08519c",
    na.value = "grey90",
    name = "Nettozuwanderung",
    labels = scales::comma_format(big.mark = ".", decimal.mark = ",")
  ) +
  theme_minimal() +
  # Titel und Untertitel der Karte
  labs(
    title = "Nettozuwanderung in die Schweiz nach Herkunftsland",
    subtitle = "Die 15 Hauptherkunftsländer im Datensatz"
  ) +
  # Entfernt Achsenbeschriftungen da sie bei Karten nicht sinnvoll sind
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()
  )

world_map_plot

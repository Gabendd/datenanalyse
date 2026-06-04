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
  "rnaturalearth",
  "countrycode",
  "huxtable",
  "scales",
  "data.table",
  "sf",
  "tibble",
  "broom",
  "stargazer"
)

options(scipen = 999)

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

# Deutsche und lowercase Aliases fuer konsistenteren Stil in neuen Code-Abschnitten.
raw_swiss_immigration <- RAW_swiss_immigration
jahresdaten_schweizer_einwanderung <- YEARLY_swiss_immigration
analysedaten_schweiz <- ANALYSIS_swiss
yearly_swiss_immigration <- jahresdaten_schweizer_einwanderung
analysis_swiss <- analysedaten_schweiz

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
  dplyr::group_by(origin_en) |>
  dplyr::summarise(
    net_migration = sum(net_migration, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    iso_a3 = countrycode::countrycode(
      origin_en,
      origin = "country.name",
      destination = "iso3c"
    )
  ) |>
  dplyr::filter(!is.na(iso_a3))

# ------------------------------------------------------------
# 3) Weltkarte mit Migrationsdaten verbinden
# ------------------------------------------------------------
# Hier werden Geodaten (world) mit den Migrationsdaten verknüpft.
# Nur Länder mit Matching ISO-Code erhalten Werte.
# Wir verwenden iso_a3_eh statt iso_a3, da einige Länder (z.B. Frankreich) in iso_a3 den Wert "-99" haben.
world_map <- world |>
  dplyr::mutate(
    iso_a3 = dplyr::coalesce(iso_a3_eh, iso_a3)
  ) |>
  dplyr::filter(
    continent == "Europe",
    !name_long %in% c("Russian Federation"),
    !is.na(iso_a3),
    iso_a3 != "-99"
  ) |>
  dplyr::left_join(map_data, by = "iso_a3")

#Jetzt können wir die Karte visualisieren.
swiss_migration_world_map <- ggplot2::ggplot(world_map) +
  ggplot2::geom_sf(
    ggplot2::aes(fill = net_migration),
    color = "white",
    linewidth = 0.2
  ) +
  ggplot2::scale_fill_gradient(
    low = "#deebf7",
    high = "#08519c",
    na.value = "grey90"
  ) +
  ggplot2::labs(
    title = "Nettozuwanderung in die Schweiz nach Herkunftsland (Europa)",
    fill = ""
  ) +
  ggplot2::theme_void() +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold", size = 14),
    legend.title = ggplot2::element_blank()
  )

swiss_migration_world_map


# ============================================================
# KAPITEL 4 — ZUSAETZLICHE VISUALISIERUNGEN
# ============================================================

# Dieses Kapitel erweitert die Analyse um weitere Darstellungen.
# Zuerst zeigen wir die Entwicklung der Nettozuwanderung über die Zeit.

# Kurvendiagramm: gesamte Nettozuwanderung pro Jahr
curve_plot <- ggplot(
  YEARLY_swiss_immigration,
  aes(x = year, y = net_migration)
) +
  geom_line(color = "#08519c", linewidth = 1) +
  geom_point(color = "#08519c", size = 2) +
  labs(
    x = "Jahr",
    y = "Nettozuwanderung",
    title = "Nettozuwanderung in die Schweiz nach Jahr",
    subtitle = "Gesamtwert fuer alle Herkunftslaender kombiniert"
  ) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(1990, 2025, by = 5)) +
  scale_y_continuous(
    labels = scales::comma_format(big.mark = ".", decimal.mark = ",")
  )

curve_plot

# Das Kurvendiagramm zeigt die Entwicklung der Nettozuwanderung von 1991 bis 2024.
# Es ist kein Kartenplot, sondern ein Zeitverlauf mit Jahreswerten.

# ============================================================
# KAPITEL 4B — BOXPLOT: NETTOZUWANDERUNG NACH VORZEICHEN
# ============================================================

#Wir nehmen wieder die Jahresdaten der Nettozuwanderung.
yearly_swiss_immigration_with_sign <- yearly_swiss_immigration |>
  dplyr::mutate(
    migration_sign = ifelse(
      net_migration > 0,
      "Einwanderung (positiv)",
      "Auswanderung (negativ)"
    )
  )

count(YEARLY_swiss_immigration_with_sign, migration_sign)

#Die Farben der Boxen definieren.

boxplot_sign <- ggplot(
  YEARLY_swiss_immigration_with_sign,
  aes(x = migration_sign, y = net_migration, fill = migration_sign)
) +
  geom_boxplot() +
  scale_fill_manual(
    values = c(
      "Einwanderung (positiv)" = "#2ca25f",
      "Auswanderung (negativ)" = "#de2d26"
    ),
    name = "Migrationsrichtung"
  ) +
  labs(
    x = "",
    y = "Nettozuwanderung",
    title = "Nettozuwanderung nach Vorzeichen",
    subtitle = "Vergleich zwischen Jahren mit Einwanderung und Auswanderung"
  ) +
  theme_minimal() +
  scale_y_continuous(
    labels = scales::comma_format(big.mark = ".", decimal.mark = ",")
  )

boxplot_sign

# Die Streuung der Nettozuwanderung ist in den positiven Jahren deutlich grösser.

# ============================================================
# KAPITEL 4C — HORIZONTALES BALKENDIAGRAMM: NETTOZUWANDERUNG PRO HERKUNFTSLAND
# ============================================================

# Hier summieren wir die Nettozuwanderung pro Herkunftsland ueber den gesamten Zeitraum.
# Danach erstellen wir ein horizontales Balkendiagramm zum Vergleich der Laender.

#Pro Land summieren.
country_totals <- raw_swiss_immigration |>
  dplyr::group_by(origin_en) |>
  dplyr::summarise(
    total_net_migration = sum(net_migration, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::arrange(desc(total_net_migration))

country_totals

#Hier sehen wir die Gesamt Nettozuwanderung pro Herkunftsland über den gesamten Zeitraum.
#Bei Spanien ist die Nettozuwanderung sogar negativ, was bedeutet, dass mehr Menschen aus
# Spanien in die Schweiz ausgewandert sind als umgekehrt.
# Eine möglich Erklärung könnten sein, dass Spanier in der Schweiz arbeiten, aber dann wieder zurück in die Heimat ziehen,
# wenn sie in Rente gehen.
horizontal_bar_plot <- ggplot(
  country_totals,
  aes(x = total_net_migration, y = reorder(origin_en, total_net_migration))
) +
  geom_col(fill = "#08519c", color = "white", linewidth = 0.2) +
  labs(
    x = "Gesamt Nettozuwanderung",
    y = "Herkunftsland",
    title = "Gesamt Nettozuwanderung in die Schweiz nach Herkunftsland",
    subtitle = "Summe ueber den gesamten Zeitraum (1991-2024)"
  ) +
  theme_minimal() +
  scale_x_continuous(
    labels = scales::comma_format(big.mark = ".", decimal.mark = ",")
  )

#Plot zeigen
horizontal_bar_plot


# ============================================================
# KAPITEL 4D — All Plots speichern
# ============================================================

#Jetzt können wir noch alle erstellten Plots als PNG Dateien speichern.

ggplot2::ggsave(
  filename = file.path("figures", "swiss_migration_world_map.png"),
  plot = swiss_migration_world_map,
  width = 10,
  height = 6,
  dpi = 300
)

ggplot2::ggsave(
  filename = file.path("figures", "curve_plot.png"),
  plot = curve_plot,
  width = 10,
  height = 6,
  dpi = 300
)

ggplot2::ggsave(
  filename = file.path("figures", "boxplot_sign.png"),
  plot = boxplot_sign,
  width = 10,
  height = 6,
  dpi = 300
)

ggplot2::ggsave(
  filename = file.path("figures", "horizontal_bar_plot.png"),
  plot = horizontal_bar_plot,
  width = 10,
  height = 6,
  dpi = 300
)


# ============================================================
# KAPITEL 5 — REGRESSIONSMODELLE (JAEHRLICHE DATEN)
# ============================================================

#Jetzt können wir eine Regressionsanalyse durchführen, um den Zusammenhang zwischen BIP-Wachstum, Arbeitslosenquote und Nettozuwanderung zu untersuchen.
#Modell 1 hat nur das BIP-Wachstum als unabhängige Variable, während Modell 2 zusätzlich die Arbeitslosenquote als Kontrollvariable enthält.
#Die Abhänige Variable in beiden Modellen ist die Nettozuwanderung, die wir bereits auf Jahresbasis aggregiert haben.

# Schritt 1: Nur die relevanten Variablen fuer die Regression auswaehlen.
regressionsdaten <- analysedaten_schweiz |>
  dplyr::select(net_migration, bip_wachstum, arbeitslosenquote)

regression_data <- regressionsdaten

#Jetzt Modell 1 schaetzen.
model_1_bip <- lm(net_migration ~ bip_wachstum, data = regression_data)

#Und jetzt Modell 2 mit der Kontrollvariable Arbeitslosenquote.
model_2_bip_arbeitslos <- lm(
  net_migration ~ bip_wachstum + arbeitslosenquote,
  data = regression_data
)

# Schritt 5: Summaries direkt in der Konsole anzeigen.
summary(model_1_bip)
summary(model_2_bip_arbeitslos)


#Hier sieht es besser aus, wenn wir es als Tabelle darstellen, damit wir die Ergebnisse besser vergleichen können.
#Wir benutzen dafür Stargazer.

stargazer::stargazer(
  model_1_bip,
  model_2_bip_arbeitslos,
  type = "text",
  title = "Regressionsergebnisse: Nettozuwanderung",
  dep.var.labels = "Nettozuwanderung",
  covariate.labels = c("BIP-Wachstum", "Arbeitslosenquote"),
  omit.stat = c("ser", "bic"),
  digits = 3,
  no.space = TRUE
)

# Export als HTML-Datei.
stargazer::stargazer(
  model_1_bip,
  model_2_bip_arbeitslos,
  type = "html",
  title = "Regressionsergebnisse: Nettozuwanderung",
  dep.var.labels = "Nettozuwanderung",
  covariate.labels = c("BIP-Wachstum", "Arbeitslosenquote"),
  omit.stat = c("ser", "bic"),
  digits = 3,
  no.space = TRUE,
  out = file.path("tables", "regression_stargazer.html")
)


#Wir sehen bei N=34, dass die Anzahl der Beobachtungen relativ klein ist,
# was die statistische Kraft der Analyse einschränken könnte.

# Modell 1:
# Ein Anstieg des BIP-Wachstums um 1 Prozentpunkt ist mit einer um
# durchschnittlich 1'163 Personen höheren Nettozuwanderung verbunden.
# Dieser Effekt ist statistisch nicht signifikant (p > 0.1).
# Das Modell erklärt nur 0.9 % der Variation der Nettozuwanderung (R² = 0.009).
# Konstante: Bei einem BIP-Wachstum von 0 % beträgt die geschätzte
# Nettozuwanderung 25'865 Personen. Der Wert ist statistisch signifikant (p < 0.05).

# Modell 2:
# Unter Kontrolle der Arbeitslosenquote ist ein zusätzlicher Prozentpunkt
# BIP-Wachstum mit 187 Personen weniger Nettozuwanderung verbunden.
# Dieser Effekt ist nicht statistisch signifikant (p > 0.1).

# Ein Anstieg der Arbeitslosenquote um 1 Prozentpunkt ist mit einer um
# durchschnittlich 11'709 Personen höheren Nettozuwanderung verbunden.
# Dieser Effekt ist statistisch hoch signifikant (p < 0.01).
# Konstante: Bei 0 % BIP-Wachstum und 0 % Arbeitslosenquote beträgt die
# geschätzte Nettozuwanderung -18'119 Personen. Der Wert ist nicht signifikant.
# Das Modell erklärt 21.4 % der Variation der Nettozuwanderung (R² = 0.214).

#Den positiven Effekt der Arbeitslosigkeit auf die Nettozuwanderung deutet dazu, dass der Modelle
#nicht alle relevanten Faktoren erfasst, die die Nettozuwanderung beeinflussen. Die Nettozuandwerung ist
#auch stark von anderen Faktoten, wie zB die Freizugkeitens Abkommen, die EU Mitgliedschaft, die Arbeitsmarktsituation
# in den Herkunftsländern etc beeinflusst.

#Können

# ============================================================
# KAPITEL 6 — DIAGNOSTISCHE TESTS UND RESIDUALANALYSE
# ============================================================

# Um die Modelle genauer zu analysieren, ist es sinnvoll, die Korrelationen zwischen den Prädiktoren zu prüfen,
# um mögliche Multikollinearität zu identifizieren. Zusätzlich prüfen wir die Autokorrelation der Residuen,
# da sich Nettozuwanderung in Zeitreihen über die Zeit selbst beeinflussen kann.
# Das könnte Standardfehler verzerren und Signifikanztests beeinflussen.

# Schritt 1: Korrelationsmatrix zwischen Nettozuwanderung, BIP-Wachstum und Arbeitslosenquote.
# Das zeigt, ob die Prädiktoren stark miteinander korrelieren.
correlation_matrix <- regression_data |>
  cor()

# Korrelationsmatrix anzeigen.
correlation_matrix

# Ein Korrelationswert von r = 0.238 zwischen den Prädiktoren spricht für eine schwache Korrelation.
# Das deutet auf kein starkes Multikollinearitätsproblem hin.

# Jetzt messen wir auch die Autokorrelation. Die Autokorrelation der Residuen könnte darauf hinweisen,
# dass die Nettozuwanderung in einem Jahr von der Nettozuwanderung im Vorjahr beeinflusst wird,
# oder anders gesagt, dass die Nettozuwanderung eine Eigendynamik aufweist und auch ohne
# weitere Einflussfaktoren von selbst wachsen würde.

# ACF der Residuen berechnen
acf_obj <- stats::acf(
  residuals(model_2_bip_arbeitslos), # Residuen des Modells
  plot = FALSE, # kein Plot erstellen
  na.action = na.pass # NA-Werte nicht entfernen
)

# Ergebnisse in eine übersichtliche Tabelle umwandeln
autocorrelation_df <- tibble::tibble(
  lag = as.numeric(acf_obj$lag), # Zeitverzögerung (Lag)
  acf = as.numeric(acf_obj$acf) # Autokorrelationswert
) |>
  dplyr::filter(lag > 0) # Lag 0 entfernen (immer 1, nicht informativ)

# Anzahl der Beobachtungen berechnen
n_obs <- length(stats::na.omit(residuals(model_2_bip_arbeitslos)))

# 95%-Konfidenzgrenze
conf_limit <- 1.96 / sqrt(n_obs)

# Tabelle ausgeben
autocorrelation_df

# Konfidenzgrenzen separat anzeigen
conf_limit

# Interpretation der ACF-Ergebnisse:
# Ein "Lag" beschreibt die zeitliche Verschiebung zwischen Beobachtungen.
# Lag 1 bedeutet z.B. den Vergleich zwischen einem Wert und dem Wert des Vorjahres,
# Lag 2 den Vergleich mit dem Wert von vor zwei Jahren usw.
#
# In den Ergebnissen zeigt sich starke positive Autokorrelation bei kleinen Lags,
# insbesondere bei Lag 1 (0.74) und Lag 2 (0.38), beide über der 95%-Grenze (±0.336).
# Das bedeutet, dass die Residuen zeitlich abhängig sind und nicht zufällig schwanken.
#
# Konsequenz:
# Das Modell wird auch von einer zeiutliche Dynmaik beiinflusst, die nicht durch die Prädiktoren erfasst wird.
# as könnte die Schätzung der Effekte verzerren.

# ============================================================
# KAPITEL 7 — OBJEKTE IN LISTEN ZUSAMMENFASSEN
# ============================================================

# Wir fassen die wichtigsten Objekte in einfachen Listen zusammen.
# Das macht das Projekt leichter zu ueberblicken und spaeter leichter exportierbar.

zeitreihe_schweizer_einwanderung <- yearly_swiss_immigration
zeitreihe_schweizer_einwanderung_mit_vorzeichen <- yearly_swiss_immigration_with_sign
analyse_daten_schweiz <- analysedaten_schweiz
regressions_daten <- regressionsdaten
indikatoren_tabelle <- indicator_list_df
schweizer_worldbank_daten <- swiss_world_bank_data
laender_summen <- country_totals
autokorrelations_tabelle <- autocorrelation_df

# Deutsche Listen-Namen.
daten_objekte <- list(
  rohe_schweizer_einwanderung = raw_swiss_immigration,
  zeitreihe_schweizer_einwanderung = zeitreihe_schweizer_einwanderung,
  zeitreihe_schweizer_einwanderung_mit_vorzeichen = zeitreihe_schweizer_einwanderung_mit_vorzeichen,
  analyse_daten_schweiz = analyse_daten_schweiz,
  regressions_daten = regressions_daten,
  indikatoren_tabelle = indikatoren_tabelle,
  schweizer_worldbank_daten = schweizer_worldbank_daten,
  weltkarten_daten = map_data,
  laender_summen = laender_summen,
  autokorrelations_tabelle = autokorrelations_tabelle
)

matrix_objekte <- list(
  korrelationsmatrix = correlation_matrix,
  acf_objekt = acf_obj,
  konfidenzgrenze = conf_limit,
  beobachtungen = n_obs
)

modell_objekte <- list(
  modell_1_bip = model_1_bip,
  modell_2_bip_arbeitslos = model_2_bip_arbeitslos
)

plot_objekte <- list(
  weltkarte_migration = swiss_migration_world_map,
  verlaufslinie = curve_plot,
  boxplot_vorzeichen = boxplot_sign,
  balkendiagramm_laender = horizontal_bar_plot
)

# Kompatibilitaets-Aliase fuer den bisherigen englischen Stil.
data_objects <- daten_objekte
matrix_objects <- matrix_objekte
model_objects <- modell_objekte
plot_objects <- plot_objekte


# Die Listen kurz in der Konsole anzeigen.
daten_objekte
matrix_objekte
modell_objekte
plot_objekte

# Aufraeum: Losche alle einzelnen Objekte, die jetzt in den Listen organisiert sind.
#
rm(
  raw_swiss_immigration,
  jahresdaten_schweizer_einwanderung,
  analysedaten_schweiz,
  yearly_swiss_immigration,
  analysis_swiss,
  yearly_swiss_immigration_with_sign,
  regression_data,
  regressionsdaten,
  indicator_list_df,
  indikatoren_tabelle,
  swiss_world_bank_data,
  schweizer_worldbank_daten,
  map_data,
  country_totals,
  laender_summen,
  world,
  world_map,
  immigration_context_reasons,
  immigration_context_table,
  kontext_daten_einwanderung,
  kontext_gruende_einwanderung,
  kontext_tabelle_einwanderung,
  autocorrelation_df,
  autokorrelations_tabelle,
  correlation_matrix,
  acf_obj,
  conf_limit,
  n_obs,
  model_1_bip,
  model_2_bip_arbeitslos,
  swiss_migration_world_map,
  curve_plot,
  boxplot_sign,
  horizontal_bar_plot,
  zeitreihe_schweizer_einwanderung,
  zeitreihe_schweizer_einwanderung_mit_vorzeichen,
  analyse_daten_schweiz,
  regressions_daten,
  world_bank_file,
  fetch_world_bank_data,
  start_year,
  end_year,
  indicators
)

message(
  "Workspace aufgeraeumt. Verbleibende Objekte: 5 deutschsprachige Listen und ihre englischen Aliase."
)

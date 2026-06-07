# ============================================================
# KAPITEL 1 — SETUP UND PAKETE
# ============================================================

# Benötigte Pakete laden
erforderliche_pakete <- c(
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
  "stargazer",
  "texreg",
  "tidyr",
  "codetools"
)

options(scipen = 999)
options(repos = c(CRAN = "https://cloud.r-project.org"))

fehlende_pakete <- setdiff(erforderliche_pakete, rownames(installed.packages()))
if (length(fehlende_pakete) > 0) {
  install.packages(fehlende_pakete)
}

invisible(lapply(erforderliche_pakete, library, character.only = TRUE))

# Arbeitsverzeichnisse definieren
pfade <- c("tables", "figures")
invisible(lapply(pfade, function(pfad) {
  if (!dir.exists(pfad)) {
    dir.create(pfad, recursive = TRUE)
  }
}))

# ============================================================
# KAPITEL 2 — DATEN IMPORTIEREN UND AUFBEREITEN
# ============================================================

# ============================================================
# KAPITEL 2A — SCHWEIZERISCHE EINWANDERUNGSDATEN IMPORTIEREN UND AUFBEREITEN
# ============================================================
# Bei den Schweizerischen Einwanderungsdaten handelt es sich um von der BFS-Webseite manuell heruntergeladene CSV-Dateien.

# Hier werden die Einwanderungsdaten importiert, die Spalten in die richtigen Formate umgewandelt und nur die relevanten
# Herkunftsländer und Zeilen behalten. Danach werden die Daten nach Herkunftsland und Jahr sortiert. Nur die relevanten
# Variablen (herkunft, jahr, netto_migration) werden behalten. Zudem werden die Zeilen mit aggregierten Regionen (z. B. "Afrique",
# "Amérique", "Asie", "Océanie") sowie die Zeilen mit nicht-informativem Text (z. B. "Renseignements|Source|© OFS") herausgefiltert.

# Immigrationsdaten einlesen, bereinigen und filtern
rohe_schweizer_einwanderung <- readr::read_csv(
  "data/swiss_immigration_countries_year.csv",
  show_col_types = FALSE
) |>
  dplyr::mutate(
    jahr = as.integer(year),
    netto_migration = as.numeric(net_migration)
  ) |>
  dplyr::rename(herkunft = origin_en) |>
  dplyr::filter(
    !herkunft %in%
      c(
        "Afrique",
        "Amérique",
        "Asie",
        "Océanie",
        "EFTA countries",
        "Yugoslavia",
        "Serbia and Montenegro"
      ),
    !stringr::str_detect(herkunft, "Renseignements|Source|© OFS")
  ) |>
  dplyr::select(herkunft, jahr, netto_migration) |>
  dplyr::arrange(herkunft, jahr)

# Jetzt werden die Daten auf Jahresbasis aggregiert, um die Nettozuwanderung pro Jahr zu erhalten.
jahresdaten_schweizer_einwanderung <- rohe_schweizer_einwanderung |>
  dplyr::group_by(jahr) |>
  dplyr::summarize(
    netto_migration = sum(netto_migration, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::arrange(jahr)

summary(jahresdaten_schweizer_einwanderung$netto_migration)
#Summary zeigt uns, dass wir teilweise Auswanderungen haben. Das Minimum von -10589
# zeigt, dass es Jahre gab, in denen die Schweiz insgesamt
# mehr Auswanderungen als Einwanderungen verzeichnete (negative Nettozuwanderung).
# Der Median (26290) liegt  unter dem Mittelwert (27759), was auf
# einzelne Jahre mit hoher Zuwanderung hindeutet.

# ============================================================
# KAPITEL 2B — WORLD BANK INDIKATOREN IMPORTIEREN UND AUFBEREITEN
# ============================================================

# Hier werden die Indikatoren der World Bank importiert, die für die Analyse benötigt werden. Ich gehe dabei so vor,
# dass ich zuerst die benötigten Indikatoren definiere und danach den API-Aufruf durchführe.
# Bei den World Bank Indikatoren handelt es sich um unsere unabhängige Variable (BIP-Wachstum) und eine Kontrollvariable
# (Arbeitslosenquote). Die abhängige Variable ist die Nettozuwanderung, die wir bereits vorher importiert und aufbereitet haben.
indikatoren_liste <- tibble::tibble(
  Code = c(
    "NY.GDP.MKTP.KD.ZG",
    "SL.UEM.TOTL.ZS"
  ),

  # Und hier die konkreten Indikatoren
  Description = c(
    "BIP-Wachstum (jährlich in %)",
    "Arbeitslosigkeit, insgesamt (% der Erwerbsbevölkerung) (modellierte ILO-Schätzung)"
  )
)

# Die Indikatoren als Referenzliste speichern.
indikatoren <- indikatoren_liste$Code

# Jetzt werden die World Bank Indikatoren mithilfe der API importiert.
# Das Skript prüft zuerst, ob die Datei bereits existiert. Wenn ja, wird sie geladen. Wenn nein, wird der API-Aufruf durchgeführt.
# Zusätzlich prüft das Skript, ob alle benötigten Indikatoren in der vorhandenen Datei enthalten sind.
# Wenn nicht, wird die Datei erneut mit den fehlenden Indikatoren aktualisiert.

# Definiere den Dateipfad
weltbank_datei_pfad <- "data/world_bank_raw.rds"

# Definiere den Zeitrahmen (min/max Jahr der Schweizer Einwanderungsdaten). Somit werden nur die Jahre abgefragt,
# für die wir auch Einwanderungsdaten haben. Das spart Zeit und API-Aufrufe.
start_jahr <- min(jahresdaten_schweizer_einwanderung$jahr, na.rm = TRUE)
end_jahr <- max(jahresdaten_schweizer_einwanderung$jahr, na.rm = TRUE)

# Funktion zum Laden der Daten via API
weltbank_daten_abrufen <- function() {
  WDI::WDI(
    country = "CHE",
    indicator = indikatoren,
    start = start_jahr,
    end = end_jahr,
    extra = TRUE
  )
}

# Lade oder aktualisiere die Daten
if (!file.exists(weltbank_datei_pfad)) {
  # Datei existiert nicht → API-Aufruf
  weltbank_rohe_daten <- weltbank_daten_abrufen()
  saveRDS(weltbank_rohe_daten, weltbank_datei_pfad)
} else {
  # Datei existiert → Prüfe auf fehlende Indikatoren
  temp_pruefung <- readRDS(weltbank_datei_pfad)
  fehlend_in_datei <- setdiff(indikatoren, names(temp_pruefung))

  if (length(fehlend_in_datei) > 0) {
    # Fehlende Indikatoren → API-Aufruf und Überschreiben
    weltbank_rohe_daten <- weltbank_daten_abrufen()
    saveRDS(weltbank_rohe_daten, weltbank_datei_pfad)
  } else {
    # Alle Indikatoren vorhanden → Verwende die Datei
    weltbank_rohe_daten <- temp_pruefung
  }
  # Bereinige temporäre Variablen
  rm(temp_pruefung, fehlend_in_datei)
}

names(weltbank_rohe_daten)

# Für die Analyse benötige ich nur die Spalten jahr, bip_wachstum und arbeitslosenquote. Alle anderen Spalten werden entfernt.
# - NY.GDP.MKTP.KD.ZG (wird zu bip_wachstum): Unabhängige Variable
# - SL.UEM.TOTL.ZS (wird zu arbeitslosenquote): Kontrollvariable

# --- Jetzt werden die World Bank Daten für die Analyse aufbereitet.
# 1. Wähle Spalten: jahr + alle Indikatoren
# 2. Benenne Indikatoren um:
#    - NY.GDP.MKTP.KD.ZG → bip_wachstum (BIP-Wachstum)
#    - SL.UEM.TOTL.ZS → arbeitslosenquote (Arbeitslosenquote)
# 3. Sortiere nach Jahr
schweizer_weltbank_daten <- weltbank_rohe_daten |>
  dplyr::select(year, dplyr::all_of(indikatoren)) |>
  dplyr::rename(
    jahr = year,
    bip_wachstum = NY.GDP.MKTP.KD.ZG,
    arbeitslosenquote = SL.UEM.TOTL.ZS
  ) |>
  dplyr::arrange(jahr)

# ============================================================
# KAPITEL 2C — ZUSAMMENGEFÜHRTE ANALYSEDATEN
# ============================================================

# Jetzt werden die aufbereiteten Einwanderungsdaten mit den World Bank Indikatoren zusammengeführt,
# um einen Datensatz zu erstellen, der alle benötigten Variablen für die Analyse enthält.

analysedaten_schweiz <- jahresdaten_schweizer_einwanderung |>
  dplyr::left_join(
    schweizer_weltbank_daten,
    by = "jahr"
  ) |>
  dplyr::arrange(jahr)

cat("NA counts per variable:\n")
print(colSums(is.na(analysedaten_schweiz)))

# Wir haben keine NA-Werte, was eine gute Nachricht ist.

# ============================================================
# KAPITEL 3 — WELTKARTE DER NETTOZUWANDERUNG
# ============================================================

# Dieses Kapitel visualisiert die Nettozuwanderung in die Schweiz
# nach Herkunftsland auf einer geografischen Weltkarte.
# Fokus: ausschließlich die 15 im Datensatz enthaltenen Herkunftsländer.

# Zuerst muss die Weltkarte geladen werden, um die geometrischen Daten für die Länder zu erhalten.
# Wir verwenden Natural Earth Daten als geometrische Basis für die Karte.
welt <- rnaturalearth::ne_countries(
  scale = "medium",
  returnclass = "sf"
)

# Die Karte zeigt nur die 15 Herkunftsländer, die in unserem Datensatz enthalten sind.
# Die Nettozuwanderung wird über den gesamten Zeitraum (1991–2024) pro Herkunftsland aufsummiert.
karten_daten <- rohe_schweizer_einwanderung |>
  dplyr::group_by(herkunft) |>
  dplyr::summarise(
    netto_migration = sum(netto_migration, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    iso_a3 = countrycode::countrycode(
      herkunft,
      origin = "country.name",
      destination = "iso3c"
    )
  ) |>
  dplyr::filter(!is.na(iso_a3))

# ------------------------------------------------------------
# 3. Weltkarte mit Migrationsdaten verbinden
# ------------------------------------------------------------
# Hier werden Geodaten (welt) mit den Migrationsdaten verknüpft.
# Nur Länder mit passendem ISO-Code erhalten Werte.
# Wir verwenden iso_a3_eh statt iso_a3, da einige Länder (z. B. Frankreich) in iso_a3 den Wert "-99" haben.
# Russland wird auch ausgeschlossen, da es geografisch teilweise in Europa liegt, aber in den Einwanderungsdaten nicht als Herkunftsland auftaucht.
#Ansonst würde die Darstellung verzerrt, da Russland eine sehr grosse Fläche hat.

weltkarte <- welt |>
  dplyr::mutate(
    iso_a3 = dplyr::coalesce(iso_a3_eh, iso_a3)
  ) |>
  dplyr::filter(
    continent == "Europe",
    !name_long %in% c("Russian Federation"),
    !is.na(iso_a3),
    iso_a3 != "-99"
  ) |>
  dplyr::left_join(karten_daten, by = "iso_a3")

# Jetzt kann die Karte visualisiert werden.
schweiz_migrations_weltkarte <- ggplot2::ggplot(weltkarte) +
  ggplot2::geom_sf(
    ggplot2::aes(fill = netto_migration),
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
    fill = "",
    caption = "Nur die 15 Herkunftsländer aus den Einwanderungsdaten"
  ) +
  ggplot2::theme_void() +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold", size = 14),
    legend.title = ggplot2::element_blank(),
    plot.caption = ggplot2::element_text(size = 9, hjust = 0)
  )

print(schweiz_migrations_weltkarte)

#Hier sehen wir also eine Karte mit nur 15 Ländern, die in den Einwanderungsdaten enthalten sind.
# ============================================================
# KAPITEL 4 — ZUSÄTZLICHE VISUALISIERUNGEN
# ============================================================

# Dieses Kapitel erweitert die Analyse um weitere Darstellungen.
# Zuerst wird die Entwicklung der Nettozuwanderung über die Zeit gezeigt.

# Kurvendiagramm: gesamte Nettozuwanderung pro Jahr
kurven_diagramm <- ggplot(
  jahresdaten_schweizer_einwanderung,
  aes(x = jahr, y = netto_migration)
) +
  geom_line(color = "#08519c", linewidth = 1) +
  geom_point(color = "#08519c", size = 2) +
  labs(
    x = "Jahr",
    y = "Nettozuwanderung",
    title = "Nettozuwanderung in die Schweiz nach Jahr",
    subtitle = "Gesamtwert für alle Herkunftsländer kombiniert"
  ) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(1990, 2025, by = 5)) +
  scale_y_continuous(
    labels = scales::comma_format(big.mark = ".", decimal.mark = ",")
  )

print(kurven_diagramm)

# ============================================================
# KAPITEL 4B — BOXPLOT: NETTOZUWANDERUNG NACH VORZEICHEN
# ============================================================

# Für den Boxplot werden die Jahresdaten der Nettozuwanderung verwendet.

# Zuerst erstellen wir eine neue Spalte migration_sign, die angibt, ob es sich um
# Einwanderung (positiv) oder Auswanderung (negativ) handelt
jahresdaten_mit_vorzeichen <- jahresdaten_schweizer_einwanderung |>
  dplyr::mutate(
    migration_sign = ifelse(
      netto_migration >= 0,
      "Einwanderung (positiv)",
      "Auswanderung (negativ)"
    )
  )

# Die Farben der Boxen definieren.

boxplot_vorzeichen <- ggplot(
  jahresdaten_mit_vorzeichen,
  aes(x = migration_sign, y = netto_migration, fill = migration_sign)
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

print(boxplot_vorzeichen)

# Die Streuung der Nettozuwanderung ist in den positiven Jahren deutlich grösser.

# ============================================================
# KAPITEL 4C — HORIZONTALES BALKENDIAGRAMM: NETTOZUWANDERUNG PRO HERKUNFTSLAND
# ============================================================

# Hier summieren wir die Nettozuwanderung pro Herkunftsland über den gesamten Zeitraum.
# Danach erstellen wir ein horizontales Balkendiagramm zum Vergleich der Länder.

# Pro Land summieren.
laender_gesamt <- rohe_schweizer_einwanderung |>
  dplyr::group_by(herkunft) |>
  dplyr::summarise(
    gesamt_netto_migration = sum(netto_migration, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::arrange(desc(gesamt_netto_migration))

laender_gesamt

# Hier sehen wir die Gesamt Nettozuwanderung pro Herkunftsland über den gesamten Zeitraum.
# Bei Spanien ist die Nettozuwanderung sogar negativ, was bedeutet, dass mehr Menschen aus
# Spanien in die Schweiz ausgewandert sind als umgekehrt.
# Eine mögliche Erklärung könnte sein, dass Spanier in der Schweiz arbeiten, aber dann wieder zurück in die Heimat ziehen,
# wenn sie in Rente gehen.
horizontales_balkendiagramm <- ggplot(
  laender_gesamt,
  aes(x = gesamt_netto_migration, y = reorder(herkunft, gesamt_netto_migration))
) +
  geom_col(fill = "#08519c", color = "white", linewidth = 0.2) +
  labs(
    x = "Gesamt Nettozuwanderung",
    y = "Herkunftsland",
    title = "Gesamt Nettozuwanderung in die Schweiz nach Herkunftsland",
    subtitle = "Summe über den gesamten Zeitraum (1991-2024)"
  ) +
  theme_minimal() +
  scale_x_continuous(
    labels = scales::comma_format(big.mark = ".", decimal.mark = ",")
  )

# Plot zeigen
print(horizontales_balkendiagramm)


# ============================================================
# KAPITEL 4D — Alle Plots speichern
# ============================================================

# Jetzt können wir noch alle erstellten Plots als PNG Dateien speichern.

ggplot2::ggsave(
  filename = file.path("figures", "schweiz_migrations_weltkarte.png"),
  plot = schweiz_migrations_weltkarte,
  width = 10,
  height = 6,
  dpi = 300
)

ggplot2::ggsave(
  filename = file.path("figures", "kurven_diagramm.png"),
  plot = kurven_diagramm,
  width = 10,
  height = 6,
  dpi = 300
)

ggplot2::ggsave(
  filename = file.path("figures", "boxplot_vorzeichen.png"),
  plot = boxplot_vorzeichen,
  width = 10,
  height = 6,
  dpi = 300
)

ggplot2::ggsave(
  filename = file.path("figures", "horizontales_balkendiagramm.png"),
  plot = horizontales_balkendiagramm,
  width = 10,
  height = 6,
  dpi = 300
)


# ============================================================
# KAPITEL 5 — REGRESSIONSMODELLE (JAEHRLICHE DATEN)
# ============================================================

# Jetzt können wir eine Regressionsanalyse durchführen, um den Zusammenhang zwischen BIP-Wachstum, Arbeitslosenquote und Nettozuwanderung zu untersuchen.
# Modell 1 hat nur das BIP-Wachstum als unabhängige Variable, während Modell 2 zusätzlich die Arbeitslosenquote als Kontrollvariable enthält.
# Die abhängige Variable in beiden Modellen ist die Nettozuwanderung, die wir bereits auf Jahresbasis aggregiert haben.

# Schritt 1: Nur die relevanten Variablen für die Regression auswählen.
regressionsdaten <- analysedaten_schweiz |>
  dplyr::select(netto_migration, bip_wachstum, arbeitslosenquote)

# Als Sicherheit speichern wir die Regressionsdaten auch als CSV Datei.

readr::write_csv(regressionsdaten, "data/regressionsdaten.csv")

# Jetzt Modell 1 schätzen.
modell_1_bip <- lm(netto_migration ~ bip_wachstum, data = regressionsdaten)

# Und jetzt Modell 2 mit der Kontrollvariable Arbeitslosenquote.
modell_2_bip_arbeitslosenquote <- lm(
  netto_migration ~ bip_wachstum + arbeitslosenquote,
  data = regressionsdaten
)

# Schritt 5: Summaries direkt in der Konsole anzeigen.
summary(modell_1_bip)
summary(modell_2_bip_arbeitslosenquote)


#Hier sieht es besser aus, wenn wir es als Tabelle darstellen, damit wir die Ergebnisse besser vergleichen können.
#Wir benutzen dafür texreg.

texreg::screenreg(
  list(Modell_1 = modell_1_bip, Modell_2 = modell_2_bip_arbeitslosenquote),
  custom.model.names = c("Modell 1", "Modell 2"),
  custom.coef.names = c("(Intercept)", "BIP-Wachstum", "Arbeitslosenquote"),
  custom.dep.var = "Nettozuwanderung",
  digits = 5,
  single.row = FALSE,
  include.f = TRUE,
  caption = "Regressionsergebnisse: Nettozuwanderung"
)

# Export als HTML-Datei.
texreg::htmlreg(
  list(Modell_1 = modell_1_bip, Modell_2 = modell_2_bip_arbeitslosenquote),
  file = file.path("tables", "regressionsergebnisse_texreg.html"),
  custom.model.names = c("Modell 1", "Modell 2"),
  custom.coef.names = c("(Intercept)", "BIP-Wachstum", "Arbeitslosenquote"),
  custom.dep.var = "Nettozuwanderung",
  digits = 5,
  single.row = FALSE,
  include.f = TRUE,
  caption = "Regressionsergebnisse: Nettozuwanderung"
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

# Den positiven Effekt der Arbeitslosigkeit auf die Nettozuwanderung deutet darauf hin, dass das Modell
# nicht alle relevanten Faktoren erfasst, die die Nettozuwanderung beeinflussen. Die Nettozuwanderung ist
# auch stark von anderen Faktoren beeinflusst, wie z. B. das Freizügigkeitsabkommen, die EU-Mitgliedschaft, die Arbeitsmarktsituation
# in den Herkunftsländern etc.

# ============================================================
# KAPITEL 6 — DIAGNOSTISCHE TESTS UND RESIDUALANALYSE
# ============================================================

# Um die Modelle genauer zu analysieren, ist es sinnvoll, die Korrelationen zwischen den Prädiktoren zu prüfen,
# um mögliche Multikollinearität zu identifizieren. Zusätzlich prüfen wir die Autokorrelation der Residuen,
# da sich Nettozuwanderung in Zeitreihen über die Zeit selbst beeinflussen kann.
# Das könnte Standardfehler verzerren und Signifikanztests beeinflussen.

# Schritt 1: Korrelationsmatrix zwischen Nettozuwanderung, BIP-Wachstum und Arbeitslosenquote.
# Das zeigt, ob die Prädiktoren stark miteinander korrelieren.
korrelationsmatrix <- regressionsdaten |>
  cor()

# Korrelationsmatrix anzeigen.
korrelationsmatrix

# Ein Korrelationswert von r = 0.238 zwischen den Prädiktoren spricht für eine schwache Korrelation.
# Das deutet auf kein starkes Multikollinearitätsproblem hin.

# Jetzt messen wir auch die Autokorrelation. Die Autokorrelation der Residuen könnte darauf hinweisen,
# dass die Nettozuwanderung in einem Jahr von der Nettozuwanderung im Vorjahr beeinflusst wird,
# oder anders gesagt, dass die Nettozuwanderung eine Eigendynamik aufweist und auch ohne
# weitere Einflussfaktoren von selbst wachsen würde.

# ACF der Residuen berechnen
acf_objekt <- stats::acf(
  residuals(modell_2_bip_arbeitslosenquote), # Residuen des Modells
  plot = FALSE, # kein Plot erstellen
  na.action = na.pass # NA-Werte nicht entfernen
)

# Ergebnisse in eine übersichtliche Tabelle umwandeln
autokorrelations_daten <- tibble::tibble(
  lag = as.numeric(acf_objekt$lag), # Zeitverzögerung (Lag)
  acf = as.numeric(acf_objekt$acf) # Autokorrelationswert
) |>
  dplyr::filter(lag > 0) # Lag 0 entfernen (immer 1, nicht informativ)

# Anzahl der Beobachtungen berechnen
n_beobachtungen <- length(stats::na.omit(residuals(
  modell_2_bip_arbeitslosenquote
)))

# 95%-Konfidenzgrenze
konfidenzgrenze <- 1.96 / sqrt(n_beobachtungen)

# Tabelle ausgeben
autokorrelations_daten

# Konfidenzgrenzen separat anzeigen
konfidenzgrenze

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
# Das Modell wird auch von einer zeitlichen Dynamik beeinflusst, die nicht durch die Prädiktoren erfasst wird.
# Das könnte die Schätzung der Effekte verzerren.

# ============================================================
# KAPITEL 7 — OBJEKTE IN LISTEN ZUSAMMENFASSEN
# ============================================================

# Wir fassen die wichtigsten Objekte in einfachen Listen zusammen.
# Das macht das Projekt leichter zu überblicken und später leichter exportierbar.

# Alle wichtigen Variablen sind bereits in Deutsch vorhanden

# Deutsche Listen-Namen.
daten_objekte <- list(
  rohe_schweizer_einwanderung = rohe_schweizer_einwanderung,
  jahresdaten_schweizer_einwanderung = jahresdaten_schweizer_einwanderung,
  analysedaten_schweiz = analysedaten_schweiz,
  regressionsdaten = regressionsdaten,
  indikatoren_tabelle = indikatoren_liste,
  schweizer_weltbank_daten = schweizer_weltbank_daten,
  weltkarten_daten = karten_daten,
  laender_summen = laender_gesamt,
  autokorrelations_tabelle = autokorrelations_daten
)

matrix_objekte <- list(
  korrelationsmatrix = korrelationsmatrix,
  acf_objekt = acf_objekt,
  konfidenzgrenze = konfidenzgrenze,
  beobachtungen = n_beobachtungen
)

modell_objekte <- list(
  modell_1_bip = modell_1_bip,
  modell_2_bip_arbeitslosenquote = modell_2_bip_arbeitslosenquote
)

plot_objekte <- list(
  weltkarte_migration = schweiz_migrations_weltkarte,
  verlaufslinie = kurven_diagramm,
  boxplot_vorzeichen = boxplot_vorzeichen,
  balkendiagramm_laender = horizontales_balkendiagramm
)

# Die Listen kurz in der Konsole anzeigen.
daten_objekte
matrix_objekte
modell_objekte
plot_objekte

# Aufräumen: Lösche alle einzelnen Objekte, die jetzt in den Listen organisiert sind.
#
rm(
  welt,
  weltbank_rohe_daten,
  indikatoren_liste,
  indikatoren,
  erforderliche_pakete,
  fehlende_pakete,
  pfade,
  weltbank_datei_pfad,
  start_jahr,
  end_jahr,
  weltbank_daten_abrufen,
  schweizer_weltbank_daten,
  karten_daten,
  weltkarte,
  rohe_schweizer_einwanderung,
  jahresdaten_schweizer_einwanderung,
  analysedaten_schweiz,
  regressionsdaten,
  modell_1_bip,
  modell_2_bip_arbeitslosenquote,
  schweiz_migrations_weltkarte,
  kurven_diagramm,
  boxplot_vorzeichen,
  horizontales_balkendiagramm,
  laender_gesamt,
  jahresdaten_mit_vorzeichen,
  korrelationsmatrix,
  acf_objekt,
  konfidenzgrenze,
  n_beobachtungen,
  autokorrelations_daten
)

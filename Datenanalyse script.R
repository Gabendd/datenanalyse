# ============================================================
# REPRODUZIERBARKEITS-PRÜFUNG
# ============================================================

# Definiert die Paketversionen, die während der Entwicklung verwendet wurden

# ============================================================
erforderliche_versionen <- list(
  readr = "2.2.0",
  dplyr = "1.2.1",
  stringr = "1.6.0",
  stringi = "1.8.7",
  tibble = "3.3.1",
  tidyr = "1.3.2",
  ggplot2 = "4.0.3",
  scales = "1.4.0",
  sf = "1.1.1",
  WDI = "2.7.10",
  rnaturalearth = "1.2.0",
  countrycode = "1.8.0",
  rvest = "1.0.5",
  huxtable = "5.8.0",
  stargazer = "5.2.3",
  texreg = "1.39.5",
  broom = "1.0.13",
  codetools = "0.2.20",
  gtsummary = "2.5.1",
  gt = "1.3.0"
)


# ============================================================
# Versionen prüfen
# ============================================================

# Liste für abweichende Versionen vorbereiten
version_mismatches <- list()

# Jedes Paket prüfen
for (pkg in names(erforderliche_versionen)) {
  # Nur prüfen, wenn Paket installiert ist
  if (requireNamespace(pkg, quietly = TRUE)) {
    # Aktuelle Version holen
    aktuelle_version <- as.character(packageVersion(pkg))
    benoetigte_version <- erforderliche_versionen[[pkg]]

    # Bei Abweichung speichern
    if (aktuelle_version != benoetigte_version) {
      version_mismatches[[pkg]] <- list(
        aktuell = aktuelle_version,
        benoetigt = benoetigte_version
      )
    }
  }
  # Pakete die nicht installiert sind werden später durch install.packages installiert
}

# ============================================================
# Warnungen oder Bestätigung ausgeben
# ============================================================
if (length(version_mismatches) > 0) {
  cat(
    "\n⚠️  VERSIONS-WARNUNG: Paketversionen weichen von der Entwicklungs-Umgebung ab\n"
  )
  cat("   Das Skript wird trotzdem ausgeführt. Bei Fehlern versuchen Sie:\n")
  cat(
    "   1. Exakte Versionen mit: remotes::install_version('paket', version = 'x.y.z')\n"
  )
  cat("   2. Oder renv für garantierte Reproduzierbarkeit einrichten\n\n")
  cat("   Installierte vs. Entwicklungs-Versionen:\n")

  for (pkg in names(version_mismatches)) {
    aktuell <- version_mismatches[[pkg]]$aktuell
    benoetigt <- version_mismatches[[pkg]]$benoetigt
    cat(paste0(
      "   • ",
      pkg,
      ": Sie haben ",
      aktuell,
      ", Entwicklung nutzte ",
      benoetigt,
      "\n"
    ))
  }
  cat("\n")
  Sys.sleep(5) # 5 Sekunden Pause, damit Nutzer die Warnung lesen kann
} else {
  cat("\n✓ Alle Paketversionen entsprechen der Entwicklungs-Umgebung\n")
  cat(
    "  Reproduzierbarkeits-Prüfung abgeschlossen. Skript wird fortgesetzt.\n\n"
  )
  Sys.sleep(5) # 5 Sekunden Pause, damit Nutzer die Meldung lesen kann
}

# ============================================================
# KAPITEL 1 — SETUP UND PAKETE
# ============================================================
# Arbeitsverzeichnis auf den Skript-Speicherort setzen
if (
  requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()
) {
  script_pfad <- dirname(rstudioapi::getActiveDocumentContext()$path)
} else {
  script_pfad <- getwd()
}

setwd(script_pfad)
cat("Arbeitsverzeichnis gesetzt auf:", script_pfad, "\n")

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
  "sf",
  "tibble",
  "broom",
  "stargazer",
  "texreg",
  "tidyr",
  "codetools",
  "rvest",
  "gtsummary",
  "gt"
)


options(scipen = 999)
options(repos = c(CRAN = "https://cloud.r-project.org"))

fehlende_pakete <- setdiff(erforderliche_pakete, rownames(installed.packages()))
if (length(fehlende_pakete) > 0) {
  install.packages(fehlende_pakete)
}

invisible(lapply(erforderliche_pakete, library, character.only = TRUE))

# Arbeitsverzeichnisse definieren
pfade <- c("tabellen", "grafiken", "data")
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
# Variablen (herkunft, jahr, netto_zuwanderung) werden behalten. Zudem werden die Zeilen mit aggregierten Regionen (z. B. "Afrique",
# "Amérique", "Asie", "Océanie") sowie die Zeilen mit nicht-informativem Text (z. B. "Renseignements|Source|© OFS") herausgefiltert.

# Immigrationsdaten einlesen, bereinigen und filtern
# Falls lokale Datei existiert, wird diese verwendet. Ansonsten wird von GitHub geladen.
if (file.exists("data/swiss_immigration_countries_year.csv")) {
  rohe_schweizer_einwanderung <- readr::read_csv(
    "data/swiss_immigration_countries_year.csv",
    show_col_types = FALSE
  )
} else {
  rohe_schweizer_einwanderung <- readr::read_csv(
    "https://raw.githubusercontent.com/Gabendd/datenanalyse/main/data/swiss_immigration_countries_year.csv",
    show_col_types = FALSE
  )
}

# Datenaufbereitung fortsetzen
rohe_schweizer_einwanderung <- rohe_schweizer_einwanderung |>
  dplyr::mutate(
    jahr = as.integer(year),
    netto_zuwanderung = as.numeric(net_migration)
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
  dplyr::select(herkunft, jahr, netto_zuwanderung) |>
  dplyr::arrange(herkunft, jahr)

# Jetzt werden die Daten auf Jahresbasis aggregiert, um die Nettozuwanderung pro Jahr zu erhalten.
jahresdaten_schweizer_einwanderung <- rohe_schweizer_einwanderung |>
  dplyr::group_by(jahr) |>
  dplyr::summarize(
    netto_zuwanderung = sum(netto_zuwanderung, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::arrange(jahr)

summary(jahresdaten_schweizer_einwanderung$netto_zuwanderung)
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

# Jetzt werden die aufbereiteten Einwanderungsd4aten mit den World Bank Indikatoren zusammengeführt,
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
    netto_zuwanderung = sum(netto_zuwanderung, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    iso_a3 = ifelse(
      herkunft == "Kosovo",
      "XKX",
      countrycode::countrycode(
        herkunft,
        origin = "country.name",
        destination = "iso3c"
      )
    )
  ) |>
  dplyr::filter(!is.na(iso_a3)) |>
  dplyr::mutate(
    herkunft_de = ifelse(
      herkunft == "Kosovo",
      "Kosovo",
      countrycode::countrycode(
        herkunft,
        origin = "country.name.en",
        destination = "country.name.de"
      )
    )
  )

missing_country <- rohe_schweizer_einwanderung |>
  dplyr::group_by(herkunft) |>
  dplyr::summarise() |>
  dplyr::mutate(
    iso_a3 = ifelse(
      herkunft == "Kosovo",
      "XKX",
      countrycode::countrycode(herkunft, "country.name", "iso3c")
    )
  ) |>
  dplyr::filter(is.na(iso_a3))
print(missing_country$herkunft)

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
    ggplot2::aes(fill = netto_zuwanderung),
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
    plot.title = ggplot2::element_text(face = "plain", size = 21, hjust = 0.5),
    plot.title.position = "plot",
    legend.title = ggplot2::element_blank(),
    legend.text = ggplot2::element_text(size = 21),
    plot.caption = ggplot2::element_text(size = 9, hjust = 0)
  )

print(schweiz_migrations_weltkarte)

#Hier sehen wir also eine Karte mit nur 15 Ländern, die in den Einwanderungsdaten enthalten sind.

# Liste aller in der Karte dargestellten Herkunftsländer (Deutsch)
karten_laender_liste <- karten_daten$herkunft_de

# Dataset mit den Kartendaten (Herkunftsländer + Nettozuwanderung, Deutsch)
karten_dataset <- karten_daten |>
  dplyr::select(herkunft_de, netto_zuwanderung, iso_a3)

# ============================================================
# KAPITEL 4 — ZUSÄTZLICHE VISUALISIERUNGEN
# ============================================================

# Dieses Kapitel erweitert die Analyse um weitere Darstellungen.
# Zuerst wird die Entwicklung der Nettozuwanderung über die Zeit gezeigt.

# Kurvendiagramm: gesamte Nettozuwanderung pro Jahr
kurven_diagramm <- ggplot(
  jahresdaten_schweizer_einwanderung,
  aes(x = jahr, y = netto_zuwanderung)
) +
  geom_line(color = "#08519c", linewidth = 1) +
  geom_point(color = "#08519c", size = 2) +
  labs(
    x = "Jahr",
    y = "Nettozuwanderung",
    title = "Nettozuwanderung in die Schweiz nach Jahr",
  ) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(1990, 2025, by = 5)) +
  scale_y_continuous(
    labels = scales::comma_format(big.mark = ".", decimal.mark = ",")
  ) +
  theme(
    plot.title = element_text(face = "plain", size = 21, hjust = 0.5),
    axis.title = element_text(size = 22),
    axis.text = element_text(size = 22),
    axis.text.x = element_text(size = 14, margin = margin(t = 10))
  )

print(kurven_diagramm)

# Nach einer Phase mit einer Abnahme der Nettozuwanderung und
# einer Auswanderungsphase zwischen 1995 und 2000 gab es ab 2000 wieder
# einen starken Anstieg der Nettozuwanderung.

# Mit der Finanzkrise 2008 gab es einen Einbruch, aber danach stieg die
# Nettozuwanderung wieder  an, bis ab 2014, und danach
# wieder eine Abnahme folgte.

# Es ist aber zu beachten, dass unsere Daten nur die Einwanderung aus
# 15 Ländern darstellen, was die Repräsentativität der Grafik
# einschränkt.

# ============================================================
# KAPITEL 4B — BOXPLOT: NETTOZUWANDERUNG NACH VORZEICHEN
# ============================================================

# Für den Boxplot werden die Jahresdaten der Nettozuwanderung verwendet.

# Zuerst erstellen wir eine neue Spalte migration_sign, die angibt, ob es sich um
# Einwanderung (positiv) oder Auswanderung (negativ) handelt
jahresdaten_mit_vorzeichen <- jahresdaten_schweizer_einwanderung |>
  dplyr::mutate(
    migration_sign = ifelse(
      netto_zuwanderung >= 0,
      "Einwanderung (positiv)",
      "Auswanderung (negativ)"
    )
  )

# Die Farben der Boxen definieren.

boxplot_vorzeichen <- ggplot(
  jahresdaten_mit_vorzeichen,
  aes(x = migration_sign, y = netto_zuwanderung, fill = migration_sign)
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
  ) +
  theme_minimal() +
  scale_y_continuous(
    labels = scales::comma_format(big.mark = ".", decimal.mark = ",")
  ) +
  theme(
    plot.title = element_text(face = "plain", size = 21, hjust = 0.5),
    plot.title.position = "plot",
    axis.title = element_text(size = 22),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text = element_text(size = 22),
    legend.title = element_text(size = 21),
    legend.text = element_text(size = 14)
  )

print(boxplot_vorzeichen)

zusammenfassung <- jahresdaten_mit_vorzeichen |>
  dplyr::group_by(migration_sign) |>
  dplyr::summarize(
    anzahl = n(),
    mittelwert = mean(netto_zuwanderung),
    median = median(netto_zuwanderung),
    sd = sd(netto_zuwanderung),
    min = min(netto_zuwanderung),
    max = max(netto_zuwanderung),
    iqr = IQR(netto_zuwanderung)
  )
print(zusammenfassung)
# Wir sehen, dass die Streuung der Nettozuwanderung in den positiven Jahren deutlich grösser ist.
# Die Schweizer Migration wird von wenigen Jahren mit extrem hoher
# Einwanderung dominiert, was man an den hohen SD-Werten ablesen kann:
# Sie liegt bei 17'609, während die SD bei den negativen Jahren nur 3'616 beträgt.
# Abwanderungsjahre sind selten und weisen moderate, stabile Verluste auf.
# Das kann man auch am Anteil der Abwanderungsjahre in der Zusammenfassung sehen:
# Es gibt nur 4 Jahre mit negativer Nettozuwanderung, aber 30 Jahre mit positiver Nettozuwanderung.

# ============================================================
# KAPITEL 4C — HORIZONTALES BALKENDIAGRAMM: NETTOZUWANDERUNG PRO HERKUNFTSLAND
# ============================================================

# Hier summieren wir die Nettozuwanderung pro Herkunftsland über den gesamten Zeitraum.
# Danach erstellen wir ein horizontales Balkendiagramm zum Vergleich der Länder.

# Pro Land summieren.
laender_gesamt <- rohe_schweizer_einwanderung |>
  dplyr::group_by(herkunft) |>
  dplyr::summarise(
    gesamt_netto_zuwanderung = sum(netto_zuwanderung, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::arrange(desc(gesamt_netto_zuwanderung)) |>
  dplyr::mutate(
    herkunft_de = ifelse(
      herkunft == "Kosovo",
      "Kosovo",
      countrycode::countrycode(
        herkunft,
        origin = "country.name.en",
        destination = "country.name.de"
      )
    )
  )

laender_gesamt

# Hier sehen wir die Gesamt-Nettozuwanderung pro Herkunftsland über den gesamten Zeitraum.
# Bei Spanien ist die Nettozuwanderung sogar negativ, was bedeutet, dass mehr Menschen aus
# der Schweiz nach Spanien ausgewandert sind als umgekehrt.
# Eine mögliche Erklärung könnte sein, dass Spanier in der Schweiz arbeiten, aber nach der
# Pensionierung wieder in ihre Heimat zurückziehen.
# Die zwei wichtigsten Nachbarn, Deutschland und Frankreich, liegen mit Abstand auf Platz 1
# und 2 bei den meisten Einwanderungen.
# Erstaunlich ist, dass Portugal mehr Einwanderer als Italien aufweist.
# Das könnte auch mit den Grenzgängern aus Italien zusammenhängen, die in der Schweiz arbeiten,
# aber in Italien wohnen, und nicht in diesen Zahlen erfasst werden,
# da sie ihren Wohnsitz in Italien behalten und nicht als Einwanderer gezählt werden.

horizontales_balkendiagramm <- ggplot(
  laender_gesamt,
  aes(
    x = gesamt_netto_zuwanderung,
    y = reorder(herkunft_de, gesamt_netto_zuwanderung)
  )
) +
  geom_col(fill = "#08519c", color = "white", linewidth = 0.2) +
  geom_text(
    aes(
      label = scales::comma(
        gesamt_netto_zuwanderung,
        big.mark = ".",
        decimal.mark = ","
      )
    ),
    hjust = -0.1,
    color = "black",
    size = 4
  ) +
  labs(
    x = "Gesamt Nettozuwanderung",
    y = "Herkunftsland",
    title = "Gesamt Nettozuwanderung in die Schweiz nach Herkunftsland",
  ) +
  theme_minimal() +
  scale_x_continuous(
    labels = scales::comma_format(big.mark = ".", decimal.mark = ",")
  ) +
  theme(
    plot.title = element_text(
      face = "plain",
      size = 16,
      hjust = 0.5,
      margin = margin(b = 10)
    ),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 20),
    axis.text.y = element_text(size = 17, margin = margin(r = 5))
  )

# Plot zeigen
print(horizontales_balkendiagramm)


# ============================================================
# KAPITEL 4D — Alle Grafiken speichern
# ============================================================

# Jetzt können wir noch alle erstellten Grafiken als PNG Dateien speichern une exportieren.
# Alle Grafiken werden in einem Unterordner "grafiken" gespeichert.

ggplot2::ggsave(
  filename = file.path("grafiken", "schweiz_migrations_weltkarte.png"),
  plot = schweiz_migrations_weltkarte,
  width = 10,
  height = 6,
  dpi = 300
)

ggplot2::ggsave(
  filename = file.path("grafiken", "kurven_diagramm.png"),
  plot = kurven_diagramm,
  width = 10,
  height = 6,
  dpi = 300
)

ggplot2::ggsave(
  filename = file.path("grafiken", "boxplot_vorzeichen.png"),
  plot = boxplot_vorzeichen,
  width = 10,
  height = 6,
  dpi = 300
)

ggplot2::ggsave(
  filename = file.path("grafiken", "horizontales_balkendiagramm.png"),
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

#Zuerst, nur die relevanten Variablen für die Regression auswählen.
regressionsdaten <- analysedaten_schweiz |>
  dplyr::select(netto_zuwanderung, bip_wachstum, arbeitslosenquote)

# Als Sicherheit speichern wir die Regressionsdaten auch als CSV Datei.
readr::write_csv(regressionsdaten, "data/regressionsdaten.csv")

# Jetzt Modell 1 schätzen.
modell_1_bip <- lm(netto_zuwanderung ~ bip_wachstum, data = regressionsdaten)

# Und jetzt Modell 2 mit der Kontrollvariable Arbeitslosenquote.
modell_2_bip_arbeitslosenquote <- lm(
  netto_zuwanderung ~ bip_wachstum + arbeitslosenquote,
  data = regressionsdaten
)

#Summaries direkt in der Konsole anzeigen.
summary(modell_1_bip)
summary(modell_2_bip_arbeitslosenquote)


#Hier sieht es besser aus, wenn wir es als Tabelle darstellen, damit wir die Ergebnisse besser vergleichen können.
#Wir benutzen dafür texreg.

texreg::screenreg(
  list(Modell_1 = modell_1_bip, Modell_2 = modell_2_bip_arbeitslosenquote),
  custom.model.names = c("Modell 1", "Modell 2"),
  custom.coef.names = c("(Intercept)", "BIP-Wachstum", "Arbeitslosenquote"),
  custom.dep.var = "Nettozuwanderung",
  custom.note = "Abhängige Variable: Nettozuwanderung. Unabhängige Variablen: BIP-Wachstum (Modell 1+2), Arbeitslosenquote (Modell 2). Signifikanz: * p < 0.05, ** p < 0.01, *** p < 0.001",
  digits = 5,
  single.row = FALSE,
  include.f = TRUE,
  caption = "Regressionsergebnisse: Nettozuwanderung"
)

# Export als HTML-Datei.
texreg::htmlreg(
  list(Modell_1 = modell_1_bip, Modell_2 = modell_2_bip_arbeitslosenquote),
  file = file.path("tabellen", "regressionsergebnisse_texreg.html"),
  custom.model.names = c("Modell 1", "Modell 2"),
  custom.coef.names = c("(Intercept)", "BIP-Wachstum", "Arbeitslosenquote"),
  custom.dep.var = "Nettozuwanderung",
  custom.note = "Abhängige Variable: Nettozuwanderung. Unabhängige Variablen: BIP-Wachstum (Modell 1+2), Arbeitslosenquote (Modell 2). Signifikanz: * p < 0.05, ** p < 0.01, *** p < 0.001",
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
# Dieser Effekt ist statistisch nicht signifikant (p > 0.05).
# Das Modell erklärt nur 0.9 % der Variation der Nettozuwanderung (R² = 0.009).
# Konstante: Bei einem BIP-Wachstum von 0 % beträgt die geschätzte
# Nettozuwanderung 25'865 Personen. Der Wert ist statistisch signifikant (p < 0.001).

# Modell 2:
# Unter Kontrolle der Arbeitslosenquote ist ein zusätzlicher Prozentpunkt
# BIP-Wachstum mit 187 Personen weniger Nettozuwanderung verbunden.
# Dieser Effekt ist nicht statistisch signifikant (p > 0.05).

# Ein Anstieg der Arbeitslosenquote um 1 Prozentpunkt ist mit einer um
# durchschnittlich 11'709 Personen höheren Nettozuwanderung verbunden.
# Dieser Effekt ist statistisch hoch signifikant (p < 0.01), aber kontraintuitiv,
# daher muss er mit Vorsicht interpretiert werden.
# Konstante: Bei 0 % BIP-Wachstum und 0 % Arbeitslosenquote beträgt die
# geschätzte Nettozuwanderung -18'119 Personen. Der Wert ist nicht signifikant.
# Das Modell erklärt 21.4 % der Variation der Nettozuwanderung (R² = 0.214).

# Den positiven Effekt der Arbeitslosigkeit auf die Nettozuwanderung deutet darauf hin, dass das Modell
# nicht alle relevanten Faktoren erfasst, die die Nettozuwanderung beeinflussen. Die Nettozuwanderung ist
# auch stark von anderen Faktoren beeinflusst, wie z. B. das Freizügigkeitsabkommen, die EU-Mitgliedschaft, die Arbeitsmarktsituation
# in den Herkunftsländern etc.

# ============================================================
# KAPITEL 6 — DESKRIPTIVE STATISTIK DER ANALYSEVARIABLEN
# ============================================================
# Mithilfe Deskriptiver Statistiken können wir die Verteilung und
# zentrale Tendenzen der Variablen in unserem Datensatz besser verstehen.
#
# Das Kapitel erstellt eine übersichtliche Zusammenfassungstabelle der wichtigsten
# Variablen, gruppiert nach Migrationsrichtung (Einwanderung/Auswanderung)

deskriptive_tabelle <- analysedaten_schweiz |>
  dplyr::mutate(
    migrationsrichtung = ifelse(
      netto_zuwanderung >= 0,
      "Einwanderung",
      "Auswanderung"
    )
  ) |>
  dplyr::select(
    migrationsrichtung,
    netto_zuwanderung,
    bip_wachstum,
    arbeitslosenquote
  ) |>
  gtsummary::tbl_summary(
    by = migrationsrichtung,
    label = list(
      netto_zuwanderung ~ "Nettozuwanderung",
      bip_wachstum ~ "BIP-Wachstum (%)",
      arbeitslosenquote ~ "Arbeitslosenquote (%)"
    ),
    statistic = list(all_continuous() ~ "{mean} ({sd})"),
    digits = all_continuous() ~ 2
  ) |>
  gtsummary::modify_header(label = "**Variable**") |>
  gtsummary::bold_labels() |>
  gtsummary::add_p() |>
  gtsummary::as_gt() |>
  gt::tab_header(title = "Deskriptive Statistik nach Migrationsrichtung")

print(deskriptive_tabelle)


# Die deskriptive Tabelle zeigt, dass sich BIP-Wachstum (p = 0.8) und
# Arbeitslosenquote (p = 0.3) zwischen Einwanderungs- und Auswanderungsjahren
# kaum unterscheiden, was mit den nicht signifikanten Regressionseffekten
# aus Kapitel 5 übereinstimmt.

# ============================================================
# KAPITEL 7 — DIAGNOSTISCHE TESTS UND RESIDUALANALYSE
# ============================================================

# Um die Modelle genauer zu analysieren, ist es sinnvoll, die Korrelationen zwischen den Prädiktoren zu prüfen,
# um mögliche Multikollinearität zu identifizieren. Zusätzlich prüfen wir die Autokorrelation der Residuen,
# da sich Nettozuwanderung in Zeitreihen über die Zeit selbst beeinflussen kann.
# Das könnte Standardfehler verzerren und Signifikanztests beeinflussen.

# Korrelationsmatrix zwischen Nettozuwanderung, BIP-Wachstum und Arbeitslosenquote.
# Das zeigt, ob die Prädiktoren stark miteinander korrelieren.
korrelationsmatrix <- regressionsdaten |>
  cor()

# Korrelationsmatrix anzeigen.
korrelationsmatrix

# Ein Korrelationswert von r = 0.238 zwischen den Prädiktoren (BIP-Wachstum und Arbeitslosenquote) spricht für eine schwache Korrelation.
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

# Interpretation der ACF-Ergebnisse:
# Ein "Lag" beschreibt die zeitliche Verschiebung zwischen Beobachtungen.
# Lag 1 bedeutet z.B. den Vergleich zwischen einem Wert und dem Wert des Vorjahres,
# Lag 2 den Vergleich mit dem Wert von vor zwei Jahren usw.
#
# In den Ergebnissen zeigt sich starke positive Autokorrelation bei kleinen Lags,
# insbesondere bei Lag 1 (0.74) und Lag 2 (0.38), beide über der 95%-Grenze (±0.336).
# Das bedeutet, dass die Residuen zeitlich abhängig sind und nicht zufällig schwanken.
#
# Das Modell wird auch von einer zeitlichen Dynamik beeinflusst, die nicht durch die Prädiktoren erfasst wird.
# Die Residuen zeigen zeitliche Abhängigkeiten und schwanken daher nicht zufällig.
# Dadurch können insbesondere die Standardfehler und Signifikanztests der geschätzten Effekte verzerrt werden.

# ============================================================
# KAPITEL 8 — OBJEKTE IN LISTEN ZUSAMMENFASSEN
# ============================================================

#Als letzte Etappe, auch für eine spärtere Erweritung der Anyse, werden alle wichtigen Objekte in Listen organisiert, und danach die
#einzelnen Objekte gelöscht, um die Arbeitsumgebung aufzuräumen.

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
  zusammenfassung,
  korrelationsmatrix,
  acf_objekt,
  konfidenzgrenze,
  n_beobachtungen,
  autokorrelations_daten
)

# ============================================================
# KAPITEL 9 — SESSION INFORMATION (für Debugging und Reproduzierbarkeit)
# ============================================================
# Diese Information hilft, die genaue R-Version und Paketversionen zu identifizieren,
# falls der Code nicht wie erwartet funktioniert.

cat("\n\n========== SESSION INFO ==========\n")
sessionInfo()

# Benötigte Pakete laden
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

missing_packages <- setdiff(required_packages, rownames(installed.packages()))
if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

invisible(lapply(required_packages, library, character.only = TRUE))

# Arbeitsverzeichnisse anlegen
paths <- c("data/raw", "data/processed", "tables", "figures")
invisible(lapply(paths, function(path) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
  }
}))

# Nur die für die Studie benötigten World-Bank-Indikatoren behalten
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

# Die durchschnittliche Nettozuwanderung pro Herkunftsland schwankt im Zeitverlauf stark.

# ============================================================
# KAPITEL 3 — WORLD-BANK-DATEN FÜR DIE SCHWEIZ
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
  rm(list = intersect(c("temp_check", "missing_in_file"), ls()))
}

swiss_world_bank_data <- world_bank_raw_data |>
  dplyr::filter(iso3c == "CHE") |>
  dplyr::select(country, iso3c, year, dplyr::all_of(indicators)) |>
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

# ============================================================
# KAPITEL 5 — BFS-DATEN AUFBEREITEN
# ============================================================

bfs_rds <- "data/raw/bfs_raw.rds"
bfs_dataset_id <- "px-x-0103020200_102"
bfs_canton_rds <- "data/raw/bfs_canton_immigration_2024.rds"
latest_year_canton <- 2024

standardize_bfs_dt <- function(data) {
  dt <- data.table::as.data.table(data)
  data.table::setnames(dt, names(dt), tolower(names(dt)))
  dt
}

first_existing_column <- function(candidates, names_vec) {
  hit <- intersect(candidates, names_vec)
  if (length(hit) == 0) {
    NA_character_
  } else {
    hit[[1]]
  }
}

keep_total_rows <- function(dt) {
  sex_col <- first_existing_column(c("geschlecht", "sex", "gender"), names(dt))
  age_col <- first_existing_column(
    c("altersklasse", "ageclass", "age_class", "age"),
    names(dt)
  )

  if (!is.na(sex_col)) {
    dt <- dt[
      tolower(as.character(get(sex_col))) %in%
        c("0", "total", "all", "both", "both sexes")
    ]
  }
  if (!is.na(age_col)) {
    dt <- dt[
      tolower(as.character(get(age_col))) %in%
        c("0", "total", "all", "all ages")
    ]
  }
  dt
}

clean_bfs_population <- function(data) {
  dt <- keep_total_rows(standardize_bfs_dt(data))

  year_col <- first_existing_column(c("jahr", "year"), names(dt))
  nationality_col <- first_existing_column(
    c(
      "staatsangehoerigkeit",
      "staatsangehörigkeit",
      "citizenship",
      "nationality"
    ),
    names(dt)
  )
  value_col <- first_existing_column(
    c(
      "einwanderung_der_standigen_wohnbevolkerung",
      "resident_population",
      "population",
      "value"
    ),
    names(dt)
  )

  if (is.na(year_col) || is.na(nationality_col) || is.na(value_col)) {
    return(data.table::data.table())
  }

  out <- dt[, .(
    nationality = stringr::str_trim(as.character(get(nationality_col))),
    year = as.integer(get(year_col)),
    pop_bfs = as.numeric(get(value_col))
  )]

  out <- out[!is.na(pop_bfs) & !is.na(nationality) & nationality != ""]
  out
}

clean_bfs_canton <- function(data) {
  dt <- keep_total_rows(standardize_bfs_dt(data))

  canton_col <- first_existing_column(c("kanton", "canton"), names(dt))
  nationality_col <- first_existing_column(
    c(
      "staatsangehoerigkeit",
      "staatsangehörigkeit",
      "citizenship",
      "nationality"
    ),
    names(dt)
  )
  year_col <- first_existing_column(c("jahr", "year"), names(dt))
  value_col <- first_existing_column(
    c(
      "einwanderung_der_standigen_wohnbevolkerung",
      "immigration_from_abroad",
      "value"
    ),
    names(dt)
  )

  if (
    is.na(canton_col) ||
      is.na(nationality_col) ||
      is.na(year_col) ||
      is.na(value_col)
  ) {
    return(data.table::data.table())
  }

  out <- dt[, .(
    canton = stringr::str_trim(as.character(get(canton_col))),
    nationality = stringr::str_trim(as.character(get(nationality_col))),
    year = as.integer(get(year_col)),
    immigration_from_abroad = as.numeric(get(value_col))
  )]

  out <- out[
    !is.na(canton) &
      canton != "" &
      !is.na(nationality) &
      nationality != "" &
      canton != "Schweiz" &
      nationality != "Schweiz"
  ]
  out
}

if (file.exists(bfs_rds)) {
  bfs_raw <- readRDS(bfs_rds)
} else {
  bfs_raw <- tryCatch(
    BFS::bfs_get_data(
      number_bfs = bfs_dataset_id,
      language = "en",
      clean_names = TRUE
    ),
    error = function(e) {
      message("BFS-API-Aufruf fehlgeschlagen: ", conditionMessage(e))
      NULL
    }
  )
  if (!is.null(bfs_raw)) {
    saveRDS(bfs_raw, bfs_rds)
  }
}

if (!is.null(bfs_raw)) {
  bfs_clean <- clean_bfs_population(bfs_raw)
  if (nrow(bfs_clean) > 0) {
    bfs_clean <- data.table::as.data.table(bfs_clean)
    data.table::setorder(bfs_clean, nationality, year)

    swiss_immigration_enhanced <- swiss_immigration_data |>
      dplyr::left_join(
        as.data.frame(bfs_clean) |>
          dplyr::select(
            origin_en = nationality,
            year,
            resident_population = pop_bfs
          ),
        by = c("origin_en", "year")
      )
  } else {
    bfs_clean <- NULL
  }
}

if (file.exists(bfs_canton_rds)) {
  bfs_canton_raw <- readRDS(bfs_canton_rds)
} else {
  meta_canton <- BFS::bfs_get_metadata(
    number_bfs = bfs_dataset_id,
    language = "de"
  )
  canton_values <- if (!is.null(meta_canton$values[[2]])) {
    meta_canton$values[[2]][2:27]
  } else {
    NULL
  }
  nationality_values <- if (!is.null(meta_canton$values[[3]])) {
    meta_canton$values[[3]][-1]
  } else {
    NULL
  }

  bfs_canton_raw <- tryCatch(
    BFS::bfs_get_data(
      number_bfs = bfs_dataset_id,
      language = "de",
      query = list(
        Jahr = as.character(latest_year_canton),
        Kanton = canton_values,
        Staatsangehörigkeit = nationality_values,
        Geschlecht = "0",
        Altersklasse = "0"
      ),
      clean_names = TRUE
    ),
    error = function(e) {
      message("BFS-Kantonsabruf fehlgeschlagen: ", conditionMessage(e))
      NULL
    }
  )
  if (!is.null(bfs_canton_raw)) {
    saveRDS(bfs_canton_raw, bfs_canton_rds)
  }
}

if (!is.null(bfs_canton_raw)) {
  bfs_canton_clean <- clean_bfs_canton(bfs_canton_raw)
  if (nrow(bfs_canton_clean) > 0) {
    bfs_canton_clean <- data.table::as.data.table(bfs_canton_clean)
    data.table::setorder(bfs_canton_clean, canton, nationality, year)

    bfs_canton_map_data <- bfs_canton_clean[
      year == latest_year_canton,
      .(
        immigration_from_abroad = sum(immigration_from_abroad, na.rm = TRUE)
      ),
      by = .(canton)
    ][,
      canton_name := data.table::fcase(
        canton == "Bern / Berne"                     , "Bern"       ,
        canton == "Fribourg / Freiburg"              , "Fribourg"   ,
        canton == "Graubünden / Grigioni / Grischun" , "Graubünden" ,
        canton == "Valais / Wallis"                  , "Valais"     ,
        default = canton
      )
    ]

    swiss_canton_map_data <- BFS::bfs_get_base_maps(
      geom = "kant",
      return_sf = TRUE
    ) |>
      dplyr::left_join(
        as.data.frame(bfs_canton_map_data) |>
          dplyr::select(canton_name, immigration_from_abroad),
        by = c("name" = "canton_name")
      )

    swiss_canton_immigration_map <- ggplot2::ggplot(swiss_canton_map_data) +
      ggplot2::geom_sf(
        ggplot2::aes(fill = immigration_from_abroad),
        color = "white",
        linewidth = 0.2
      ) +
      ggplot2::scale_fill_gradient(
        low = "#deebf7",
        high = "#08519c",
        na.value = "grey90",
        labels = scales::comma_format(big.mark = ".", decimal.mark = ",")
      ) +
      ggplot2::labs(
        title = paste(
          "Immigration aus dem Ausland nach Kanton",
          latest_year_canton
        ),
        fill = "Immigration"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold", size = 14),
        axis.text = ggplot2::element_blank(),
        axis.title = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        panel.grid = ggplot2::element_blank()
      )
  } else {
    bfs_canton_clean <- NULL
    bfs_canton_map_data <- NULL
  }
}

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

# ============================================================
# KAPITEL 9 — OBJEKTORGANISATION UND EXPORT
# ============================================================

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

get_if_exists <- function(name) {
  if (exists(name, inherits = FALSE)) get(name) else NULL
}

imported_data_list <- list(
  swiss_immigration_data = get_if_exists("swiss_immigration_data"),
  swiss_immigration_yearly_data = get_if_exists(
    "swiss_immigration_yearly_data"
  ),
  swiss_world_bank_data = get_if_exists("swiss_world_bank_data"),
  swiss_analysis_data = get_if_exists("swiss_analysis_data"),
  swiss_immigration_enhanced = get_if_exists("swiss_immigration_enhanced"),
  bfs_raw = get_if_exists("bfs_raw"),
  bfs_clean = get_if_exists("bfs_clean"),
  bfs_canton_raw = get_if_exists("bfs_canton_raw"),
  bfs_canton_clean = get_if_exists("bfs_canton_clean"),
  world_bank_raw_data = get_if_exists("world_bank_raw_data")
)
imported_data_list <- imported_data_list[
  !vapply(imported_data_list, is.null, logical(1))
]

plots_list <- list(
  swiss_migration_over_time = get_if_exists("swiss_migration_over_time"),
  swiss_migration_by_country = get_if_exists("swiss_migration_by_country"),
  pie_total_net = get_if_exists("pie_total_net"),
  swiss_migration_world_map = get_if_exists("swiss_migration_world_map"),
  swiss_canton_immigration_map = get_if_exists("swiss_canton_immigration_map"),
  scatter_gdp_migration = get_if_exists("scatter_gdp_migration"),
  hist_net_migration = get_if_exists("hist_net_migration"),
  boxplot_net_migration_quartiles = get_if_exists(
    "boxplot_net_migration_quartiles"
  )
)
plots_list <- plots_list[!vapply(plots_list, is.null, logical(1))]

model_list <- list(
  swiss_migration_model = get_if_exists("swiss_migration_model"),
  swiss_migration_model_controlled = get_if_exists(
    "swiss_migration_model_controlled"
  ),
  swiss_rolling_regression_models = get_if_exists(
    "swiss_rolling_regression_models"
  )
)
model_list <- model_list[!vapply(model_list, is.null, logical(1))]

table_list <- list(
  swiss_regression_table = get_if_exists("swiss_regression_table"),
  countries_df = get_if_exists("countries_df"),
  t_test_summary_table = get_if_exists("t_test_summary_table"),
  rolling_regression_results = get_if_exists("rolling_regression_results"),
  mean_by_year = get_if_exists("mean_by_year")
)
table_list <- table_list[!vapply(table_list, is.null, logical(1))]

session_objects <- list(
  imported_data = imported_data_list,
  plots = plots_list,
  models = model_list,
  tables = table_list
)

saveRDS(session_objects, file = "data/processed/session_objects.rds")

message(
  "Objekte wurden in Listen zusammengefasst und nach data/processed/session_objects.rds gespeichert."
)

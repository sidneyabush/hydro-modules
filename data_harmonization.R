# Build the local CSVs that sit between the raw inputs and the Shiny app.
#
# Put the raw inputs in data/raw_inputs/, then run:
#   Rscript data_harmonization.R data data
#
# You can also pass directories with:
#   HYDRO_MODULES_SOURCE_DATA_DIR
#   HYDRO_MODULES_HARMONIZED_OUTPUT_DIR

rm(list = ls())

if (!require("librarian")) {
  install.packages("librarian")
}
librarian::shelf(dplyr, ggplot2, data.table, lubridate, tidyr, stringr, readr)

args <- commandArgs(trailingOnly = TRUE)

resolve_dir <- function(cli_value, env_var, default = NULL, label) {
  env_value <- Sys.getenv(env_var, unset = "")
  value <- if (!is.na(cli_value) && nzchar(cli_value)) {
    cli_value
  } else if (nzchar(env_value)) {
    env_value
  } else {
    default
  }

  if (is.null(value) || !nzchar(value)) {
    stop(
      paste(
        "Missing", label, "- pass it as a command-line argument or set", env_var
      )
    )
  }

  value
}

find_input_file <- function(search_root, file_name, preferred_rel_paths = character()) {
  candidate_paths <- unique(c(
    file.path(search_root, preferred_rel_paths),
    file.path(search_root, file_name)
  ))
  existing_candidates <- candidate_paths[file.exists(candidate_paths)]

  if (length(existing_candidates) > 0) {
    return(existing_candidates[1])
  }

  pattern <- paste0("^", gsub("\\.", "\\\\.", basename(file_name)), "$")
  discovered <- list.files(
    search_root,
    pattern = pattern,
    recursive = TRUE,
    full.names = TRUE
  )
  discovered <- discovered[file.exists(discovered)]
  if (length(discovered) > 0) {
    discovered <- discovered[order(nchar(discovered))]
    return(discovered[1])
  }

  stop(
    paste(
      "Could not locate required input", file_name,
      "under", normalizePath(search_root, mustWork = FALSE)
    )
  )
}

input_root <- resolve_dir(
  cli_value = args[1],
  env_var = "HYDRO_MODULES_SOURCE_DATA_DIR",
  default = "data",
  label = "source data search directory"
)
output_path <- if (length(args) >= 2 && nzchar(args[2])) {
  args[2]
} else {
  Sys.getenv(
    "HYDRO_MODULES_HARMONIZED_OUTPUT_DIR",
    unset = input_root
  )
}
if (!nzchar(output_path)) {
  output_path <- input_root
}
dir.create(output_path, recursive = TRUE, showWarnings = FALSE)

chem_file <- find_input_file(
  input_root,
  "20260105_masterdata_chem.csv",
  preferred_rel_paths = c(
    "raw_inputs/20260105_masterdata_chem.csv",
    "20260105_masterdata_chem.csv"
  )
)
discharge_file <- find_input_file(
  input_root,
  "20260106_masterdata_discharge.csv",
  preferred_rel_paths = c(
    "raw_inputs/20260106_masterdata_discharge.csv",
    "20260106_masterdata_discharge.csv"
  )
)
kg_file <- find_input_file(
  input_root,
  "Koeppen_Geiger_2.csv",
  preferred_rel_paths = c(
    "raw_inputs/Koeppen_Geiger_2.csv",
    "Koeppen_Geiger_2.csv"
  )
)
spatial_driver_file <- find_input_file(
  input_root,
  "all-data_si-extract_2_20250325.csv",
  preferred_rel_paths = c(
    "raw_inputs/all-data_si-extract_2_20250325.csv",
    "all-data_si-extract_2_20250325.csv"
  )
)
lulc_file <- find_input_file(
  input_root,
  "DSi_LULC_filled_interpolated_Simple.csv",
  preferred_rel_paths = c(
    "raw_inputs/DSi_LULC_filled_interpolated_Simple.csv",
    "DSi_LULC_filled_interpolated_Simple.csv"
  )
)

cat("Searching inputs under:", normalizePath(input_root), "\n")
cat("Writing workflow CSVs to:", normalizePath(output_path, mustWork = FALSE), "\n")
cat("  chem:", chem_file, "\n")
cat("  discharge:", discharge_file, "\n")
cat("  climate:", kg_file, "\n")
cat("  spatial drivers:", spatial_driver_file, "\n")
cat("  LULC:", lulc_file, "\n")

# Standardize the LTER + Stream_Name combo into one join key.
create_stream_id <- function(df) {
  df %>%
    mutate(
      Stream_ID = paste(LTER, Stream_Name, sep = "_"),
      Stream_ID = str_trim(Stream_ID),
      Stream_ID = str_replace_all(Stream_ID, "\\s+", "_")
    )
}

safe_max_numeric <- function(x) {
  if (all(is.na(x))) {
    return(NA_real_)
  }
  max(x, na.rm = TRUE)
}

safe_mean_numeric <- function(x) {
  if (length(x) == 0 || all(is.na(x))) {
    return(NA_real_)
  }
  mean(x, na.rm = TRUE)
}

# Use the climate file to define the North American site pool instead of
# hard-coding network names. That keeps the filter tied to site location.
kg_data <- read.csv(
  kg_file,
  stringsAsFactors = FALSE
) %>%
  create_stream_id() %>%
  select(Stream_ID, ClimateZ, Latitude, Longitude, Name)

north_american_site_ids <- kg_data %>%
  filter(
    !is.na(Longitude),
    !is.na(Latitude),
    between(Longitude, -170, -50),
    between(Latitude, 15, 85)
  ) %>%
  pull(Stream_ID) %>%
  unique()

kg_data <- kg_data %>%
  filter(Stream_ID %in% north_american_site_ids)

spatial_drivers_raw <- read.csv(
  spatial_driver_file,
  stringsAsFactors = FALSE
) %>%
  create_stream_id() %>%
  filter(Stream_ID %in% north_american_site_ids)

cat("North American sites in climate table:", length(north_american_site_ids), "\n")


# --- Load and filter raw data ---------------------------------------------

chem_na <- read.csv(
  chem_file,
  stringsAsFactors = FALSE
) %>%
  create_stream_id() %>%
  filter(Stream_ID %in% north_american_site_ids) %>%
  filter(variable %in% c("Cl", "NO3", "NOx")) %>%
  mutate(variable = if_else(variable == "NOx", "NO3", variable)) %>%
  filter(!is.na(value))

unexpected_chem_units <- chem_na %>%
  filter(variable %in% c("Cl", "NO3")) %>%
  filter(is.na(units) | units != "uM")

if (nrow(unexpected_chem_units) > 0) {
  stop(
    paste(
      "Expected Cl and NO3 in the master chemistry file to be reported in uM.",
      "Unexpected units were found for:",
      paste(sort(unique(unexpected_chem_units$units)), collapse = ", ")
    )
  )
}

chem_na <- chem_na %>%
  mutate(
    value = case_when(
      variable == "Cl" ~ value * 35.453 / 1000,
      variable == "NO3" ~ value * 62.004 / 1000
    )
  )

discharge_na <- read.csv(
  discharge_file,
  stringsAsFactors = FALSE
) %>%
  create_stream_id() %>%
  filter(Stream_ID %in% north_american_site_ids) %>%
  rename(Q = Qcms) %>%
  mutate(Date = as.Date(Date))


# --- Compute discharge metrics --------------------------------------------

# RBI: sum of absolute day-to-day changes / total discharge
# only keep sites with at least a year of data
rbi_results <- discharge_na %>%
  group_by(Stream_ID, LTER, Stream_Name) %>%
  arrange(Date) %>%
  mutate(abs_dQ = abs(Q - lag(Q))) %>%
  filter(!is.na(abs_dQ)) %>%
  summarise(
    n_days = n(),
    total_discharge = sum(Q, na.rm = TRUE),
    total_change = sum(abs_dQ, na.rm = TRUE),
    RBI = total_change / total_discharge,
    .groups = "drop"
  ) %>%
  filter(n_days >= 365)

# RCS: fit log-log regression on recession limbs
# keep only days where flow dropped but not too abruptly (Q_t / Q_t-1 >= 0.7)
Q_diff <- discharge_na %>%
  arrange(Stream_ID, Date) %>%
  group_by(Stream_ID) %>%
  mutate(
    dQ = Q - lag(Q),
    change_dQ = Q / lag(Q),
    dQ_dt = dQ / as.numeric(Date - lag(Date))
  ) %>%
  filter(!is.na(dQ_dt), change_dQ >= 0.7)

recession_data <- Q_diff %>%
  filter(dQ < 0) %>%
  mutate(recession_slope = -dQ_dt) %>%
  filter(is.finite(recession_slope), recession_slope > 0)

recession_slopes <- recession_data %>%
  group_by(Stream_ID, LTER, Stream_Name) %>%
  summarise(
    n_recession_days = n(),
    recession_slope = if (n_recession_days >= 50) {
      tryCatch(
        {
          lm_model <- lm(
            log(recession_slope) ~ log(Q),
            data = pick(everything())
          )
          unname(coef(lm_model)[2])
        },
        error = function(e) NA_real_
      )
    } else {
      NA_real_
    },
    .groups = "drop"
  ) %>%
  filter(!is.na(recession_slope), recession_slope >= 0)

discharge_metrics <- rbi_results %>%
  left_join(
    recession_slopes %>% select(Stream_ID, recession_slope, n_recession_days),
    by = "Stream_ID"
  )


# --- Merge site info + climate + spatial drivers + LULC --------------------

sites_info <- chem_na %>%
  select(Stream_ID, LTER, Stream_Name) %>%
  distinct()

sites_with_discharge <- sites_info %>%
  left_join(discharge_metrics, by = c("Stream_ID", "LTER", "Stream_Name"))

spatial_drivers <- spatial_drivers_raw %>%
  select(
    Stream_ID,
    LTER,
    Stream_Name,
    Shapefile_Name,
    basin_slope_mean_degree,
    basin_slope_median_degree,
    elevation_mean_m,
    elevation_median_m,
    starts_with("precip_"),
    starts_with("temp_"),
    starts_with("evapotrans_"),
    major_land,
    major_rock,
    major_soil
  ) %>%
  rename(
    major_land_spatial = major_land
  )

# Keep the yearly climate columns, then add site-average climate summaries
# alongside them so the app can use either the annual record or the
# condensed site-level version.
yearly_precip_cols <- grep(
  "^precip_[0-9]{4}_mm_per_day$",
  names(spatial_drivers_raw),
  value = TRUE
)
yearly_temp_cols <- grep(
  "^temp_[0-9]{4}_degC$",
  names(spatial_drivers_raw),
  value = TRUE
)
yearly_evapotrans_cols <- grep(
  "^evapotrans_[0-9]{4}_kg_m2$",
  names(spatial_drivers_raw),
  value = TRUE
)
yearly_snow_day_cols <- grep(
  "^snow_[0-9]{4}_num_days$",
  names(spatial_drivers_raw),
  value = TRUE
)
monthly_snow_cover_cols <- grep(
  "^snow_(jan|feb|mar|apr|may|jun|jul|aug|sep|oct|nov|dec)_avg_prop_area$",
  names(spatial_drivers_raw),
  value = TRUE
)

climate_summary_data <- spatial_drivers_raw %>%
  rowwise() %>%
  mutate(
    mean_annual_precip = safe_mean_numeric(c_across(any_of(yearly_precip_cols))) * 365,
    mean_annual_temp = safe_mean_numeric(c_across(any_of(yearly_temp_cols))),
    mean_annual_evapotrans = safe_mean_numeric(c_across(any_of(yearly_evapotrans_cols))),
    mean_snow_days = safe_mean_numeric(c_across(any_of(yearly_snow_day_cols))),
    # MODIS snow-cover fields are stored by month, so keep the site-level
    # annual summary as the simple mean of those monthly proportions.
    snow_cover = if (length(monthly_snow_cover_cols) > 0) {
      safe_mean_numeric(c_across(all_of(monthly_snow_cover_cols)))
    } else {
      mean_snow_days / 365
    },
    mean_snow_prop_area = safe_mean_numeric(
      c_across(matches("snow_[0-9]{4}_max_prop_area"))
    ),
    peak_snow_prop_area = safe_max_numeric(
      c_across(matches("snow_[0-9]{4}_max_prop_area"))
    )
  ) %>%
  ungroup() %>%
  select(
    Stream_ID,
    mean_annual_precip,
    mean_annual_temp,
    mean_annual_evapotrans,
    mean_snow_days,
    snow_cover,
    mean_snow_prop_area,
    peak_snow_prop_area,
    all_of(monthly_snow_cover_cols)
  )

lulc_data <- read.csv(
  lulc_file,
  stringsAsFactors = FALSE
) %>%
  filter(Year >= 2002, Year <= 2022) %>%
  mutate(
    LandClass_sum = if_else(
      is.na(LandClass_sum) | LandClass_sum == 0,
      LandClass_sum,
      LandClass_sum * 100
    )
  ) %>%
  filter(Simple_Class != "Filled_Value") %>%
  pivot_wider(
    names_from = Simple_Class,
    values_from = LandClass_sum,
    names_prefix = "land_"
  )

lulc_avg <- lulc_data %>%
  group_by(Stream_Name) %>%
  summarise(
    across(starts_with("land_"), ~ mean(.x, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  mutate(
    major_land = apply(select(., starts_with("land_")), 1, function(x) {
      if (all(is.na(x))) {
        NA_character_
      } else {
        sub("^land_", "", names(x)[which.max(x)])
      }
    })
  )


# --- Assemble and write outputs -------------------------------------------

harmonized_data <- sites_with_discharge %>%
  left_join(kg_data, by = "Stream_ID") %>%
  left_join(spatial_drivers, by = c("Stream_ID", "LTER", "Stream_Name")) %>%
  left_join(lulc_avg, by = "Stream_Name") %>%
  left_join(climate_summary_data, by = "Stream_ID") %>%
  mutate(
    major_land = coalesce(major_land, major_land_spatial)
  ) %>%
  # drop sites outside North America (catches some Russian GRO sites)
  filter(
    is.na(Longitude) | (Longitude >= -170 & Longitude <= -50),
    is.na(Latitude) | (Latitude >= 15 & Latitude <= 85)
  )

write.csv(
  harmonized_data,
  file.path(output_path, "harmonized_north_america_partial.csv"),
  row.names = FALSE
)

# complete cases: RBI, RCS, climate zone, precip, snow, land cover
complete_cases <- harmonized_data %>%
  filter(
    !is.na(RBI),
    !is.na(recession_slope),
    !is.na(ClimateZ),
    !is.na(mean_annual_precip),
    !is.na(snow_cover),
    !is.na(major_land)
  )

write.csv(
  complete_cases,
  file.path(output_path, "harmonized_north_america_complete.csv"),
  row.names = FALSE
)


# --- Pre-filter discharge for the app ----------------------------------------
# The raw discharge file is ~920 MB. Save just the NA-filtered subset so the
# app doesn't have to load the full thing on every startup.

discharge_na_export <- discharge_na %>%
  rename(Qcms = Q) %>%
  select(Qcms, Date, LTER, Stream_Name, Stream_ID)

write.csv(
  discharge_na_export,
  file.path(output_path, "discharge_north_america.csv"),
  row.names = FALSE
)


# --- Pre-compute Cl summaries for Activity 2 --------------------------------
# Avoids loading the full 345 MB chem file at app startup

cl_data <- chem_na %>%
  filter(variable == "Cl", !is.na(value)) %>%
  mutate(date = as.Date(date), month = month(date))

# site-level summary: mean, median, observation count
cl_site_stats <- cl_data %>%
  group_by(Stream_ID, LTER, Stream_Name) %>%
  summarise(
    mean_Cl_mgL = mean(value, na.rm = TRUE),
    median_Cl_mgL = median(value, na.rm = TRUE),
    n_obs = n(),
    .groups = "drop"
  )

# monthly averages per site (for seasonal plot)
cl_monthly <- cl_data %>%
  group_by(Stream_ID, LTER, Stream_Name, month) %>%
  summarise(
    mean_Cl_mgL = mean(value, na.rm = TRUE),
    n_obs = n(),
    .groups = "drop"
  )

write.csv(
  cl_monthly,
  file.path(output_path, "cl_monthly_summary.csv"),
  row.names = FALSE
)

# overwrite the earlier partial export with the chloride-enriched version that
# Activity 2 uses for mapping and site selection
harmonized_with_cl <- harmonized_data %>%
  left_join(
    cl_site_stats %>%
      select(Stream_ID, mean_Cl_mgL, median_Cl_mgL, n_cl_obs = n_obs),
    by = "Stream_ID"
  )

write.csv(
  harmonized_with_cl,
  file.path(output_path, "harmonized_north_america_partial.csv"),
  row.names = FALSE
)


# --- Pre-compute C-Q paired observations and slopes for Activity 3 ----------
# Pairs same-day chemistry + discharge for Cl and NO3, then fits
# log-log regressions to get C-Q slopes per site×solute.

cq_solutes <- c("Cl", "NO3")

# paired observations: inner join chem + discharge on Stream_ID + date
cq_chem <- chem_na %>%
  filter(variable %in% cq_solutes, !is.na(value), value > 0) %>%
  mutate(date = as.Date(date)) %>%
  select(Stream_ID, LTER, Stream_Name, date, variable, value)

# average duplicate discharge dates per site before joining
cq_discharge <- discharge_na %>%
  filter(Q > 0) %>%
  group_by(Stream_ID, Date) %>%
  summarise(Q = mean(Q), .groups = "drop") %>%
  rename(date = Date)

cq_paired <- cq_chem %>%
  inner_join(cq_discharge, by = c("Stream_ID", "date"))

write.csv(
  cq_paired,
  file.path(output_path, "cq_paired_obs.csv"),
  row.names = FALSE
)


# slopes: log-log regression per site×solute (min 10 observations)
fit_cq_slope <- function(df) {
  empty <- data.frame(
    n_paired_obs = integer(0),
    cq_slope = numeric(0),
    r_squared = numeric(0)
  )
  if (nrow(df) < 10) {
    return(empty)
  }
  mod <- tryCatch(
    lm(log10(value) ~ log10(Q), data = df),
    error = function(e) NULL
  )
  if (is.null(mod) || length(coef(mod)) < 2) {
    return(empty)
  }
  data.frame(
    n_paired_obs = nrow(df),
    cq_slope = unname(coef(mod)[2]),
    r_squared = summary(mod)$r.squared
  )
}

cq_slopes <- cq_paired %>%
  group_by(Stream_ID, LTER, Stream_Name, variable) %>%
  group_modify(~ fit_cq_slope(.x)) %>%
  ungroup()

write.csv(cq_slopes, file.path(output_path, "cq_slopes.csv"), row.names = FALSE)

cat(
  "\nDone! Wrote workflow CSVs to",
  normalizePath(output_path),
  "\n",
  " - harmonized_north_america_partial.csv\n",
  " - harmonized_north_america_complete.csv\n",
  " - discharge_north_america.csv\n",
  " - cl_monthly_summary.csv\n",
  " - cq_paired_obs.csv\n",
  " - cq_slopes.csv\n",
  sep = ""
)

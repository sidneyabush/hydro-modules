# Turn the harmonized CSVs into the smaller .rds files that the app reads.
#
# Run this after data_harmonization.R:
#   Rscript prep_data.R data data
#
# Or set:
#   HYDRO_MODULES_SOURCE_DATA_DIR
#   HYDRO_MODULES_APP_DATA_DIR

suppressPackageStartupMessages({
  library(dplyr)
  library(data.table)
  library(terra)
})

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

assert_required_files <- function(base_dir, relative_paths, label) {
  missing <- relative_paths[!file.exists(file.path(base_dir, relative_paths))]
  if (length(missing) > 0) {
    stop(
      paste(
        "Missing", label, "in", normalizePath(base_dir, mustWork = FALSE), ":",
        paste(missing, collapse = ", ")
      )
    )
  }
}

find_first_existing <- function(candidates, label) {
  match <- candidates[file.exists(candidates)][1]
  if (length(match) == 0 || is.na(match)) {
    stop(
      paste(
        "Missing", label, "- looked for:",
        paste(normalizePath(candidates, mustWork = FALSE), collapse = ", ")
      )
    )
  }
  match
}

write_activity2_precip_raster <- function(source_file, output_file, aggregate_fact = 8) {
  north_america_extent <- ext(-179, -50, 5, 85)
  output_template <- rast(
    north_america_extent,
    resolution = 0.05,
    crs = "EPSG:4326"
  )
  precip_raster <- rast(source_file)

  if (aggregate_fact > 1) {
    precip_raster <- aggregate(
      precip_raster,
      fact = aggregate_fact,
      fun = mean,
      na.rm = TRUE
    )
  }

  precip_raster <- precip_raster %>%
    project(output_template, method = "bilinear") %>%
    crop(north_america_extent)

  names(precip_raster) <- "map_mm"
  writeRaster(precip_raster, output_file, overwrite = TRUE)
}

write_activity2_percent_cover_raster <- function(
  source_file,
  output_file,
  class_code,
  aggregate_fact = 200
) {
  north_america_extent <- ext(-179, -50, 5, 85)
  output_template <- rast(
    north_america_extent,
    resolution = 0.05,
    crs = "EPSG:4326"
  )
  land_cover_raster <- rast(source_file)
  mask_file <- tempfile("activity2_mask_", fileext = ".tif")
  aggregate_file <- tempfile("activity2_aggregate_", fileext = ".tif")
  on.exit(unlink(c(mask_file, aggregate_file), force = TRUE), add = TRUE)

  # Build the binary class mask on disk first. That avoids carrying the
  # full 30 m continental raster in memory through every later step.
  class_mask <- ifel(land_cover_raster == class_code, 1, 0)
  writeRaster(
    class_mask,
    mask_file,
    overwrite = TRUE,
    wopt = list(datatype = "INT1U", gdal = c("COMPRESS=LZW"))
  )

  # Aggregate counts of class pixels per coarse cell, then convert to percent.
  class_hits <- aggregate(
    rast(mask_file),
    fact = aggregate_fact,
    fun = sum,
    na.rm = TRUE,
    filename = aggregate_file,
    overwrite = TRUE,
    wopt = list(datatype = "FLT4S", gdal = c("COMPRESS=LZW"))
  )

  percent_cover <- (class_hits / (aggregate_fact^2)) * 100
  percent_cover <- project(percent_cover, output_template, method = "bilinear")

  names(percent_cover) <- "percent_cover"
  writeRaster(
    percent_cover,
    output_file,
    overwrite = TRUE,
    wopt = list(datatype = "FLT4S", gdal = c("COMPRESS=LZW"))
  )
}

raw_path <- resolve_dir(
  cli_value = args[1],
  env_var = "HYDRO_MODULES_SOURCE_DATA_DIR",
  default = "data",
  label = "source data directory"
)
out_path <- resolve_dir(
  cli_value = args[2],
  env_var = "HYDRO_MODULES_APP_DATA_DIR",
  default = "data",
  label = "app data output directory"
)

required_inputs <- c(
  "harmonized_north_america_complete.csv",
  "harmonized_north_america_partial.csv",
  "discharge_north_america.csv",
  "cl_monthly_summary.csv",
  "cq_paired_obs.csv",
  "cq_slopes.csv"
)

assert_required_files(raw_path, required_inputs, "prep_data inputs")
dir.create(out_path, showWarnings = FALSE, recursive = TRUE)

raw_input_path <- file.path(raw_path, "raw_inputs")
precip_raster_input <- file.path(raw_input_path, "na_1981_2010_annual_precip.tif")
land_cover_raster_input <- find_first_existing(
  c(
    file.path(raw_input_path, "land_cover_2020v2_30m.tif"),
    file.path(raw_input_path, "na_land_cover_2020v2_30m.tif"),
    file.path(raw_input_path, "land_cover_2020_30m.tif")
  ),
  "North America 2020 land-cover raster"
)

cat("Reading harmonized CSVs from:", normalizePath(raw_path), "\n")
cat("Writing Shiny-ready RDS files to:", normalizePath(out_path, mustWork = FALSE), "\n")

# --- 1. Harmonized site data (complete + partial) ---
complete <- read.csv(
  file.path(raw_path, "harmonized_north_america_complete.csv"),
  stringsAsFactors = FALSE
)
partial <- read.csv(
  file.path(raw_path, "harmonized_north_america_partial.csv"),
  stringsAsFactors = FALSE
)

saveRDS(complete, file.path(out_path, "harmonized_complete.rds"))
saveRDS(partial, file.path(out_path, "harmonized_partial.rds"))
cat("harmonized:", nrow(complete), "complete,", nrow(partial), "partial\n")

# --- 2. Discharge — keep sites that appear anywhere in the partial table ---
keep_sites <- unique(partial$Stream_ID)
discharge <- fread(file.path(raw_path, "discharge_north_america.csv")) %>%
  as.data.frame() %>%
  filter(Stream_ID %in% keep_sites) %>%
  mutate(Date = as.Date(Date)) %>%
  select(Stream_ID, Stream_Name, LTER, Date, Qcms)

saveRDS(discharge, file.path(out_path, "discharge.rds"))
cat(
  "discharge:",
  nrow(discharge),
  "rows,",
  length(unique(discharge$Stream_ID)),
  "sites\n"
)

# --- 3. Cl monthly summary ---
cl_monthly <- read.csv(
  file.path(raw_path, "cl_monthly_summary.csv"),
  stringsAsFactors = FALSE
)
saveRDS(cl_monthly, file.path(out_path, "cl_monthly.rds"))
cat("cl_monthly:", nrow(cl_monthly), "rows\n")

# --- 4. CQ paired observations — Cl and NO3 only ---
cq_paired <- read.csv(
  file.path(raw_path, "cq_paired_obs.csv"),
  stringsAsFactors = FALSE
) %>%
  filter(variable %in% c("Cl", "NO3")) %>%
  mutate(date = as.Date(date))

saveRDS(cq_paired, file.path(out_path, "cq_paired.rds"))
cat("cq_paired:", nrow(cq_paired), "rows\n")

# --- 5. CQ slopes — Cl and NO3 only ---
cq_slopes <- read.csv(
  file.path(raw_path, "cq_slopes.csv"),
  stringsAsFactors = FALSE
) %>%
  filter(variable %in% c("Cl", "NO3"))

saveRDS(cq_slopes, file.path(out_path, "cq_slopes.rds"))
cat("cq_slopes:", nrow(cq_slopes), "rows\n")

# --- 6. Activity 2 background rasters ---
write_activity2_precip_raster(
  source_file = precip_raster_input,
  output_file = file.path(out_path, "activity2_map_precip_mm.tif"),
  aggregate_fact = 4
)
cat("activity2_map_precip_mm.tif written\n")

write_activity2_percent_cover_raster(
  source_file = land_cover_raster_input,
  output_file = file.path(out_path, "activity2_map_cropland_pct.tif"),
  class_code = 15,
  aggregate_fact = 160
)
cat("activity2_map_cropland_pct.tif written\n")

write_activity2_percent_cover_raster(
  source_file = land_cover_raster_input,
  output_file = file.path(out_path, "activity2_map_impervious_pct.tif"),
  class_code = 17,
  aggregate_fact = 160
)
cat("activity2_map_impervious_pct.tif written\n")

cat("\nDone! All .rds files written to", normalizePath(out_path), "\n")

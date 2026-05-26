if (!requireNamespace("rsconnect", quietly = TRUE)) {
  stop("Install the rsconnect package before deploying.")
}

app_name <- "hydro-modules"
app_title <- "HydroViz"
account_name <- "sidneyabush"

bundle_files <- unique(c(
  "app.R",
  "data/harmonized_complete.rds",
  "data/harmonized_partial.rds",
  "data/discharge.rds",
  "data/cl_monthly.rds",
  "data/cq_paired.rds",
  "data/cq_slopes.rds",
  "data/activity2_map_precip_mm.tif",
  "data/activity2_map_cropland_pct.tif",
  "data/activity2_map_impervious_pct.tif",
  list.files("www", recursive = TRUE, full.names = TRUE)
))

missing_files <- bundle_files[!file.exists(bundle_files)]
if (length(missing_files) > 0) {
  stop(
    paste(
      "These deployment files are missing:",
      paste(missing_files, collapse = ", ")
    )
  )
}

bundle_size_mb <- sum(file.info(bundle_files)$size, na.rm = TRUE) / 1024^2
message(
  sprintf(
    "Deploying %d files (%.1f MB) to shinyapps.io.",
    length(bundle_files),
    bundle_size_mb
  )
)

rsconnect::deployApp(
  appDir = ".",
  appFiles = bundle_files,
  appName = app_name,
  appTitle = app_title,
  account = account_name,
  server = "shinyapps.io",
  launch.browser = FALSE
)

# Stream Hydrology Teaching Module
#
# Interactive app for exploring hydrology metrics (RBI, recession slope)
# across North American LTER and USGS sites. Built for CUAHSI workshops.
#
# The app expects pre-processed .rds files produced by prep_data.R.

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(dplyr)
  library(ggplot2)
  library(leaflet)
  library(leaflet.extras)
  library(plotly)
  library(terra)
  library(viridis)
})

# App data lives in the repo by default, but can be overridden for local testing
# or deployment with HYDRO_MODULES_APP_DATA_DIR.
data_path <- Sys.getenv("HYDRO_MODULES_APP_DATA_DIR", unset = "data")

# shared palette — keeps colors consistent between the map, plots, and UI
module_colors <- c(
  "primary" = "#2f6c8f",
  "secondary" = "#5a86a6",
  "success" = "#7a9e63",
  "danger" = "#c98066",
  "warning" = "#d5b066"
)

precip_palette <- c(
  "#eef7fd",
  "#d9ecf7",
  "#bfdcf0",
  "#9fc9e4",
  "#7db4d7",
  "#5d9fc8",
  "#3f88b5",
  "#2f729b"
)

activity2_precip_raster_breaks <- c(
  0, 400, 600, 1000, 1500, 2000, 2500, 3000, 4000, 5000, Inf
)

activity2_precip_raster_colors <- c(
  "#a55f47",
  "#c27f4e",
  "#d9a25b",
  "#ebc96a",
  "#bfd169",
  "#83b865",
  "#4f9b6b",
  "#3f8d7d",
  "#3b7692",
  "#315d86"
)

activity2_precip_raster_labels <- c(
  "0 - 400",
  "400 - 600",
  "600 - 1,000",
  "1,000 - 1,500",
  "1,500 - 2,000",
  "2,000 - 2,500",
  "2,500 - 3,000",
  "3,000 - 4,000",
  "4,000 - 5,000",
  "> 5,000"
)

activity2_cropland_breaks <- c(0.5, 2, 10, 25, 50, Inf)
activity2_cropland_colors <- c(
  "#f1e6b2",
  "#ddc56d",
  "#bf9c3d",
  "#946f22",
  "#654612"
)
activity2_cropland_labels <- c(
  "0.5 - 2",
  "2 - 10",
  "10 - 25",
  "25 - 50",
  "> 50"
)

activity2_impervious_breaks <- c(0.1, 5, 10, 50, Inf)
activity2_impervious_colors <- c(
  "#dfe5e8",
  "#7f8c96",
  "#66727c",
  "#353d44"
)
activity2_impervious_labels <- c(
  "< 5",
  "5 - 10",
  "10 - 50",
  "> 50"
)

snow_palette <- c(
  "#ffffff",
  "#f6fbff",
  "#edf6fd",
  "#e0f0fb",
  "#d0e7f7",
  "#bddcf0",
  "#a5cee5",
  "#88bdd7"
)

land_use_colors <- c(
  "Cropland" = "#d7b63f",
  "Grassland / Shrubland" = "#9fc86b",
  "Forest" = "#2f6b3b",
  "Wetland / Marsh" = "#6f90a8",
  "Tidal Wetland" = "#58748e",
  "Impervious" = "#c3c8cc",
  "Bare" = "#ffffff",
  "Water" = "#3f73b5",
  "Salt Water" = "#7da7d6",
  "Ice / Snow" = "#e7f2f8",
  "Tundra" = "#b9c7a0",
  "Other / Unclassified" = "#b9b3ab"
)

climate_zone_colors <- c(
  "Arid" = "#c77b63",
  "Semi-Arid" = "#d8a160",
  "Mediterranean" = "#b8a55d",
  "Humid Subtropical" = "#6f996f",
  "Tropical" = "#4f8c7e",
  "Humid Continental" = "#6f93b1",
  "Subarctic" = "#5b78a0",
  "Tundra" = "#8fb3c2"
)

activity2_cl_accent <- "#975379"
activity2_q_accent <- "#7f878d"
activity3_no3_accent <- "#355c8a"
chloride_excluded_stream_names <- c("DMF Brazos River")

solute_colors <- c(
  "Cl" = activity2_cl_accent,
  "NO3" = activity3_no3_accent
)

cq_solute_shade_palettes <- list(
  Cl = c("#c7a3b6", activity2_cl_accent, "#6e3456"),
  NO3 = c("#8ca8c4", activity3_no3_accent, "#1e416c")
)

select_cq_site_colors <- function(solute, n_sites) {
  palette <- cq_solute_shade_palettes[[solute]]
  if (is.null(palette) || n_sites <= 0) {
    return(character(0))
  }

  palette_indices <- switch(
    as.character(min(n_sites, 3)),
    "1" = 2L,
    "2" = c(1L, 3L),
    "3" = c(1L, 2L, 3L)
  )

  palette[palette_indices]
}

# these get reused in multiple ggplots, so pulling them out here
base_plot_theme <- theme_minimal(base_family = "Work Sans") +
  theme(
    plot.background = element_rect(fill = "#fcfbf7", color = NA),
    panel.background = element_rect(fill = "#ffffff", color = NA),
    panel.grid.major = element_line(color = "#d7e3ea", linewidth = 0.32),
    panel.grid.minor = element_line(color = "#e7eef3", linewidth = 0.18),
    text = element_text(color = "#24323d"),
    axis.text = element_text(color = "#31424c"),
    axis.title = element_text(face = "plain")
  )

plotly_bg <- list(paper_bgcolor = "#fcfbf7", plot_bgcolor = "#ffffff")
plotly_modebar_remove <- c(
  "select2d",
  "lasso2d",
  "hoverCompareCartesian",
  "hoverClosestCartesian",
  "autoScale2d",
  "toggleSpikelines"
)

polish_plotly <- function(p, register_click = FALSE) {
  if (isTRUE(register_click)) {
    p <- event_register(p, "plotly_click")
  }

  p %>%
    layout(
      font = list(family = "Work Sans, sans-serif", color = "#24323d"),
      hoverlabel = list(
        bgcolor = "rgba(255,255,255,0.98)",
        bordercolor = "#bfd0db",
        font = list(color = "#24323d", size = 12)
      )
    ) %>%
    config(
      displaylogo = FALSE,
      responsive = TRUE,
      scrollZoom = FALSE,
      modeBarButtonsToRemove = plotly_modebar_remove
    )
}

right_side_legend <- function(font_size = 11) {
  list(
    orientation = "v",
    x = 1.02,
    y = 1,
    xanchor = "left",
    yanchor = "top",
    bgcolor = "rgba(255,255,255,0.88)",
    bordercolor = "#d4e3f0",
    borderwidth = 1,
    font = list(size = font_size, color = "#24323d")
  )
}

# keep the app startup failure explicit if a required data product is missing
required_data_files <- c(
  "harmonized_complete.rds",
  "harmonized_partial.rds",
  "discharge.rds",
  "cl_monthly.rds",
  "cq_paired.rds",
  "cq_slopes.rds"
)
missing_data_files <- required_data_files[
  !file.exists(file.path(data_path, required_data_files))
]

if (length(missing_data_files) > 0) {
  stop(
    paste(
      "Missing app data files in", normalizePath(data_path, mustWork = FALSE), ":",
      paste(missing_data_files, collapse = ", "),
      "\nRun prep_data.R first or set HYDRO_MODULES_APP_DATA_DIR."
    )
  )
}

read_app_data <- function(file_name) {
  readRDS(file.path(data_path, file_name))
}

build_monthly_discharge <- function(discharge_df) {
  discharge_df %>%
    mutate(month = as.integer(format(Date, "%m"))) %>%
    group_by(Stream_ID, Stream_Name, LTER, month) %>%
    summarise(mean_Q_cms = mean(Qcms, na.rm = TRUE), .groups = "drop")
}

month_labels <- c(
  "Jan",
  "Feb",
  "Mar",
  "Apr",
  "May",
  "Jun",
  "Jul",
  "Aug",
  "Sep",
  "Oct",
  "Nov",
  "Dec"
)
month_keys <- tolower(month_labels)
days_in_month <- c(
  "jan" = 31,
  "feb" = 28,
  "mar" = 31,
  "apr" = 30,
  "may" = 31,
  "jun" = 30,
  "jul" = 31,
  "aug" = 31,
  "sep" = 30,
  "oct" = 31,
  "nov" = 30,
  "dec" = 31
)

extract_monthly_site_values <- function(site_row, prefix, suffix) {
  vapply(
    month_keys,
    function(key) {
      col_name <- paste0(prefix, key, suffix)
      if (col_name %in% names(site_row)) {
        as.numeric(site_row[[col_name]][1])
      } else {
        NA_real_
      }
    },
    numeric(1)
  )
}

clean_land_use_label <- function(x) {
  x <- trimws(as.character(x))
  x <- gsub("^land_", "", x)

  dplyr::case_when(
    is.na(x) | x == "" ~ "Other / Unclassified",
    x %in% c(
      "deciduous_broadleaf_forest",
      "evergreen_needleleaf_forest",
      "mixed_forest",
      "evergreen_broadleaf_forest",
      "deciduous_needleleaf_forest",
      "Forest"
    ) ~ "Forest",
    x %in% c("shrubland_grassland", "Grassland_Shrubland") ~ "Grassland / Shrubland",
    x %in% c("cropland", "Cropland") ~ "Cropland",
    x %in% c("urban_and_built_up_land", "Impervious") ~ "Impervious",
    x %in% c("wetland", "Wetland_Marsh") ~ "Wetland / Marsh",
    x %in% c("Tidal_Wetland") ~ "Tidal Wetland",
    x %in% c("Water") ~ "Water",
    x %in% c("Salt_Water") ~ "Salt Water",
    x %in% c("Ice_Snow", "tundra") ~ "Ice / Snow",
    x %in% c("Bare", "barren_or_sparsely_vegetated") ~ "Bare",
    TRUE ~ tools::toTitleCase(gsub("_", " ", x))
  )
}

named_color_lookup <- function(values, palette, default = "#b9b3ab") {
  vapply(
    as.character(values),
    function(value) {
      if (!is.na(value) && value %in% names(palette)) {
        unname(palette[[value]])
      } else {
        default
      }
    },
    character(1)
  )
}

land_use_legend_order <- c(
  "Cropland",
  "Grassland / Shrubland",
  "Forest",
  "Wetland / Marsh",
  "Tidal Wetland",
  "Impervious",
  "Bare",
  "Water",
  "Salt Water",
  "Ice / Snow",
  "Tundra",
  "Other / Unclassified"
)

land_use_legend_levels <- function(values) {
  present_levels <- unique(as.character(values[!is.na(values)]))
  c(
    intersect(land_use_legend_order, present_levels),
    sort(setdiff(present_levels, land_use_legend_order))
  )
}

load_activity2_raster <- function(file_path) {
  if (!file.exists(file_path)) {
    return(NULL)
  }
  raster_layer <- terra::rast(file_path)

  # Keep Activity 2 focused on North America so the map opens on the
  # teaching region instead of the full raster extent.
  north_america_extent <- terra::ext(-179, -50, 5, 85)
  terra::crop(raster_layer, north_america_extent)
}

activity2_map_bounds <- list(
  xmin = -179,
  ymin = 5,
  xmax = -50,
  ymax = 85
)

activity2_background_focus_bounds <- list(
  "map" = activity2_map_bounds,
  "cropland" = list(
    xmin = -135,
    ymin = 15,
    xmax = -60,
    ymax = 63
  ),
  "impervious" = list(
    xmin = -135,
    ymin = 15,
    xmax = -60,
    ymax = 63
  )
)

# load the largest data object once at startup; the rest are read on demand
discharge_global <- read_app_data("discharge.rds")

activity2_background_specs <- list(
  "map" = list(
    label = "MAP (mm)",
    file_name = "activity2_map_precip_mm.tif",
    fallback_file = file.path("raw_inputs", "na_1981_2010_annual_precip.tif"),
    colors = activity2_precip_raster_colors,
    breaks = activity2_precip_raster_breaks,
    labels = activity2_precip_raster_labels
  ),
  "cropland" = list(
    label = "% Cropland",
    file_name = "activity2_map_cropland_pct.tif",
    colors = activity2_cropland_colors,
    breaks = activity2_cropland_breaks,
    labels = activity2_cropland_labels
  ),
  "impervious" = list(
    label = "% Impervious",
    file_name = "activity2_map_impervious_pct.tif",
    colors = activity2_impervious_colors,
    breaks = activity2_impervious_breaks,
    labels = activity2_impervious_labels
  )
)

load_activity2_background_rasters <- function(base_dir, specs) {
  lapply(specs, function(spec) {
    primary_path <- file.path(base_dir, spec$file_name)
    raster_layer <- load_activity2_raster(primary_path)

    if (is.null(raster_layer) && !is.null(spec$fallback_file)) {
      raster_layer <- load_activity2_raster(file.path(base_dir, spec$fallback_file))
    }

    raster_layer
  })
}

activity2_background_rasters_global <- load_activity2_background_rasters(
  base_dir = data_path,
  specs = activity2_background_specs
)

activity2_background_choices <- setNames(
  names(activity2_background_specs)[
    vapply(activity2_background_rasters_global, Negate(is.null), logical(1))
  ],
  vapply(
    activity2_background_specs[
      names(activity2_background_specs)[
        vapply(activity2_background_rasters_global, Negate(is.null), logical(1))
      ]
    ],
    `[[`,
    character(1),
    "label"
  )
)


# --- UI -------------------------------------------------------------------

ui <- page_navbar(
  title = tags$div(
    class = "app-title-block",
    tags$span("Hydro Modules", class = "app-title-kicker"),
    tags$span("Stream Hydrology Teaching Module", class = "app-title-main")
  ),
  theme = bs_theme(
    base_font = font_google("Work Sans", wght = "400..700"),
    heading_font = font_google("Work Sans", wght = "500..700"),
    bg = "#fcfbf7",
    fg = "#24323d",
    navbar_bg = "#ffffff",
    navbar_fg = "#24323d",
    primary = module_colors[["primary"]],
    secondary = module_colors[["secondary"]],
    success = module_colors[["success"]],
    danger = module_colors[["danger"]],
    "card-bg" = "#ffffff",
    "card-border-color" = "#d7e3ea"
  ),

  header = tags$head(
    tags$style(HTML(
      "
      :root {
        --hydro-paper: #f3f7fa;
        --hydro-canvas: #fcfbf7;
        --hydro-card: rgba(255,255,255,0.94);
        --hydro-card-strong: #ffffff;
        --hydro-ink: #24323d;
        --hydro-muted: #5d6d76;
        --hydro-line: #d7e3ea;
        --hydro-line-strong: #bfd0db;
        --hydro-blue: #2f6c8f;
        --hydro-blue-soft: rgba(47,108,143,0.14);
        --hydro-green-soft: rgba(122,158,99,0.13);
        --hydro-sand-soft: rgba(213,176,102,0.14);
        --hydro-shadow: 0 18px 42px rgba(53,79,92,0.08);
        --hydro-shadow-strong: 0 22px 48px rgba(53,79,92,0.12);
      }

      body {
        background:
          radial-gradient(circle at 8% 2%, rgba(47,108,143,0.15), transparent 28%),
          radial-gradient(circle at 88% 10%, rgba(122,158,99,0.13), transparent 26%),
          linear-gradient(180deg, var(--hydro-paper) 0%, #f8f5ee 48%, var(--hydro-canvas) 100%) !important;
        font-family: 'Work Sans', sans-serif !important;
        color: var(--hydro-ink) !important;
        min-height: 100vh;
      }

      body::before {
        content: '';
        position: fixed;
        right: 4%;
        bottom: 7%;
        width: 22rem;
        height: 22rem;
        background: radial-gradient(circle, var(--hydro-sand-soft), transparent 68%);
        filter: blur(10px);
        pointer-events: none;
        z-index: 0;
      }

      .bslib-page-navbar,
      .container-fluid,
      .navbar,
      .main {
        position: relative;
        z-index: 1;
      }

      #map, .leaflet-container {
        background: #f7fafc !important;
      }

      .card {
        border: 1px solid var(--hydro-line) !important;
        box-shadow: var(--hydro-shadow) !important;
        border-radius: 18px !important;
        background: var(--hydro-card) !important;
        overflow: hidden !important;
        backdrop-filter: blur(10px);
      }

      .card:hover {
        transform: translateY(-2px);
        box-shadow: var(--hydro-shadow-strong) !important;
      }

      .card-header {
        background: linear-gradient(180deg, rgba(243,248,251,0.98), rgba(236,244,248,0.94)) !important;
        border-bottom: 1px solid var(--hydro-line) !important;
        color: var(--hydro-ink) !important;
        font-weight: 700 !important;
        letter-spacing: 0.01em;
        border-radius: 18px 18px 0 0 !important;
      }

      .bslib-value-box {
        border: 1px solid var(--hydro-line) !important;
        box-shadow: var(--hydro-shadow) !important;
        border-radius: 18px !important;
        background: var(--hydro-card-strong) !important;
      }

      .sidebar {
        background: rgba(255,255,255,0.9) !important;
        border: 1px solid var(--hydro-line) !important;
        box-shadow: var(--hydro-shadow) !important;
        border-radius: 18px !important;
        backdrop-filter: blur(8px);
      }

      @media (min-width: 992px) {
        .sidebar {
          position: sticky;
          top: 5.4rem;
          max-height: calc(100vh - 7rem);
          overflow-y: auto;
        }
      }

      .navbar {
        box-shadow: 0 10px 28px rgba(53,79,92,0.08) !important;
        background: rgba(255,255,255,0.84) !important;
        border-bottom: 1px solid rgba(215,227,234,0.9) !important;
        backdrop-filter: blur(14px);
      }

      .app-title-block {
        display: flex;
        flex-direction: column;
        gap: 0.05rem;
        line-height: 1.02;
      }

      .app-title-kicker {
        font-size: 0.72rem;
        text-transform: uppercase;
        letter-spacing: 0.18em;
        font-weight: 700;
        color: #5f8da9;
      }

      .app-title-main {
        font-size: 1.14rem;
        font-weight: 700;
        letter-spacing: -0.01em;
        color: var(--hydro-ink);
      }

      .navbar-brand {
        padding-top: 0.25rem;
        padding-bottom: 0.25rem;
      }

      .navbar-nav .nav-link {
        border-radius: 999px;
        padding: 0.58rem 0.95rem !important;
        margin: 0 0.12rem;
        font-weight: 600;
        color: #52636c !important;
        transition: all 0.24s ease;
      }

      .navbar-nav .nav-link:hover {
        background: rgba(47,108,143,0.1) !important;
        color: #24465b !important;
        transform: translateY(-1px);
      }

      .navbar-nav .nav-link.active {
        color: #24465b !important;
        background: linear-gradient(135deg, rgba(47,108,143,0.18), rgba(47,108,143,0.08)) !important;
        border-bottom: none !important;
        box-shadow: inset 0 0 0 1px rgba(47,108,143,0.2);
      }

      .nav-tabs {
        gap: 0.45rem;
        padding: 0.35rem 0.35rem 0.15rem;
        border-bottom: none !important;
      }

      .nav-tabs .nav-link {
        border: none !important;
        border-radius: 999px !important;
        padding: 0.55rem 0.95rem !important;
        background: rgba(243,248,251,0.92);
        color: #5b6b74;
        font-weight: 600;
      }

      .nav-tabs .nav-link.active {
        background: linear-gradient(135deg, #2f6c8f, #5a86a6) !important;
        color: #ffffff !important;
        box-shadow: 0 10px 22px rgba(47,108,143,0.18);
      }

      .form-label,
      .control-label,
      .sidebar h4 {
        font-weight: 700 !important;
        color: #344650 !important;
        letter-spacing: 0.01em;
      }

      .card p,
      .card li {
        line-height: 1.66;
      }

      .sidebar p {
        font-size: 0.93rem !important;
        line-height: 1.6 !important;
        color: #55656e !important;
      }

      .form-select,
      .form-control,
      .selectize-input,
      .selectize-dropdown {
        border-radius: 12px !important;
      }

      .form-select,
      .form-control,
      .selectize-input {
        border: 1px solid var(--hydro-line-strong) !important;
        box-shadow: none !important;
        background: #ffffff !important;
        min-height: 46px;
      }

      .form-select:focus,
      .form-control:focus,
      .selectize-input.focus {
        border-color: #7ea8c4 !important;
        box-shadow: 0 0 0 0.22rem rgba(47,108,143,0.16) !important;
      }

      .selectize-dropdown {
        border: 1px solid var(--hydro-line) !important;
        box-shadow: 0 18px 32px rgba(53,79,92,0.12) !important;
      }

      .btn-primary {
        background: linear-gradient(135deg, #2f6c8f, #5a86a6) !important;
        border: none !important;
        border-radius: 999px !important;
        font-weight: 600 !important;
        padding: 0.55rem 1rem !important;
        box-shadow: 0 10px 24px rgba(47,108,143,0.22);
      }

      .btn-outline-secondary {
        border-radius: 999px !important;
        border-color: var(--hydro-line-strong) !important;
        color: #405560 !important;
        background: #ffffff !important;
      }

      .btn-outline-secondary:hover {
        background: #eef5f8 !important;
        color: var(--hydro-ink) !important;
        border-color: #9fb8c8 !important;
      }

      .leaflet-container,
      .html-widget,
      .js-plotly-plot {
        border-radius: 14px !important;
      }

      .leaflet-container {
        background: #edf2f4 !important;
        box-shadow: inset 0 0 0 1px rgba(215,227,234,0.75);
      }

      .leaflet-control-zoom a,
      .leaflet-bar a {
        border-radius: 10px !important;
        border: none !important;
        color: #264557 !important;
        background: #ffffff !important;
        box-shadow: 0 8px 18px rgba(53,79,92,0.12);
      }

      .leaflet-control-attribution {
        background: rgba(255,255,255,0.88) !important;
        border-radius: 10px 0 0 0;
      }

      .custom-legend {
        background: rgba(255,255,255,0.94);
        border: 1px solid var(--hydro-line);
        border-radius: 14px;
        box-shadow: 0 12px 28px rgba(53,79,92,0.14);
        padding: 0.8rem 0.9rem;
        color: var(--hydro-ink);
        max-width: 168px;
      }

      .custom-legend-wide {
        max-width: 248px;
      }

      .custom-legend-title {
        font-weight: 700;
        font-size: 0.88rem;
        margin-bottom: 0.55rem;
        line-height: 1.18;
        white-space: normal;
      }

      .custom-legend-body {
        display: flex;
        align-items: stretch;
        gap: 0.65rem;
      }

      .custom-legend-ramp {
        width: 16px;
        min-width: 16px;
        height: 144px;
        border-radius: 999px;
        box-shadow: inset 0 0 0 1px rgba(36,50,61,0.12);
      }

      .custom-legend-labels {
        height: 144px;
        display: flex;
        flex-direction: column;
        justify-content: space-between;
        font-size: 0.8rem;
        color: #4f616b;
      }

      .custom-legend-labels span {
        line-height: 1;
      }

      .custom-legend-list {
        display: flex;
        flex-direction: column;
        gap: 0.42rem;
        max-height: 220px;
        overflow-y: auto;
        padding-right: 0.15rem;
      }

      .custom-legend-item {
        display: flex;
        align-items: center;
        gap: 0.55rem;
        font-size: 0.8rem;
        color: #4f616b;
        line-height: 1.2;
      }

      .custom-legend-swatch {
        width: 14px;
        min-width: 14px;
        height: 14px;
        border-radius: 999px;
        box-shadow: inset 0 0 0 1px rgba(36,50,61,0.14);
      }

      a:focus-visible,
      button:focus-visible,
      .nav-link:focus-visible,
      .leaflet-bar a:focus-visible,
      .selectize-input.focus,
      .form-select:focus-visible,
      .form-control:focus-visible {
        outline: 3px solid #d5b066 !important;
        outline-offset: 2px !important;
      }

      .js-plotly-plot .plotly .modebar {
        opacity: 0;
        transition: opacity 0.2s ease;
      }

      .js-plotly-plot:hover .plotly .modebar {
        opacity: 1;
      }

      .js-plotly-plot .plotly .modebar-group {
        background: rgba(255,255,255,0.9) !important;
        border-radius: 999px !important;
        box-shadow: 0 10px 22px rgba(53,79,92,0.12);
      }

      hr {
        border-top: 1px solid var(--hydro-line);
        opacity: 0.85;
      }

      .recalculating {
        opacity: 0.55;
        transition: opacity 0.18s ease;
      }

      .shiny-output-error-validation {
        margin-top: 0.75rem;
        padding: 0.85rem 1rem;
        border-radius: 12px;
        background: #f5f8fa;
        color: #51616b;
        border: 1px solid var(--hydro-line);
      }

      .visually-hidden {
        position: absolute !important;
        width: 1px !important;
        height: 1px !important;
        padding: 0 !important;
        margin: -1px !important;
        overflow: hidden !important;
        clip: rect(0, 0, 0, 0) !important;
        white-space: nowrap !important;
        border: 0 !important;
      }

      .card, .btn, .bslib-value-box, .nav-link, .nav-tabs .nav-link {
        transition: all 0.3s ease !important;
      }
    "
    ))
  ),

  nav_panel(
    "Overview",
    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        h4("Map Controls"),
        selectInput(
          "map_color_by",
          "Color sites by:",
          choices = c(
            "Climate Zone" = "Name",
            "LULC" = "major_land",
            "MAP (mm)" = "mean_annual_precip",
            "MAT (°C)" = "mean_annual_temp",
            "Mean Annual ET" = "mean_annual_evapotrans",
            "Snow Cover (%)" = "snow_cover",
            "Mean Peak Snow Cover (%)" = "mean_snow_prop_area",
            "Peak Snow Cover (%)" = "peak_snow_prop_area",
            "RBI" = "RBI",
            "RCS" = "recession_slope"
          ),
          selected = "Name"
        )
      ),

      layout_columns(
        col_widths = c(8, 4),
        card(
          card_header("Study Sites Across North America"),
          tags$p(
            "Interactive map of study sites across North America. Use the map control to switch between climate, land cover, snow, and hydrograph metrics.",
            class = "visually-hidden"
          ),
          leafletOutput("site_map", height = 600)
        ),
        card(
          card_header("Key Metrics"),
          tags$ul(
            style = "font-size: 0.9em; line-height: 1.6; padding-left: 18px;",
            tags$li(HTML(
              "<span style='font-weight:700;'>Climate Zone</span>: Koppen-Geiger climate classification"
            )),
            tags$li(HTML(
              "<span style='font-weight:700;'>Land-use / Land-cover</span> (LULC): Dominant land cover type within the watershed"
            )),
            tags$li(HTML(
              "<span style='font-weight:700;'>Mean Annual Precipitation</span> (MAP, mm): Average yearly precipitation across the watershed"
            )),
            tags$li(HTML(
              "<span style='font-weight:700;'>Mean Annual Temperature</span> (MAT, °C): Average yearly temperature across the watershed"
            )),
            tags$li(HTML(
              "<span style='font-weight:700;'>Mean Annual Evapotranspiration</span>: Average yearly evapotranspiration across the watershed"
            )),
            tags$li(HTML(
              "<span style='font-weight:700;'>Snow Cover</span> (%): Average percent of the watershed covered by snow over the full period of record"
            )),
            tags$li(HTML(
              "<span style='font-weight:700;'>Mean Peak Snow Cover</span> (%): Average of the yearly maximum snow-cover values over the full period of record"
            )),
            tags$li(HTML(
              "<span style='font-weight:700;'>Peak Snow Cover</span> (%): Highest yearly maximum snow-cover value in the record"
            )),
            tags$li(HTML(
              "<span style='font-weight:700;'>Richards-Baker Flashiness Index</span> (RBI): Measures how rapidly streamflow changes over time"
            )),
            tags$li(HTML(
              "<span style='font-weight:700;'>Recession-curve Slope</span> (RCS): Characterizes subsurface heterogeneity"
            ))
          )
        )
      )
    )
  ),

  nav_panel(
    "Activity 1: Hydrographs & Subsurface",
    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        h4("Controls"),
        p(
          "Start by picking four sites in the precipitation and snow panel:
          two with lower snow-cover values and two with higher snow-cover values.
          Those same sites stay highlighted in the hydrographs and the
          RBI vs RCS comparison.",
          style = "font-size: 0.85em; color: #666;"
        ),
        conditionalPanel(
          condition = "input.activity1_tab == 'RCS vs RBI'",
          selectInput(
            "rcs_rbi_color_by",
            "Color full-site plot by:",
            choices = c(
              "Snow Cover (%)" = "snow_cover",
              "MAP (mm)" = "mean_annual_precip",
              "Land Use" = "major_land"
            ),
            selected = "snow_cover"
          )
        ),
        conditionalPanel(
          condition = "input.activity1_tab == 'Average Hydrographs'",
          checkboxInput(
            "hydrograph_log_scale",
            "Log-scale discharge axis",
            value = FALSE
          )
        ),
        uiOutput("selected_sites_display"),
        actionButton(
          "clear_sites",
          "Clear selections",
          class = "btn-outline-secondary btn-sm mt-2 w-100"
        )
      ),

      navset_card_tab(
        id = "activity1_tab",
        nav_panel(
          "Precipitation & Snow Cover",
          layout_columns(
            col_widths = c(9, 3),
            div(
              style = "display: flex; flex-direction: column; gap: 1rem;",
              card(
                full_screen = TRUE,
                card_header("Use Precipitation and Snow Cover to Choose Four Sites"),
                tags$p(
                  "Scatterplot of mean annual precipitation and snow cover for the full site set. Select up to four sites to compare across Activity 1.",
                  class = "visually-hidden"
                ),
                plotlyOutput("hydroclimate_selector_plot", height = 520)
              ),
              card(
                full_screen = TRUE,
                card_header("Seasonal Precipitation and Snow Cover for Selected Sites"),
                tags$p(
                  "Monthly precipitation and snow-cover profile for the selected sites.",
                  class = "visually-hidden"
                ),
                div(
                  style = "display: flex; flex-direction: column; gap: 0.35rem;",
                  plotlyOutput("hydroclimate_profile", height = 400),
                  uiOutput("hydroclimate_profile_legend")
                )
              )
            ),
            card(
              card_header("Guide"),
              tags$div(
                style = "font-size: 0.88em; line-height: 1.65; padding: 8px 10px;",
                tags$p(HTML(
                  "<b>Why this matters:</b> Hydrographs are one of the main
                  ways hydrologists think about drought, floods, storage, and
                  streamflow generation in real basins."
                )),
                hr(),
                tags$p(HTML(
                  "<b>Review before starting:</b> snow- vs rain-dominated
                  watersheds, what a hydrograph shows, and what the
                  Richards-Baker Flashiness Index (RBI) and recession-curve
                  slope (RCS) represent."
                )),
                hr(),
                tags$p(HTML("<b>Learning objectives</b>")),
                tags$ul(
                  tags$li("Analyze and interpret stream hydrographs to evaluate basin flashiness and recession behaviors across contrasting precipitation regimes."),
                  tags$li("Relate quantitative hydrograph metrics to subsurface storage and streamflow generation processes.")
                ),
                hr(),
                tags$p(
                  "Use this panel to identify two sites with
                  snow cover < 25% and two with snow cover > 25%."
                ),
                hr(),
                tags$p(
                  style = "color: #444;",
                  HTML(
                    "<b>What to do here:</b> Compare how precipitation and
                    snow cover differ across the four sites you picked, then
                    carry those same sites into the hydrograph and RBI vs RCS
                    panels."
                  )
                ),
                hr(),
                tags$p(
                  style = "color: #444;",
                  HTML(
                    "<b>Below:</b> The monthly plot shows the seasonal
                    precipitation and snow-cover pattern for the selected
                    sites."
                  )
                )
              )
            )
          )
        ),
        nav_panel(
          "RCS vs RBI",
          layout_columns(
            col_widths = c(8, 4),
            card(
              full_screen = TRUE,
              card_header(
                "RCS vs RBI Across the Full Site Set"
              ),
              tags$p(
                "Scatterplot of recession curve slope and Richards-Baker flashiness index for all sites.",
                class = "visually-hidden"
              ),
              plotlyOutput("rcs_rbi_plot", height = 700)
            ),
            card(
              card_header("Guide"),
              tags$div(
                style = "font-size: 0.88em; line-height: 1.6; padding: 8px;",
                tags$p(HTML(
                  "<b>Learning objective:</b> Relate quantitative hydrograph metrics to subsurface storage and streamflow generation processes."
                )),
                hr(),
                tags$p(
                  style = "color: #444;",
                  HTML(
                    "<b>Prompt 1:</b> Describe the relationship between RCS
                    and RBI across the entire dataset. How do you interpret
                    that pattern?"
                  )
                ),
                hr(),
                tags$p(
                  style = "color: #444;",
                  HTML(
                    "<b>Prompt 2:</b> You should see an inverse relationship
                    between RCS and RBI. Why might flashier basins be expected
                    to have lower RCS?"
                  )
                ),
                hr(),
                tags$p(
                  style = "color: #444;",
                  HTML(
                    "<b>Tip:</b> Use the color selector in the sidebar to
                    compare this pattern by snow cover, MAP, or land use."
                  )
                )
              )
            )
          )
        ),
        nav_panel(
          "Average Hydrographs",
          layout_columns(
            col_widths = c(8, 4),
            div(
              style = "display: flex; flex-direction: column; gap: 1rem;",
              card(
                full_screen = TRUE,
                card_header("Compare Average Monthly Discharge Patterns"),
                div(
                  style = "display: flex; flex-direction: column; gap: 0.35rem;",
                  plotlyOutput("hydrograph_grid", height = 650),
                  uiOutput("hydrograph_grid_legend")
                )
              ),
              card(
                full_screen = TRUE,
                card_header("Selected Sites in RBI-RCS Space"),
                plotlyOutput("selected_rcs_rbi", height = 560)
              )
            ),
            card(
              card_header("Guide"),
              tags$div(
                style = "font-size: 0.9em; line-height: 1.7; padding: 10px 12px;",
                tags$p(HTML(
                  "<b>Learning objective:</b> Analyze and interpret stream hydrographs to evaluate basin flashiness and recession behaviors across contrasting precipitation regimes."
                )),
                hr(),
                tags$p(
                  "Using the same four sites, describe the flashiness of each
                  hydrograph and compare their timing of peak flow."
                ),
                hr(),
                tags$p(
                  style = "color: #444;",
                  HTML(
                    "<b>Prompt 1:</b> How does the recession period vary
                    across the hydrographs? Which sites have longer or shorter
                    recessions, and how does that line up with RCS?"
                  )
                ),
                hr(),
                tags$p(
                  style = "color: #444;",
                  HTML(
                    "<b>Prompt 2:</b> Given what you know about precipitation
                    regime, what relationships do you see between precipitation
                    type and flashiness, and between precipitation type and
                    RCS? What hypotheses can you make about why those patterns
                    appear?"
                  )
                )
              )
            )
          )
        )
      )
    )
  ),

  nav_panel(
    "Activity 2: Mapping Stream Salinity",
    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        h4("Controls"),

        # chloride map controls
        conditionalPanel(
          condition = "input.activity2_tab == 'Chloride Map'",
          selectInput(
            "cl_map_background",
            "Background raster:",
            choices = activity2_background_choices,
            selected = if (length(activity2_background_choices) > 0) {
              unname(activity2_background_choices[[1]])
            } else {
              character(0)
            }
          ),
          p(
            "Choose a North America background raster and compare it with the
            mean chloride points plotted on top. Click a site marker for exact
            values.",
            style = "font-size: 0.85em; color: #666;"
          )
        ),

        # seasonal plot controls
        conditionalPanel(
          condition = "input.activity2_tab == 'Seasonal Cl & Discharge'",
          p(
            "Choose sites from different regions and compare seasonal chloride
            with the hydrograph. Map clicks update this selector automatically.",
            style = "font-size: 0.85em; color: #666;"
          ),
          selectInput(
            "cl_site_select",
            "Choose a site:",
            choices = NULL
          ),
          checkboxInput(
            "cl_show_discharge",
            "Overlay monthly discharge",
            value = FALSE
          )
        )
      ),

      navset_card_tab(
        id = "activity2_tab",
        nav_panel(
          "Chloride Map",
          layout_columns(
            col_widths = c(8, 4),
            card(
              full_screen = TRUE,
              card_header("Stream Chloride Across North America"),
              tags$p(
                "Interactive chloride map with switchable North America raster backgrounds for MAP, cropland cover, and impervious cover, with chloride points plotted on top.",
                class = "visually-hidden"
              ),
              leafletOutput("cl_map", height = 600)
            ),
            card(
              card_header("About Stream Chloride"),
              tags$div(
                style = "font-size: 0.88em; line-height: 1.6; padding: 8px;",
                tags$p(HTML(
                  "<b>Review before starting:</b> major sources of chloride to
                  streams."
                )),
                hr(),
                tags$p(HTML(
                  "<b>Learning objective:</b> Evaluate spatial patterns of stream chloride concentrations across the United States and interpret how climate and land use influence salinity."
                )),
                hr(),
                tags$p(HTML(
                  "<b>Chloride (Cl<sup>&minus;</sup>)</b> is a conservative
                  tracer &mdash; it doesn't react or degrade in most
                  freshwater systems, making it useful for tracking sources
                  and transport."
                )),
                hr(),
                tags$p(
                  style = "color: #444;",
                  HTML(
                    "<b>Prompts:</b> What areas of the US have higher Cl?
                    How does that pattern compare with mean annual
                    precipitation across the map? Then click a few sites and
                    compare their chloride values directly."
                  )
                )
              )
            )
          )
        ),
        nav_panel(
          "Seasonal Cl & Discharge",
          layout_columns(
            col_widths = c(8, 4),
            card(
              full_screen = TRUE,
              card_header("Monthly Chloride & Discharge Patterns"),
              div(
                style = "display: flex; flex-direction: column; gap: 0.35rem;",
                plotlyOutput("cl_seasonal_plot", height = 600),
                uiOutput("cl_seasonal_plot_legend")
              )
            ),
            card(
              card_header("Seasonal Patterns"),
              tags$div(
                style = "font-size: 0.88em; line-height: 1.6; padding: 8px;",
                tags$p(HTML(
                  "<b>Learning objective:</b> Evaluate temporal patterns of stream chloride and hypothesize how hydrologic and land use processes influence seasonal variation in salinity."
                )),
                hr(),
                tags$p(
                  "Choose a few sites from different regions and look at how
                  chloride changes over the course of a year."
                ),

                hr(),
                tags$p(HTML(
                  "<b>Prompt 1:</b> When do you see high chloride
                  concentrations and when do you see low concentrations?"
                )),
                hr(),
                tags$p(
                  style = "color: #444;",
                  HTML(
                    "<b>Prompt 2:</b> Overlay discharge and describe the
                    relationship between seasonal chloride and the hydrograph.
                    Do they appear to be related? Why or why not?"
                  )
                )
              )
            )
          )
        )
      )
    )
  ),

  nav_panel(
    "Activity 3: Exploring C-Q Relationships",
    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        h4("Controls"),

        # time series controls
        conditionalPanel(
          condition = "input.activity3_tab == 'Average Seasonal Hydrograph'",
          p(
            "Start with one site, identify low- and high-flow periods, and
            then overlay Cl or NO3 to compare concentration with seasonality.",
            style = "font-size: 0.85em; color: #666;"
          ),
          selectInput(
            "cq_ts_site",
            "Select a site:",
            choices = NULL
          ),
          checkboxGroupInput(
            "cq_ts_solutes",
            "Overlay concentration:",
            choices = c("Chloride (Cl)" = "Cl", "Nitrate (NO3)" = "NO3"),
            selected = character(0)
          ),
          checkboxInput(
            "cq_ts_normalize",
            "Normalize chemistry (z-score)",
            value = FALSE
          )
        ),

        # C-Q scatter controls
        conditionalPanel(
          condition = "input.activity3_tab == 'C-Q Relationships'",
          p(
            "Use the same site first, then add up to two more sites and
            compare Cl and NO3 C-Q behavior across them.",
            style = "font-size: 0.85em; color: #666;"
          ),
          selectInput(
            "cq_sites",
            "Sites (max 3):",
            choices = NULL,
            multiple = TRUE
          ),
          checkboxGroupInput(
            "cq_solutes",
            "Solutes:",
            choices = c("Chloride (Cl)" = "Cl", "Nitrate (NO3)" = "NO3"),
            selected = character(0)
          ),
          checkboxInput(
            "cq_show_trendline",
            "Show trendlines",
            value = TRUE
          )
        ),

        # histogram controls
        conditionalPanel(
          condition = "input.activity3_tab == 'C-Q Slope Distribution'",
          p(
            "Compare the national C-Q slope distributions for Cl and NO3.",
            style = "font-size: 0.85em; color: #666;"
          ),
          checkboxGroupInput(
            "cq_hist_solutes",
            "Show:",
            choices = c("Chloride (Cl)" = "Cl", "Nitrate (NO3)" = "NO3"),
            selected = c("Cl", "NO3")
          )
        )
      ),

      navset_card_tab(
        id = "activity3_tab",
        nav_panel(
          "Average Seasonal Hydrograph",
          layout_columns(
            col_widths = c(8, 4),
            card(
              full_screen = TRUE,
              card_header("Average Monthly Discharge & Concentration"),
              tags$div(
                style = "display: flex; flex-direction: column; gap: 0.5rem;",
                plotlyOutput("cq_timeseries_plot", height = 600),
                uiOutput("cq_timeseries_plot_legend")
              )
            ),
            card(
              card_header("Getting Started"),
              tags$div(
                style = "font-size: 0.88em; line-height: 1.6; padding: 8px;",
                tags$p(HTML(
                  "<b>Review before starting:</b> conservative vs
                  non-conservative tracers."
                )),
                hr(),
                tags$p(HTML(
                  "<b>Learning objective:</b> Analyze concentration-discharge (C-Q) relationships to infer patterns of solute storage and transport across diverse hydrologic settings."
                )),
                hr(),
                tags$p(
                  "Start by examining the average monthly hydrograph for a
                  single site. Identify low-flow and high-flow seasons."
                ),
                hr(),
                tags$p(HTML(
                  "<b>Prompt 1:</b> Overlay <b>Cl</b> concentration. Where are
                  concentrations high and where are they low? How do they
                  relate to the low- and high-flow periods?"
                )),
                tags$p(HTML(
                  "<b>Next:</b> This sets up the C-Q plot, where concentration
                  is plotted directly against discharge."
                )),
                hr(),
                tags$p(
                  style = "color: #444;",
                  HTML(
                    "<b>Move on when ready:</b> Once you identify the seasonal
                    flow and concentration patterns, go to the
                    <em>C-Q Relationships</em> tab."
                  )
                )
              )
            )
          )
        ),
        nav_panel(
          "C-Q Relationships",
          layout_columns(
            col_widths = c(8, 4),
            tags$div(
              style = "display: flex; flex-direction: column; gap: 1rem;",
              card(
                full_screen = TRUE,
                card_header(HTML(
                  "log<sub>10</sub>(Concentration) vs log<sub>10</sub>(Discharge)"
                )),
                div(
                  style = "display: flex; flex-direction: column; gap: 0.5rem;",
                  plotlyOutput("cq_scatter_plot", height = 600),
                  uiOutput("cq_scatter_legend")
                )
              ),
              card(
                card_header("Selected Trendline Fits"),
                uiOutput("cq_fit_summaries")
              )
            ),
            card(
              card_header("C-Q Framework"),
              tags$div(
                style = "font-size: 0.88em; line-height: 1.6; padding: 8px;",
                tags$p(HTML("<b>Learning objectives</b>")),
                tags$ul(
                  tags$li("Analyze concentration-discharge (C-Q) relationships to infer patterns of solute storage and transport across diverse hydrologic settings."),
                  tags$li("Compare C-Q behavior across conservative and non-conservative solutes and interpret how differences in slope, variability, and distribution reflect underlying biogeochemical and hydrologic processes.")
                ),
                hr(),
                tags$p(HTML(
                  "The C-Q framework follows the power-law model from
                  <a href='https://doi.org/10.1002/hyp.7315'>
                  Godsey et al. (2009)</a>:"
                )),
                tags$p(
                  style = "text-align: center; font-size: 1.05em; margin: 6px 0;",
                  HTML("<em>C = a Q<sup>b</sup></em>")
                ),
                tags$p(HTML(
                  "The exponent <em>b</em> is the C-Q slope, estimated via
                  log-log regression: log(C) = log(a) + <em>b</em> &middot; log(Q).
                  It tells us how solutes are stored and mobilized."
                )),
                hr(),
                tags$p(HTML("<b>Interpreting C-Q slopes:</b>")),
                tags$ul(
                  tags$li(HTML(
                    "<b>Slope > 0.1</b> = enrichment (concentration rises with flow)"
                  )),
                  tags$li(HTML(
                    "<b>Slope between &plusmn;0.1</b> = chemostatic (concentration stable)"
                  )),
                  tags$li(HTML(
                    "<b>Slope < &minus;0.1</b> = dilution (concentration falls with flow)"
                  ))
                ),
                hr(),
                tags$p(HTML(
                  "<b>Cl</b> is a <em>conservative</em> tracer &mdash; it
                  doesn't react in most freshwater systems."
                )),
                tags$p(HTML(
                  "<b>NO<sub>3</sub></b> is <em>non-conservative</em> &mdash;
                  it is actively cycled by biological and chemical processes."
                )),
                hr(),
                tags$p(
                  style = "color: #444;",
                  HTML(
                    "<b>Prompts:</b> Describe the relationship between C and Q
                    for Cl at one site, then compare it with two additional
                    sites. What does a positive, negative, or flat slope tell
                    you? Then plot NO<sub>3</sub> for the same sites and
                    compare the slopes and line fits."
                  )
                )
              )
            )
          )
        ),
        nav_panel(
          "C-Q Slope Distribution",
          layout_columns(
            col_widths = c(8, 4),
            card(
              full_screen = TRUE,
              card_header("Distribution of C-Q Slopes Across All Sites"),
              plotlyOutput("cq_histogram", height = 600)
            ),
            card(
              card_header("Reading the Histogram"),
              tags$div(
                style = "font-size: 0.88em; line-height: 1.6; padding: 8px;",
                tags$p(HTML(
                  "<b>Learning objective:</b> Compare C-Q behavior across conservative and non-conservative solutes and interpret how differences in slope, variability, and distribution reflect underlying biogeochemical and hydrologic processes."
                )),
                hr(),
                tags$p(
                  "Each bar represents a group of sites with similar C-Q
                  slopes. Both Cl and NO3 are shown so you can compare
                  their distributions directly."
                ),
                hr(),
                tags$p(HTML(
                  "The dashed lines at <b>&plusmn;0.1</b> mark the boundaries
                  between C-Q behaviors:"
                )),
                tags$ul(
                  tags$li(HTML(
                    "Left of &minus;0.1: <b>Dilution</b> &mdash; concentration decreases with flow"
                  )),
                  tags$li(HTML(
                    "Between &plusmn;0.1: <b>Chemostatic</b> &mdash; concentration is stable"
                  )),
                  tags$li(HTML(
                    "Right of +0.1: <b>Enrichment</b> &mdash; concentration increases with flow"
                  ))
                ),
                hr(),
                tags$p(
                  style = "color: #444;",
                  HTML(
                    "<b>Prompt:</b> Which solute has a larger range in slopes?
                    Which is more likely to be chemostatic? What does that
                    suggest about the processes controlling storage and
                    transport of Cl compared to NO<sub>3</sub>?"
                  )
                )
              )
            )
          )
        )
      )
    )
  ),

  nav_panel(
    "About",
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header("About These Modules"),
        tags$div(
          style = "font-size: 0.9em; line-height: 1.7; padding: 8px;",
          tags$p("Plug for Si, Si 4 Lyfe")
        )
      ),
      card(
        card_header("Data, Funding, and Credits"),
        tags$div(
          style = "font-size: 0.9em; line-height: 1.7; padding: 8px;"
        )
      )
    )
  )
)


# --- Server ----------------------------------------------------------------

server <- function(input, output, session) {
  # Keep the core tables in a few shared reactives so the plots stay simple.
  harmonized_complete <- reactive({
    read_app_data("harmonized_complete.rds")
  })

  harmonized_partial <- reactive({
    read_app_data("harmonized_partial.rds")
  })

  discharge_data <- reactive({
    discharge_global
  })

  hydro_sites <- reactive({
    harmonized_complete() %>%
      filter(
        !is.na(RBI),
        !is.na(recession_slope),
        !is.na(snow_cover)
      )
  })

  hydroclimate_sites <- reactive({
    hydro_sites() %>%
      filter(!is.na(mean_annual_precip), !is.na(snow_cover))
  })

  # Monthly discharge shows up in more than one activity, so build it once.
  discharge_monthly <- reactive({
    build_monthly_discharge(discharge_data())
  })

  hydro_site_colors <- c("#0072B2", "#D55E00", "#009E73", "#CC79A7")

  # Keep one shared site selection for all Activity 1 panels.
  selected_sites <- reactiveVal(character(0))

  toggle_selected_site <- function(site_id) {
    if (is.null(site_id) || site_id == "") {
      return()
    }
    current <- selected_sites()
    if (site_id %in% current) {
      selected_sites(setdiff(current, site_id))
    } else if (length(current) < 4) {
      selected_sites(c(current, site_id))
    }
  }

  observeEvent(event_data("plotly_click", source = "hydro_selector"), {
    click <- event_data("plotly_click", source = "hydro_selector")
    if (is.null(click)) {
      return()
    }
    toggle_selected_site(click$key)
  })

  observeEvent(event_data("plotly_click", source = "rcs_rbi"), {
    click <- event_data("plotly_click", source = "rcs_rbi")
    if (is.null(click)) {
      return()
    }
    toggle_selected_site(click$key)
  })

  observeEvent(input$clear_sites, {
    selected_sites(character(0))
  })

  # show selected sites in sidebar
  output$selected_sites_display <- renderUI({
    site_data <- hydro_sites()
    ids <- selected_sites()

    label <- if (length(ids) == 0) {
      tags$em("None", style = "color: #999;")
    } else {
      names <- site_data$Stream_Name[match(ids, site_data$Stream_ID)]
      tags$span(paste(names, collapse = ", "))
    }

    tags$div(
      style = "font-size: 0.85em; line-height: 1.6;",
      tags$div(
        tags$strong("Selected sites: ", style = "color: #2d2926;"),
        label,
        paste0(" (", length(ids), "/4)")
      )
    )
  })

  selected_site_palette <- reactive({
    ids <- selected_sites()
    setNames(hydro_site_colors[seq_len(length(ids))], ids)
  })

  format_legend_number <- function(x, digits = 0) {
    formatC(x, format = "f", digits = digits, drop0trailing = TRUE)
  }

  build_numeric_legend <- function(title, values, legend_colors, digits = 0, n_breaks = 6) {
    value_range <- range(values, na.rm = TRUE)
    legend_breaks <- pretty(value_range, n = n_breaks)
    legend_breaks <- legend_breaks[
      legend_breaks >= value_range[1] &
        legend_breaks <= value_range[2]
    ]

    if (length(legend_breaks) < 2) {
      legend_breaks <- sort(unique(round(value_range, digits + 1)))
    }

    if (length(legend_breaks) < 2) {
      legend_breaks <- c(value_range[1], value_range[2])
    }

    legend_labels <- rev(format_legend_number(legend_breaks, digits = digits))

    as.character(
      tags$div(
        class = "custom-legend",
        tags$div(title, class = "custom-legend-title"),
        tags$div(
          class = "custom-legend-body",
          tags$div(
            class = "custom-legend-ramp",
            style = paste0(
              "background: linear-gradient(to top, ",
              paste(legend_colors, collapse = ", "),
              ");"
            )
          ),
          tags$div(
            class = "custom-legend-labels",
            lapply(legend_labels, tags$span)
          )
        )
      )
    )
  }

  build_custom_numeric_legend <- function(title, legend_colors, legend_labels) {
    legend_labels <- rev(legend_labels)

    as.character(
      tags$div(
        class = "custom-legend",
        tags$div(title, class = "custom-legend-title"),
        tags$div(
          class = "custom-legend-body",
          tags$div(
            class = "custom-legend-ramp",
            style = paste0(
              "background: linear-gradient(to top, ",
              paste(legend_colors, collapse = ", "),
              ");"
            )
          ),
          tags$div(
            class = "custom-legend-labels",
            lapply(legend_labels, tags$span)
          )
        )
      )
    )
  }

  build_categorical_legend <- function(title, legend_items, label_overrides = NULL, extra_class = NULL) {
    as.character(
      tags$div(
        class = trimws(paste("custom-legend", extra_class)),
        tags$div(title, class = "custom-legend-title"),
        tags$div(
          class = "custom-legend-list",
          lapply(
            names(legend_items),
            function(label) {
              display_label <- if (!is.null(label_overrides) && label %in% names(label_overrides)) {
                label_overrides[[label]]
              } else {
                label
              }
              tags$div(
                class = "custom-legend-item",
                tags$span(
                  class = "custom-legend-swatch",
                  style = paste0("background:", legend_items[[label]], ";")
                ),
                tags$span(display_label)
              )
            }
          )
        )
      )
    )
  }

  # --- Map -----------------------------------------------------------------

  output$site_map <- renderLeaflet({
    req(input$map_color_by)

    map_data <- harmonized_partial() %>%
      filter(!is.na(Latitude), !is.na(Longitude)) %>%
      mutate(major_land_display = clean_land_use_label(major_land))

    selected_var <- input$map_color_by
    map_data <- if (selected_var %in% c("major_land", "Name")) {
      map_data %>%
        filter(
          !is.na(.data[[selected_var]]),
          trimws(as.character(.data[[selected_var]])) != ""
        )
    } else {
      map_data %>%
        filter(is.finite(.data[[selected_var]]))
    }

    req(nrow(map_data) > 0)

    # use cleaned land-cover labels for display and color matching
    if (input$map_color_by == "major_land") {
      map_data <- map_data %>%
        mutate(major_land = major_land_display)
    }

    color_var <- map_data[[input$map_color_by]]

    # show snow-cover metrics as percentages in the overview map
    if (input$map_color_by %in% c("snow_cover", "mean_snow_prop_area", "peak_snow_prop_area")) {
      color_var <- color_var * 100
    }

    numeric_legend_specs <- list(
      "mean_annual_precip" = list(
        colors = precip_palette,
        digits = 0
      ),
      "mean_annual_temp" = list(
        colors = rev(RColorBrewer::brewer.pal(9, "RdYlBu")),
        digits = 1
      ),
      "mean_annual_evapotrans" = list(
        colors = RColorBrewer::brewer.pal(9, "Oranges"),
        digits = 0
      ),
      "snow_cover" = list(
        colors = precip_palette,
        digits = 0
      ),
      "mean_snow_prop_area" = list(
        colors = precip_palette,
        digits = 0
      ),
      "peak_snow_prop_area" = list(
        colors = precip_palette,
        digits = 0
      ),
      "RBI" = list(
        colors = RColorBrewer::brewer.pal(9, "Greens"),
        digits = 2
      ),
      "recession_slope" = list(
        colors = RColorBrewer::brewer.pal(9, "Greens"),
        digits = 2
      )
    )
    is_numeric_map_var <- input$map_color_by %in% names(numeric_legend_specs)
    numeric_spec <- if (is_numeric_map_var) numeric_legend_specs[[input$map_color_by]] else NULL

    legend_titles <- c(
      "Name" = "Climate Zone",
      "snow_cover" = "Snow Cover (%)",
      "mean_annual_precip" = "MAP (mm)",
      "mean_annual_temp" = "MAT (°C)",
      "mean_annual_evapotrans" = "Mean Annual ET",
      "mean_snow_prop_area" = "Mean Peak Snow Cover (%)",
      "peak_snow_prop_area" = "Peak Snow Cover (%)",
      "RBI" = "RBI",
      "recession_slope" = "RCS",
      "major_land" = "LULC"
    )
    legend_title <- switch(
      input$map_color_by,
      "mean_snow_prop_area" = HTML("Mean Peak<br>Snow Cover (%)"),
      "peak_snow_prop_area" = HTML("Peak Snow<br>Cover (%)"),
      legend_titles[[input$map_color_by]]
    )

    # 20 high-contrast colors for categorical variables
    distinct_colors <- c(
      "#e41a1c",
      "#377eb8",
      "#4daf4a",
      "#984ea3",
      "#ff7f00",
      "#ffff33",
      "#a65628",
      "#f781bf",
      "#66c2a5",
      "#fc8d62",
      "#8da0cb",
      "#e78ac3",
      "#a6d854",
      "#ffd92f",
      "#e5c494",
      "#b3b3b3",
      "#1b9e77",
      "#d95f02",
      "#7570b3",
      "#e7298a"
    )

    # Choose color palette based on variable type
    pal <- switch(
      input$map_color_by,

      # Numeric variables
      "mean_annual_precip" = colorNumeric(
        numeric_legend_specs[["mean_annual_precip"]]$colors,
        domain = color_var
      ),
      "mean_annual_temp" = colorNumeric(
        numeric_legend_specs[["mean_annual_temp"]]$colors,
        domain = color_var
      ),
      "mean_annual_evapotrans" = colorNumeric(
        numeric_legend_specs[["mean_annual_evapotrans"]]$colors,
        domain = color_var
      ),
      "snow_cover" = colorNumeric(
        numeric_legend_specs[["snow_cover"]]$colors,
        domain = color_var
      ),
      "mean_snow_prop_area" = colorNumeric(
        numeric_legend_specs[["mean_snow_prop_area"]]$colors,
        domain = color_var
      ),
      "peak_snow_prop_area" = colorNumeric(
        numeric_legend_specs[["peak_snow_prop_area"]]$colors,
        domain = color_var
      ),
      "RBI" = colorNumeric(
        numeric_legend_specs[["RBI"]]$colors,
        domain = color_var
      ),
      "recession_slope" = colorNumeric(
        numeric_legend_specs[["recession_slope"]]$colors,
        domain = color_var
      ),

      # Qualitative variables
      "Name" = function(values) {
        named_color_lookup(
          values,
          palette = climate_zone_colors,
          default = "#b9c7d3"
        )
      },
      "major_land" = function(values) {
        named_color_lookup(
          values,
          palette = land_use_colors,
          default = land_use_colors[["Other / Unclassified"]]
        )
      },

      # Default fallback for any remaining categorical variables
      {
        colorFactor(
          palette = rep(
            distinct_colors,
            length.out = length(unique(color_var))
          ),
          domain = color_var
        )
      }
    )

    map_fill_color <- if (input$map_color_by == "major_land") {
      named_color_lookup(
        color_var,
        palette = land_use_colors,
        default = land_use_colors[["Other / Unclassified"]]
      )
    } else if (input$map_color_by == "Name") {
      named_color_lookup(
        color_var,
        palette = climate_zone_colors,
        default = "#b9c7d3"
      )
    } else {
      unname(pal(color_var))
    }

    map_data <- map_data %>%
      mutate(
        map_fill_color = map_fill_color,
        popup_html = paste0(
          "<b>",
          Stream_Name,
          "</b><br>",
          "LTER: ",
          LTER,
          "<br>",
          "LULC: ",
          major_land_display,
          "<br>",
          "RBI: ",
          round(RBI, 3),
          "<br>",
          "RCS: ",
          round(recession_slope, 3),
          "<br>",
          "Climate: ",
          Name,
          "<br>",
          "Snow Cover: ",
          round(snow_cover * 100, 0),
          "%<br>",
          "Mean Peak Snow Cover: ",
          round(mean_snow_prop_area * 100, 0),
          "%<br>",
          "Peak Snow Cover: ",
          round(peak_snow_prop_area * 100, 0),
          "%<br>",
          "Mean Annual Precip: ",
          round(mean_annual_precip, 1),
          " mm<br>",
          "MAT: ",
          round(mean_annual_temp, 1),
          " °C<br>",
          "Mean Annual ET: ",
          round(mean_annual_evapotrans, 1),
          " kg/m2"
        )
      )

    lng_bounds <- range(map_data$Longitude, na.rm = TRUE) + c(-6, 6)
    lat_bounds <- range(map_data$Latitude, na.rm = TRUE) + c(-4, 4)

    m <- leaflet(
      map_data,
      options = leafletOptions(
        preferCanvas = TRUE,
        worldCopyJump = FALSE
      )
      ) %>%
      addProviderTiles(
        providers$CartoDB.PositronNoLabels,
        options = tileOptions(opacity = 0.9)
      ) %>%
      addProviderTiles(
        providers$CartoDB.PositronOnlyLabels,
        options = tileOptions(opacity = 0.75)
      ) %>%
      fitBounds(lng_bounds[1], lat_bounds[1], lng_bounds[2], lat_bounds[2]) %>%
      addScaleBar(position = "bottomright", options = scaleBarOptions(imperial = FALSE))

    if (input$map_color_by %in% c("major_land", "Name")) {
      class_palette <- if (input$map_color_by == "major_land") {
        land_use_colors
      } else {
        climate_zone_colors
      }
      class_var <- if (input$map_color_by == "major_land") {
        "major_land"
      } else {
        "Name"
      }
      class_levels <- names(class_palette)[
        names(class_palette) %in% unique(as.character(map_data[[class_var]]))
      ]

      for (class_label in class_levels) {
        class_data <- map_data %>%
          filter(.data[[class_var]] == class_label)

        if (nrow(class_data) == 0) {
          next
        }

        class_color <- unname(class_palette[[class_label]])
        class_fill_opacity <- if (identical(class_label, "Bare")) 0.94 else 0.78

        m <- m %>%
          addCircleMarkers(
            data = class_data,
            lng = ~Longitude,
            lat = ~Latitude,
            radius = 6.5,
            stroke = TRUE,
            fill = TRUE,
            fillColor = class_color,
            color = "#7f878d",
            weight = 0.9,
            opacity = 0.85,
            fillOpacity = class_fill_opacity,
            popup = ~popup_html,
            label = ~Stream_Name,
            group = class_label
          )
      }
    } else {
      m <- m %>%
        addCircleMarkers(
          data = map_data,
          lng = ~Longitude,
          lat = ~Latitude,
          radius = 6.5,
          stroke = TRUE,
          fill = TRUE,
          fillColor = ~map_fill_color,
          color = "#7f878d",
          weight = 0.9,
          opacity = 0.85,
          fillOpacity = 0.78,
          popup = ~popup_html,
          label = ~Stream_Name
        )
    }

    if (is_numeric_map_var) {
      m %>%
        addControl(
          html = build_numeric_legend(
            title = legend_title,
            values = color_var,
            legend_colors = numeric_spec$colors,
            digits = numeric_spec$digits
          ),
          position = "bottomleft"
        )
    } else {
      legend_levels <- if (input$map_color_by == "major_land") {
        land_use_legend_levels(color_var)
      } else if (input$map_color_by == "Name") {
        names(climate_zone_colors)[
          names(climate_zone_colors) %in% unique(as.character(color_var))
        ]
      } else {
        sort(unique(as.character(color_var)))
      }
      legend_items <- if (input$map_color_by == "major_land") {
        setNames(
          named_color_lookup(
            legend_levels,
            palette = land_use_colors,
            default = land_use_colors[["Other / Unclassified"]]
          ),
          legend_levels
        )
      } else if (input$map_color_by == "Name") {
        setNames(
          unname(climate_zone_colors[legend_levels]),
          legend_levels
        )
      } else {
        setNames(
          unname(pal(legend_levels)),
          legend_levels
        )
      }

      m %>%
        addControl(
          html = build_categorical_legend(
            title = legend_title,
            legend_items = legend_items,
            label_overrides = if (input$map_color_by == "major_land") {
              c("Grassland / Shrubland" = "Grassland\u00A0/\u00A0Shrubland")
            } else {
              NULL
            },
            extra_class = if (input$map_color_by == "major_land") {
              "custom-legend-wide"
            } else {
              NULL
            }
          ),
          position = "bottomleft"
        )
    }
  })

  # --- Activity 1: Hydroclimate selector -----------------------------------

  hydroclimate_profile_data <- reactive({
    ids <- selected_sites()

    if (length(ids) < 1) {
      return(NULL)
    }

    site_meta <- hydroclimate_sites() %>%
      filter(Stream_ID %in% ids) %>%
      mutate(order = match(Stream_ID, ids)) %>%
      arrange(order)

    bind_rows(lapply(seq_len(nrow(site_meta)), function(i) {
      row <- site_meta[i, , drop = FALSE]
      tibble(
        Stream_ID = row$Stream_ID,
        Stream_Name = row$Stream_Name,
        LTER = row$LTER,
        month = 1:12,
        month_label = month_labels,
        precip_mm = extract_monthly_site_values(
          row,
          prefix = "precip_",
          suffix = "_mm_per_day"
        ) * unname(days_in_month[month_keys]),
        snow_cover = extract_monthly_site_values(
          row,
          prefix = "snow_",
          suffix = "_avg_prop_area"
        ),
        snow_cover_pct = extract_monthly_site_values(
          row,
          prefix = "snow_",
          suffix = "_avg_prop_area"
        ) * 100
      )
    }))
  })

  output$hydroclimate_selector_plot <- renderPlotly({
    plot_data <- hydroclimate_sites() %>%
      mutate(is_highlighted = Stream_ID %in% selected_sites())

    if (nrow(plot_data) == 0) {
      return(
        plotly_empty() %>%
          layout(
            title = list(
              text = "No hydroclimate data are available for the site selector",
              font = list(color = "#666", size = 14)
            )
          )
      )
    }

    hover_text <- paste0(
      "<b>", plot_data$Stream_Name, "</b><br>",
      "LTER: ", plot_data$LTER, "<br>",
      "MAP: ", round(plot_data$mean_annual_precip, 0), " mm/yr<br>",
      "Average Snow Cover: ", round(plot_data$snow_cover * 100, 0), "%<br>",
      "Mean Peak Snow Cover: ", round(plot_data$mean_snow_prop_area * 100, 0), "%<br>",
      "RBI: ", round(plot_data$RBI, 3), "<br>",
      "RCS: ", round(plot_data$recession_slope, 3)
    )

    p <- ggplot(
      plot_data,
      aes(
        x = mean_annual_precip,
        y = snow_cover,
        fill = mean_snow_prop_area,
        text = hover_text,
        key = Stream_ID
      )
    ) +
      geom_point(
        shape = 21,
        color = "#7f878d",
        size = 3.3,
        stroke = 0.3,
        alpha = 0.8
      ) +
      labs(
        x = "Mean Annual Precipitation (mm/yr)",
        y = "Average Snow Cover (%)",
        fill = "Mean Peak\nSnow Cover (%)"
      ) +
      base_plot_theme +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      scale_fill_gradientn(
        colours = precip_palette,
        labels = scales::percent_format(accuracy = 1)
      )

    if (any(plot_data$is_highlighted)) {
      highlighted <- filter(plot_data, is_highlighted)
      p <- p +
        geom_point(
          data = highlighted,
          aes(
            x = mean_annual_precip,
            y = snow_cover,
            fill = mean_snow_prop_area
          ),
          shape = 21,
          color = "#7f878d",
          size = 5.2,
          stroke = 0.45,
          alpha = 1,
          show.legend = FALSE,
          inherit.aes = FALSE
        ) +
        geom_text(
          data = highlighted,
          aes(
            x = mean_annual_precip,
            y = snow_cover,
            label = Stream_Name
          ),
          hjust = -0.08,
          vjust = 0,
          size = 3,
          show.legend = FALSE,
          inherit.aes = FALSE
        )
    }

    ggplotly(p, tooltip = "text", source = "hydro_selector") %>%
      layout(
        paper_bgcolor = plotly_bg$paper_bgcolor,
        plot_bgcolor = plotly_bg$plot_bgcolor,
        legend = right_side_legend(font_size = 10),
        margin = list(r = 160),
        title = FALSE
      ) %>%
      polish_plotly(register_click = TRUE)
  })

  output$hydroclimate_profile <- renderPlotly({
    plot_data <- hydroclimate_profile_data()

    if (is.null(plot_data) || nrow(plot_data) == 0) {
      return(
        plotly_empty() %>%
          layout(
            title = list(
              text = "Select sites above to compare monthly precipitation and snow cover",
              font = list(color = "#666", size = 13)
            )
          )
      )
    }

    palette <- selected_site_palette()
    p <- plot_ly()
    for (site_id in selected_sites()) {
      site_data <- filter(plot_data, Stream_ID == site_id)
      clr <- palette[[site_id]]
      if (nrow(site_data) == 0) {
        next
      }
      label <- paste0(site_data$Stream_Name[1], " [", site_data$LTER[1], "]")

      p <- p %>%
        add_trace(
          data = site_data,
          x = ~month,
          y = ~precip_mm,
          type = "scatter",
          mode = "lines+markers",
          name = paste0(label, " — P"),
          showlegend = FALSE,
          line = list(color = clr, width = 2.5),
          marker = list(color = clr, size = 6),
          hovertemplate = paste0(
            label,
            "<br>Month: %{x}<br>",
            "Precipitation: %{y:.1f} mm/month<extra></extra>"
          )
        ) %>%
        add_trace(
          data = site_data,
          x = ~month,
          y = ~snow_cover_pct,
          type = "scatter",
          mode = "lines+markers",
          name = paste0(label, " — Snow Cover"),
          showlegend = FALSE,
          yaxis = "y2",
          line = list(color = clr, width = 2, dash = "dash"),
          marker = list(color = clr, size = 5, symbol = "diamond"),
          hovertemplate = paste0(
            label,
            "<br>Month: %{x}<br>",
            "Snow Cover: %{y:.0f}%<extra></extra>"
          )
        )
    }

    p %>%
      layout(
        paper_bgcolor = plotly_bg$paper_bgcolor,
        plot_bgcolor = plotly_bg$plot_bgcolor,
        xaxis = list(
          title = NULL,
          tickmode = "array",
          tickvals = 1:12,
          ticktext = month_labels,
          gridcolor = "#d4e3f0"
        ),
        yaxis = list(
          title = "Precipitation (mm/month)",
          gridcolor = "#d4e3f0"
        ),
        yaxis2 = list(
          title = "Snow Cover (%)",
          overlaying = "y",
          side = "right",
          showgrid = FALSE
        ),
        showlegend = FALSE,
        margin = list(r = 80, b = 55),
        hovermode = "closest"
      ) %>%
      polish_plotly()
  })

  output$hydroclimate_profile_legend <- renderUI({
    ids <- selected_sites()
    if (length(ids) == 0) {
      return(NULL)
    }

    site_meta <- hydroclimate_sites() %>%
      filter(Stream_ID %in% ids) %>%
      mutate(order = match(Stream_ID, ids)) %>%
      arrange(order)

    palette <- selected_site_palette()

    build_precip_key <- function(color) {
      tags$span(
        style = "display: inline-flex; align-items: center; width: 34px; position: relative;",
        tags$span(
          style = paste0(
            "display: block; width: 28px; border-top: 3px solid ", color, ";"
          )
        ),
        tags$span(
          style = paste0(
            "position: absolute; left: 10px; top: -3px;",
            "width: 8px; height: 8px; border-radius: 50%;",
            "background: ", color, ";"
          )
        )
      )
    }

    build_snow_key <- function(color) {
      tags$span(
        style = "display: inline-flex; align-items: center; width: 34px; position: relative;",
        tags$span(
          style = paste0(
            "display: block; width: 28px; border-top: 2px dashed ", color, ";"
          )
        ),
        tags$span(
          style = paste0(
            "position: absolute; left: 10px; top: -5px;",
            "width: 8px; height: 8px; background: ", color, ";",
            "transform: rotate(45deg);"
          )
        )
      )
    }

    legend_items <- lapply(seq_len(nrow(site_meta)), function(i) {
      row <- site_meta[i, , drop = FALSE]
      color <- palette[[row$Stream_ID]]
      label <- paste0(row$Stream_Name, " [", row$LTER, "]")

      tags$div(
        style = paste(
          "display: flex;",
          "flex-direction: column;",
          "gap: 0.45rem;",
          "min-width: 0;",
          "padding: 0.7rem 0.85rem;",
          "background: rgba(255,255,255,0.78);",
          "border: 1px solid #e1ebf0;",
          "border-radius: 12px;"
        ),
        tags$span(
          style = "font-size: 0.84rem; color: #31424c; line-height: 1.3; white-space: normal;",
          label
        ),
        tags$div(
          style = "display: flex; align-items: center; gap: 1rem; flex-wrap: wrap;",
          tags$div(
            style = "display: flex; align-items: center; gap: 8px; min-width: 0;",
            build_precip_key(color),
            tags$span(
              style = "font-size: 0.8rem; color: #4f616b;",
              "P"
            )
          ),
          tags$div(
            style = "display: flex; align-items: center; gap: 8px; min-width: 0;",
            build_snow_key(color),
            tags$span(
              style = "font-size: 0.8rem; color: #4f616b;",
              "Snow Cover"
            )
          )
        )
      )
    })

    tags$div(
      style = paste(
        "display: grid;",
        "grid-template-columns: repeat(2, minmax(0, 1fr));",
        "column-gap: 14px;",
        "row-gap: 10px;",
        "padding: 0 10px 10px 10px;",
        "border-top: 1px solid #e1ebf0;"
      ),
      legend_items
    )
  })

  # --- Hydrograph ----------------------------------------------------------

  # hydrograph data reacts to the shared site selections
  hydrograph_data <- reactive({
    all_selected <- selected_sites()

    if (length(all_selected) < 1) {
      return(NULL)
    }

    selected_site_meta <- hydro_sites() %>%
      filter(Stream_ID %in% all_selected) %>%
      select(
        Stream_ID,
        Stream_Name,
        LTER,
        snow_cover,
        RBI,
        recession_slope
      )

    discharge_monthly() %>%
      filter(Stream_ID %in% all_selected) %>%
      left_join(selected_site_meta, by = c("Stream_ID", "Stream_Name", "LTER")) %>%
      mutate(
        site_label = paste0(
          Stream_Name,
          " (RBI=",
          round(RBI, 3),
          ", RCS=",
          round(recession_slope, 3),
          ", Snow Cover=",
          round(snow_cover * 100, 0),
          "%)"
        )
      )
  })

  # build the shared color map for both plots
  hydro_color_map <- reactive({
    selected_site_palette()
  })

  # --- Average monthly hydrograph comparison ---
  output$hydrograph_grid <- renderPlotly({
    plot_data <- hydrograph_data()
    log_scale <- isTRUE(input$hydrograph_log_scale)

    if (is.null(plot_data) || nrow(plot_data) == 0) {
      return(
        plotly_empty() %>%
          layout(
            title = list(
              text = "Select sites in the precipitation and snow panel to compare average monthly hydrographs",
              font = list(color = "#666", size = 14)
            )
          )
      )
    }

    if (log_scale) {
      plot_data <- plot_data %>%
        filter(is.finite(mean_Q_cms), mean_Q_cms > 0)

      if (nrow(plot_data) == 0) {
        return(
          plotly_empty() %>%
            layout(
              title = list(
                text = "No positive discharge values are available for log scaling",
                font = list(color = "#666", size = 14)
              )
            )
        )
      }
    }

    colors <- hydro_color_map()
    site_meta <- plot_data %>%
      select(Stream_ID, Stream_Name, LTER, site_label, RBI, recession_slope) %>%
      distinct() %>%
      mutate(order = match(Stream_ID, selected_sites())) %>%
      arrange(order)

    p <- plot_ly()
    for (i in seq_len(nrow(site_meta))) {
      row <- site_meta[i, ]
      d <- filter(plot_data, Stream_ID == row$Stream_ID)
      clr <- colors[[row$Stream_ID]]

      p <- p %>%
        add_trace(
          data = d,
          x = ~month,
          y = ~mean_Q_cms,
          type = "scatter",
          mode = "lines+markers",
          name = paste0(row$Stream_Name, " [", row$LTER, "]"),
          showlegend = FALSE,
          line = list(color = clr, width = 3),
          marker = list(color = clr, size = 8),
          hovertemplate = paste0(
            row$Stream_Name,
            "<br>Month: %{x}<br>",
            "Mean Q: %{y:.3f} cms<extra></extra>"
          )
        )
    }

    p %>%
      layout(
        title = list(
          text = "Selected Sites: Average Monthly Hydrographs",
          font = list(size = 17, color = "#24323d")
        ),
        paper_bgcolor = plotly_bg$paper_bgcolor,
        plot_bgcolor = plotly_bg$plot_bgcolor,
        xaxis = list(
          title = list(text = "Month", font = list(size = 14)),
          tickmode = "array",
          tickvals = 1:12,
          ticktext = month_labels,
          tickfont = list(size = 12),
          gridcolor = "#d4e3f0"
        ),
        yaxis = list(
          title = list(
            text = if (log_scale) {
              "Mean Discharge (cms, log scale)"
            } else {
              "Mean Discharge (cms)"
            },
            font = list(size = 14)
          ),
          type = if (log_scale) "log" else "linear",
          tickfont = list(size = 12),
          gridcolor = "#d4e3f0"
        ),
        showlegend = FALSE,
        margin = list(t = 60, r = 40, b = 70, l = 60),
        hovermode = "closest",
        hoverdistance = 12
      ) %>%
      polish_plotly()
  })

  output$hydrograph_grid_legend <- renderUI({
    ids <- selected_sites()
    if (length(ids) == 0) {
      return(NULL)
    }

    site_meta <- hydro_sites() %>%
      filter(Stream_ID %in% ids) %>%
      mutate(order = match(Stream_ID, ids)) %>%
      arrange(order)

    colors <- hydro_color_map()

    build_hydro_key <- function(color) {
      tags$span(
        style = "display: inline-flex; align-items: center; width: 34px; position: relative;",
        tags$span(
          style = paste0(
            "display: block; width: 28px; border-top: 3px solid ", color, ";"
          )
        ),
        tags$span(
          style = paste0(
            "position: absolute; left: 10px; top: -3px;",
            "width: 8px; height: 8px; border-radius: 50%;",
            "background: ", color, ";"
          )
        )
      )
    }

    legend_items <- lapply(seq_len(nrow(site_meta)), function(i) {
      row <- site_meta[i, , drop = FALSE]
      color <- colors[[row$Stream_ID]]
      label <- paste0(row$Stream_Name, " [", row$LTER, "]")

      tags$div(
        style = "display: flex; align-items: center; gap: 8px; min-width: 0;",
        build_hydro_key(color),
        tags$span(
          style = "font-size: 0.84rem; color: #31424c; white-space: nowrap;",
          label
        )
      )
    })

    tags$div(
      style = paste(
        "display: grid;",
        "grid-template-columns: repeat(2, minmax(0, 1fr));",
        "column-gap: 20px;",
        "row-gap: 8px;",
        "padding: 0 10px 10px 10px;",
        "border-top: 1px solid #e1ebf0;"
      ),
      legend_items
    )
  })

  # --- RCS vs RBI for the 4 selected sites ---
  output$selected_rcs_rbi <- renderPlotly({
    plot_data <- hydrograph_data()

    if (is.null(plot_data) || nrow(plot_data) == 0) {
      return(plotly_empty())
    }

    colors <- hydro_color_map()
    site_meta <- plot_data %>%
      select(
        Stream_ID,
        Stream_Name,
        LTER,
        RBI,
        recession_slope,
        snow_cover
      ) %>%
      distinct()

    p <- plot_ly()
    label_annotations <- list()
    for (i in seq_len(nrow(site_meta))) {
      row <- site_meta[i, ]
      clr <- colors[[row$Stream_ID]]
      site_label <- paste0(row$LTER, " - ", row$Stream_Name)
      p <- p %>%
        add_trace(
          x = row$RBI,
          y = row$recession_slope,
          type = "scatter",
          mode = "markers",
          marker = list(
            color = clr,
            size = 14,
            line = list(color = "#7f878d", width = 0.9)
          ),
          name = site_label,
          hovertemplate = paste0(
            "<b>",
            site_label,
            "</b><br>",
            "RBI: ",
            round(row$RBI, 3),
            "<br>",
            "RCS: ",
            round(row$recession_slope, 3),
            "<extra></extra>"
          )
        )
      label_annotations[[i]] <- list(
        x = row$RBI,
        y = row$recession_slope,
        text = paste0("<b>", site_label, "</b>"),
        showarrow = FALSE,
        xshift = 12,
        yshift = 10,
        font = list(size = 14, color = clr),
        xanchor = "left",
        bgcolor = "rgba(255,255,255,0.7)",
        borderpad = 2
      )
    }

    # pad axes generously so labels don't get clipped
    rbi_vals <- site_meta$RBI
    rcs_vals <- site_meta$recession_slope
    rbi_span <- diff(range(rbi_vals))
    rcs_span <- diff(range(rcs_vals))
    rbi_pad <- if (rbi_span == 0) max(abs(rbi_vals[1]) * 0.15, 0.05) else rbi_span * 0.4
    rcs_pad <- if (rcs_span == 0) max(abs(rcs_vals[1]) * 0.15, 0.05) else rcs_span * 0.4

    p %>%
      layout(
        title = list(
          text = "Selected Sites in RBI-RCS Space",
          font = list(size = 17, color = "#24323d")
        ),
        xaxis = list(
          title = list(text = "RBI", font = list(size = 15)),
          tickfont = list(size = 13),
          gridcolor = "#d4e3f0",
          range = list(min(rbi_vals) - rbi_pad, max(rbi_vals) + rbi_pad)
        ),
        yaxis = list(
          title = list(text = "RCS", font = list(size = 15)),
          tickfont = list(size = 13),
          gridcolor = "#d4e3f0",
          range = list(min(rcs_vals) - rcs_pad, max(rcs_vals) + rcs_pad)
        ),
        paper_bgcolor = plotly_bg$paper_bgcolor,
        plot_bgcolor = plotly_bg$plot_bgcolor,
        showlegend = FALSE,
        annotations = label_annotations,
        margin = list(t = 65, r = 50, b = 60, l = 60),
        hovermode = "closest",
        hoverdistance = 12
      ) %>%
      polish_plotly()
  })

  # --- RCS vs RBI scatter for all Activity 1 sites -------------------------

  all_highlighted <- reactive({
    selected_sites()
  })

  output$rcs_rbi_plot <- renderPlotly({
    color_var_name <- if (is.null(input$rcs_rbi_color_by)) {
      "snow_cover"
    } else {
      input$rcs_rbi_color_by
    }
    color_var_label <- c(
      "snow_cover" = "Snow Cover (%)",
      "mean_annual_precip" = "MAP (mm)",
      "major_land" = "LULC"
    )[[color_var_name]]

    plot_data <- hydro_sites() %>%
      mutate(
        is_highlighted = Stream_ID %in% all_highlighted(),
        major_land_display = clean_land_use_label(major_land),
        color_value = if (color_var_name == "major_land") {
          major_land_display
        } else {
          .data[[color_var_name]]
        }
      ) %>%
      filter(!is.na(color_value))

    if (nrow(plot_data) == 0) {
      return(
        plotly_empty() %>%
          layout(
            title = list(
              text = "No sites are available for the RBI/RCS comparison",
              font = list(color = "#666", size = 14)
            )
          )
      )
    }

    hover_text <- paste0(
      "<b>",
      plot_data$Stream_Name,
      "</b><br>",
      "LTER: ",
      plot_data$LTER,
      "<br>",
      "Snow Cover: ",
      round(plot_data$snow_cover * 100, 0),
      "%<br>",
      "MAP: ",
      round(plot_data$mean_annual_precip, 0),
      " mm/yr<br>",
      "Land Use: ",
      plot_data$major_land_display,
      "<br>",
      "Snow Days/Year: ",
      round(plot_data$mean_snow_days, 0),
      "<br>",
      "Mean Peak Snow Cover: ",
      round(plot_data$mean_snow_prop_area * 100, 0),
      "%<br>",
      "Peak Snow Cover: ",
      round(plot_data$peak_snow_prop_area * 100, 0),
      "%<br>",
      "RBI: ",
      round(plot_data$RBI, 3),
      "<br>",
      "RCS: ",
      round(plot_data$recession_slope, 3)
    )

    p <- ggplot(
      plot_data,
      aes(
        x = RBI,
        y = recession_slope,
        fill = color_value,
        text = hover_text,
        key = Stream_ID
      )
    ) +
      geom_point(
        shape = 21,
        color = "#7f878d",
        size = 3,
        stroke = 0.3,
        alpha = 0.78
      ) +
      labs(
        x = "RBI",
        y = "RCS",
        fill = color_var_label
      ) +
      base_plot_theme

    if (color_var_name == "major_land") {
      land_levels <- land_use_legend_levels(plot_data$color_value)
      fallback_colors <- c(
        "#1b9e77",
        "#d95f02",
        "#7570b3",
        "#e7298a",
        "#66a61e",
        "#e6ab02",
        "#a6761d",
        "#666666"
      )
      land_palette <- setNames(
        rep(fallback_colors, length.out = length(land_levels)),
        land_levels
      )
      matched_levels <- intersect(names(land_use_colors), names(land_palette))
      land_palette[matched_levels] <- land_use_colors[matched_levels]

      p <- p +
        scale_fill_manual(
          values = land_palette,
          breaks = land_levels,
          na.translate = FALSE
        )
    } else if (color_var_name == "snow_cover") {
      p <- p +
        scale_fill_gradientn(
          colours = snow_palette,
          labels = scales::percent_format(accuracy = 1)
        )
    } else if (color_var_name == "mean_annual_precip") {
      p <- p +
        scale_fill_gradientn(
          colours = precip_palette,
          labels = scales::label_number(big.mark = ",", accuracy = 1)
        )
    } else {
      p <- p +
        scale_fill_viridis_c()
    }

    if (any(plot_data$is_highlighted)) {
      highlight_df <- filter(plot_data, is_highlighted) %>%
        mutate(
          hover = paste0(
            "<b>",
            Stream_Name,
            "</b><br>",
            "LTER: ",
            LTER,
            "<br>",
            "Snow Cover: ",
            round(snow_cover * 100, 0),
            "%<br>",
            "RBI: ",
            round(RBI, 3),
            "<br>",
            "RCS: ",
            round(recession_slope, 3)
          )
        )
      p <- p +
        geom_point(
          data = highlight_df,
          aes(
            x = RBI,
            y = recession_slope,
            fill = color_value,
            text = hover
          ),
          shape = 21,
          color = "#7f878d",
          size = 5,
          stroke = 0.45,
          alpha = 1,
          show.legend = FALSE,
          inherit.aes = FALSE
        ) +
        geom_text(
          data = highlight_df,
          aes(x = RBI, y = recession_slope, label = Stream_Name),
          hjust = -0.1,
          vjust = 0,
          size = 3,
          show.legend = FALSE,
          inherit.aes = FALSE
        )
    }

    ggplotly(p, tooltip = "text", source = "rcs_rbi") %>%
      layout(
        paper_bgcolor = plotly_bg$paper_bgcolor,
        plot_bgcolor = plotly_bg$plot_bgcolor,
        legend = right_side_legend(font_size = 10),
        margin = list(r = 170),
        title = FALSE
      ) %>%
      polish_plotly(register_click = TRUE)
  })

  # --- Activity 2: Stream Salinity ------------------------------------------

  # sites from harmonized partial that have Cl data
  cl_sites <- reactive({
    harmonized_partial() %>%
      filter(!is.na(mean_Cl_mgL), !is.na(Latitude), !is.na(Longitude)) %>%
      filter(!Stream_Name %in% chloride_excluded_stream_names)
  })

  cl_monthly <- reactive({
    read_app_data("cl_monthly.rds")
  })

  # populate the site dropdown for the seasonal plot
  observe({
    q_sites <- unique(discharge_monthly()$Stream_ID)
    sites <- cl_sites() %>%
      arrange(LTER, Stream_Name)
    choices <- setNames(
      sites$Stream_ID,
      ifelse(
        sites$Stream_ID %in% q_sites,
        paste0(sites$Stream_Name, " [", sites$LTER, "]"),
        paste0(sites$Stream_Name, " [", sites$LTER, "; Cl only]")
      )
    )
    updateSelectInput(session, "cl_site_select", choices = choices)
  })

  observeEvent(input$cl_map_marker_click, {
    click <- input$cl_map_marker_click
    req(click$id)
    updateSelectInput(session, "cl_site_select", selected = click$id)
  })

  observeEvent(input$cl_map_background, {
    req(identical(input$activity2_tab, "Chloride Map"))

    background_key <- input$cl_map_background
    if (is.null(background_key) || !background_key %in% names(activity2_background_focus_bounds)) {
      background_key <- "map"
    }

    bounds <- activity2_background_focus_bounds[[background_key]]

    leafletProxy("cl_map") %>%
      fitBounds(
        bounds$xmin,
        bounds$ymin,
        bounds$xmax,
        bounds$ymax
      )
  }, ignoreInit = TRUE)

  # --- Chloride Map ---------------------------------------------------------
  output$cl_map <- renderLeaflet({
    leaflet(
      options = leafletOptions(
        preferCanvas = TRUE,
        worldCopyJump = FALSE
      )
    ) %>%
      addProviderTiles(
        providers$CartoDB.PositronNoLabels,
        options = tileOptions(opacity = 0.58)
      ) %>%
      addProviderTiles(
        providers$CartoDB.PositronOnlyLabels,
        options = tileOptions(opacity = 0.68)
      ) %>%
      fitBounds(
        activity2_map_bounds$xmin,
        activity2_map_bounds$ymin,
        activity2_map_bounds$xmax,
        activity2_map_bounds$ymax
      ) %>%
      addScaleBar(position = "topleft", options = scaleBarOptions(imperial = FALSE))
  })

  outputOptions(output, "cl_map", suspendWhenHidden = FALSE)

  observe({
    req(identical(input$activity2_tab, "Chloride Map"))
    marker_data <- cl_sites()
    req(nrow(marker_data) > 0)

    available_backgrounds <- names(activity2_background_rasters_global)[
      vapply(activity2_background_rasters_global, Negate(is.null), logical(1))
    ]
    req(length(available_backgrounds) > 0)

    background_key <- input$cl_map_background
    if (is.null(background_key) || !background_key %in% available_backgrounds) {
      background_key <- available_backgrounds[[1]]
    }

    background_spec <- activity2_background_specs[[background_key]]
    background_raster <- activity2_background_rasters_global[[background_key]]
    req(!is.null(background_raster))

    display_background_raster <- background_raster
    if (identical(background_key, "cropland")) {
      display_background_raster <- terra::ifel(
        background_raster <= 0.5,
        NA,
        background_raster
      )
    } else if (identical(background_key, "impervious")) {
      display_background_raster <- terra::ifel(
        background_raster <= 0.1,
        NA,
        background_raster
      )
    }

    cl_point_palette <- c(
      "#f8f1f5",
      "#eddbe7",
      "#ddb9cf",
      "#ca92b3",
      "#b36d95",
      "#975379",
      "#7a3e62",
      "#5f2f4c"
    )
    linear_cl_values <- marker_data$mean_Cl_mgL[
      is.finite(marker_data$mean_Cl_mgL)
    ]
    req(length(linear_cl_values) > 0)
    linear_cl_domain <- c(0, max(linear_cl_values))

    marker_data <- marker_data %>%
      mutate(mean_Cl_color_value = mean_Cl_mgL)

    cl_pal <- colorNumeric(
      palette = cl_point_palette,
      domain = linear_cl_domain
    )
    cl_legend_title <- "Mean Cl (mg/L)"
    cl_legend_values <- c(0, linear_cl_values)

    background_vals <- terra::values(display_background_raster, mat = FALSE)
    background_vals <- background_vals[is.finite(background_vals)]
    background_pal <- colorBin(
      palette = background_spec$colors,
      domain = background_vals,
      bins = background_spec$breaks,
      na.color = "transparent",
      right = FALSE
    )
    background_opacity <- if (identical(background_key, "map")) 0.84 else 0.72

    leafletProxy("cl_map", data = marker_data) %>%
      clearImages() %>%
      clearMarkers() %>%
      clearControls() %>%
      addRasterImage(
        display_background_raster,
        colors = background_pal,
        opacity = background_opacity,
        project = TRUE,
        method = "bilinear",
        maxBytes = 40 * 1024 * 1024
      ) %>%
      addScaleBar(position = "topleft", options = scaleBarOptions(imperial = FALSE)) %>%
      addCircleMarkers(
        lng = ~Longitude,
        lat = ~Latitude,
        radius = 6.5,
        fillColor = ~cl_pal(mean_Cl_color_value),
        color = "#f8fbfc",
        weight = 1.15,
        opacity = 0.84,
        fillOpacity = 0.76,
        layerId = ~Stream_ID,
        popup = ~ paste0(
          "<b>",
          Stream_Name,
          "</b><br>",
          "Mean Cl: ",
          round(mean_Cl_mgL, 1),
          " mg/L"
        )
      ) %>%
      addControl(
        html = build_categorical_legend(
          title = background_spec$label,
          legend_items = setNames(
            rev(background_spec$colors),
            rev(background_spec$labels)
          )
        ),
        position = "bottomleft"
      ) %>%
      addControl(
        html = build_numeric_legend(
          title = cl_legend_title,
          values = cl_legend_values,
          legend_colors = cl_point_palette,
          digits = 0,
          n_breaks = 4
        ),
        position = "bottomright"
      )
  })

  # --- Seasonal Cl & Discharge plot -----------------------------------------

  output$cl_seasonal_plot <- renderPlotly({
    req(input$cl_site_select)

    site_id <- input$cl_site_select

    # monthly Cl for this site
    cl_data <- cl_monthly() %>%
      filter(Stream_ID == site_id) %>%
      arrange(month)

    if (nrow(cl_data) == 0) {
      return(
        plotly_empty() %>%
          layout(
            title = list(
              text = "No chloride data for this site",
              font = list(color = "#666", size = 14)
            )
          )
      )
    }

    site_name <- cl_data$Stream_Name[1]
    site_lter <- cl_data$LTER[1]
    q_data <- discharge_monthly() %>%
      filter(Stream_ID == site_id) %>%
      arrange(month)

    p <- plot_ly() %>%
      add_trace(
        data = cl_data,
        x = ~month,
        y = ~mean_Cl_mgL,
        type = "scatter",
        mode = "lines+markers",
        name = "Mean Cl (mg/L)",
        showlegend = FALSE,
        line = list(color = activity2_cl_accent, width = 3),
        marker = list(color = activity2_cl_accent, size = 8),
        hovertemplate = paste0(
          "Month: %{x}<br>",
          "Mean Cl: %{y:.1f} mg/L<br>",
          "<extra></extra>"
        )
      )

    # dual y-axis with discharge if toggled on
    if (isTRUE(input$cl_show_discharge)) {
      if (nrow(q_data) > 0) {
        p <- p %>%
          add_trace(
            data = q_data,
            x = ~month,
            y = ~mean_Q_cms,
            type = "scatter",
            mode = "lines",
            name = "Mean Q (cms)",
            showlegend = FALSE,
            yaxis = "y2",
            line = list(color = activity2_q_accent, width = 3.2, dash = "dash"),
            hovertemplate = paste0(
              "Month: %{x}<br>",
              "Mean Q: %{y:.3f} cms<br>",
              "<extra></extra>"
            )
          )
      }
    }

    missing_q_note <- if (isTRUE(input$cl_show_discharge)) {
      if (nrow(q_data) == 0) {
        "<br><sup>No monthly discharge is available for this site in the local data.</sup>"
      } else {
        ""
      }
    } else {
      ""
    }

    y2_config <- if (isTRUE(input$cl_show_discharge)) {
      list(
        title = list(
          text = "Mean Discharge (cms)",
          font = list(color = activity2_q_accent)
        ),
        overlaying = "y",
        side = "right",
        showgrid = FALSE,
        tickfont = list(color = activity2_q_accent)
      )
    } else {
      list(overlaying = "y", side = "right", visible = FALSE)
    }

    p %>%
      layout(
        title = list(
          text = paste0(site_name, " (", site_lter, ")", missing_q_note),
          font = list(size = 14, color = "#2d2926")
        ),
        xaxis = list(
          title = "Month",
          tickmode = "array",
          tickvals = 1:12,
          ticktext = month_labels,
          gridcolor = "#d4e3f0"
        ),
        yaxis = list(
          title = list(
            text = "Mean Chloride (mg/L)",
            font = list(color = activity2_cl_accent)
          ),
          gridcolor = "#d4e3f0",
          tickfont = list(color = activity2_cl_accent)
        ),
        yaxis2 = y2_config,
        paper_bgcolor = plotly_bg$paper_bgcolor,
        plot_bgcolor = plotly_bg$plot_bgcolor,
        margin = list(r = if (isTRUE(input$cl_show_discharge)) 90 else 40),
        showlegend = FALSE,
        hovermode = "x unified"
      ) %>%
      polish_plotly()
  })

  output$cl_seasonal_plot_legend <- renderUI({
    has_discharge <- isTRUE(input$cl_show_discharge) &&
      !is.null(input$cl_site_select) &&
      nrow(
        discharge_monthly() %>%
          filter(Stream_ID == input$cl_site_select)
      ) > 0

    chloride_key <- tags$div(
      style = "display: flex; align-items: center; gap: 8px;",
      tags$span(
        style = "display: inline-flex; align-items: center; width: 34px; position: relative;",
        tags$span(
          style = paste0(
            "display: block; width: 28px; border-top: 3px solid ",
            activity2_cl_accent,
            ";"
          )
        ),
        tags$span(
          style = paste0(
            "position: absolute; left: 10px; top: -3px;",
            "width: 8px; height: 8px; border-radius: 50%;",
            "background: ",
            activity2_cl_accent,
            ";"
          )
        )
      ),
      tags$span(
        style = "font-size: 0.84rem; color: #31424c;",
        "Mean Chloride (mg/L)"
      )
    )

    discharge_key <- tags$div(
      style = "display: flex; align-items: center; gap: 8px;",
      tags$span(
        style = "display: inline-flex; align-items: center; width: 34px;",
        tags$span(
          style = paste0(
            "display: block; width: 28px; border-top: 3px dashed ",
            activity2_q_accent,
            ";"
          )
        )
      ),
      tags$span(
        style = "font-size: 0.84rem; color: #31424c;",
        "Mean Discharge (cms)"
      )
    )

    tags$div(
      style = paste(
        "display: grid;",
        "grid-template-columns: repeat(2, minmax(0, 1fr));",
        "column-gap: 20px;",
        "row-gap: 8px;",
        "padding: 0 10px 10px 10px;",
        "border-top: 1px solid #e1ebf0;"
      ),
      chloride_key,
      if (has_discharge) discharge_key else NULL
    )
  })

  # --- Activity 3: C-Q Analysis -----------------------------------------------

  cq_paired_data <- reactive({
    read_app_data("cq_paired.rds")
  })

  cq_slopes_data <- reactive({
    read_app_data("cq_slopes.rds")
  })

  cq_solute_choices <- c(
    "Chloride (Cl)" = "Cl",
    "Nitrate (NO3)" = "NO3"
  )

  # populate site dropdown — only sites that have C-Q slopes AND discharge data
  observe({
    has_q <- unique(discharge_data()$Stream_ID)
    eligible_sites <- cq_paired_data() %>%
      count(Stream_ID, LTER, Stream_Name, variable, name = "n_paired") %>%
      group_by(Stream_ID, LTER, Stream_Name) %>%
      summarise(max_paired = max(n_paired, na.rm = TRUE), .groups = "drop")

    sites <- eligible_sites %>%
      filter(Stream_ID %in% has_q) %>%
      filter(max_paired >= 3) %>%
      select(Stream_ID, LTER, Stream_Name) %>%
      distinct() %>%
      arrange(LTER, Stream_Name)
    choices <- setNames(
      sites$Stream_ID,
      paste0(sites$Stream_Name, " [", sites$LTER, "]")
    )
    updateSelectInput(session, "cq_sites", choices = choices)
    updateSelectInput(session, "cq_ts_site", choices = choices)
  })

  # update solute checkboxes to only show available solutes
  observe({
    available <- unique(cq_slopes_data()$variable)
    scatter_choices <- cq_solute_choices[cq_solute_choices %in% available]
    updateCheckboxGroupInput(
      session,
      "cq_solutes",
      choices = scatter_choices,
      selected = character(0)
    )
  })

  # enforce max 3 sites
  observe({
    if (length(input$cq_sites) > 3) {
      updateSelectInput(session, "cq_sites", selected = input$cq_sites[1:3])
    }
  })

  cq_site_symbols <- c(
    "circle-open",
    "x-thin",
    "triangle-up",
    "square-open"
  )
  cq_site_symbol_labels <- c(
    "circle-open" = "Open Circle",
    "x-thin" = "X",
    "triangle-up" = "Triangle",
    "square-open" = "Open Square"
  )
  cq_site_symbol_glyphs <- c(
    "circle-open" = "○",
    "x-thin" = "×",
    "triangle-up" = "▲",
    "square-open" = "□"
  )
  cq_site_dashes <- c(
    "solid",
    "dash",
    "dot",
    "longdash"
  )

  # --- C-Q Monthly Hydrograph -------------------------------------------------

  output$cq_timeseries_plot <- renderPlotly({
    req(input$cq_ts_site)

    site_id <- input$cq_ts_site
    has_conc <- length(input$cq_ts_solutes) > 0

    # average monthly discharge for this site
    q_data <- discharge_monthly() %>%
      filter(Stream_ID == site_id) %>%
      arrange(month)

    if (nrow(q_data) == 0) {
      return(
        plotly_empty() %>%
          layout(
            title = list(
              text = "No discharge data for this site",
              font = list(color = "#666", size = 14)
            )
          )
      )
    }

    site_name <- q_data$Stream_Name[1]
    site_lter <- q_data$LTER[1]

    p <- plot_ly() %>%
      add_trace(
        data = q_data,
        x = ~month,
        y = ~mean_Q_cms,
        type = "scatter",
        mode = "lines",
        name = "Mean Q (cms)",
        yaxis = if (has_conc) "y2" else "y",
        line = list(color = activity2_q_accent, width = 3.2, dash = "dash"),
        hovertemplate = "Month: %{x}<br>Mean Q: %{y:.4f} cms<extra></extra>"
      )

    if (length(input$cq_ts_solutes) > 0) {
      chem <- cq_paired_data() %>%
        filter(Stream_ID == site_id, variable %in% input$cq_ts_solutes) %>%
        mutate(month = as.integer(format(date, "%m"))) %>%
        group_by(Stream_ID, Stream_Name, LTER, variable, month) %>%
        summarise(mean_value = mean(value, na.rm = TRUE), n_obs = n(), .groups = "drop") %>%
        group_by(variable) %>%
        mutate(
          plot_value = if (isTRUE(input$cq_ts_normalize)) {
            value_sd <- sd(mean_value, na.rm = TRUE)
            if (is.finite(value_sd) && value_sd > 0) {
              (mean_value - mean(mean_value, na.rm = TRUE)) / value_sd
            } else {
              rep(0, dplyr::n())
            }
          } else {
            mean_value
          }
        ) %>%
        ungroup()

      for (sol in input$cq_ts_solutes) {
        sol_data <- filter(chem, variable == sol)
        if (nrow(sol_data) == 0) {
          next
        }
        sol_label <- names(cq_solute_choices)[cq_solute_choices == sol]
        p <- p %>%
          add_trace(
            data = sol_data,
            x = ~month,
            y = ~plot_value,
            type = "scatter",
            mode = "lines+markers",
            name = sol_label,
            yaxis = "y",
            line = list(color = solute_colors[[sol]], width = 2),
            marker = list(color = solute_colors[[sol]], size = 6, opacity = 0.8),
            hovertemplate = paste0(
              sol_label,
              "<br>Month: %{x}<br>",
              if (isTRUE(input$cq_ts_normalize)) {
                "Z-score: %{y:.2f}<br>Mean conc: %{customdata:.2f}<extra></extra>"
              } else {
                "Mean conc: %{y:.2f}<extra></extra>"
              }
            ),
            customdata = ~mean_value
          )
      }
    }

    chemistry_axis_title <- if (isTRUE(input$cq_ts_normalize)) {
      "Normalized Concentration (z-score)"
    } else {
      "Concentration (mg/L)"
    }

    p %>%
      layout(
        title = list(
          text = paste0(site_name, " (", site_lter, ")"),
          font = list(size = 14, color = "#2d2926")
        ),
        xaxis = list(
          title = "Month",
          tickmode = "array",
          tickvals = 1:12,
          ticktext = month_labels,
          gridcolor = "#d4e3f0",
          zeroline = FALSE
        ),
        yaxis = list(
          title = list(
            text = if (has_conc) chemistry_axis_title else "Mean Discharge (cms)",
            font = list(color = if (has_conc) "#2d2926" else activity2_q_accent)
          ),
          gridcolor = "#d4e3f0",
          tickfont = list(color = if (has_conc) "#2d2926" else activity2_q_accent),
          zeroline = FALSE
        ),
        yaxis2 = if (has_conc) {
          list(
            title = list(
              text = "Mean Discharge (cms)",
              font = list(color = activity2_q_accent)
            ),
            overlaying = "y",
            side = "right",
            showgrid = FALSE,
            tickfont = list(color = activity2_q_accent),
            zeroline = FALSE
          )
        } else {
          list(overlaying = "y", side = "right", visible = FALSE)
        },
        paper_bgcolor = plotly_bg$paper_bgcolor,
        plot_bgcolor = plotly_bg$plot_bgcolor,
        showlegend = FALSE,
        hovermode = "closest",
        margin = list(r = if (has_conc) 95 else 55)
      ) %>%
      polish_plotly()
  })

  output$cq_timeseries_plot_legend <- renderUI({
    req(input$cq_ts_site)

    solute_entries <- lapply(input$cq_ts_solutes, function(sol) {
      sol_label <- names(cq_solute_choices)[cq_solute_choices == sol]
      tags$div(
        style = "display: flex; align-items: center; gap: 8px;",
        tags$span(
          style = "display: inline-flex; align-items: center; width: 34px; position: relative;",
          tags$span(
            style = paste0(
              "display: block; width: 28px; border-top: 3px solid ",
              solute_colors[[sol]],
              ";"
            )
          ),
          tags$span(
            style = paste0(
              "position: absolute; left: 10px; top: -3px;",
              "width: 8px; height: 8px; border-radius: 50%;",
              "background: ",
              solute_colors[[sol]],
              ";"
            )
          )
        ),
        tags$span(
          style = "font-size: 0.84rem; color: #31424c;",
          sol_label
        )
      )
    })

    discharge_entry <- tags$div(
      style = "display: flex; align-items: center; gap: 8px;",
      tags$span(
        style = "display: inline-flex; align-items: center; width: 34px;",
        tags$span(
          style = paste0(
            "display: block; width: 28px; border-top: 3px dashed ",
            activity2_q_accent,
            ";"
          )
        )
      ),
      tags$span(
        style = "font-size: 0.84rem; color: #31424c;",
        "Mean Discharge (cms)"
      )
    )

    legend_entries <- c(solute_entries, list(discharge_entry))

    tags$div(
      style = paste(
        "display: grid;",
        "grid-template-columns: repeat(2, minmax(0, 1fr));",
        "column-gap: 20px;",
        "row-gap: 8px;",
        "padding: 0 10px 10px 10px;",
        "border-top: 1px solid #e1ebf0;"
      ),
      legend_entries
    )
  })

  cq_trendline_summaries <- reactive({
    req(input$cq_sites, input$cq_solutes)

    paired <- cq_paired_data() %>%
      filter(Stream_ID %in% input$cq_sites, variable %in% input$cq_solutes)

    if (!isTRUE(input$cq_show_trendline) || nrow(paired) == 0) {
      return(NULL)
    }

    selected_site_ids <- input$cq_sites
    site_symbol_map <- setNames(
      cq_site_symbols[seq_len(length(selected_site_ids))],
      selected_site_ids
    )
    solute_site_color_maps <- lapply(names(cq_solute_shade_palettes), function(sol) {
      setNames(select_cq_site_colors(sol, length(selected_site_ids)), selected_site_ids)
    })
    names(solute_site_color_maps) <- names(cq_solute_shade_palettes)

    paired %>%
      group_by(Stream_ID, Stream_Name, LTER, variable) %>%
      group_modify(~ {
        if (nrow(.x) < 10) {
          return(data.frame(
            n_obs = nrow(.x),
            intercept = NA_real_,
            slope = NA_real_,
            r2 = NA_real_
          ))
        }

        mod <- lm(log10(value) ~ log10(Q), data = .x)
        data.frame(
          n_obs = nrow(.x),
          intercept = unname(coef(mod)[1]),
          slope = unname(coef(mod)[2]),
          r2 = summary(mod)$r.squared
        )
      }) %>%
      ungroup() %>%
      mutate(
        solute_label = names(cq_solute_choices)[match(variable, cq_solute_choices)],
        solute_color = mapply(
          function(variable, Stream_ID) {
            unname(solute_site_color_maps[[variable]][[Stream_ID]])
          },
          variable,
          Stream_ID
        ),
        site_symbol = site_symbol_map[Stream_ID],
        site_symbol_label = cq_site_symbol_labels[site_symbol],
        site_symbol_glyph = cq_site_symbol_glyphs[site_symbol],
        site_order = match(Stream_ID, selected_site_ids)
      ) %>%
      arrange(site_order, variable)
  })

  # --- C-Q Scatter Plot -------------------------------------------------------

  output$cq_scatter_plot <- renderPlotly({
    req(input$cq_sites, input$cq_solutes)

    paired <- cq_paired_data() %>%
      filter(Stream_ID %in% input$cq_sites, variable %in% input$cq_solutes)

    if (nrow(paired) == 0) {
      return(
        plotly_empty() %>%
          layout(
            title = list(
              text = "No paired C-Q data for selected sites/solutes",
              font = list(color = "#666", size = 14)
            )
          )
      )
    }

    # build one trace per site×solute combo
    combos <- paired %>%
      select(Stream_ID, Stream_Name, LTER, variable) %>%
      distinct()

    selected_site_ids <- input$cq_sites
    site_symbol_map <- setNames(
      cq_site_symbols[seq_len(length(selected_site_ids))],
      selected_site_ids
    )
    site_dash_map <- setNames(
      cq_site_dashes[seq_len(length(selected_site_ids))],
      selected_site_ids
    )
    solute_site_color_maps <- lapply(names(cq_solute_shade_palettes), function(sol) {
      setNames(select_cq_site_colors(sol, length(selected_site_ids)), selected_site_ids)
    })
    names(solute_site_color_maps) <- names(cq_solute_shade_palettes)

    p <- plot_ly()

    for (i in seq_len(nrow(combos))) {
      row <- combos[i, ]
      d <- paired %>%
        filter(Stream_ID == row$Stream_ID, variable == row$variable)

      solute_label <- names(cq_solute_choices)[
        cq_solute_choices == row$variable
      ]
      trace_name <- paste0(row$Stream_Name, " — ", solute_label)
      clr <- unname(solute_site_color_maps[[row$variable]][[row$Stream_ID]])
      symbol <- unname(site_symbol_map[[row$Stream_ID]])
      dash <- unname(site_dash_map[[row$Stream_ID]])
      symbol_size <- if (identical(symbol, "x-thin")) 9.2 else 9.5
      symbol_line_width <- if (identical(symbol, "x-thin")) 1.2 else 1.1
      symbol_opacity <- if (identical(symbol, "x-thin")) 1 else 0.85

      p <- p %>%
        add_trace(
          data = d,
          x = ~ log10(Q),
          y = ~ log10(value),
          type = "scatter",
          mode = "markers",
          name = trace_name,
          marker = list(
            color = clr,
            size = symbol_size,
            opacity = symbol_opacity,
            symbol = symbol,
            line = list(color = clr, width = symbol_line_width)
          ),
          hovertemplate = paste0(
            row$Stream_Name,
            "<br>",
            solute_label,
            "<br>",
            "Q: %{customdata:.4f} cms<br>",
            "C: %{meta:.2f}<br>",
            "<extra></extra>"
          ),
          customdata = d$Q,
          meta = d$value,
          showlegend = FALSE
        )

      # optional trendline + annotation
      if (isTRUE(input$cq_show_trendline) && nrow(d) >= 10) {
        mod <- lm(log10(value) ~ log10(Q), data = d)

        x_range <- range(log10(d$Q))
        x_seq <- seq(x_range[1], x_range[2], length.out = 50)
        y_seq <- coef(mod)[1] + coef(mod)[2] * x_seq

        p <- p %>%
          add_trace(
            x = x_seq,
            y = y_seq,
            type = "scatter",
            mode = "lines",
            name = paste0(trace_name, " fit"),
            line = list(color = clr, width = 2.5, dash = dash),
            hoverinfo = "skip",
            showlegend = FALSE
          )
      }
    }

    p %>%
      layout(
        xaxis = list(
          title = "log\u2081\u2080(Discharge, cms)",
          gridcolor = "#d4e3f0"
        ),
        yaxis = list(
          title = "log\u2081\u2080(Concentration)",
          gridcolor = "#d4e3f0"
        ),
        paper_bgcolor = plotly_bg$paper_bgcolor,
        plot_bgcolor = plotly_bg$plot_bgcolor,
        showlegend = FALSE,
        margin = list(r = 30, b = 20)
      ) %>%
      polish_plotly()
  })

  output$cq_scatter_legend <- renderUI({
    req(input$cq_sites, input$cq_solutes)

    selected_site_ids <- input$cq_sites
    selected_site_count <- length(selected_site_ids)
    site_symbol_map <- setNames(
      cq_site_symbols[seq_len(length(selected_site_ids))],
      selected_site_ids
    )
    solute_site_color_maps <- lapply(input$cq_solutes, function(sol) {
      setNames(select_cq_site_colors(sol, selected_site_count), selected_site_ids)
    })
    names(solute_site_color_maps) <- input$cq_solutes

    selected_sites <- cq_slopes_data() %>%
      filter(Stream_ID %in% selected_site_ids) %>%
      select(Stream_ID, Stream_Name) %>%
      distinct() %>%
      mutate(site_order = match(Stream_ID, selected_site_ids)) %>%
      arrange(site_order)

    selected_solutes <- input$cq_solutes

    tags$div(
      style = paste(
        "overflow-x: auto;",
        "border: 1px solid #d7e3ea;",
        "border-radius: 12px;",
        "background: rgba(255,255,255,0.86);",
        "margin: 0 8px 8px;"
      ),
      tags$table(
        style = paste(
          "width: 100%;",
          "border-collapse: collapse;",
          "font-size: 0.84rem;",
          "line-height: 1.35;"
        ),
        tags$thead(
          tags$tr(
            style = "background: #f4f7f9; color: #24323d;",
            tags$th(style = "text-align:left; padding: 10px 12px; border-bottom: 1px solid #d7e3ea;", "Site"),
            tags$th(style = "text-align:center; padding: 10px 12px; border-bottom: 1px solid #d7e3ea; width: 90px;", "Symbol"),
            lapply(selected_solutes, function(sol) {
              tags$th(
                style = "text-align:center; padding: 10px 12px; border-bottom: 1px solid #d7e3ea; min-width: 120px;",
                names(cq_solute_choices)[cq_solute_choices == sol]
              )
            })
          )
        ),
        tags$tbody(
          lapply(seq_len(nrow(selected_sites)), function(i) {
            row <- selected_sites[i, ]
            symbol_name <- site_symbol_map[[row$Stream_ID]]

            tags$tr(
              style = "border-bottom: 1px solid #e5edf2;",
              tags$td(
                style = "padding: 10px 12px; vertical-align: middle; color: #24323d;",
                row$Stream_Name
              ),
              tags$td(
                style = "padding: 10px 12px; vertical-align: middle; text-align: center; color: #4f616b; font-size: 1.05rem; font-weight: 700;",
                cq_site_symbol_glyphs[[symbol_name]]
              ),
              lapply(selected_solutes, function(sol) {
                clr <- unname(solute_site_color_maps[[sol]][[row$Stream_ID]])
                tags$td(
                  style = "padding: 10px 12px; vertical-align: middle; text-align: center;",
                  tags$span(
                    style = "display: inline-flex; align-items: center; gap: 8px;",
                    tags$span(
                      style = paste0(
                        "display:inline-block;",
                        "width:24px;",
                        "height:3px;",
                        "border-radius:999px;",
                        "background:", clr, ";"
                      )
                    ),
                    tags$span(
                      style = paste0(
                        "display:inline-block;",
                        "width:10px;",
                        "height:10px;",
                        "border-radius:999px;",
                        "background:", clr, ";"
                      )
                    )
                  )
                )
              })
            )
          })
        )
      )
    )
  })

  output$cq_fit_summaries <- renderUI({
    trendlines <- cq_trendline_summaries()

    if (!isTRUE(input$cq_show_trendline)) {
      return(
        tags$p(
          "Turn on trendlines in the sidebar to show fitted equations and R-squared values here.",
          style = "color: #5d6d76; margin-bottom: 0;"
        )
      )
    }

    if (is.null(trendlines) || nrow(trendlines) == 0) {
      return(
        tags$p(
          "No fitted C-Q lines are available for the current site and solute selection.",
          style = "color: #5d6d76; margin-bottom: 0;"
        )
      )
    }

    tags$div(
      style = paste(
        "overflow-x: auto;",
        "border: 1px solid #d7e3ea;",
        "border-radius: 12px;",
        "background: rgba(255,255,255,0.86);"
      ),
      tags$table(
        style = paste(
          "width: 100%;",
          "border-collapse: collapse;",
          "font-size: 0.84rem;",
          "line-height: 1.35;"
        ),
        tags$thead(
          tags$tr(
            style = "background: #f4f7f9; color: #24323d;",
            tags$th(style = "text-align:left; padding: 10px 12px; border-bottom: 1px solid #d7e3ea;", "Site"),
            tags$th(style = "text-align:left; padding: 10px 12px; border-bottom: 1px solid #d7e3ea;", "Solute"),
            tags$th(style = "text-align:left; padding: 10px 12px; border-bottom: 1px solid #d7e3ea;", "Fit"),
            tags$th(style = "text-align:right; padding: 10px 12px; border-bottom: 1px solid #d7e3ea;", "Slope"),
            tags$th(style = "text-align:right; padding: 10px 12px; border-bottom: 1px solid #d7e3ea;", "R²"),
            tags$th(style = "text-align:right; padding: 10px 12px; border-bottom: 1px solid #d7e3ea;", "n")
          )
        ),
        tags$tbody(
          lapply(seq_len(nrow(trendlines)), function(i) {
            row <- trendlines[i, ]

            if (is.na(row$slope) || is.na(row$intercept) || is.na(row$r2)) {
              fit_text <- "Not enough paired observations to fit a line"
              slope_text <- "\u2014"
              r2_text <- "\u2014"
            } else {
              fit_text <- sprintf(
                "log10(C) = %.3f + %.3f x log10(Q)",
                row$intercept,
                row$slope
              )
              slope_text <- sprintf("%.3f", row$slope)
              r2_text <- sprintf("%.3f", row$r2)
            }

            tags$tr(
              style = "border-bottom: 1px solid #e5edf2;",
              tags$td(
                style = "padding: 10px 12px; vertical-align: top; color: #24323d;",
                tags$div(
                  style = "display: flex; align-items: center; gap: 0.45rem;",
                  tags$span(
                    style = "min-width: 18px; font-size: 1rem; color: #4f616b; text-align: center;",
                    row$site_symbol_glyph
                  ),
                  tags$span(row$Stream_Name)
                )
              ),
              tags$td(
                style = "padding: 10px 12px; vertical-align: top; color: #24323d;",
                tags$div(
                  style = "display: flex; align-items: center; gap: 0.45rem;",
                  tags$span(
                    style = paste0(
                      "display:inline-block;",
                      "width:12px;",
                      "height:12px;",
                      "border-radius:999px;",
                      "background:", row$solute_color, ";"
                    )
                  ),
                  tags$span(row$solute_label)
                )
              ),
              tags$td(
                style = "padding: 10px 12px; vertical-align: top; font-family: 'SFMono-Regular', 'Menlo', monospace; color: #24323d;",
                fit_text
              ),
              tags$td(
                style = "padding: 10px 12px; vertical-align: top; text-align: right; color: #24323d;",
                slope_text
              ),
              tags$td(
                style = "padding: 10px 12px; vertical-align: top; text-align: right; color: #24323d;",
                r2_text
              ),
              tags$td(
                style = "padding: 10px 12px; vertical-align: top; text-align: right; color: #24323d;",
                as.integer(row$n_obs)
              )
            )
          })
        )
      )
    )
  })

  # --- C-Q Slope Histogram ----------------------------------------------------

  output$cq_histogram <- renderPlotly({
    req(input$cq_hist_solutes)

    slopes <- cq_slopes_data() %>%
      filter(variable %in% input$cq_hist_solutes)

    if (nrow(slopes) == 0) {
      return(
        plotly_empty() %>%
          layout(
            title = list(
              text = "No C-Q slopes available",
              font = list(color = "#666", size = 14)
            )
          )
      )
    }

    # y-range for annotation placement (use combined data)
    bin_edges <- seq(
      floor(min(slopes$cq_slope, na.rm = TRUE) / 0.025) * 0.025,
      ceiling(max(slopes$cq_slope, na.rm = TRUE) / 0.025) * 0.025,
      by = 0.025
    )
    hist_obj <- hist(slopes$cq_slope, breaks = bin_edges, plot = FALSE)
    y_max <- max(hist_obj$counts) * 1.1

    p <- plot_ly()
    for (sol in input$cq_hist_solutes) {
      sol_data <- filter(slopes, variable == sol)
      sol_label <- names(cq_solute_choices)[cq_solute_choices == sol]
      p <- p %>%
        add_histogram(
          x = sol_data$cq_slope,
          name = sol_label,
          marker = list(
            color = paste0(solute_colors[[sol]], "99"),
            line = list(color = solute_colors[[sol]], width = 1)
          ),
          xbins = list(
            start = min(bin_edges),
            end = max(bin_edges),
            size = 0.025
          ),
          hovertemplate = paste0(
            sol_label,
            "<br>Slope: %{x:.2f}<br>Count: %{y}<extra></extra>"
          )
        )
    }

    p %>%
      layout(
        barmode = "overlay",
        title = list(
          text = "C-Q Slope Distribution \u2014 Cl vs NO3",
          font = list(size = 14, color = "#2d2926")
        ),
        xaxis = list(title = "C-Q Slope", gridcolor = "#d4e3f0"),
        yaxis = list(title = "Number of Sites", gridcolor = "#d4e3f0"),
        paper_bgcolor = plotly_bg$paper_bgcolor,
        plot_bgcolor = plotly_bg$plot_bgcolor,
        legend = right_side_legend(font_size = 10),
        margin = list(r = 170),
        shapes = list(
          list(
            type = "line",
            x0 = -0.1,
            x1 = -0.1,
            y0 = 0,
            y1 = y_max,
            line = list(color = "#2d2926", width = 1.5, dash = "dash")
          ),
          list(
            type = "line",
            x0 = 0.1,
            x1 = 0.1,
            y0 = 0,
            y1 = y_max,
            line = list(color = "#2d2926", width = 1.5, dash = "dash")
          )
        ),
        annotations = list(
          list(
            x = -0.1,
            y = y_max * 0.95,
            text = "\u2190 Dilution",
            showarrow = FALSE,
            xanchor = "right",
            font = list(size = 12, color = "#666"),
            xshift = -6
          ),
          list(
            x = 0,
            y = y_max * 0.95,
            text = "Chemostatic",
            showarrow = FALSE,
            xanchor = "center",
            font = list(size = 8, color = "#999")
          ),
          list(
            x = 0.1,
            y = y_max * 0.95,
            text = "Enrichment \u2192",
            showarrow = FALSE,
            xanchor = "left",
            font = list(size = 12, color = "#666"),
            xshift = 6
          )
        )
      ) %>%
      polish_plotly()
  })
}

shinyApp(ui = ui, server = server)

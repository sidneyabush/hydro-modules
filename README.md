# Stream Hydrology Teaching Module

This repo holds the Shiny app plus the small data workflow that feeds it.

## Main pieces

- `app.R`: the Shiny app
- `data_harmonization.R`: reads the raw input files and writes the working CSVs
- `prep_data.R`: turns those CSVs into the `.rds` files the app reads
- `data/raw_inputs/`: raw files copied in for local updates
- `data/`: app-ready CSV and `.rds` outputs
- `docs/shiny-workflow.md`: a few notes on how the pieces fit together

## App flow

- `Overview`: site map plus climate, snow, and hydrology context
- `Activity 1`: start with precipitation vs snow cover, then compare `RCS` vs `RBI`, then look at average hydrographs
- `Activity 2`: use the map to explore chloride, then click a site to send it to the seasonal `Cl` and discharge plot
- `Activity 3`: compare average seasonal patterns, direct `C-Q` relationships, and slope distributions for `Cl` and `NO3`
- `About`: space for module links, funding notes, and credits

## Updating the data

Copy the latest raw files into `data/raw_inputs/`. The current workflow expects:

- `20260105_masterdata_chem.csv`
- `20260106_masterdata_discharge.csv`
- `Koeppen_Geiger_2.csv`
- `all-data_si-extract_2_20250325.csv`
- `DSi_LULC_filled_interpolated_Simple.csv`
- `na_1981_2010_annual_precip.tif`
- `land_cover_2020v2_30m.tif`

Then run:

```bash
Rscript data_harmonization.R data data
Rscript prep_data.R data data
Rscript -e "shiny::runApp()"
```

## Deploying to shinyapps.io

The easiest way to publish this app for feedback is to deploy only the runtime files, not the full repo.

This repo now includes:

- `.rscignore` to keep raw inputs, workflow CSVs, and other local-only files out of a shinyapps.io bundle
- `deploy_shinyapps.R` to deploy just `app.R`, the app-ready `.rds` files, and the three Activity 2 raster backgrounds

If your shinyapps.io account is already connected in `rsconnect`, run:

```bash
Rscript deploy_shinyapps.R
```

If you need to connect the account first, use the command shown in the shinyapps.io dashboard under `Tokens`, then run the deploy script again.

## Files written by the workflow

`data_harmonization.R` writes:

- `harmonized_north_america_partial.csv`
- `harmonized_north_america_complete.csv`
- `discharge_north_america.csv`
- `cl_monthly_summary.csv`
- `cq_paired_obs.csv`
- `cq_slopes.csv`

`prep_data.R` writes:

- `harmonized_complete.rds`
  This is the stricter Activity 1 table. It keeps only sites that have `RBI`, `RCS`, climate zone, mean annual precipitation, `snow_cover`, and dominant land-cover information.
- `harmonized_partial.rds`
  This is the broader app table. It keeps the larger North American site set even when some Activity 1 fields are missing, so the overview map and chloride activity can still use those sites.
- `discharge.rds`
- `cl_monthly.rds`
- `cq_paired.rds`
- `cq_slopes.rds`
- `activity2_map_precip_mm.tif`
- `activity2_map_cropland_pct.tif`
- `activity2_map_impervious_pct.tif`

## A couple notes

- The app reads from `data/` by default.
- The yearly climate columns from the spatial extract are retained in the harmonized outputs.
- Site-average climate summaries are also written for precipitation, temperature, evapotranspiration, and snow metrics.
- The app uses a `snow_cover` field built from the MODIS monthly snow-cover proportions in the spatial extract.
- That site-level value is stored as the simple annual mean of the monthly snow-cover proportions so the app can stay fast without recalculating it on the fly.
- `discharge.rds` keeps sites from the partial harmonized table too, so Activity 2 can still show discharge for chloride sites that are missing some of the Activity 1 inputs.
- Activity 2 now uses app-ready North America raster backgrounds for `MAP`, `% Cropland`, and `% Impervious` behind the chloride points.
- `prep_data.R` builds those map rasters from `data/raw_inputs/na_1981_2010_annual_precip.tif` and `data/raw_inputs/land_cover_2020v2_30m.tif`.

## Basemap sources

- The chloride map uses `CartoDB Positron` tiles for the light geographic basemap and label overlay.
- `MAP` comes from `data/raw_inputs/na_1981_2010_annual_precip.tif`, which is the CEC North American Environmental Atlas annual precipitation layer for `1981-2010`.
- That precipitation layer is derived from `CHELSA v2.1` / `CHELSA-climatologies v2.1`.
- `% Cropland` and `% Impervious` come from `data/raw_inputs/land_cover_2020v2_30m.tif`, which is the CEC / NALCMS North American Land Cover `2020` raster at `30 m`.
- In the current app workflow, `% Cropland` is derived from land-cover class `15` (`Cropland`) and `% Impervious` is derived from land-cover class `17` (`Urban and built-up`).
- The source precipitation and land-cover rasters are both stored in Lambert Azimuthal Equal Area projections and already cover northern Canada.
- `prep_data.R` first aggregates the land-cover classes in that native equal-area grid, then reprojects the percent-cover raster to a shared `EPSG:4326` `0.05°` grid for Leaflet display.
- That reprojection step is useful for app display, but it can smooth low nonzero `% Cropland` and `% Impervious` values across neighboring cells, so the legend breaks matter a lot for how sparse northern areas look.

# Shiny Workflow Notes

These are just the quick notes I want handy when the app or data need an update.

## Data flow

1. Put the raw files in `data/raw_inputs/`.
2. Run `data_harmonization.R` to build the working CSVs in `data/`.
3. Run `prep_data.R` to build the `.rds` files in `data/`.
4. Launch `app.R`.

## Raw files used right now

- `20260105_masterdata_chem.csv`
- `20260106_masterdata_discharge.csv`
- `Koeppen_Geiger_2.csv`
- `all-data_si-extract_2_20250325.csv`
- `DSi_LULC_filled_interpolated_Simple.csv`
- `na_1981_2010_annual_precip.tif`
- `land_cover_2020v2_30m.tif`

## What each script writes

`data_harmonization.R`

- `harmonized_north_america_partial.csv`
- `harmonized_north_america_complete.csv`
- `discharge_north_america.csv`
- `cl_monthly_summary.csv`
- `cq_paired_obs.csv`
- `cq_slopes.csv`

`prep_data.R`

- `harmonized_complete.rds`
  This is the stricter Activity 1 table. It only keeps sites with `RBI`, `RCS`, climate zone, mean annual precipitation, `snow_cover`, and dominant land-cover values.
- `harmonized_partial.rds`
  This is the broader app table. It keeps the larger North American site set so the overview map and chloride activity can still use sites that are missing some Activity 1 inputs.
- `discharge.rds`
- `cl_monthly.rds`
- `cq_paired.rds`
- `cq_slopes.rds`
- `activity2_map_precip_mm.tif`
- `activity2_map_cropland_pct.tif`
- `activity2_map_impervious_pct.tif`

## App data by section

- `Overview` reads the partial harmonized table so the map can show the broadest site set possible.
- `Activity 1` uses the complete harmonized table plus monthly discharge summaries.
- `Activity 2` uses the partial harmonized table, monthly chloride summaries, and discharge.
- `Activity 2` also reads `activity2_map_precip_mm.tif`, `activity2_map_cropland_pct.tif`, and `activity2_map_impervious_pct.tif` for the chloride-map backgrounds.
- `Activity 3` uses discharge plus the paired `C-Q` tables and slope table.

## Things worth remembering

- The North America filter now comes from site coordinates in the climate table, not a hand-built `LTER` list.
- The harmonized tables keep the year-by-year climate columns and also add site-average climate summaries.
- `snow_cover` is the site-level MODIS snow-cover metric used in the app.
- It is calculated in the harmonization step as the simple annual mean of the monthly snow-cover proportions from the spatial extract.
- `discharge.rds` keeps sites from the partial harmonized table so the chloride activity can still show discharge even when a site is missing some Activity 1 drivers.
- `prep_data.R` now builds the Activity 2 background rasters from the North America precipitation and land-cover TIFFs in `data/raw_inputs/`.
- If you rename or swap a workflow file, update `data_harmonization.R`, `prep_data.R`, and the startup file check in `app.R`.

## Activity 2 basemap sources

- The chloride map sits on top of `CartoDB Positron` tiles for the light geographic base and labels.
- `activity2_map_precip_mm.tif` comes from `data/raw_inputs/na_1981_2010_annual_precip.tif`.
- That raw precipitation raster is the CEC North American Environmental Atlas annual precipitation layer for `1981-2010`, derived from `CHELSA v2.1` / `CHELSA-climatologies v2.1`.
- `activity2_map_cropland_pct.tif` and `activity2_map_impervious_pct.tif` come from `data/raw_inputs/land_cover_2020v2_30m.tif`.
- That land-cover TIFF is the CEC / NALCMS North American Land Cover `2020` raster at `30 m`.
- `% Cropland` is built from class `15` (`Cropland`).
- `% Impervious` is built from class `17` (`Urban and built-up`).
- Both raw rasters already cover northern Canada in their native Lambert Azimuthal Equal Area projections.
- `prep_data.R` aggregates the land-cover classes in that equal-area grid first, then reprojects the resulting percent-cover rasters to a shared `EPSG:4326` `0.05°` grid so Leaflet can draw them consistently with the site points.
- That final reprojection is a display step, but it can smooth very small nonzero land-cover percentages into neighboring cells, especially for sparse `% Impervious` values.

## Quick checks

```bash
Rscript -e "parse(file='app.R'); parse(file='prep_data.R'); parse(file='data_harmonization.R')"
Rscript data_harmonization.R data data
Rscript prep_data.R data data
Rscript -e "Sys.setenv(HYDRO_MODULES_APP_DATA_DIR='data'); source('app.R', local = TRUE)"
```

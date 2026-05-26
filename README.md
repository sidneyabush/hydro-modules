# Stream Hydrology Teaching Module

This repo holds the Shiny app plus the small data workflow that feeds it for North American stream sites.

## Main pieces

- `app.R`: the Shiny app
- `data_harmonization.R`: reads the raw input files and writes the working CSVs
- `prep_data.R`: turns those CSVs into the `.rds` files the app reads
- `data/raw_inputs/`: raw files copied in for local updates
- `data/`: app-ready CSV and `.rds` outputs

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

## References

- Commission for Environmental Cooperation. (n.d.). *Precipitation (1981–2010) – annual* [Data set]. *North American Environmental Atlas*. Retrieved May 26, 2026, from https://www.cec.org/north-american-environmental-atlas/precipitation-1981-2010-annual/

- Commission for Environmental Cooperation. (n.d.). *North American land cover, 2020 (Landsat, 30 m)* [Data set]. *North American Environmental Atlas*. Retrieved May 26, 2026, from https://www.cec.org/north-american-environmental-atlas/land-cover-30m-2020/

- Jankowski, K. J., Johnson, K., Lyon, N. J., Bush, S. A., Julian, P., Sethna, L. R., McKnight, D. M., McDowell, W. H., Wymore, A. S., Kortelainen, P., Laudon, H., Heindel, R. C., Poste, A. E., Shogren, A., Worrall, F., Mosley, L., Sullivan, P. L., & Carey, J. C. (2025). GlASS - Global Aggregation of Stream Silica. *Scientific Data, 12*, 1658. https://doi.org/10.1038/s41597-025-05937-2

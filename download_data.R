library(tidyverse)
library(tidycensus)
library(tigris)
library(sf)
library(jsonlite)

options(tigris_use_cache = TRUE)
dir.create("data", showWarnings = FALSE, recursive = TRUE)

# Mutually exclusive race/ethnicity variables from ACS Table B03002
b03002_vars <- c(
  total         = "B03002_001",
  nh_white      = "B03002_003",
  nh_black      = "B03002_004",
  nh_aian       = "B03002_005",
  nh_asian      = "B03002_006",
  nh_nhpi       = "B03002_007",
  nh_other      = "B03002_008",
  nh_two_more   = "B03002_009",
  hispanic      = "B03002_012"
)

# Helper to query the official U.S. Census Bureau API (data.census.gov)
fetch_census_api_b03002 <- \(geo_spec) {
  url <- paste0(
    "https://data.census.gov/api/access/data/table?id=ACSDT5Y2024.B03002&g=",
    geo_spec
  )
  raw <- fromJSON(url)$response$data
  df <- as.data.frame(raw[-1, , drop = FALSE], stringsAsFactors = FALSE)
  names(df) <- raw[1, ]
  as_tibble(df) |>
    mutate(GEOID = str_remove(GEO_ID, "^.*US"))
}

if (nzchar(Sys.getenv("CENSUS_API_KEY"))) {
  message("Using tidycensus with CENSUS_API_KEY...")
  sarasota_county_raw <- get_acs(
    geography = "county",
    variables = b03002_vars,
    state = "FL",
    county = "Sarasota",
    year = 2024,
    survey = "acs5",
    output = "wide"
  )

  sarasota_tracts_sf <- get_acs(
    geography = "tract",
    variables = b03002_vars,
    state = "FL",
    county = "Sarasota",
    year = 2024,
    survey = "acs5",
    output = "wide",
    geometry = TRUE
  ) |>
    st_transform(4326)
} else {
  message("Downloading 2024 ACS 5-year Table B03002 from official U.S. Census Bureau API...")
  county_api <- fetch_census_api_b03002("050XX00US12115")
  tracts_api <- fetch_census_api_b03002("0500000US12115%241400000")

  # Standardize column names to match tidycensus wide format
  standardize_b03002 <- \(df) {
    out <- df |>
      select(GEOID, NAME)
    for (nm in names(b03002_vars)) {
      code <- b03002_vars[[nm]]
      out[[paste0(nm, "E")]] <- as.numeric(df[[paste0(code, "E")]])
      out[[paste0(nm, "M")]] <- as.numeric(df[[paste0(code, "M")]])
    }
    out
  }

  sarasota_county_raw <- standardize_b03002(county_api)
  sarasota_tracts_tab <- standardize_b03002(tracts_api)

  message("Downloading official 2024 Census tract boundaries via tigris...")
  tracts_geo <- tracts(
    state = "FL",
    county = "Sarasota",
    year = 2024,
    cb = TRUE,
    progress_bar = FALSE
  ) |>
    st_transform(4326) |>
    select(GEOID, TRACTCE, NAME_SHORT = NAME, NAMELSAD, ALAND, AWATER, geometry)

  sarasota_tracts_sf <- tracts_geo |>
    left_join(sarasota_tracts_tab, by = "GEOID")
}

saveRDS(sarasota_county_raw, "data/sarasota_county_2024.rds")
saveRDS(sarasota_tracts_sf, "data/sarasota_tracts_2024_sf.rds")
message("Saved data/sarasota_county_2024.rds and data/sarasota_tracts_2024_sf.rds successfully.")

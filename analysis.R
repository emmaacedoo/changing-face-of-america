library(tidyverse)
library(sf)
library(leaflet)
library(plotly)
library(scales)
library(htmltools)

if (!file.exists("data/sarasota_county_2024.rds") ||
    !file.exists("data/sarasota_tracts_2024_sf.rds")) {
  source("download_data.R")
}

sarasota_county_raw <- readRDS("data/sarasota_county_2024.rds")
sarasota_tracts_raw <- readRDS("data/sarasota_tracts_2024_sf.rds")

# 1. Clean County-Level Mutually Exclusive Racial & Ethnic Composition (Table B03002)
race_labels <- c(
  nh_white    = "White Alone (Non-Hispanic)",
  hispanic    = "Hispanic or Latino (Any Race)",
  nh_black    = "Black or African American Alone (Non-Hispanic)",
  nh_asian    = "Asian Alone (Non-Hispanic)",
  nh_two_more = "Two or More Races (Non-Hispanic)",
  nh_other    = "Some Other Race Alone (Non-Hispanic)",
  nh_aian     = "American Indian & Alaska Native Alone (Non-Hispanic)",
  nh_nhpi     = "Native Hawaiian & Pacific Islander Alone (Non-Hispanic)"
)

race_colors <- c(
  "White Alone (Non-Hispanic)"                              = "#1d4e89",
  "Hispanic or Latino (Any Race)"                           = "#e05a47",
  "Black or African American Alone (Non-Hispanic)"          = "#2a9d8f",
  "Asian Alone (Non-Hispanic)"                              = "#f4a261",
  "Two or More Races (Non-Hispanic)"                        = "#6c5ce7",
  "Some Other Race Alone (Non-Hispanic)"                    = "#00b4d8",
  "American Indian & Alaska Native Alone (Non-Hispanic)"    = "#8d99ae",
  "Native Hawaiian & Pacific Islander Alone (Non-Hispanic)" = "#52b788"
)

total_pop <- sarasota_county_raw$totalE[[1]]

sarasota_race_composition <- tibble(
  code = names(race_labels),
  group = unname(race_labels),
  estimate = map_dbl(names(race_labels), \(k) sarasota_county_raw[[paste0(k, "E")]][[1]]),
  moe = map_dbl(names(race_labels), \(k) sarasota_county_raw[[paste0(k, "M")]][[1]])
) |>
  mutate(
    moe = if_else(is.na(moe) | moe < 0, 0, moe),
    pct = (estimate / total_pop) * 100,
    pct_label = sprintf("%.2f%%", pct),
    color = race_colors[group]
  ) |>
  arrange(desc(estimate))

# Verify mutually exclusive categories sum to 100% of total population
stopifnot(sum(sarasota_race_composition$estimate) == total_pop)

# 2. Clean Tract-Level Spatial Data
sarasota_tracts_clean <- sarasota_tracts_raw |>
  mutate(
    tract_label = str_remove(NAME, ";.*$|,.*$"),
    nh_other_combinedE = nh_aianE + nh_nhpiE + nh_otherE + nh_two_moreE,
    pct_hispanic = if_else(totalE > 0, (hispanicE / totalE) * 100, NA_real_),
    pct_nh_white = if_else(totalE > 0, (nh_whiteE / totalE) * 100, NA_real_),
    pct_nh_black = if_else(totalE > 0, (nh_blackE / totalE) * 100, NA_real_),
    pct_nh_asian = if_else(totalE > 0, (nh_asianE / totalE) * 100, NA_real_),
    pct_nh_other = if_else(totalE > 0, (nh_other_combinedE / totalE) * 100, NA_real_)
  )

# Summary metrics for Sarasota County
sarasota_summary <- list(
  total_pop = total_pop,
  hispanic_pop = sarasota_county_raw$hispanicE[[1]],
  hispanic_pct = (sarasota_county_raw$hispanicE[[1]] / total_pop) * 100,
  nh_white_pop = sarasota_county_raw$nh_whiteE[[1]],
  nh_white_pct = (sarasota_county_raw$nh_whiteE[[1]] / total_pop) * 100,
  bipoc_pop = total_pop - sarasota_county_raw$nh_whiteE[[1]],
  bipoc_pct = ((total_pop - sarasota_county_raw$nh_whiteE[[1]]) / total_pop) * 100,
  n_tracts = nrow(sarasota_tracts_clean),
  max_hispanic_tract = sarasota_tracts_clean |>
    st_drop_geometry() |>
    filter(!is.na(pct_hispanic)) |>
    slice_max(pct_hispanic, n = 1)
)

# 3. Interactive Choropleth Map of % Hispanic or Latino by Census Tract
build_hispanic_map <- \(tracts_sf = sarasota_tracts_clean) {
  pal <- colorNumeric(
    palette = "YlOrRd",
    domain = tracts_sf$pct_hispanic,
    na.color = "#d9d9d9"
  )

  labels <- sprintf(
    "<div style='font-family: Inter, sans-serif; font-size: 13px; line-height: 1.45;'>
      <strong style='font-size: 14px; color: #1d3557;'>%s</strong><br/>
      <span style='color: #555;'>GEOID: %s</span>
      <hr style='margin: 6px 0; border: 0; border-top: 1px solid #ddd;'/>
      <strong>Hispanic or Latino:</strong> <span style='color: #c1121f; font-weight: 700;'>%s</span> (%s &plusmn; %s)<br/>
      <strong>Total Population:</strong> %s<br/>
      <strong>White (Non-Hispanic):</strong> %s<br/>
      <strong>Black (Non-Hispanic):</strong> %s<br/>
      <strong>Asian (Non-Hispanic):</strong> %s<br/>
      <strong>Multiracial / Other (NH):</strong> %s
    </div>",
    tracts_sf$tract_label,
    tracts_sf$GEOID,
    ifelse(is.na(tracts_sf$pct_hispanic), "N/A", sprintf("%.1f%%", tracts_sf$pct_hispanic)),
    comma(tracts_sf$hispanicE),
    comma(tracts_sf$hispanicM),
    comma(tracts_sf$totalE),
    ifelse(is.na(tracts_sf$pct_nh_white), "N/A", sprintf("%.1f%%", tracts_sf$pct_nh_white)),
    ifelse(is.na(tracts_sf$pct_nh_black), "N/A", sprintf("%.1f%%", tracts_sf$pct_nh_black)),
    ifelse(is.na(tracts_sf$pct_nh_asian), "N/A", sprintf("%.1f%%", tracts_sf$pct_nh_asian)),
    ifelse(is.na(tracts_sf$pct_nh_other), "N/A", sprintf("%.1f%%", tracts_sf$pct_nh_other))
  ) |>
    map(HTML)

  leaflet(tracts_sf, options = leafletOptions(minZoom = 9)) |>
    addProviderTiles(providers$CartoDB.Positron, group = "Light Basemap") |>
    addProviderTiles(providers$OpenStreetMap, group = "Street Map") |>
    addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") |>
    addPolygons(
      fillColor = ~pal(pct_hispanic),
      weight = 1.2,
      opacity = 1,
      color = "#ffffff",
      dashArray = "",
      fillOpacity = 0.78,
      highlightOptions = highlightOptions(
        weight = 2.8,
        color = "#1d3557",
        dashArray = "",
        fillOpacity = 0.92,
        bringToFront = TRUE
      ),
      label = labels,
      labelOptions = labelOptions(
        style = list(
          "font-weight" = "normal",
          padding = "8px 10px",
          "border-radius" = "8px",
          "box-shadow" = "0 4px 14px rgba(0,0,0,0.15)"
        ),
        textsize = "13px",
        direction = "auto"
      ),
      popup = labels,
      group = "Hispanic or Latino (%)"
    ) |>
    addLegend(
      pal = pal,
      values = ~pct_hispanic,
      opacity = 0.85,
      title = "Hispanic or Latino (%)",
      labFormat = labelFormat(suffix = "%"),
      position = "bottomright"
    ) |>
    addLayersControl(
      baseGroups = c("Light Basemap", "Street Map", "Satellite"),
      overlayGroups = c("Hispanic or Latino (%)"),
      options = layersControlOptions(collapsed = FALSE)
    )
}

# 4. Interactive Chart: Mutually Exclusive County Racial & Ethnic Composition
build_race_ethnicity_bar <- \(comp_df = sarasota_race_composition) {
  plot_df <- comp_df |>
    mutate(
      group = fct_reorder(group, pct),
      hover_text = sprintf(
        "<b>%s</b><br>Population: %s (\u00b1%s)<br>Share of County: %.2f%%",
        group, comma(estimate), comma(moe), pct
      ),
      bar_label = sprintf("%.1f%% (%s)", pct, comma(estimate))
    )

  plot_ly(
    data = plot_df,
    x = ~pct,
    y = ~group,
    type = "bar",
    orientation = "h",
    marker = list(
      color = ~color,
      line = list(color = "#ffffff", width = 1)
    ),
    text = ~bar_label,
    textposition = "outside",
    hovertext = ~hover_text,
    hoverinfo = "text",
    cliponaxis = FALSE
  ) |>
    layout(
      title = list(
        text = "<b>Racial & Ethnic Composition of Sarasota County (2024 ACS 5-Year)</b>",
        x = 0.01,
        font = list(size = 16, color = "#1d3557")
      ),
      xaxis = list(
        title = "Percentage of Total Population (%)",
        ticksuffix = "%",
        range = c(0, 95),
        gridcolor = "#edf2f4"
      ),
      yaxis = list(
        title = "",
        automargin = TRUE
      ),
      margin = list(l = 240, r = 90, t = 55, b = 45),
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor = "rgba(0,0,0,0)",
      showlegend = FALSE
    ) |>
    config(displayModeBar = FALSE)
}

# 5. Interactive Chart: Top 15 Census Tracts by Hispanic or Latino Share
build_top_tracts_chart <- \(tracts_sf = sarasota_tracts_clean, top_n = 15) {
  top_df <- tracts_sf |>
    st_drop_geometry() |>
    filter(!is.na(pct_hispanic), totalE >= 100) |>
    slice_max(pct_hispanic, n = top_n) |>
    mutate(
      tract_short = str_replace(tract_label, "Census Tract ", "Tract "),
      tract_short = fct_reorder(tract_short, pct_hispanic),
      hover_text = sprintf(
        "<b>%s</b> (GEOID: %s)<br>Hispanic or Latino: %.1f%% (%s \u00b1 %s)<br>Total Population: %s",
        tract_label, GEOID, pct_hispanic, comma(hispanicE), comma(hispanicM), comma(totalE)
      )
    )

  plot_ly(
    data = top_df,
    x = ~pct_hispanic,
    y = ~tract_short,
    type = "bar",
    orientation = "h",
    marker = list(
      color = ~pct_hispanic,
      colorscale = "YlOrRd",
      reversescale = FALSE,
      line = list(color = "#ffffff", width = 1)
    ),
    text = ~sprintf("%.1f%%", pct_hispanic),
    textposition = "outside",
    hovertext = ~hover_text,
    hoverinfo = "text",
    cliponaxis = FALSE
  ) |>
    layout(
      title = list(
        text = "<b>Top 15 Sarasota County Census Tracts by Hispanic or Latino Share</b>",
        x = 0.01,
        font = list(size = 16, color = "#1d3557")
      ),
      xaxis = list(
        title = "Hispanic or Latino Residents (%)",
        ticksuffix = "%",
        range = c(0, max(top_df$pct_hispanic, na.rm = TRUE) * 1.18),
        gridcolor = "#edf2f4"
      ),
      yaxis = list(
        title = "",
        automargin = TRUE
      ),
      margin = list(l = 120, r = 60, t = 55, b = 45),
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor = "rgba(0,0,0,0)",
      showlegend = FALSE
    ) |>
    config(displayModeBar = FALSE)
}

# 6. Interactive Chart: Stacked Tract-Level Composition Across Top 20 Most Populous Tracts
build_tract_stacked_chart <- \(tracts_sf = sarasota_tracts_clean, top_n = 20) {
  tract_sub <- tracts_sf |>
    st_drop_geometry() |>
    filter(!is.na(pct_hispanic), totalE > 0) |>
    slice_max(hispanicE, n = top_n) |>
    mutate(
      tract_short = str_replace(tract_label, "Census Tract ", "Tract "),
      tract_short = fct_reorder(tract_short, pct_hispanic)
    )

  plot_ly(data = tract_sub, y = ~tract_short) |>
    add_bars(
      x = ~pct_hispanic,
      name = "Hispanic or Latino",
      orientation = "h",
      marker = list(color = "#e05a47"),
      hovertemplate = "<b>%{y}</b><br>Hispanic or Latino: %{x:.1f}%<extra></extra>"
    ) |>
    add_bars(
      x = ~pct_nh_black,
      name = "Black Alone (NH)",
      orientation = "h",
      marker = list(color = "#2a9d8f"),
      hovertemplate = "<b>%{y}</b><br>Black Alone (NH): %{x:.1f}%<extra></extra>"
    ) |>
    add_bars(
      x = ~pct_nh_asian,
      name = "Asian Alone (NH)",
      orientation = "h",
      marker = list(color = "#f4a261"),
      hovertemplate = "<b>%{y}</b><br>Asian Alone (NH): %{x:.1f}%<extra></extra>"
    ) |>
    add_bars(
      x = ~pct_nh_other,
      name = "Multiracial / Other (NH)",
      orientation = "h",
      marker = list(color = "#6c5ce7"),
      hovertemplate = "<b>%{y}</b><br>Multiracial / Other (NH): %{x:.1f}%<extra></extra>"
    ) |>
    add_bars(
      x = ~pct_nh_white,
      name = "White Alone (NH)",
      orientation = "h",
      marker = list(color = "#1d4e89"),
      hovertemplate = "<b>%{y}</b><br>White Alone (NH): %{x:.1f}%<extra></extra>"
    ) |>
    layout(
      barmode = "stack",
      title = list(
        text = "<b>Mutually Exclusive Racial/Ethnic Breakdown in Tracts with Largest Hispanic Populations</b>",
        x = 0.01,
        font = list(size = 15, color = "#1d3557")
      ),
      xaxis = list(
        title = "Share of Tract Population (Sums to 100%)",
        ticksuffix = "%",
        range = c(0, 100)
      ),
      yaxis = list(title = "", automargin = TRUE),
      legend = list(orientation = "h", y = -0.15, x = 0),
      margin = list(l = 120, r = 30, t = 55, b = 75),
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor = "rgba(0,0,0,0)"
    ) |>
    config(displayModeBar = FALSE)
}

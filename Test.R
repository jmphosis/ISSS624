# traffic collision of Hong Kong from 2014 to 2019
hk_accidents = hkdatasets::download_data("hk_accidents")

# collisions in Kowloon City District, 2017
test_data = subset(hk_accidents, District_Council_District == "KC" & Year == 2017)

# turn it to sf object
test_points = test_data %>%
  # lng/lat value are missing in some records
  filter(!is.na(Grid_E) & !is.na(Grid_N)) %>%
  st_as_sf(coords = c("Grid_E", "Grid_N"), crs = 2326, remove = FALSE)

area_honeycomb_grid = st_make_grid(test_points, c(150, 150), what = "polygons", square = FALSE)

# To sf and add grid ID
honeycomb_grid_sf = st_sf(area_honeycomb_grid) %>%
  # add grid ID
  mutate(grid_id = 1:length(lengths(area_honeycomb_grid)))

# count number of points in each grid
# https://gis.stackexchange.com/questions/323698/counting-points-in-polygons-with-sf-package-of-r
honeycomb_grid_sf$n_colli = lengths(st_intersects(honeycomb_grid_sf, test_points))

# remove grid without value of 0 (i.e. no points in side that grid)
honeycomb_count = filter(honeycomb_grid_sf, n_colli > 0)


tmap_mode("view")

map_honeycomb = tm_shape(honeycomb_count) +
  tm_fill(
    col = "n_colli",
    palette = "Reds",
    style = "cont",
    title = "Number of collisions",
    id = "grid_id",
    showNA = FALSE,
    alpha = 0.6,
    popup.vars = c(
      "Number of collisions: " = "n_colli"
    ),
    popup.format = list(
      n_colli = list(format = "f", digits = 0)
    )
  ) +
  tm_borders(col = "grey40", lwd = 0.7)

map_honeycomb
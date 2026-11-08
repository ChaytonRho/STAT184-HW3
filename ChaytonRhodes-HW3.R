# Homework 3
# Chayton Rhodes

library(ggplot2); library(dplyr); library(tidyr)
library(readr); library(sf)

# Keep HW3.R and both data files in the project working directory

# Create a folder for the generated files
if (!dir.exists("HW3_outputs")) dir.create("HW3_outputs")

# Setup code
vehicles <- ggplot2::mpg
gems <- ggplot2::diamonds


# Problem 1

# Part A

p_engine <- ggplot(vehicles, aes(x = displ, y = hwy)) +
  geom_point(aes(color = drv), alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  facet_wrap(~ drv) +
  labs(x = "Engine displacement (litres)", y = "Highway fuel economy (mpg)",
       color = "drv", title = "Engine size and highway fuel economy") +
  theme_bw()

p_engine

# There are three fitted lines because facet_wrap creates separate panels for each drive type
# geom_smooth fits a separate regression to the observations in each panel
# Mapping drv to color in the point layer is not necessary for these separate fits


# Part B

p_drive_counts <- ggplot(vehicles, aes(x = drv, fill = factor(cyl))) +
  geom_bar() +
  labs(x = "drv", y = "count", fill = "factor(cyl)",
       title = "Vehicle counts by drive type and cylinders") +
  theme_bw()

p_drive_counts

p_drive_proportion <- ggplot(vehicles, aes(x = drv, fill = factor(cyl))) +
  geom_bar(position = "fill") +
  labs(x = "drv", y = "Proportion within drive type", fill = "factor(cyl)",
       title = "Cylinder proportions within drive type") +
  theme_bw()

p_drive_proportion

vehicles |> count(drv)

# The count plot shows the number of vehicles in each drive type
# The proportion plot scales each drive type to a total height of 1
# Equal heights do not imply equal sample sizes because the groups have different counts


# Part C

# Use one smaller table for both histograms
# Keep diamonds with carat from 0 through 2, including the endpoints
# This restriction keeps the two plots on the same x-axis range
gems_small <- gems |> filter(!is.na(carat), carat >= 0, carat <= 2)

nrow(gems_small)
nrow(gems) - nrow(gems_small)

# Count histograms with the requested widths and bin boundaries
p_fine <- ggplot(gems_small, aes(x = carat)) +
  geom_histogram(binwidth = 0.01, boundary = 0, closed = "right",
                 fill = "steelblue", color = "white") +
  coord_cartesian(xlim = c(0, 2)) +
  labs(x = "Carat", y = "Count", title = "Diamond carat (bin width = 0.01)") +
  theme_bw()

p_coarse <- ggplot(gems_small, aes(x = carat)) +
  geom_histogram(binwidth = 0.5, boundary = 0, closed = "right",
                 fill = "steelblue", color = "white") +
  coord_cartesian(xlim = c(0, 2)) +
  labs(x = "Carat", y = "Count", title = "Diamond carat (bin width = 0.5)") +
  theme_bw()

p_fine
p_coarse

# Count exact carat values from the original full dataset
gems |> filter(carat %in% c(0.99, 1.00)) |> count(carat)

# The narrow bins make clustering at particular carat values easier to see
# The wide bins combine many nearby values and hide those sharp peaks

# Save the plots from Problem 1
ggsave("HW3_outputs/p_engine.png", p_engine, width = 10, height = 5, dpi = 300)
ggsave("HW3_outputs/p_drive_counts.png", p_drive_counts, width = 7, height = 5, dpi = 300)
ggsave("HW3_outputs/p_drive_proportion.png", p_drive_proportion, width = 7, height = 5, dpi = 300)
ggsave("HW3_outputs/p_fine.png", p_fine, width = 8, height = 5, dpi = 300)
ggsave("HW3_outputs/p_coarse.png", p_coarse, width = 8, height = 5, dpi = 300)


# Problem 2

# Setup code
# Use real Chicago Public Library locations and Chicago community areas
# Source: City of Chicago Open Data Portal, manually downloaded November 8, 2026
# Keep both raw data files in the same folder as HW3.R
# The city's data license is described on the source pages under Terms of Use

library_url <- "https://data.cityofchicago.org/d/x8fc-8rcq"
area_url <- "https://data.cityofchicago.org/d/igwz-8jzy"
library_file <- "libraries.csv"
area_file <- "community_areas.geojson"
download_date <- "2026-11-08"

# Read the manually downloaded raw files without making changes to them

sources <- tibble(
  dataset = c("Chicago Public Library locations", "Chicago community area boundaries"),
  publisher = c("City of Chicago / Chicago Public Library", "City of Chicago"),
  source_url = c(library_url, area_url),
  local_file = c(library_file, area_file),
  file_format = c("CSV", "GeoJSON"),
  download_method = "Manual export from source portal",
  download_date = download_date,
  license = c("City of Chicago Open Data Terms of Use", "City of Chicago Open Data Terms of Use")
)
write_csv(sources, "HW3_outputs/data_sources.csv")

sites_raw <- read_csv("libraries.csv")
areas_raw <- st_read("community_areas.geojson", quiet = TRUE)

names(sites_raw)
nrow(sites_raw)

# LOCATION contains latitude first and longitude second
sites_raw <- separate_wider_delim(sites_raw, LOCATION, ",",
                                  names = c("latitude", "longitude"), cols_remove = FALSE)
sites_raw <- sites_raw |> mutate(latitude = parse_number(latitude),
                                 longitude = parse_number(longitude))

sites_checked <- sites_raw |>
  mutate(
    source_row = row_number(),
    facility_name = BRANCH,
    coordinate_problem = is.na(longitude) | is.na(latitude) |
      longitude < -88 | longitude > -87 | latitude < 41 | latitude > 43,
    missing_name = is.na(facility_name) | trimws(facility_name) == ""
  )

# Count duplicate names and duplicate coordinates without dropping shared sites
sites_checked <- sites_checked |>
  mutate(duplicate_name = duplicated(facility_name),
         duplicate_coordinates = duplicated(paste(longitude, latitude)))

nrow(sites_checked)
sum(sites_checked$coordinate_problem)
sum(sites_checked$missing_name)
sum(sites_checked$duplicate_name)
sum(sites_checked$duplicate_coordinates)

# Report removed records and their reasons before filtering
sites_checked |>
  filter(coordinate_problem | missing_name) |>
  select(source_row, facility_name, coordinate_problem, missing_name)

# Only remove records missing a name or having unusable coordinates
sites_clean <- sites_checked |>
  filter(!coordinate_problem, !missing_name) |>
  mutate(site_id = paste0("LIB", sprintf("%03d", row_number()))) |>
  select(site_id, facility_name, longitude, latitude, everything()) |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)

st_crs(sites_clean)

nrow(sites_clean)
nrow(sites_checked) - nrow(sites_clean)

# EPSG:26916 = NAD83 / UTM zone 16N; coordinate units are meters
sites_m <- st_transform(sites_clean, 26916)
st_crs(sites_m)

# Retain every valid location, even if two libraries share coordinates
nrow(sites_m) >= 20
anyDuplicated(sites_m$site_id) == 0

write_csv(st_drop_geometry(sites_m) |>
            select(site_id, facility_name, longitude, latitude),
          "HW3_outputs/cleaned_sites.csv")
st_write(sites_m, "HW3_outputs/sites_m.gpkg", delete_dsn = TRUE, quiet = TRUE)

# Part A

p_sites <- ggplot() +
  geom_sf(data = sites_m, aes(color = "Libraries"), size = 1.8) +
  scale_color_manual(values = c("Libraries" = "blue"), name = "Layer") +
  labs(title = "Chicago Public Library locations",
       caption = paste("Source: City of Chicago Open Data Portal | Downloaded", download_date)) +
  theme_bw()

p_sites


# Part B

# Buffer each library and union the buffers so overlap is counted once
buffer_500 <- st_union(st_buffer(sites_m, dist = 500))
buffer_1000 <- st_union(st_buffer(sites_m, dist = 1000))

# Do not clip these areas to the study boundary
buffer_500_sf <- st_sf(radius_m = 500, geometry = buffer_500)
buffer_1000_sf <- st_sf(radius_m = 1000, geometry = buffer_1000)

p_buffers <- ggplot() +
  geom_sf(data = buffer_500_sf, aes(fill = "500-meter coverage"), alpha = 0.45) +
  geom_sf(data = sites_m, aes(color = "Libraries"), size = 1.1) +
  scale_fill_manual(values = c("500-meter coverage" = "skyblue"), name = "Buffer") +
  scale_color_manual(values = c("Libraries" = "darkblue"), name = "Sites") +
  labs(title = "Chicago libraries and combined 500-meter buffers",
       caption = paste("Source: City of Chicago Open Data Portal | Downloaded", download_date)) +
  theme_bw()

p_buffers


# Part C

buffer_summary <- tibble(
  radius_m = c(500, 1000),
  union_area_km2 = c(as.numeric(st_area(buffer_500)),
                     as.numeric(st_area(buffer_1000))) / 1000000
)

buffer_summary
buffer_summary$union_area_km2[2] >= buffer_summary$union_area_km2[1]

write_csv(buffer_summary, "HW3_outputs/buffer_summary.csv")
st_write(buffer_500_sf, "HW3_outputs/buffer_500.gpkg", delete_dsn = TRUE, quiet = TRUE)
st_write(buffer_1000_sf, "HW3_outputs/buffer_1000.gpkg", delete_dsn = TRUE, quiet = TRUE)

ggsave("HW3_outputs/p_sites.png", p_sites, width = 8, height = 7, dpi = 300)
ggsave("HW3_outputs/p_buffers.png", p_buffers, width = 8, height = 7, dpi = 300)

# The 1000-meter union area must be at least as large as the 500-meter union area
# Unioning the individual buffers prevents shared coverage from being counted twice
# These buffer areas include land outside the community area boundaries


# Problem 3

# Part A

# Use a separate real polygon dataset for Chicago community areas
names(areas_raw)
nrow(areas_raw)
st_crs(areas_raw)

# Check for empty and invalid shapes before repairing them
sum(st_is_empty(areas_raw))
sum(!st_is_valid(areas_raw))

areas_clean <- areas_raw |>
  filter(!st_is_empty(geometry)) |>
  st_make_valid() |>
  st_collection_extract("POLYGON", warn = FALSE) |>
  st_transform(26916)

# Use the community area number and name from the boundary file
areas_clean <- areas_clean |>
  mutate(district_id = as.character(area_num_1),
         district_name = community) |>
  select(district_id, district_name, everything())

sum(is.na(areas_clean$district_id))
sum(duplicated(areas_clean$district_id))
sum(st_is_empty(areas_clean))
sum(!st_is_valid(areas_clean))

# Check that there are at least five community areas
nrow(areas_clean) >= 5

areas_clean <- areas_clean |>
  mutate(area_km2 = as.numeric(st_area(geometry)) / 1000000)
all(areas_clean$area_km2 > 0)

st_write(areas_clean, "HW3_outputs/areas_m.gpkg", delete_dsn = TRUE, quiet = TRUE)

p_boundaries <- ggplot() +
  geom_sf(data = areas_clean, fill = "white", color = "gray50") +
  geom_sf(data = sites_m, aes(color = "Libraries"), size = 1) +
  scale_color_manual(values = c("Libraries" = "blue"), name = "Sites") +
  labs(title = "Chicago libraries and community areas",
       caption = paste("Source: City of Chicago Open Data Portal | Downloaded", download_date)) +
  theme_bw()

p_boundaries

# Both files are from the City of Chicago but may have different update dates
# The library sites represent points while community areas represent polygons


# Part B

# st_intersects lists each area that intersects a given library point
site_matches <- st_intersects(sites_m, areas_clean)

site_match_audit <- tibble(
  site_id = sites_m$site_id,
  facility_name = sites_m$facility_name,
  n_matches = lengths(site_matches)
)

write_csv(site_match_audit, "HW3_outputs/site_match_audit.csv")
site_match_audit |> count(n_matches)
site_match_audit |> filter(n_matches != 1)

# Only sites in exactly one community area are assigned a district
assigned_rows <- which(lengths(site_matches) == 1)
assigned_sites <- tibble(
  site_id = sites_m$site_id[assigned_rows],
  district_id = areas_clean$district_id[unlist(site_matches[assigned_rows])]
)

nrow(assigned_sites)
anyDuplicated(assigned_sites$site_id) == 0

# Keep all areas, including areas with zero assigned libraries
area_counts <- assigned_sites |> count(district_id, name = "n_sites")

district_counts <- areas_clean |>
  st_drop_geometry() |>
  select(district_id, district_name, area_km2) |>
  left_join(area_counts, join_by(district_id), relationship = "one-to-one") |>
  mutate(n_sites = coalesce(n_sites, 0L),
         sites_per_km2 = n_sites / area_km2) |>
  arrange(desc(n_sites), district_name)

district_counts
sum(district_counts$n_sites)
sum(district_counts$n_sites) == nrow(assigned_sites)

write_csv(district_counts, "HW3_outputs/district_counts.csv")

unassigned_n <- sum(site_match_audit$n_matches != 1)

# Part C

areas_results <- areas_clean |>
  left_join(district_counts |>
              select(district_id, n_sites, sites_per_km2),
            join_by(district_id), relationship = "one-to-one")

# The same boundaries and map extent are used in both maps
map_limits <- st_bbox(areas_results)
source_caption <- paste("Source: City of Chicago Open Data Portal | Downloaded", download_date,
                        "| Sites without exactly one area:", unassigned_n)

p_count <- ggplot(areas_results) +
  geom_sf(aes(fill = n_sites), color = "white", linewidth = 0.2) +
  scale_fill_gradient(low = "white", high = "darkblue", na.value = "gray80",
                      name = "Library count") +
  coord_sf(xlim = c(map_limits["xmin"], map_limits["xmax"]),
           ylim = c(map_limits["ymin"], map_limits["ymax"])) +
  labs(title = "Libraries per Chicago community area", caption = source_caption) +
  theme_bw()

p_density <- ggplot(areas_results) +
  geom_sf(aes(fill = sites_per_km2), color = "white", linewidth = 0.2) +
  scale_fill_gradient(low = "white", high = "darkgreen", na.value = "gray80",
                      name = "Libraries per km2") +
  coord_sf(xlim = c(map_limits["xmin"], map_limits["xmax"]),
           ylim = c(map_limits["ymin"], map_limits["ymax"])) +
  labs(title = "Library density by Chicago community area", caption = source_caption) +
  theme_bw()

p_count
p_density

ggsave("HW3_outputs/p_boundaries.png", p_boundaries, width = 8, height = 7, dpi = 300)
ggsave("HW3_outputs/p_count.png", p_count, width = 8, height = 7, dpi = 300)
ggsave("HW3_outputs/p_density.png", p_density, width = 8, height = 7, dpi = 300)

# Larger areas can have high counts simply because they cover more space
# Dividing by area gives the number of libraries per square kilometer
# The count and density rankings can differ because community areas are not equally sized
# A count of zero is a known value, while NA would mean the result is missing
# Sites matching zero or multiple areas are reported in the audit and not assigned

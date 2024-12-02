# ---------------------------
# Wildfire-Specific Treatment Code
# ---------------------------
# TO DO:
# 1. Develop a wildfire-specific code:
#    a. Remove wildfire boundaries from polygon processing.
#    b. Add a wildfire mask in all DART runs except for post-fire recovery treatments.
#    c. Ensure a second DART run for post-fire recovery polygons.
#
# Masks for DART runs:
# - MTBS Fire Boundary
# - InterAgency Fire Perimeter: https://data-nifc.opendata.arcgis.com/datasets/nifc::interagencyfireperimeterhistory-all-years-view/explore?location=13.716471%2C57.912975%2C2.68&showTable=true
# - Treatments within WRI and LTDL
# - Roads and parking lots (create a CONUS-wide mask)
#
# Additional TO DO:
# - Set a cutoff for environmental pixel matching between treatment and control (e.g., Gower's distance).
# - Aggregate treatment types.
# - Compile model options.

# Load Required Libraries
library(sf)
library(tidyverse)
library(ggplot2)
library(rnaturalearth)
library(devtools)
library(writexl)
library(ggplot2)
library(beepr)

### The code in this file is cleaning up the polygons so that they are:
# Step 1: Filter Polygons within Utah State Boundary
# Step 2: Filter Applicable Restoration Treatments
# STEP 3: All Projects Occur Within a Single Year
# STEP 4: Initiation and completion occur within roughly 3 years
# STEP 5: Exclude Wetland and Riparian Areas
# STEP 6. have not experienced a wildfire **** THIS REQUIREMENT HAS BEEN REMOVED 
# STEP 7. are less than 1/2 km squared **** THIS REQUIREMENT HAS BEEN REMOVED FOR NOW

# Read in cleaned shapefile of treatment polygons
treatment_polygons_cleaned_sf <- st_read("treatment_polygons_cleaned_sf.shp")
head(treatment_polygons_cleaned_sf)

### Step 1: Filter Polygons within Utah State Boundary ###
usa <- ne_states(country = "united states of america", returnclass = "sf")
utah_boundary <- subset(usa, name == "Utah") %>%
  st_transform(crs = st_crs(treatment_polygons_cleaned_sf))
utah_polygons_sf <- treatment_polygons_cleaned_sf[st_intersects(treatment_polygons_cleaned_sf , utah_boundary, sparse = FALSE), ]


### Step 2: Filter Applicable Restoration Treatments ###
unique_values_mjr <- unique(utah_polygons_sf$Trt_T_M)
print(unique_values_mjr)

# Define treatment types
treatment_types <- c("Herbicide/Weeds/Chemical", "Prescribed Burn", "Seeding", 
                     "Biological Control", "Vegetation/Soil Manipulation")
secondary_trt_types <- c("Soil Stabilization (other than seeding/planting): Silt Fences")

# Filter restoration polygons
restoration_polygons_sf <- subset(
  utah_polygons_sf, 
  Year > 1986 & 
    Pln_Imp == "Implemented" & 
    !Trtmn_T %in% secondary_trt_types & 
    Trt_T_M %in% treatment_types
)

# Inspect the unique values
print(unique(restoration_polygons_sf$Trt_T_S))
print(unique(restoration_polygons_sf$Trtmn_T))

# Extract unique Prj_ID values and their count
num_unique_prj_ids <- restoration_polygons_sf %>%
  summarize(unique_prj_ids = n_distinct(Prj_ID)) %>%
  pull(unique_prj_ids)

# Print the number of unique Prj_ID values
cat("Number of unique Prj_ID values:", num_unique_prj_ids, "\n")

# Identify projects with multiple treatments
duplicate_prj_ids <- restoration_polygons_sf %>%
  group_by(Prj_ID) %>%
  summarize(count = n(), .groups = "drop") %>%
  filter(count > 1)

# View the duplicate Prj_ID table
cat("Number of projects with multiple treatments:", nrow(duplicate_prj_ids), "\n")

# Calculate average and range of treatments per project
treatment_stats <- restoration_polygons_sf %>%
  group_by(Prj_ID) %>%
  summarize(count = n(), .groups = "drop") %>%
  summarize(
    average_treatments = mean(count),
    min_treatments = min(count),
    max_treatments = max(count)
  )

# View the average and range statistics
cat("Treatment statistics (average, min, max):\n")
print(treatment_stats)


# Map the Utah restoration polygons
ggplot() +
  geom_sf(data = utah_boundary, fill = "gray90", color = "black", linetype = "dashed") + # Utah boundary
  geom_sf(data = restoration_polygons_sf, aes(fill = Trt_T_M), alpha = 0.7) + # Filtered polygons
  scale_fill_viridis_d(name = "Treatment Type") + # Color scale for treatments
  theme_minimal() +
  labs(
    title = "Restoration Polygons within Utah",
    subtitle = "Filtered by State Boundary",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )


#### STEP 3: All Projects Occur Within a Single Year ####

# Remove polygons without a treatment completion year
restoration_polygons_sf <- restoration_polygons_sf %>%
  filter(!is.na(Yer_cmp))

# Count distinct years for each Prj_ID
year_counts <- restoration_polygons_sf %>%
  group_by(Prj_ID) %>%
  summarise(distinct_years = n_distinct(Year), .groups = 'drop')
head(year_counts)

# Identify Prj_IDs with treatments in exactly one distinct year
single_year_ids <- year_counts %>%
  filter(distinct_years == 1) %>%
  pull(Prj_ID)

# Filter original data for single-year treatments
final_single_year_sf <- restoration_polygons_sf %>%
  filter(Prj_ID %in% single_year_ids)

# Count distinct years for each Prj_ID
year_counts_check <- final_single_year_sf %>%
  group_by(Prj_ID) %>%
  summarise(distinct_years = n_distinct(Year), .groups = 'drop')
head(year_counts)

# Add a column indicating multiple treatments for a Prj_ID
final_year_sf <- final_single_year_sf %>%
  group_by(Prj_ID) %>%
  mutate(multiple_treatments = n() > 1) %>%
  ungroup()

# Confirm that spatial data is retained
is_spatial_inherits <- inherits(final_year_sf, "sf")
print(is_spatial_inherits)


### STEP 4: Initiation and completion occur within roughly 3 years ###

# Ensure initiation and completion year are the same
result <- final_year_sf %>%
  mutate(
    Yer_cmp = as.numeric(Yer_cmp),
    Year_nt = as.numeric(Year_nt)
  ) %>%
  filter((Yer_cmp - Year_nt) > 1) %>%
  select(Prj_ID, Yer_cmp, Year_nt)

# Drop geometry from result
# result_df <- result %>%
  # st_drop_geometry()

# Filter by initiation and completion year range
final_year_sf_filtered <- final_year_sf %>%
  filter(Year_nt >= 1991 & Yer_cmp <= 2018)

# Remove Prj_IDs from result to maintain spatial features in final data
final_year_sf_filtered2 <- final_year_sf_filtered %>%
  filter(!(Prj_ID %in% result$Prj_ID))

# Summarize treatments per Prj_ID
result <- final_year_sf_filtered2 %>%
  group_by(Prj_ID) %>%
  summarise(
    unique_Trt_IDs = n_distinct(Trt_ID)) %>%
  filter(unique_Trt_IDs > 1)

length(unique(final_year_sf_filtered2$Prj_ID))

# Print the final result
# print(result)

#### STEP 5: Exclude Wetland and Riparian Areas ####
# Source for Wetland and Riparian Spatial Data:
# https://www.fws.gov/program/national-wetlands-inventory/download-state-wetlands-data

## Wetland Boundaries
wetlands_UT <- sf::st_read("Utah_Wetlands.shp")

# Retain only valid geometries
valid_wetland_boundaries <- wetlands_UT %>%
  filter(st_is_valid(.))

# Ensure coordinate reference system (CRS) matches and transform if necessary
wetland_boundaries <- st_transform(valid_wetland_boundaries, st_crs(restoration_polygons_sf))

# Optionally, remove overlaps in wetland polygons
wetland_boundaries_no_overlap <- st_union(wetland_boundaries)

# Identify intersections with wetlands
intersections <- st_intersects(wetland_boundaries_no_overlap, restoration_polygons_sf, sparse = FALSE)

# Extract polygons intersecting with wetlands
intersecting_indices <- which(rowSums(intersections) > 0)
polys_intersecting <- restoration_polygons_sf[intersecting_indices, ]

# Subset to polygons that do not intersect with wetlands
restoration_filtered_sf <- restoration_polygons_sf[-intersecting_indices, ]

beep(1)

## Riparian Boundaries
riparian_UT <- sf::st_read("UT_Riparian.shp")

# Retain only valid geometries for riparian areas
valid_riparian_boundaries <- riparian_UT %>%
  filter(st_is_valid(.))

# Ensure CRS matches and transform if necessary
riparian_boundaries <- st_transform(valid_riparian_boundaries, st_crs(restoration_filtered_sf))

# Identify intersections with riparian boundaries
intersections_rip <- st_intersects(riparian_boundaries, restoration_filtered_sf, sparse = FALSE)

# Extract polygons intersecting with riparian areas
intersecting_indices_rip <- which(rowSums(intersections_rip) > 0)
polys_intersecting_rip <- restoration_filtered_sf[intersecting_indices_rip, ]

# Subset to polygons that do not intersect with riparian areas
final_year_sf_filtered4 <- restoration_filtered_sf[-intersecting_indices_rip, ]

beep(1)


#### STEP 6: Areas that have not experienced a wildfire ####

# Read fire boundaries (commented out if not needed)
# fire_boundaries <- sf::st_read("mtbs_perims_DD.shp")

# Filter valid geometries (if needed)
# valid_fire_boundaries <- fire_boundaries[st_is_valid(fire_boundaries), ]

# Restrict to wildfire areas
# wildfires <- valid_fire_boundaries %>% filter(Incid_Type == "Wildfire")

# Check CRS compatibility
# st_crs(wildfires) == st_crs(final_year_sf_filtered4)

# Transform fire boundaries to match CRS of the treatments
# fire_boundaries <- st_transform(wildfires, st_crs(final_year_sf_filtered4))

# Check if CRS transformation was successful
# st_crs(fire_boundaries) == st_crs(final_year_sf_filtered4)

# Identify intersections between fire boundaries and treatment polygons
# intersections <- st_intersects(fire_boundaries, final_year_sf_filtered4, sparse = FALSE)

# Find indices of polygons intersecting with fire boundaries
# intersecting_indices <- which(rowSums(intersections) > 0)

# Create subset of polygons that intersect with fire boundaries (post-fire recovery)
# polys_intersecting <- final_year_sf_filtered4[intersecting_indices, ]

# Create subset of polygons that do not intersect with fire boundaries (no wildfire exposure)
# filtered_nonwildfire_sf5 <- final_year_sf_filtered4[-intersecting_indices, ]


#### STEP 7: Filter Polygons Smaller Than 0.5 km² ####

# Calculate area (in the same units as the projection, e.g., square meters for UTM)
# final_year_sf_filtered4$area <- st_area(final_year_sf_filtered4)

# Calculate area in square meters, then convert to square kilometers
# final_year_sf_filtered4$area_km2 <- (final_year_sf_filtered4$area / 1e6) / 2  # 1 km² = 1,000,000 m², divide by 2 for half

# Define a function to calculate the shortest side of the bounding box for each geometry
# shortest_axis_length <- function(geom) {
  # bbox <- st_bbox(geom)
 # min(abs(bbox["xmax"] - bbox["xmin"]), abs(bbox["ymax"] - bbox["ymin"]))
# }

# Apply the function to calculate the shortest axis length in meters for each geometry
# final_year_sf_filtered4$shortest_axis_length_m <- sapply(st_geometry(final_year_sf_filtered4), shortest_axis_length)

# Display a preview of the updated data frame
# head(final_year_sf_filtered4)

# Plot histogram of all treated areas
# ggplot(final_year_sf_filtered4, aes(x = area_km2)) +
  # geom_histogram(binwidth = 0.1, fill = "steelblue", color = "black") +
  # labs(
    # title = "Distribution of Treated Area Size",
    # x = "Treated Area Size (km²)",
    # y = "Frequency"
  # ) +
  # theme_minimal() +
  # scale_x_continuous(breaks = seq(0, 80, by = 10))

# Filter for areas less than or equal to 10 km²
# filtered_data <- final_year_sf_filtered4 %>%
  # filter(area_km2 <= 10)

# Plot histogram of treated areas <= 10 km²
# ggplot(filtered_data, aes(x = area_km2)) +
  # geom_histogram(binwidth = 1, fill = "steelblue", color = "black") +
  # labs(
    # title = "Distribution of Treated Area Size (≤ 10 km²)",
    # x = "Treated Area Size (km²)",
    # y = "Frequency"
  # ) +
  # theme_minimal() +
  # scale_x_continuous(breaks = seq(0, 10, by = 1))

# Convert area_km2 column to numeric by extracting numeric values
# filtered_nonwildfire_sf4$area_km2_numeric <- as.numeric(str_extract(filtered_nonwildfire_sf3$area_km2, "^[0-9.]+"))

# Keep only geometries with an area greater than 0.5 km²
# filtered_nonwildfire_sf4 <- filtered_nonwildfire_sf3 %>%
#   filter(area_km2_numeric > 0.5)

# Optionally, remove the temporary numeric area column after filtering
# filtered_nonwildfire_sf4$area_km2_numeric <- NULL

# Preview the filtered results
# head(filtered_nonwildfire_sf4)

# write this polygon as a shapefile
st_write(final_year_sf_filtered4, "C:\\Users\\Kristina\\OneDrive - New Mexico State University\\Desktop\\GIT REPOs\\Restorationsuccess\\Restorationsuccess\\filtered_LTDL_polys.shp")


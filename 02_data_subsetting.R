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

# Install packages if necessary
cran_packages <- c("sf", "tidyverse", "ggplot2", "rnaturalearth", "writexl", "beepr")

# Install CRAN packages
# for (pkg in cran_packages) {
# if (!require(pkg, character.only = TRUE)) {
# install.packages(pkg, dependencies = TRUE)
# }
# }

# Load Required Libraries
library(sf)
library(tidyverse)
library(ggplot2)
library(rnaturalearth)
library(devtools)
library(writexl)
library(ggplot2)
library(beepr)

setwd("/90daydata/aeroldc/LTDL")

### The code in this file is cleaning up the polygons so that they are:
# Step 1: Filter Polygons within Utah State Boundary
# Step 2: Filter Applicable Restoration Treatments
# STEP 3: All Projects Occur Within a Single Year
# STEP 4: Initiation and completion occur within roughly 3 years
# STEP 5: Exclude Wetland and Riparian Areas **** THIS REQUIREMENT HAS BEEN REMOVED 
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

### STEP 4: Initiation and completion occur within roughly 18 months ###

# Drop geometry from result
# result_df <- result %>%
# st_drop_geometry()

# Ensure initiation and completion year are the same
# Filter records with a difference of 2 years and include initiation and completion dates
result_years <- final_year_sf %>%
  mutate(
    Yer_cmp = as.numeric(Yer_cmp),  # Ensure end year is numeric
    Year_nt = as.numeric(Year_nt)  # Ensure start year is numeric
  ) %>%
  filter((Yer_cmp - Year_nt) > 1) %>%  # Check for a 3-year difference
  select(Prj_ID, Yer_cmp, Year_nt, Init_Dt, Comp_Dt)  # Include Init_Dt and Comp_Dt

# Display the result
result_years

# Looking at the data. Perform a spatial semi-join to match rows
three_year_sf <- final_year_sf %>%
  filter(Prj_ID %in% result_years$Prj_ID)

# Remove Prj_IDs from result to maintain spatial features in final data
final_year_sf_filtered <- final_year_sf %>%
  filter(!(Prj_ID %in% result_years$Prj_ID))

# How many unique projects
length(unique(final_year_sf_filtered$Prj_ID))

# Filter by initiation and completion year range
final_LTDL_sf <- final_year_sf_filtered %>%
  filter(Year_nt >= 1991 & Yer_cmp <= 2018)

# Summarize treatments per Prj_ID that have more than 1 Prj_ID
result_info <- final_LTDL_sf %>%
  group_by(Prj_ID) %>%
  summarise(
    unique_Trt_IDs = n_distinct(Trt_ID)) %>%
  filter(unique_Trt_IDs > 1)

length(unique(final_LTDL_sf$Prj_ID))

# Print the final result
# print(result)

#### STEP 5: Exclude Wetland and Riparian Areas ####
# Source for Wetland and Riparian Spatial Data:
# https://www.fws.gov/program/national-wetlands-inventory/download-state-wetlands-data

# UT_riparian <- st_read("/90daydata/aeroldc/WRI/UT_geopackage_wetlands/UT_Wetlands_Geopackage.gpkg", 
# layer = "UT_Riparian")

# UT_wetlands <- st_read("/90daydata/aeroldc/WRI/UT_geopackage_wetlands/UT_Wetlands_Geopackage.gpkg", 
# layer = "UT_Wetlands")

# merge the riparian and wetland data 
# UT_rip_wetlands <- rbind(UT_riparian, UT_wetlands)

# convert the riparian and wetland data to the CRS we are using 
# UT_rip_wetlands_crstran <- st_transform(UT_rip_wetlands, st_crs(final_LTDL_sf))

# check for intersecting treatments with the riparian and wetland data
# riparian_trt <- st_intersection(UT_rip_wetlands_crstran, final_LTDL_sf, sparse = TRUE)

# head(riparian_trts)

#### get the FeatureIDs from the riparian_trts dataset
# Get FeatureIDs from the riparian_trts dataset
# riparian_feature_ids <- riparian_trts %>%
# dplyr::select(Prj_ID) %>%
# distinct()  # to ensure no duplicates

# remove those feature IDs from terrestrial dataset terrestrial_trts
# only_terrestrial_trts <- final_LTDL_sf %>%
# filter(!Prj_ID %in% riparian_feature_ids$Prj_ID)


#### STEP 6: Areas that have not experienced a wildfire ####

final_LTDL_sf$treated_area_m2 <- st_area(final_LTDL_sf)

# Read fire boundaries (commented out if not needed)
# On Desktop: fire_boundaries <- sf::st_read("mtbs_perims_DD.shp")

fire_boundaries <- sf::st_read("/90daydata/aeroldc/WRI/InputData/mtbs_perimeter_data_84_22/mtbs_perims_DD.shp")

# Filter valid geometries (if needed)
valid_fire_boundaries <- fire_boundaries[st_is_valid(fire_boundaries), ]

# Restrict to wildfire areas
wildfires <- valid_fire_boundaries %>% filter(Incid_Type == "Wildfire")

# Check CRS compatibility
st_crs(wildfires) == st_crs(final_LTDL_sf)

# Transform fire boundaries to match CRS of the treatments
fire_boundaries <- st_transform(wildfires, st_crs(final_LTDL_sf))

# Check if CRS transformation was successful
st_crs(fire_boundaries) == st_crs(final_LTDL_sf)

# Contiguous US state boundaries
usa = st_as_sf(maps::map("state", fill = TRUE, plot = FALSE))

# filter to just utah 
UT <- usa |> filter(ID == "utah")

# transform 
UT <- st_transform(UT, st_crs(final_LTDL_sf))

# Trim the fire boundaries to the state of Utah
fires_utah <- st_intersection(fire_boundaries, UT)

# gather the key information for the treatment intersections 
fires_utah_key <- fires_utah |>
  rename(fire_event_id = Event_ID, 
         fire_incid_name = Incid_Name, 
         fire_incid_type = Incid_Type, 
         total_fire_size_ac = BurnBndAc) |>
  select(fire_event_id, fire_incid_name, fire_incid_type, Ig_Date, total_fire_size_ac,  
         geometry)

# Identify intersections between fire boundaries and treatment polygons
intersecting_treatments <- st_intersection(fires_utah_key, final_LTDL_sf, sparse = FALSE)

# gather treatment year and burned treated area
intersecting_treatments <- intersecting_treatments %>%
  mutate(Ig_Date = as.Date(Ig_Date),  # Ensure Ig_Date is in Date format
         FireYear = year(Ig_Date)) # Extract the year) # Convert area to acres (1 square meter = 0.0000229568 acres)        

# so the intersect area is just the burn perimeter 
# for each feature,  calculate area burned
intersecting_treatments$burned_trt_area_m2 <- st_area(intersecting_treatments)

head(intersecting_treatments)

intersecting_treatments$Yer_cmp <- as.numeric(intersecting_treatments$Yer_cmp)

########## for each treatment summarize some fire info
# Summarize key info for the burned treatments
trt_fire <- intersecting_treatments %>%
  mutate(
    prop_area_burned = (burned_trt_area_m2/treated_area_m2)*100, 
    burned_before_trt_Y_N = ifelse(FireYear < Yer_cmp, "Yes", "No"),  # Check if FireYear is before Year_Activity_end
    years_before_treatment_fire = Yer_cmp - FireYear # Calculate years before treatment of fire
  )

## the treatments which burned the same year as treatment (years_before_treatment_fire = 0), seem like they are mostly all immediate post-fire recovery
## those need to be handled seperately
post_fire_trts <- trt_fire |>  filter(years_before_treatment_fire == 0 | years_before_treatment_fire == 1)

n_distinct(post_fire_trts$FeatureID)
# x features burned year of treatment or year before treament

## get the ids for those post-fire treatments
post_fire_trt_ids <- post_fire_trts |> 
  pull(Prj_ID)

############## now sort out the other non-fire treatments 
non_fire_recovery_trts <- final_LTDL_sf |> 
  filter(!Prj_ID %in% post_fire_trt_ids)

# do this for both the post_fire_trts and non_fire_recovery_trts

# Step 1: Mark non-fire recovery treatments with "N"
non_fire_recovery_trts <- final_LTDL_sf |> 
  filter(!Prj_ID %in% post_fire_trt_ids) |>
  mutate(Post_fire_recovery_trt_Y_N = "N")

# Step 2: Mark the remaining treatments (those in post_fire_trt_ids) with "Y"
fire_recovery_trts <- final_LTDL_sf |> 
  filter(Prj_ID %in% post_fire_trt_ids) |>
  mutate(Post_fire_recovery_trt_Y_N = "Y")

# Step 3: Combine both data frames into trts_with_fire_info
trts_with_fire_info <- bind_rows(non_fire_recovery_trts, fire_recovery_trts)

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


#### STEP 8: Create a file that will match with WRI ####

# Summarize by Treatment ID
trts_summarized <- trts_with_fire_info %>%
  group_by(Prj_ID, Year, Post_fire_recovery_trt_Y_N) %>%
  summarize(
    trtID = list(unique(Trt_ID)), # Collect unique trtID values as a list
    ActionDescription = list(unique(Trt_T_M)), # Collect unique descriptions as a list
    Shape = st_union(geometry), # Combine geometries into a single multipolygon
    .groups = "drop" # Avoid the warning about grouping
  ) %>%
  mutate(
    trtID = sapply(trtID, function(x) paste(x, collapse = ", ")), # Collapse list of trtIDs into a string
    ActionDescription = sapply(ActionDescription, function(x) paste(x, collapse = "; ")) # Collapse list of ActionDescriptions into a string
  )

trts_summarized <- trts_summarized %>%
  rename(
    polyID = Prj_ID,
    trtYear = Year,
    post_fire = Post_fire_recovery_trt_Y_N,
    geometry = Shape
  )

# make them the correct CRS
trts_for_DART_reprojected <- st_transform(trts_summarized, crs = 4269)

st_crs(trts_for_DART_reprojected)

# save 
st_write(trts_for_DART_reprojected, 
         "/90daydata/aeroldc/LTDL/LTDL_DART_reprojected.shp", 
         append=FALSE) # append = F means we need to overwrite old layer


### Setup ###
library(sf)
library(tidyverse)
library(ggplot2)
install.packages("rnaturalearth")
library(rnaturalearth)
install.packages("devtools")
library(devtools)
library(writexl)


### The code in this file is cleaning up the polygons so that they are:
# 1. applicable restoration treatments
# 2. occurred within the state of Utah
# 3. occurred within a 12 month period
# 4. have not experienced a wildfire
# 5. are less than 1 km squared

# Reading the in imported and combined shape file
treatment_polygons_cleaned_sf <- st_read("treatment_polygons_cleaned_sf.shp")
head(treatment_polygons_cleaned_sf)


#### STEP 1: applicable restoration treatments ####
# creating an object for the restoration treatments of interest
treatment_types <- c("Herbicide/Weeds/Chemical",
                     "Prescribed Burn",
                     "Seeding",
                     "Soil Stabilization",
                     "Vegetation/Soil Manipulation")

# subsetting the data to include years at 1986, implemented plan, and treatment types
restoration_polygons_sf <- subset(treatment_polygons_cleaned_sf, Year > 1986 & Pln_Imp == "Implemented" & Trt_T_M == treatment_types)


#### STEP 2. occurred within the state of Utah ####
# Step 1: Get Utah boundary (example using rnaturalearth)
usa <- ne_states(country = "united states of america", returnclass = "sf")
utah_boundary <- subset(usa, name == "Utah")

# Step 2: Ensure CRS match
utah_boundary <- sf::st_transform(utah_boundary, crs = sf::st_crs(restoration_polygons_sf))

# Step 3: Filter polygons located within Utah
utah_polygons_sf <- restoration_polygons_sf[sf::st_intersects(restoration_polygons_sf, utah_boundary, sparse = FALSE), ]

# Step 4: View the resulting polygons
head(utah_polygons_sf)

# write this polygon as a shapefile
# st_write(utah_polygons_sf, "C:\\Users\\Kristina\\OneDrive - New Mexico State University\\Desktop\\GIT REPOs\\RAP_DART_WRI\\RAP_DART_WRI\\utah_polygons.shp")

# utah_polygons_df <- sf::st_drop_geometry(utah_polygons_sf)


#### STEP 3: occurred within a 12 month period ####
# Count the number of distinct years for each Prj_ID
year_counts <- utah_polygons_sf %>%
  group_by(Prj_ID) %>%
  summarise(distinct_years = n_distinct(Year), .groups = 'drop')
head(year_counts)
summary(year_counts$distinct_years)

# Step 2: Identify Prj_IDs that occur in exactly one distinct year
single_year_ids <- year_counts$Prj_ID[year_counts$distinct_years == 1]

# Step 3: Filter the original data frame to include only those Prj_IDs
final_single_year_sf <- utah_polygons_sf %>%
  filter(Prj_ID %in% single_year_ids)

# Checking the result
head(final_single_year_sf)

checking <- final_single_year_sf %>%
  filter(Prj_ID == 5431) %>%
  select(Year)

# Display the Year column
checking

# Add a new column to indicate if multiple treatments have occurred in the same year
# Step 1: Add a column that indicates if a Prj_ID occurs multiple times
final_year_sf <- final_single_year_sf  %>%
  group_by(Prj_ID) %>%
  mutate(multiple_treatments = n() > 1) %>%
  ungroup()  # Ensure ungrouping after mutation

# filter out rows that have Year_init before 1991 and rows that have Year_comp after 2018
# Filter rows based on Year_init and Year_comp conditions
final_year_sf_filtered <- final_year_sf %>%
  filter(Year_nt >= 1991 & Yer_cmp <= 2018)

# View the filtered data
head(final_year_sf_filtered)

# confirming that the spatial data is still there
is_spatial_inherits <- inherits(final_year_sf_filtered, "sf")
print(is_spatial_inherits)


#### STEP 4. have not experienced a wildfire ####
## fire boundaries 
fire_boundaries <- sf::st_read("mtbs_perims_DD.shp")

# Filter to keep only valid geometries
valid_fire_boundaries <- fire_boundaries[st_is_valid(fire_boundaries), ]

## now to restrict the treatments to areas outside of wildfires 
wildfires <- valid_fire_boundaries %>%   filter(Incid_Type == "Wildfire")

# check Coordinate refernce system
st_crs(wildfires) == st_crs(final_year_sf_filtered)

# transform one of the polygons to the other 
fire_boundaries <- st_transform(wildfires, st_crs(final_year_sf_filtered))

# check if we fixed it 
st_crs(fire_boundaries) == st_crs(final_year_sf_filtered)

# Identify intersections
intersections <- st_intersects(fire_boundaries, final_year_sf_filtered, sparse = FALSE)

# Find indices of polygons in 'polys' that intersect with any polygon in 'fire_boundaries'
intersecting_indices <- which(rowSums(intersections) > 0)

# Create subset of polys that intersect with fire_boundaries
polys_intersecting <- final_year_sf_filtered[intersecting_indices, ]

# Create subset of polys that do not intersect with fire_boundaries
filtered_nonwildfire_sf <- final_year_sf_filtered[-intersecting_indices, ]


#### STEP 5: are less than 1 km squared ####

# Calculate area (in the same units as the projection, e.g., square meters for UTM)
filtered_nonwildfire_sf$area <- st_area(filtered_nonwildfire_sf)

# Convert area from square meters to square kilometers
filtered_nonwildfire_sf$area_km2 <- filtered_nonwildfire_sf$area / 1e6  # 1 km² = 1,000,000 m²

# Function to calculate the shortest axis length based on the bounding box of each geometry
shortest_axis_length <- function(geom) {
  bbox <- st_bbox(geom)
  min_length <- min(abs(bbox["xmax"] - bbox["xmin"]), abs(bbox["ymax"] - bbox["ymin"]))
  return(min_length)
}

# Apply the function to each geometry in the sf object to get the shortest axis length
filtered_nonwildfire_sf$shortest_axis_length_m <- sapply(st_geometry(filtered_nonwildfire_sf), shortest_axis_length)

# Now you have two new columns: `area` and `shortest_axis_length`
head(filtered_nonwildfire_sf)

# write this polygon as a shapefile
st_write(filtered_nonwildfire_sf, "C:\\Users\\Kristina\\OneDrive - New Mexico State University\\Desktop\\GIT REPOs\\RAP_DART_WRI\\RAP_DART_WRI\\filtered_nonwildfire_sf.shp")



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
# 5. are less than 1/2 km squared **** THIS REQUIREMENT HAS BEEN REMOVE FOR NOW

# Reading the in imported and combined shape file
treatment_polygons_cleaned_sf <- st_read("treatment_polygons_cleaned_sf.shp")
head(treatment_polygons_cleaned_sf)


#### STEP 1: applicable restoration treatments ####
# creating an object for the restoration treatments of interest
treatment_types <- c(198 )

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


#### STEP 3: Received all treatments in the same year ####
# removing any polygons that do not have a treatment completion year
trt_dates <- utah_polygons_sf |>   filter(!is.na(Yer_cmp))
summary(trt_dates)
# none exist!

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

#### STEP 5: Make sure initiation year and end year are the same ####

# Filter and display rows from final_year_sf_filtered where Yer_cmp - Year_nt > 1
result <- final_year_sf_filtered %>%
  # Ensure both columns are numeric
  mutate(
    Yer_cmp = as.numeric(Yer_cmp),
    Year_nt = as.numeric(Year_nt)
  ) %>%
  # Filter rows where the difference is greater than 1
  filter((Yer_cmp - Year_nt) > 1) %>%
  # Select the desired columns
  select(Prj_ID, Yer_cmp, Year_nt)

# Show the resulting dataframe
result

examine <- final_year_sf_filtered %>%
  filter(Prj_ID %in% c(5731, 7840, 10341)) %>%
  select(Prj_ID, Init_Dt, Comp_Dt)

# Show the examine
examine

# Drop the geometry from the result dataframe
result_df <- result %>%
  st_drop_geometry() # Convert 'result' to a regular dataframe by dropping geometry

# Perform the anti_join to remove rows in result while maintaining spatial features
final_year_sf_filtered2 <- final_year_sf_filtered %>%
  filter(!(Prj_ID %in% result_df$Prj_ID))

# Show the new spatial dataframe
final_year_sf_filtered2


#### STEP 4. have not experienced a wildfire ####
## fire boundaries 
fire_boundaries <- sf::st_read("mtbs_perims_DD.shp")

# Filter to keep only valid geometries
valid_fire_boundaries <- fire_boundaries[st_is_valid(fire_boundaries), ]

## now to restrict the treatments to areas outside of wildfires 
wildfires <- valid_fire_boundaries %>%   filter(Incid_Type == "Wildfire")

# check Coordinate refernce system
st_crs(wildfires) == st_crs(final_year_sf_filtered2)

# transform one of the polygons to the other 
fire_boundaries <- st_transform(wildfires, st_crs(final_year_sf_filtered2))

# check if we fixed it 
st_crs(fire_boundaries) == st_crs(final_year_sf_filtered2)

# Identify intersections
intersections <- st_intersects(fire_boundaries, final_year_sf_filtered2, sparse = FALSE)

# Find indices of polygons in 'polys' that intersect with any polygon in 'fire_boundaries'
intersecting_indices <- which(rowSums(intersections) > 0)

# Create subset of polys that intersect with fire_boundaries
polys_intersecting <- final_year_sf_filtered2[intersecting_indices, ]
# this single polygon is not related to a post-fire recovery

# Create subset of polys that do not intersect with fire_boundaries
filtered_nonwildfire_sf3 <- final_year_sf_filtered2[-intersecting_indices, ]


#### STEP 5: are less than half a km squared  ####
# WE ARE REMOVING THIS FOR NOW

# Calculate area (in the same units as the projection, e.g., square meters for UTM)
# filtered_nonwildfire_sf3$area <- st_area(filtered_nonwildfire_sf3)

# Convert area from square meters to square kilometers
# filtered_nonwildfire_sf3$area_km2 <- (filtered_nonwildfire_sf3$area / 1e6) / 2  # 1 km² = 1,000,000 m², divide by 2 for half

# Function to calculate the shortest axis length based on the bounding box of each geometry
# shortest_axis_length <- function(geom) {
  # bbox <- st_bbox(geom)
  # min_length <- min(abs(bbox["xmax"] - bbox["xmin"]), abs(bbox["ymax"] - bbox["ymin"]))
  # return(min_length)
# }

# Apply the function to each geometry in the sf object to get the shortest axis length
# filtered_nonwildfire_sf3$shortest_axis_length_m <- sapply(st_geometry(filtered_nonwildfire_sf3), shortest_axis_length)

# Now you have two new columns: area and shortest_axis_length
# head(filtered_nonwildfire_sf3)

# Extract numeric values from the area_km2 column and convert to numeric
# filtered_nonwildfire_sf3$area_km2_numeric <- as.numeric(str_extract(filtered_nonwildfire_sf3$area_km2, "^[0-9.]+"))

# Filter to keep only those geometries with area greater than 0.5 km²
# filtered_nonwildfire_sf3 <- filtered_nonwildfire_sf3 %>%
  # filter(area_km2_numeric > 0.5)

# Optionally, you can remove the temporary column after filtering
# filtered_nonwildfire_sf3$area_km2_numeric <- NULL

# View the result
# head(filtered_nonwildfire_sf3)

# write this polygon as a shapefile
st_write(filtered_nonwildfire_sf3, "C:\\Users\\Kristina\\OneDrive - New Mexico State University\\Desktop\\GIT REPOs\\Restorationsuccess\\Restorationsuccess\\filtered_nonwildfire_sf.shp")


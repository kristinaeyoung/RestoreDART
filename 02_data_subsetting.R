# NOTES:
# To Do List:
# 1. MAKE A WILDFIRE SPECIFIC CODE FOR TREATMENTS
#   a. remove wildfire boundary from polygon winnowing
#   b. add a wildfire mask when running DART in all cases EXCEPT when looking 
#       at post-fire recovery treatments
#   c. ensure a second run of DART when the post-fire recovery polygons are considered

# MASKS TO INCLUDE IN DART RUN:
# MTBS Fire Boundary
# InterAgencyFire Perimeter https://data-nifc.opendata.arcgis.com/datasets/nifc::interagencyfireperimeterhistory-all-years-view/explore?location=13.716471%2C57.912975%2C2.68&showTable=true
# All treatments within WRI and LTDL
# Mask out roads and parking lots - need to make CONUS wide

# TO DO:
# determine cut off for how well the environmental pixel match between trt and cntr (e.g., Gowers distance)
# start lumping treatment types
# pull together different options for models



### Setup ###
library(sf)
library(tidyverse)
library(ggplot2)
# install.packages("rnaturalearth")
library(rnaturalearth)
# install.packages("devtools")
library(devtools)
library(writexl)


### The code in this file is cleaning up the polygons so that they are:
# STEP 1. applicable restoration treatments
# STEP 2. occurred within the state of Utah
# STEP 3. Received all treatments in the same year
# STEP 4. are not in wetlands or riparian areas
# STEP 5. have not experienced a wildfire **** THIS REQUIREMENT HAS BEEN REMOVED 
# STEP 6. are less than 1/2 km squared **** THIS REQUIREMENT HAS BEEN REMOVED FOR NOW

# Reading the in imported and combined shape file
treatment_polygons_cleaned_sf <- st_read("treatment_polygons_cleaned_sf.shp")
head(treatment_polygons_cleaned_sf)


#### STEP 1: applicable restoration treatments ####
unique_values_mjr <- unique(treatment_polygons_cleaned_sf$Trt_T_M)
print(unique_values_mjr)

# creating an object for the restoration treatments of interest
treatment_types <- c("Herbicide/Weeds/Chemical",
                     "Prescribed Burn",
                     "Seeding",
                     "Soil Stabilization",
                     "Vegetation/Soil Manipulation")

# subsetting the data to include years at 1986, implemented plan, and treatment types
restoration_polygons_sf <- subset(treatment_polygons_cleaned_sf, Year > 1986 & Pln_Imp == "Implemented" & Trt_T_M == treatment_types)

# Looking at categorizations of treatments
unique_values_sub <- unique(restoration_polygons_sf$Trt_T_S)
print(unique_values_sub)

unique_values_T <- unique(restoration_polygons_sf$Trtmn_T)
print(unique_values_T)


#### STEP 2. occurred within the state of Utah ####
# Step 2.1: Get Utah boundary (example using rnaturalearth)
usa <- ne_states(country = "united states of america", returnclass = "sf")
utah_boundary <- subset(usa, name == "Utah")

# Step 2.2: Ensure CRS match
utah_boundary <- sf::st_transform(utah_boundary, crs = sf::st_crs(restoration_polygons_sf))

# Step 2.3: Filter polygons located within Utah
utah_polygons_sf <- restoration_polygons_sf[sf::st_intersects(restoration_polygons_sf, utah_boundary, sparse = FALSE), ]

# Step 2.4: View the resulting polygons
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

# Step 3.1: Identify Prj_IDs that occur in exactly one distinct year
single_year_ids <- year_counts$Prj_ID[year_counts$distinct_years == 1]

# Step 3.2: Filter the original data frame to include only those Prj_IDs
final_single_year_sf <- utah_polygons_sf %>%
  filter(Prj_ID %in% single_year_ids)

# Checking the result
head(final_single_year_sf)

checking <- final_single_year_sf %>%
  filter(Prj_ID == 5431) %>%
  select(Year)

# Display the Year column
checking

# Step 3.3: Add a column that indicates if a Prj_ID occurs multiple times
# Add a new column to indicate if multiple treatments have occurred in the same year
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

#### Step 3.4: Make sure initiation year and end year are the same ####

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

# Assuming final_year_sf_filtered is your data frame
result <- final_year_sf_filtered %>%
  group_by(Prj_ID) %>%                                 # Group by Prj_ID
  summarise(
    unique_Trt_IDs = n_distinct(Trt_ID),              # Count distinct Trt_IDs
    Trt_T_M = paste(unique(Trt_T_M), collapse = ", ") # Concatenate unique Trt_T_M values
  ) %>%
  filter(unique_Trt_IDs > 1)                          # Keep only those with more than 1 unique Trt_ID

# Print the result
print(result)

# Show the new spatial dataframe
final_year_sf_filtered2

# showing how many rows have the same Prj_ID but different Trt_ID
# Assuming final_year_sf_filtered2 is your data frame
result <- final_year_sf_filtered2 %>%
  group_by(Prj_ID) %>%                                 # Group by Prj_ID
  summarise(
    unique_Trt_IDs = n_distinct(Trt_ID),              # Count distinct Trt_IDs
    Trt_T_M = paste(unique(Trt_T_M), collapse = ", ") # Concatenate unique Trt_T_M values
  ) %>%
  filter(unique_Trt_IDs > 1)                          # Keep only those with more than 1 unique Trt_ID

# Print the result
print(result)


#### STEP 4. are not in wetlands or riparian areas ####
# SOURCE of Wetland and riparian spatial data:
# https://www.fws.gov/program/national-wetlands-inventory/download-state-wetlands-data

## wetland boundaries 
wetlands_UT <- sf::st_read("Utah_Wetlands.shp")

# Filter to keep only valid geometries
valid_wetland_boundaries <- wetlands_UT[st_is_valid(wetlands_UT), ]

# check Coordinate reference system
st_crs(valid_wetland_boundaries) == st_crs(final_year_sf_filtered2)

# transform one of the polygons to the other 
wetland_boundaries <- st_transform(valid_wetland_boundaries, st_crs(final_year_sf_filtered2))

# check if we fixed it 
st_crs(wetland_boundaries) == st_crs(final_year_sf_filtered2)

# Identify intersections
intersections <- st_intersects(wetland_boundaries, final_year_sf_filtered2, sparse = FALSE)

# Find indices of polygons in 'polys' that intersect with any polygon in 'wetland_boundaries'
intersecting_indices <- which(rowSums(intersections) > 0)

# Create subset of polys that intersect with wetland_boundaries
polys_intersecting <- final_year_sf_filtered2[intersecting_indices, ]

# Create subset of polys that do not intersect with wetland_boundaries
final_year_sf_filtered3 <- final_year_sf_filtered2[-intersecting_indices, ]
# There are no overlaps with wetlands

## RIPARIAN BOUNDARIES
riparian_UT <- sf::st_read("UT_Riparian.shp")

# Filter to keep only valid geometries
valid_riparian_boundaries <- riparian_UT[st_is_valid(riparian_UT), ]

# check Coordinate reference system
st_crs(valid_riparian_boundaries) == st_crs(final_year_sf_filtered3)

# transform one of the polygons to the other 
riparian_boundaries <- st_transform(valid_riparian_boundaries, st_crs(final_year_sf_filtered3))

# check if we fixed it 
st_crs(riparian_boundaries) == st_crs(final_year_sf_filtered3)

# Identify intersections
intersections_rip <- st_intersects(riparian_boundaries, final_year_sf_filtered3, sparse = FALSE)

# Find indices of polygons in 'polys' that intersect with any polygon in 'riparian_boundaries'
intersecting_indices_rip <- which(rowSums(intersections_rip) > 0)

# Create subset of polys that intersect with riparian_boundaries
polys_intersecting_rip <- final_year_sf_filtered3[intersecting_indices_rip, ]

# Create subset of polys that do not intersect with riparian_boundaries
final_year_sf_filtered4 <- final_year_sf_filtered3[-intersecting_indices_rip, ]
# There are no overlaps with riparian


#### STEP 5. have not experienced a wildfire ####
## fire boundaries 
# fire_boundaries <- sf::st_read("mtbs_perims_DD.shp")

# Filter to keep only valid geometries
# valid_fire_boundaries <- fire_boundaries[st_is_valid(fire_boundaries), ]

## now to restrict the treatments to areas outside of wildfires 
# wildfires <- valid_fire_boundaries %>%   filter(Incid_Type == "Wildfire")

# check Coordinate reference system
# st_crs(wildfires) == st_crs(final_year_sf_filtered4)

# transform one of the polygons to the other 
# fire_boundaries <- st_transform(wildfires, st_crs(final_year_sf_filtered4))

# check if we fixed it 
# st_crs(fire_boundaries) == st_crs(final_year_sf_filtered4)

# Identify intersections
# intersections <- st_intersects(fire_boundaries, final_year_sf_filtered4, sparse = FALSE)

# Find indices of polygons in 'polys' that intersect with any polygon in 'fire_boundaries'
# intersecting_indices <- which(rowSums(intersections) > 0)

# Create subset of polys that intersect with fire_boundaries
# polys_intersecting <- final_year_sf_filtered4[intersecting_indices, ]
# this single polygon is not related to a post-fire recovery

# Create subset of polys that do not intersect with fire_boundaries
# filtered_nonwildfire_sf5 <- final_year_sf_filtered4[-intersecting_indices, ]


#### STEP 6: are less than half a km squared  ####

# Calculate area (in the same units as the projection, e.g., square meters for UTM)
final_year_sf_filtered4$area <- st_area(final_year_sf_filtered4)

# Convert area from square meters to square kilometers
final_year_sf_filtered4$area_km2 <- (final_year_sf_filtered4$area / 1e6) / 2  # 1 km² = 1,000,000 m², divide by 2 for half

# Function to calculate the shortest axis length based on the bounding box of each geometry
shortest_axis_length <- function(geom) {
  bbox <- st_bbox(geom)
  min_length <- min(abs(bbox["xmax"] - bbox["xmin"]), abs(bbox["ymax"] - bbox["ymin"]))
  return(min_length)
 }

# Apply the function to each geometry in the sf object to get the shortest axis length
final_year_sf_filtered4$shortest_axis_length_m <- sapply(st_geometry(final_year_sf_filtered4), shortest_axis_length)

# Now you have two new columns: area and shortest_axis_length
head(final_year_sf_filtered4)

# Ensure area_km2 is numeric
final_year_sf_filtered4$area_km2 <- as.numeric(final_year_sf_filtered4$area_km2)

# Create a histogram of treated area sizes
ggplot(final_year_sf_filtered4, aes(x = area_km2)) +
  geom_histogram(binwidth = 0.1, fill = "steelblue", color = "black") +  # Adjust binwidth as needed
  labs(
    title = "Distribution of Treated Area Size",
    x = "Treated Area Size (km²)",
    y = "Frequency"
  ) +
  theme_minimal() +                             # Use a minimal theme for better appearance
  scale_x_continuous(breaks = seq(0, 80, by = 10))

# Filter out areas greater than 10 km²
filtered_data <- final_year_sf_filtered4[final_year_sf_filtered4$area_km2 <= 10, ]

# Create a histogram of treated area sizes
ggplot(filtered_data, aes(x = area_km2)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "black") +  # Set binwidth to 1 km²
  labs(
    title = "Distribution of Treated Area Size (≤ 10 km²)",
    x = "Treated Area Size (km²)",
    y = "Frequency"
  ) +
  theme_minimal() +                             # Use a minimal theme for better appearance
  scale_x_continuous(breaks = seq(0, 10, by = 1))

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
st_write(final_year_sf_filtered4, "C:\\Users\\Kristina\\OneDrive - New Mexico State University\\Desktop\\GIT REPOs\\Restorationsuccess\\Restorationsuccess\\filtered_nonwildfire_sf.shp")


# Looking at categorizations of treatments
# Count unique values in Trt_T_S column
unique_values_sub_counts <- table(final_year_sf_filtered4$Trt_T_S)
print(unique_values_sub_counts)

# Count unique values in Trtmn_T column
unique_values_T_counts <- table(final_year_sf_filtered4$Trtmn_T)
print(unique_values_T_counts)

counts_table <- final_year_sf_filtered4 %>%
group_by(Trt_T_M, Trt_T_S, Trtmn_T) %>%
  summarise(count = n()) %>%
  arrange(Trt_T_S, Trtmn_T)

# Print the resulting table
print(counts_table)

# Export the table to an Excel file
write_xlsx(counts_table, "counts_table1.xlsx")

##### THIS IS THE ORIGINAL FILE OF ALL THE CODE
# THIS HAS BEEN SEGMENTED DOWN to 01 and 02 R script files
# Do NOT use this code after 2024_10_03


#### Figure out a way to filter out riparian projects
#### Get rid of complex spatial features

---
  # title: "Scope of Work - USDA Jornada Experimental Range Post-Doc"
  # author: "Kristina Young"
  # date STARTED: "2023-03-14"
  # date UPDATED: "2023-07-20"
  ---
  
#### SETUP ####
# Attach sf for its functions. Note: you will receive an error, this is something related to the package
install.packages("sf")
library(sf)
# Attach the tidyverse for tidyr, dplyr, and ggplot functions
install.packages("tidyverse")
library(tidyverse)
# Attach trex for the LDC access
# install.packages("trex")

#library(trex)
# Attach beepr to tell you when your code has run
# install.packages("beepr")
library(beepr)
# Attach beeper to give dings when code has run
# beepr::beep(3) 
# install.packages(lub)
library(lubridate)
# Attach lubridate to work on dates

#### CONFIG ####
# Setting the path to the geodatabase 
gdb_path <- "C:\\Users\\Kristina\\OneDrive - New Mexico State University\\Desktop\\GIT REPOs\\Restorationsuccess\\Restorationsuccess\\LTDL_July_2022_Release_Geodatabase\\LTDL_Release_20220715.gdb"
# Setting the path for Nelson who is helping with this code
# gdb_path <- "C:/Users/Nelson/Desktop/garbage/ltdl/LTDL_Release_20220715.gdb"

# Reading in the treatment polygon feature class
polygons_layer_name <- "LTDL_Treatment_Polygons"

# Reading in the treatment information from the geodatabase
lookup_table_name_treatment <- "treatment_info"
# Reading in the project information from the geodatabase
lookup_table_name_project <- "project_info"

#### READING ####
# Reading in the polygons
# Maintaining _sf and _df in naming conventions as indicators of data base type 
# Maintaining argument names for future readability (e.g., dsn = gdb_path) 
treatment_polygons_sf <- sf::st_read(dsn = gdb_path,
                                     layer = polygons_layer_name)

# saving as a shape file

# st_write(treatment_polygons_sf, "C:/Users/Kristina/Documents/USDA_ARS/PROJECTS/RESEARCH/CURRENT/LDC_LTDL/Data/my_shapefile.shp")

# converting to kml for google earth enginge
# filepath <- "C:/Users/Kristina/Documents/USDA_ARS/PROJECTS/RESEARCH/CURRENT/LDC_LTDL/Data/LTDL_July_2022_Release_Geodatabase"
# output_path <- "C:/Users/Kristina/Documents/USDA_ARS/PROJECTS/RESEARCH/CURRENT/LDC_LTDL/Data"
# gdb_name <- "LTDL_Release_20220715.gdb"

# What feature classes are in the GDB?
# available_features <- sf::st_layers(dsn = paste0(filepath, "/",
                                               #  gdb_name))

# Which of the available features have the word "Polygons" in their name?
# polygon_feature_names <- available_features$name[grep(x = available_features$name,
                                                     # pattern = "Polygons")]

# Read in the polygon features
# polygon_features <- lapply(X = polygon_feature_names,
                          # dsn = paste0(filepath, "/",
                                       # gdb_name),
                          # FUN = function(X, dsn){
                            # sf::st_read(dsn = dsn,
                                       # layer = X)
                          # })

# Make sure that list of polygon features is named (for writing purposes)
# names(polygon_features) <- polygon_feature_names

# Write out the polygons!
# lapply(X = polygon_feature_names,
  #     polygons = polygon_features,
   #    output_path = output_path,
    #   FUN = function(X, polygons, output_path){
     #    sf::st_write(obj = polygons[[X]], "ltdl_two.shp")
      # })

# Reading in the lookup tables
treatment_lookup_table_treatment <- sf::st_read(dsn = gdb_path,
                                        layer = lookup_table_name_treatment)
treatment_lookup_table_project <- sf::st_read(dsn = gdb_path,
                                        layer = lookup_table_name_project)

# Using a left_join to bring in the treatment and project tables
treatment_lookup_table <- left_join(x = treatment_lookup_table_treatment,
                                    y = treatment_lookup_table_project)
# Note you will get a warning message here
# This is because the lookup tables do not store geometries 

# Import the headers info from the Landscape Data Commons API
# headers_df <- trex::fetch_ldc(data_type = "header")
# Import the headers info from a save desktop file
# headers_df <- read.csv("headers_df.csv")

######Indicators#####
# Import the headers info from the Landscape Data Commons API
# indicators_df <- trex::fetch_ldc(data_type = "indicators")
# Import the indicators data from a save desktop file
# indicators_df <- read.csv("indicators_df.csv")

# Note: headers_df and indicators_df have different numbers of observations
# missing_primarykeys <- headers_df$PrimaryKey[!(headers_df$PrimaryKey %in% indicators_df$PrimaryKey)]
# There are 90 PrimaryKeys that are missing their indicator information

#### MUNGING ####
##### Cleaning coordinates #####
# Header info includes coordinates in NAD8 
# Converting the data frame into an sf object
#headers_sf <- sf::st_as_sf(x = headers_df,
                           #coords = c("Longitude_NAD83",
                                     #"Latitude_NAD83"),
                          #crs = "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs +type=crs")

#####Changing geometries#####

# Changing the treatment polygons from "GEOMETRY" to the more specific "MULTIPOLYGON"
treatment_polygons_sf <- sf::st_cast(x = treatment_polygons_sf,
                                     to = "MULTIPOLYGON")

# Ensuring the projections match
# Applying the Coordinate Reference System from headers_sf to the polygons
# treatment_polygons_sf <- sf::st_transform(x = treatment_polygons_sf,
                                         # crs = sf::st_crs(headers_sf))

##### Cleaning errors in database #####
# Sanitizing geometry errors within the polygon collection
# Turn off spherical coordinates (the devs recommend this)
sf::sf_use_s2(FALSE)
# Make the geometry valid
treatment_polygons_repaired_sf <- sf::st_make_valid(treatment_polygons_sf)
# Turn the spherical coordinates back on
# Note: we are leaving spherical coordinates off to avoid errors. 
# Package developers indicate it is ok to leave off
# sf::sf_use_s2(TRUE)
# Buffer by 0 to make sure that there are no self-intersections
# Using st_transform() because st_buffer() uses meters instead of degrees 
treatment_polygons_repaired_sf <- sf::st_buffer(x = sf::st_transform(treatment_polygons_repaired_sf, crs = "+proj=aea +lat_1=29.5 +lat_2=42.5"),
                                                dist = 0)
# Note: Maintaining each sf object as a distinct object to avoid overwriting data
# Re-projecting back into degrees now that it's buffered
treatment_polygons_repaired_sf <- sf::st_transform(x = treatment_polygons_repaired_sf,
                                                   crs = "+proj=aea +lat_1=29.5 +lat_2=42.5")

# Note: This will result in a warning due to the re-projections from degrees to meters (Albers Equal Area)

#### ATTRIBUTION ####
#####Joining polygons - treatment table #####
# Step one: add the lookup table info to the polygons
# Using merge() from the base installation of R or dplyr::left_join()
# left_join keeps all records in x (the polygons) 
# Don't need to specify a by argument because the identifying variables have
# the same names in both: Trt_ID and Prj_ID
treatment_polygons_attributed_sf <- dplyr::left_join(x = treatment_polygons_repaired_sf,
                                                     y = treatment_lookup_table)

########################## GOOGLE EARTH ENGINE ################################
#### Writing out a shape file of restoration LTDL for Google Earth Engine ####

# Creating comparable years for the completed date
# Using stringr::str_extract() to extract part of the date strings
# Specifically:
# 1) the first four consecutive digits it can find (the year)
# 2) coerces that from a string into a numeric value
treatment_polygons_attributed_sf$Year_comp <- as.numeric(stringr::str_extract 
                                        (string = treatment_polygons_attributed_sf$Comp_Date, pattern = "\\d{4}"))
# Removing all of the NAs
bad_date_indices <- is.na(treatment_polygons_attributed_sf$Year_comp)

# Stripping out all the rows/observations with bad dates in either variable:
# 1) gather all the indices where the sampling dates were not (!): NAs 
# 2) AND the completion dates were not (!): bad
treatment_polygons_attributed_sf <- treatment_polygons_attributed_sf[!bad_date_indices, ]
# Checking to see if the data framelooks good
treatment_polygons_attributed_sf$Year_comp
# examining the 
head(treatment_polygons_attributed_sf)

# Changing the year to a character (string) from a number
treatment_polygons_attributed_sf$Year <- as.character(treatment_polygons_attributed_sf$Year_comp)




# Creating comparable years for the completed date
# Using stringr::str_extract() to extract part of the date strings
# Specifically:
# 1) the first four consecutive digits it can find (the year)
# 2) coerces that from a string into a numeric value
treatment_polygons_attributed_sf$Year_comp <- as.numeric(stringr::str_extract 
                                                         (string = treatment_polygons_attributed_sf$Comp_Date, pattern = "\\d{4}"))
# Removing all of the NAs
bad_date_indices <- is.na(treatment_polygons_attributed_sf$Year_comp)

# Stripping out all the rows/observations with bad dates in either variable:
# 1) gather all the indices where the sampling dates were not (!): NAs 
# 2) AND the completion dates were not (!): bad
treatment_polygons_attributed_sf <- treatment_polygons_attributed_sf[!bad_date_indices, ]
# Checking to see if the data framelooks good
treatment_polygons_attributed_sf$Year_comp
# examining the 
head(treatment_polygons_attributed_sf)

# Changing the year to a character (string) from a number
treatment_polygons_attributed_sf$Year_comp <- as.character(treatment_polygons_attributed_sf$Year_comp)


# Creating comparable years for the initiation date
# Using stringr::str_extract() to extract part of the date strings
# Specifically:
# 1) the first four consecutive digits it can find (the year)
# 2) coerces that from a string into a numeric value
treatment_polygons_attributed_sf$Year_init <- as.numeric(stringr::str_extract 
                                                         (string = treatment_polygons_attributed_sf$Init_Date, pattern = "\\d{4}"))
# Removing all of the NAs
bad_date_indices <- is.na(treatment_polygons_attributed_sf$Year_init)

# Stripping out all the rows/observations with bad dates in either variable:
# 1) gather all the indices where the sampling dates were not (!): NAs 
# 2) AND the completion dates were not (!): bad
treatment_polygons_attributed_sf <- treatment_polygons_attributed_sf[!bad_date_indices, ]
# Checking to see if the data framelooks good
treatment_polygons_attributed_sf$Year_init
# examining the 
head(treatment_polygons_attributed_sf)

# Changing the year to a character (string) from a number
treatment_polygons_attributed_sf$Year_init <- as.character(treatment_polygons_attributed_sf$Year_init)


# creating an object for the restoration treatments of interest
treatment_types <- c("Herbicide/Weeds/Chemical",
                     "Prescribed Burn",
                     "Seeding",
                     "Soil Stabilization",
                     "Vegetation/Soil Manipulation")


# subsetting the data to include years at 1986, implemented plan, and treatment types
restoration_polygons_sf <- subset(treatment_polygons_attributed_sf, Year > 1986 & Plan_Imp == "Implemented" & Trt_Type_Major == treatment_types)

# Export to Excel
# install.packages("writexl")

# writexl::write_xlsx(implemented_polygons_sf, "C:\\Users\\Kristina\\OneDrive - New Mexico State University\\Desktop\\GIT REPOs\\Restorationsuccess\\Restorationsuccess\\implemented_ltdl_df.xlsx")

### Reducing polygon to state of Utah
# Step 1: Get Utah boundary (example using rnaturalearth)
install.packages("rnaturalearth")
library(rnaturalearth)
install.packages("devtools")
library(devtools)

usa <- ne_states(country = "united states of america", returnclass = "sf")
utah_boundary <- subset(usa, name == "Utah")

# Step 2: Ensure CRS match
utah_boundary <- sf::st_transform(utah_boundary, crs = sf::st_crs(restoration_polygons_sf))

# Step 3: Filter polygons located within Utah
utah_polygons_sf <- restoration_polygons_sf[sf::st_intersects(restoration_polygons_sf, utah_boundary, sparse = FALSE), ]

# Step 4: View the resulting polygons
head(utah_polygons_sf)

# write this polygon as a shapefile
st_write(utah_polygons_sf, "C:\\Users\\Kristina\\OneDrive - New Mexico State University\\Desktop\\GIT REPOs\\RAP_DART_WRI\\RAP_DART_WRI\\utah_polygons.shp")

is_spatial <- st_is(utah_polygons_sf, "sf")
print(is_spatial)

# exporting Utah as a dataframe
# library(writexl)

# utah_polygons_df <- sf::st_drop_geometry(utah_polygons_sf)


##### Removing Prj_IDs with multiple Trt_IDs from utah_polygons_sf #####
# Removing projects that had multiple treatment events
project_record_counts <- table(treatment_lookup_table$Prj_ID)
# Finding projects that occur only once
single_project_ids <- names(project_record_counts)[project_record_counts == 1]
# Slicing data to only records that correspond to those projects
utah_sf <- utah_polygons_sf[utah_polygons_sf$Prj_ID %in% single_project_ids, ]

object.size(single_records_sf)

attach(single_records_sf)
sorted_df <- single_records_sf[order(GlobalID), ]
detach(single_records_sf)


#### MAINTAINING POLYGONS THAT HAVE MULTIPLE TREATMENTS IN THE SAME YEAR ####

# Step 1: Count the number of distinct years for each Prj_ID
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
is_spatial <- st_is(final_single_year_sf, "sf")
print(is_spatial)

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
# Step 1: Filter rows based on Year_init and Year_comp conditions
final_year_sf_filtered <- final_year_sf %>%
  filter(Year_init >= 1991 & Year_comp <= 2018)

# View the filtered data
head(final_year_sf_filtered)

# confirming that the spatial data is still there
is_spatial_inherits <- inherits(final_year_sf_filtered, "sf")
print(is_spatial_inherits)

# removing layers with fire

## fire boundareis 
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

####### FILTERING BY POLYGON SIZE #######

# 1. Calculate area (in the same units as the projection, e.g., square meters for UTM)
filtered_nonwildfire_sf$area <- st_area(filtered_nonwildfire_sf)
# Convert area from square meters to square kilometers
filtered_nonwildfire_sf$area_km2 <- filtered_nonwildfire_sf$area / 1e6  # 1 km² = 1,000,000 m²

# 2. Function to calculate the shortest axis length based on the bounding box of each geometry
shortest_axis_length <- function(geom) {
  bbox <- st_bbox(geom)
  min_length <- min(abs(bbox["xmax"] - bbox["xmin"]), abs(bbox["ymax"] - bbox["ymin"]))
  return(min_length)
}

# 3. Apply the function to each geometry in the sf object to get the shortest axis length
filtered_nonwildfire_sf$shortest_axis_length_m <- sapply(st_geometry(filtered_nonwildfire_sf), shortest_axis_length)

# Now you have two new columns: `area` and `shortest_axis_length`
head(filtered_nonwildfire_sf)



# write this polygon as a shapefile
# st_write(filtered_nonwildfire_sf, "C:\\Users\\Kristina\\OneDrive - New Mexico State University\\Desktop\\GIT REPOs\\RAP_DART_WRI\\RAP_DART_WRI\\filtered_nonwildfire_sf.shp")

# Deciding on the polygons to send to Gayle to test

specific_prj_id <- 157  # Replace this with the Prj_ID you are interested in

# Filter the dataset for the specific Prj_ID
specific_row <- filtered_nonwildfire_sf %>%
  filter(Prj_ID == specific_prj_id)

# Display the full row (with all headers/columns) for the specific Prj_ID
print(specific_row)


### Picking specific prj_IDs to sent to for DART analysis
# Seeding 157, 1033
# Vegetation/Soil Manipulation 1101, 2389 
# Prescribed burn 3352, 5550
# Herbicide/Weesd/Chemicals 16774,1223

# Define the specific Prj_IDs to include
selected_prj_ids <- c(157, 1033, 1101, 2389, 3352, 5550, 16774, 12233)

# Create a new sf object that filters the selected Prj_IDs, selects the desired columns, and renames them
filtered_sf <- filtered_nonwildfire_sf %>%
  filter(Prj_ID %in% selected_prj_ids) %>%  # Filter based on the selected Prj_IDs
  select(Prj_ID, Trt_Type_Major, Year_comp) %>%  # Select the specified columns
  rename(polyID = Prj_ID, trtYear = Year_comp)  # Rename the columns

# Display the new sf data
print(filtered_sf)


## Mapping out the polygons to look at interactively
# Load the required libraries
# Load the required libraries
library(leaflet)
library(sf)
library(RColorBrewer)

# Ensure polyID is a factor
filtered_map_sf$polyID <- as.factor(filtered_sf$polyID)

# Create a color palette based on polyID as a factor
palette <- colorFactor(palette = brewer.pal(n = length(unique(filtered_sf$polyID)), "Set3"), 
                       domain = filtered_sf$polyID)

# Calculate centroids of the polygons for map centering
centroids <- st_centroid(filtered_sf)

# Extract mean longitude and latitude for centering the map
mean_lon <- mean(st_coordinates(centroids)[, 1], na.rm = TRUE)
mean_lat <- mean(st_coordinates(centroids)[, 2], na.rm = TRUE)

# Create a leaflet map
leaflet(data = filtered_map_sf) %>%
  addTiles() %>%  # Add default OpenStreetMap tiles
  addPolygons(color = ~palette(polyID), weight = 1, fillOpacity = 0.5, 
              highlightOptions = highlightOptions(color = "red", weight = 2, bringToFront = TRUE),
              popup = ~paste("polyID:", polyID, "<br>",
                             "Activity ended in:", trtYear, "<br>",
                             "Methods: ", Trt_Type_Major, "<br>")) %>%
  setView(lng = mean_lon, lat = mean_lat, zoom = 10) %>%  # Center the map
  addLegend("bottomright", pal = palette, values = filtered_sf$polyID, 
            title = "polyID", 
            labFormat = labelFormat(prefix = "polyID: "))


# writing poylgons to send for DART analysis
st_write(filtered_sf, "C:\\Users\\Kristina\\OneDrive - New Mexico State University\\Desktop\\GIT REPOs\\RAP_DART_WRI\\RAP_DART_WRI\\filtered_sf.shp")

## read in the WRI polygons
WRI_sf <- sf::st_read("wri_test_trt.shp")

head(WRI_sf)
head(filtered_sf)

WRI_renamed_sf <- WRI_sf %>% rename(Trt_Type_Major = Trt_T_M, SHAPE = geometry) %>% select (polyID, Trt_Type_Major, trtYear, SHAPE)

test_dart_polys_sf <- rbind(WRI_renamed_sf, filtered_sf)

st_write(test_dart_polys_sf, "C:\\Users\\Kristina\\OneDrive - New Mexico State University\\Desktop\\GIT REPOs\\Restorationsuccess\\Restorationsuccess\\test_dart_polys_sf.shp")












#######NEW CODE##########
#### MAINTAINING FULL LTDL
# Export to Excel
writexl::write_xlsx(utah_polygons_df, "C:\\Users\\Kristina\\OneDrive - New Mexico State University\\Desktop\\GIT REPOs\\RAP_DART_WRI\\RAP_DART_WRI\\utah_polygons.xlsx")

# creating an object for the restoration treatments of interest
treatment_types <- c("Closure/Exclosure",
                              "Herbicide/Weeds/Chemical",
                              "Prescribed Burn",
                              "Seeding",
                              "Soil Stabilization",
                              "Biological Control",
                              "Vegetation/Soil Manipulation")


# subsetting the data to include years at 1986, implemented plan, and treatment types
restoration_polygons_sf <- subset(treatment_polygons_attributed_sf, Year > 1986 & Plan_Imp == "Implemented" & Trt_Type_Major == treatment_types)

joined_sf <- sf::st_join(x = restoration_polygons_sf,
                         y = headers_sf)







##### Removing Prj_IDs with multiple Trt_IDs from restoration_df #####
# Removing projects that had multiple treatment events
project_record_counts <- table(treatment_lookup_table$Prj_ID)
# Finding projects that occur only once
single_project_ids <- names(project_record_counts)[project_record_counts == 1]
# Slicing data to only records that correspond to those projects
single_records_sf <- joined_sf[joined_sf$Prj_ID %in% single_project_ids, ]

object.size(single_records_sf)

attach(single_records_sf)
sorted_df <- single_records_sf[order(GlobalID), ]
detach(single_records_sf)

st_write(single_records_sf, "C:/Users/Kristina/Documents/USDA_ARS/PROJECTS/RESEARCH/CURRENT/LDC_LTDL/Data/shapefile_subset/combined_polys_real.shp")

single_one_sf <- subset(single_records_sf, Prj_ID == "978")
single_two_sf <- subset(single_records_sf, Prj_ID == "71")
single_three_sf <- subset(single_records_sf, Prj_ID == "122")
single_four_sf <- subset(single_records_sf, Prj_ID == "1871")
single_five_sf <- subset(single_records_sf, Prj_ID == "173")
single_six_sf <- subset(single_records_sf, Prj_ID == "22190")
single_seven_sf <- subset(single_records_sf, Prj_ID == "7861")
single_eight_sf <- subset(single_records_sf, Prj_ID == "11258")
single_nine_sf <- subset(single_records_sf, Prj_ID == "2793")
single_ten_sf <- subset(single_records_sf, Prj_ID == "206")

combined_polys <- rbind(single_one_sf, single_two_sf, single_three_sf, single_four_sf, single_five_sf, single_six_sf, single_seven_sf, single_eight_sf, single_nine_sf, single_ten_sf)

coordinates_deg <- spTransform(combined_polys, CRS("+init=epsg:4326"))

st_write(combined_polys, "C:/Users/Kristina/Documents/USDA_ARS/PROJECTS/RESEARCH/CURRENT/LDC_LTDL/Data/shapefile_subset/combined_polys.shp")


# Writing out the subset Shape File
one <- st_write(single_one_sf, "C:/Users/Kristina/Documents/USDA_ARS/PROJECTS/RESEARCH/CURRENT/LDC_LTDL/Data/shapefile_subset/single_one.shp")
st_write(single_two_sf, "C:/Users/Kristina/Documents/USDA_ARS/PROJECTS/RESEARCH/CURRENT/LDC_LTDL/Data/shapefile_subset/single_two.shp")
st_write(single_three_sf, "C:/Users/Kristina/Documents/USDA_ARS/PROJECTS/RESEARCH/CURRENT/LDC_LTDL/Data/shapefile_subset/single_three.shp")
st_write(single_four_sf, "C:/Users/Kristina/Documents/USDA_ARS/PROJECTS/RESEARCH/CURRENT/LDC_LTDL/Data/shapefile_subset/single_four.shp")
st_write(single_five_sf, "C:/Users/Kristina/Documents/USDA_ARS/PROJECTS/RESEARCH/CURRENT/LDC_LTDL/Data/shapefile_subset/single_five.shp")
st_write(single_six_sf, "C:/Users/Kristina/Documents/USDA_ARS/PROJECTS/RESEARCH/CURRENT/LDC_LTDL/Data/shapefile_subset/single_six.shp")
st_write(single_seven_sf, "C:/Users/Kristina/Documents/USDA_ARS/PROJECTS/RESEARCH/CURRENT/LDC_LTDL/Data/shapefile_subset/single_seven.shp")
st_write(single_eight_sf, "C:/Users/Kristina/Documents/USDA_ARS/PROJECTS/RESEARCH/CURRENT/LDC_LTDL/Data/shapefile_subset/single_eight.shp")
st_write(single_nine_sf, "C:/Users/Kristina/Documents/USDA_ARS/PROJECTS/RESEARCH/CURRENT/LDC_LTDL/Data/shapefile_subset/single_nine.shp")
st_write(single_ten_sf, "C:/Users/Kristina/Documents/USDA_ARS/PROJECTS/RESEARCH/CURRENT/LDC_LTDL/Data/shapefile_subset/single_ten.shp")

# addressing problem shapefiles

poly_fourteen <- subset(single_records_sf, Prj_ID == "14")
poly_fifteen <- subset(single_records_sf, Prj_ID == "15")
poly_sixteen <- subset(single_records_sf, Prj_ID == "16")

st_write(poly_fourteen, "C:/Users/Kristina/Documents/USDA_ARS/PROJECTS/RESEARCH/CURRENT/LDC_LTDL/Data/shapefile_subset/poly_fourteen.shp")
st_write(poly_fifteen, "C:/Users/Kristina/Documents/USDA_ARS/PROJECTS/RESEARCH/CURRENT/LDC_LTDL/Data/shapefile_subset/poly_fifteen.shp")
st_write(poly_sixteen, "C:/Users/Kristina/Documents/USDA_ARS/PROJECTS/RESEARCH/CURRENT/LDC_LTDL/Data/shapefile_subset/poly_sixteen.shp")


#################### END GOOGLE EARTH ENGINGE SCRIP ##########################


#################### CREATING A DATAFRAME FILE ##########################

MLRA_sf <- st_read("NA_CEC_Eco_Level2.shp")
MLRA_sf

combined_polys <- st_read("combined_polys_real.shp")
combined_polys

combined_polys_sf <- sf::st_transform(x = combined_polys,
                                          crs = sf::st_crs(MLRA_sf))

dataframe_sf <- sf::st_join(x = MLRA_sf,
                                    y = combined_polys_sf)

dataframe_con_sf <- dataframe_sf[!is.na(as.numeric(dataframe_sf$Prj_ID)), ] 

export.df <- dataframe_con_sf %>% st_drop_geometry()
class(export.df)

write.csv(export.df, "C:/Users/Kristina/Documents/USDA_ARS/PROJECTS/RESEARCH/CURRENT/LDC_LTDL/Data/shapefile_subset/exportdf_1.csv", row.names=TRUE)



#####Joining LDC and LTDL#####
# Polygons now have their additional attributes
# Using a spatial join to add header info to the points
# Note: st_join() has an argument called "left" that defaults to TRUE
# This will keep everything from x whether it intersected something from y or not
# To change this: left = FALSE, 
points_attributed_sf <- sf::st_join(x = headers_sf,
                                    y = treatment_polygons_attributed_sf)

# Making a data frame from the sf object
points_attributed_df <- sf::st_drop_geometry(points_attributed_sf)

#### QUALITY ASSURANCE ####
# Ensure that the join worked properly
# Checking for duplicate rows using the unique PrimaryKey
# How to check for duplicates in a data frame variable:
# 1) Make contents of the variable as a vector (technically, a data frame is
# a list of vectors)
attributed_primarykeys <- points_attributed_df$PrimaryKey
# 2) Use table() to summarize the vector. 
# This will give us a named numeric vector where each value in the vector is a count 
# and the name for each value is the value from the original vector that was counted
# Example: table(c("hello", "hi", "hello")) 
# results in: c("hello" = 2, "hi" = 1)
attributed_primarykeys_counts <- table(attributed_primarykeys)
# 3) Figure out which counts are > 1
# >1 counts are duplicated values
# To do this: make a logical vector
# Logical vector: all indices with a value > 1 are TRUE and all others are FALSE
duplicated_primarykeys_indices <- attributed_primarykeys_counts > 1
# 4) Make an object with names of the indices where the count was > 1
duplicate_primarykeys <- names(attributed_primarykeys_counts)[duplicated_primarykeys_indices]

#####Removing duplicates#####
# This data frame has duplicated PrimaryKey values
# Filter the attributed points to only duplicates and inspect manually
# To do this:
# 1) Get the indices in the sf object of all the values in the PrimaryKey variable 
# that are also found in our vector of duplicated keys
indices_of_duplicate_keys <- points_attributed_df$PrimaryKey %in% duplicate_primarykeys
# 2) Use View() to look at the sf_object filtered to only those indices
# When filtering a two-dimensional object like a data frame:
# Use [, ] specifying the rows to keep before the comma, variables to keep after the comma but not specifying results in returning all of them,
# e.g., dataframe[1:10, ] would return all the variables, but only the first 10 rows
# e.g., dataframe[, c("type", "date")] would return all rows, but only the variables type and date
# e.g., dataframe[, ] would return everything

# Inspect the duplicated data
# View(points_attributed_sf[indices_of_duplicate_keys, ])

# Within this dataframe: 
# single Prj_ID can have multiple overlapping polygons with distinct Trt_ID values
# Take into account when producing summary statistics

# Joining the points attributed data with the indicators data. 
# Specifying the specific keys to join by (PrimaryKey, DBKey, ProjectKey, and DateVisited)
points_attributed_df <- left_join(x = points_attributed_df,
                                  y = indicators_df,
                                  by = c("PrimaryKey", "DBKey", "ProjectKey", "DateVisited"))

#### SUMMARIZING ####

# Examining the points/plots pairing related to restoration
# Making a vector of all the restoration treatment types 
relevant_treatment_types <- c("Closure/Exclosure",
                              "Herbicide/Weeds/Chemical",
                              "Prescribed Burn",
                              "Seeding",
                              "Vegetation/Soil Manipulation")

# Making a data from for just restoration actions:
# Creating a logical vector that's TRUE for every index where: 
# Trt_Type_Major had a value that appears in relevant_treatment_types
# Note: use %in% because asking if NA == anything returns "NA" and not "FALSE"
relevant_treatment_indices <- points_attributed_df$Trt_Type_Major %in% relevant_treatment_types
# Creating a logical vector that's TRUE at every index where:
# the value in Plan_Imp was "Implemented"
implemented_indices <- points_attributed_df$Plan_Imp %in% "Implemented"
# Taking points_attributed_df and slicing/subseting it using [, ] to create restoration_df
# Note: don't specify anything after the comma 
# Anything after the comma is for subsetting columns and we want all the columns
# Indicating the row indices where:
# relevant_treatment_indices AND implemented_indices are true before the comma
restoration_df <- points_attributed_df[relevant_treatment_indices & implemented_indices, ]


##### How long since sampled #####
# Creating comparable dates 
# 1) Convert the variable Comp_Date ("completion date") from:
# a string (e.g. "1/1/1964") to a date value
# Use lubridate::mdy() (can do this because dats are formatted as Month/Day/Year) 
# restoration_df$treatment_completion_date <- lubridate::mdy(restoration_df$Comp_Date)
# Converting the DateVisited from the AIM data
# restoration_df$sampling_date <- lubridate::as_date(restoration_df$DateVisited)
# Note: this is turned off because we are focusing on year instead of full dates

# Creating comparable years
# Using stringr::str_extract() to extract part of the date strings
# Specifically:
# 1) the first four consecutive digits it can find (the year)
# 2) coerces that from a string into a numeric value
restoration_df$treatment_completion_year <- as.numeric(stringr::str_extract 
                                                       (string = restoration_df$Comp_Date, pattern = "\\d{4}"))
# Note: multiple rows will fail to parse but will be fixed below 

# Grabbing the year from the string and coercing it to numeric
restoration_df$sampling_year <- as.numeric(stringr::str_extract
                                           (string = restoration_df$DateVisited,pattern = "\\d{4}"))
# No parsing errors returned
# Checking for NA values
na_sampling_year_indices <- is.na(restoration_df$sampling_year)
sum(na_sampling_year_indices)
# NAs present
# Note: multiple failures to parse because:
# 1) this is a large & homogeneous data 
# 2) its been built over years by many different people
# Examining the kinds of values failed to parse and produced NAs
bad_date_indices <- is.na(restoration_df$treatment_completion_year)
bad_dates <- restoration_df$Comp_Date[bad_date_indices]
# table(bad_dates)
# Many bad dates that should be thrown out during analysis
# Checking for: "1/1/1970" which is often present in big datasets
# Because: time is calculated as "seconds elapsed since midnight January 1st, 1970"
# Asking if it's %in% because any inequality compared with NA will return NA
sum(restoration_df$Comp_Date %in% c("1/1/1970"))
# No hits returned

# Stripping out all the rows/observations with bad dates in either variable:
# 1) gather all the indices where the sampling dates were not (!): NAs 
# 2) AND the completion dates were not (!): bad
restoration_df <- restoration_df[!na_sampling_year_indices & !bad_date_indices, ]

#####Creating columns for time difference##### 

# Creating new columns for differences in years between:
# When the treatment occurred and when the sampling took place
restoration_df$year_diff <- restoration_df$sampling_year - restoration_df$treatment_completion_year

# Removing the treatments that occurred after the LDC plots were surveyed
restoration_df <- filter(restoration_df, year_diff > 0)

##### Making an output data frame #####
# Creating a data frame with *all* the points in it
# Including whether they coincide with a restoration treatment
# 1) get all the unique PrimaryKey values in restoration_df
# those are all the keys associated with restoration treatments
primarykeys_associated_with_treatment <- unique(restoration_df$PrimaryKey)
# This makes a logical vector that's TRUE for: 
# a) every index in data_good_dates_df$PrimaryKey
# b) that corresponds to a value found in primarykeys_associated_with_treatment
restoration_treated_indices_in_attributed_points <- points_attributed_df$PrimaryKey %in% primarykeys_associated_with_treatment
# Making a new column to store the TRUE/FALSE value for whether it's associated
# Populate new column with FALSE for now
points_attributed_df$restoration_treated <- FALSE
# In "restoration_treated" column: write TRUE for indices associated with restoration 
points_attributed_df[restoration_treated_indices_in_attributed_points, "restoration_treated"] <- TRUE

##### Keeping only the most recent records within the LDC for points_attributed_df #####
# Removing plots that have been visited multiple times by LDC
# List data in order according to date sampled
# We get the indices in ascending order for the dates
data_order <- order(points_attributed_df$DateVisited,
                    decreasing = TRUE)
# Then we use those indices to reorder the data frame
data_ordered_df <- points_attributed_df[data_order, ]

# Splitting the data frame into a list of data frames
# Each of the new data frames is the records for a single plot
data_list <- split(x = data_ordered_df,
                   f = data_ordered_df$PrimaryKey)
# Using lapply():
# 1) Go through each index of the list 
# 2) execute code we'll define as a temporary function
# lapply() returns a list where:
# each of the indices corresponds to one of the items in the input list
# Here it will return a list of data frames with one row each:
# the first row from each of those data frames we made with split()
data_reduced_list <- lapply(X = data_list,
                            FUN = function(X) {
                              # Inside this function, X is going to be whatever
                              # the current index is, so a data frame here.
                              # Slicing it so we just get the first row,
                              # which should be the most recent sampling event
                              # because we reordered everything already
                              X[1, ]
                              # The functions inside apply(), lapply(), and mapply()
                              # can be as long as they need to. In this case,
                              # we only needed one line
                            })
# Combining a list of data frames:
# do.call() lets us repeat rbind() for all the items in the list
# avoids writing a loop to bind them all
clean_attributed_df <- do.call(rbind,
                                  data_reduced_list)

# CREATING COLUMNS FOR COMPARABLE YEARS WITHIN clean_attributed_df
# Using stringr::str_extract() to extract part of the date strings
# Specifically:
# 1) the first four consecutive digits it can find (the year)
# 2) coerces that from a string into a numeric value
clean_attributed_df$treatment_completion_year <- as.numeric(stringr::str_extract (string = clean_attributed_df$Comp_Date, pattern = "\\d{4}"))
# Note: multiple rows will fail to parse but will be fixed below 

# Grabbing the year from the string and coercing it to numeric
clean_attributed_df$sampling_year <- as.numeric(stringr::str_extract
                                           (string = clean_attributed_df$DateVisited,pattern = "\\d{4}"))
# No parsing errors returned
# Checking for NA values
na_sampling_year_indices <- is.na(clean_attributed_df$sampling_year)
sum(na_sampling_year_indices)
# NAs present
# Note: multiple failures to parse because:
# 1) this is a large & homogeneous data 
# 2) its been built over years by many different people
# Examining the kinds of values failed to parse and produced NAs
bad_date_indices <- is.na(clean_attributed_df$treatment_completion_year)
bad_dates <- clean_attributed_df$Comp_Date[bad_date_indices]
table(bad_dates)

# Stripping out all the rows/observations with bad dates in either variable:
# 1) gather all the indices where the sampling dates were not (!): NAs 
# 2) AND the completion dates were not (!): bad
shiny_attributed_df <- clean_attributed_df[!na_sampling_year_indices, ]

#####Creating columns for time difference##### 

# Creating new columns for differences in years between:
# When the treatment occurred and when the sampling took place
shiny_attributed_df$year_diff <- shiny_attributed_df$sampling_year - shiny_attributed_df$treatment_completion_year

#### THINGS TO CONSIDER WITHIN THIS DATA SET:
# 1) Other types of land treatments not assigned as "restoration" are present
#   in points_attributed_df
# 2) There are polygons where:
#    a. multiple restoration treatments were administered
#    b. restoration treatments occurred in multiple years

write.csv(shiny_attributed_df, "/Users/Kristina/complete_df.csv")

beepr::beep(3)

################################################################################
###The following lines of code are dealing with the RESTORATION_DF only####
##### Removing Prj_IDs with multiple Trt_IDs from restoration_df #####
# Removing projects that had multiple treatment events
# Identifying all the Prj_ID values that:
# 1) correspond to multiple Trt_ID values
# 2) occur more than once 
# 3) correspond to multiple treatments 
# Every combination of Prj_ID and Trt_ID should be unique
project_record_counts <- table(treatment_lookup_table$Prj_ID)
# Finding projects that occur only once
single_project_ids <- names(project_record_counts)[project_record_counts == 1]
# Slicing data to only records that correspond to those projects
single_records_df <- restoration_df[restoration_df$Prj_ID %in% single_project_ids, ]

write.csv(single_records_df, "/Users/Kristina/single_records.csv")

##### Keeping only the most recent records within the LDC #####
# Removing plots that have been visited multiple times by LDC
# List data in order according to date sampled
# We get the indices in ascending order for the dates
data_order <- order(single_records_df$DateVisited,
                    decreasing = TRUE)
# Then we use those indices to reorder the data frame
data_ordered_df <- single_records_df[data_order, ]

# Splitting the data frame into a list of data frames
# Each of the new data frames is the records for a single plot
data_list <- split(x = data_ordered_df,
                   f = data_ordered_df$PrimaryKey)
# Using lapply():
# 1) Go through each index of the list 
# 2) execute code we'll define as a temporary function
# lapply() returns a list where:
# each of the indices corresponds to one of the items in the input list
# Here it will return a list of data frames with one row each:
# the first row from each of those data frames we made with split()
data_reduced_list <- lapply(X = data_list,
                            FUN = function(X) {
                              # Inside this function, X is going to be whatever
                              # the current index is, so a data frame here.
                              # Slicing it so we just get the first row,
                              # which should be the most recent sampling event
                              # because we reordered everything already
                              X[1, ]
                              # The functions inside apply(), lapply(), and mapply()
                              # can be as long as they need to. In this case,
                              # we only needed one line
                            })
# Combining a list of data frames:
# do.call() lets us repeat rbind() for all the items in the list
# avoids writing a loop to bind them all
reduced_restoration_df <- do.call(rbind,
                           data_reduced_list)

write.csv(reduced_restoration_df, "/Users/Kristina/reduced_restoration_df1.csv")

beepr::beep(3)

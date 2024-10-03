### Setup ###
library(sf)
library(tidyverse)
library(ggplot2)
install.packages("rnaturalearth")
library(rnaturalearth)
install.packages("devtools")
library(devtools)
library(writexl)

### This is code to create a series of test poylgons to run DART on
# This was done in Sept 2024

# Reading the in imported and combined shape file
st_read(filtered_nonwildfire_sf, "filtered_nonwildfire_sf.shp")

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



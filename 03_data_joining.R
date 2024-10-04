# JOINING LTDL POLYGONS WITH LDC POINTS

# STEP 1: Attach LDC and import heads and indicators
# STEP 2: Create a new dataframe with only overlapping LTDL and LDC plots
# STEP 3: Create map showing overlap LTDL and LDC plots


### Setup ###
library(sf)
library(tidyverse)
library(ggplot2)
install.packages("rnaturalearth")
library(rnaturalearth)
install.packages("devtools")
library(devtools)
library(writexl)
library(lubridate)

devtools::install_github("landscape-data-commons/trex", build_vignettes = TRUE)

# Attach trex for the LDC access
# install.packages("trex")
library(trex)

# read in data from 02_data_subsetting
filtered_sf <- st_read("filtered_nonwildfire_sf.shp")
head(filtered_sf)


# STEP 1: Attach LDC and import heads and indicators
# Import the headers info from the Landscape Data Commons API
headers_df <- trex::fetch_ldc(data_type = "header")
# Import the headers info from a save desktop file
# headers_df <- read.csv("headers_df.csv")

######Indicators#####
# Import the headers info from the Landscape Data Commons API
indicators_df <- trex::fetch_ldc(data_type = "indicators")
# Import the indicators data from a save desktop file
# indicators_df <- read.csv("indicators_df.csv")

##### Cleaning coordinates #####
# Header info includes coordinates in NAD8 
# Converting the data frame into an sf object
headers_sf <- sf::st_as_sf(x = headers_df,
                           coords = c("Longitude_NAD83",
                                      "Latitude_NAD83"),
                           crs = "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs +type=crs")
# Check CRS
st_crs(filtered_sf)  # For polygons
st_crs(headers_sf)      # For the fetched data

# Reproject if necessary
filtered_sf <- st_transform(filtered_sf, st_crs(headers_sf))

joined_sf <- sf::st_join(x = filtered_sf,
                         y = headers_sf)

# Attach 'indicators_df' to 'joined_sf' using 'PrimaryKey' as the join key
joined_sf <- joined_sf %>%
  left_join(indicators_df, by = "PrimaryKey")

number <- sum(!is.na(joined_sf$Prj_ID) & !is.na(joined_sf$PrimaryKey))
number

# Create a new column 'YearVisited' by extracting the year from 'DateVisited.y'
joined_sf$YearLDCVisited <- year(ymd_hms(joined_sf$DateVisited.y))

# Ensure that YearLDCVisited and Yer_cmp are numeric
joined_sf$YearLDCVisited <- as.numeric(joined_sf$YearLDCVisited)
joined_sf$Yer_cmp <- as.numeric(joined_sf$Yer_cmp)

# Check if the conversion was successful
str(joined_sf)


#### STEP 2: Create a new dataframe with only overlapping LTDL and LDC plots
# Make a df that only includes rows with LDC data and that only include post-treatment visits
LTDL_LDC_sf <- joined_sf %>%
  filter(!is.na(PrimaryKey) & (YearLDCVisited - Yer_cmp) > 1)

# Check to make sure the above command worked
check_df <- LTDL_LDC_sf %>%
  select(PrimaryKey, YearLDCVisited, Yer_cmp)


#### STEP 3: Create map showing overlap LTDL and LDC plots
# Make a map of the polygons that also have LDC data
ggplot() +
  # Add the polygons from joined_sf
  geom_sf(data = LTDL_LDC_sf, fill = "lightgreen", color = "darkgreen", alpha = 0.5) +
  
  # Add labels and titles
  labs(title = "Map of Polygons and Points",
       subtitle = "Polygons from geodatabase and points from columns x and y in joined_sf",
       x = "Longitude",
       y = "Latitude") +
  
  # Set the minimal theme for a clean map appearance
  theme_minimal()

  
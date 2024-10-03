# this is code that joins the filtered LTDL polygons with LDC plots

devtools::install_github("landscape-data-commons/trex", build_vignettes = TRUE)

# Attach trex for the LDC access
# install.packages("trex")
library(trex)

filtered_sf <- st_read("filtered_nonwildfire_sf.shp")
head(filtered_sf)

wri_ldc <- trex::fetch_ldc_spatial(polygons = filtered_sf,
                                   data_type = "indicators")


# Import the headers info from the Landscape Data Commons API
headers_df <- trex::fetch_ldc(data_type = "header")
# Import the headers info from a save desktop file
headers_df <- read.csv("headers_df.csv")

######Indicators#####
# Import the headers info from the Landscape Data Commons API
indicators_df <- trex::fetch_ldc(data_type = "indicators")
# Import the indicators data from a save desktop file
indicators_df <- read.csv("indicators_df.csv")

# Note: headers_df and indicators_df have different numbers of observations
missing_primarykeys <- headers_df$PrimaryKey[!(headers_df$PrimaryKey %in% indicators_df$PrimaryKey)]
# There are 90 PrimaryKeys that are missing their indicator information

##### Cleaning coordinates #####
# Header info includes coordinates in NAD8 
# Converting the data frame into an sf object
headers_sf <- sf::st_as_sf(x = headers_df,
                           coords = c("Longitude_NAD83",
                                      "Latitude_NAD83"),
                           crs = "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs +type=crs")

joined_sf <- sf::st_join(x = restoration_polygons_sf,
                         y = headers_sf)
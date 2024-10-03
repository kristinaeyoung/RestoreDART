
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
library(sf)
# Attach the tidyverse for tidyr, dplyr, and ggplot functions
library(tidyverse)
# Attach lubridate to deal with dates
library(lubridate)

#### CONFIG ####
# Setting the path to the geodatabase 
gdb_path <- "C:\\Users\\Kristina\\OneDrive - New Mexico State University\\Desktop\\GIT REPOs\\Restorationsuccess\\Restorationsuccess\\LTDL_July_2022_Release_Geodatabase\\LTDL_Release_20220715.gdb"

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

#####Changing geometries#####

# Changing the treatment polygons from "GEOMETRY" to the more specific "MULTIPOLYGON"
treatment_polygons_sf <- sf::st_cast(x = treatment_polygons_sf,
                                     to = "MULTIPOLYGON")

# Ensuring the projections match
# Applying the Coordinate Reference System from headers_sf to the polygons
# treatment_polygons_sf <- sf::st_transform(x = treatment_polygons_sf,
# crs = sf::st_crs(headers_sf))

##### Projecting into the same geometry #####
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

st_write(treatment_polygons_attributed_sf, "C:\\Users\\Kristina\\OneDrive - New Mexico State University\\Desktop\\GIT REPOs\\Restorationsuccess\\Restorationsuccess\\treatment_polygons_cleaned_sf.shp")


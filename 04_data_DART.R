---
  # title: "DART Run for WRI/LTDL"
  # author: "Kristina Young"
  # date STARTED: "2023-03-14"
  # date UPDATED: "2023-07-20"
  ---
  
# Accesing SciNet:
  # 1: Log into SciNet https://scinet.usda.gov/guides/access/
  # 2: 90daydata/aeroldc
  # 3: Request R studio session, my interactive session, RStudio, server
      # Run under aeroldc (or home directory)
      # To change: how many hours, adjust cores (5 or 10)
      # Memory required: 50 - 100G
      # Queue: scavenger mode (Max Time: 21:00)
      # Submit request
  # 4: Connect to RStudio Session
  
  # 5: DART Landscape: 
      # GH -> DART -> DART_running -> needed to run DART
      # DART_rasters - environmental data
      # DART_regions_of_interest -> polygons that we are working from
  
  # 6: DART paramterizations (params_conus_GH.R)
        # dart_functions_conus -> creating the functions used within DART (run these)
        # params_conus_GH -> changes settings in DART and adjusting file paths to rasters and shape files
            # outdir = (whatever folder you are working in)
            # Treatment areas -> update filepath to wherever it is in CERES (90daydata/aeroldc/GH)
            # rad = radisu to search for reference pixels
  # 7 : DART running (1_run_dart_conus.R)
        # setwd(" X")
        # dpar is the parameter needed
  
  
  #### SETUP ####
# Attach sf for its functions. Note: you will receive an error, this is something related to the package
library(sf)
# Attach the tidyverse for tidyr, dplyr, and ggplot functions
library(tidyverse)
# Attach lubridate to deal with dates
library(lubridate)
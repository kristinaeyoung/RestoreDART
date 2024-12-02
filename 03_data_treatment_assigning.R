# Load Required Libraries
library(sf)
library(tidyverse)
library(ggplot2)
library(rnaturalearth)
library(devtools)
library(writexl)

LTDL_Data <- st_read("C:\\Users\\Kristina\\OneDrive - New Mexico State University\\Desktop\\GIT REPOs\\Restorationsuccess\\Restorationsuccess\\filtered_LTDL_polys.shp")

# Checking for the number of Projects that have multiple treatments
duplicate_prj_ids <- LTDL_Data %>%
  group_by(Prj_ID) %>%
  summarize(count = n(), .groups = "drop") %>%
  filter(count > 1)

# View the resulting table
print(duplicate_prj_ids)

# Viewing the full data table of Project duplicates
duplicate_rows <- LTDL_Data %>%
  filter(Prj_ID %in% duplicate_prj_ids$Prj_ID)

# View the resulting data frame
print(duplicate_rows)


#### STEP 1: Examine treatment type distribution ####
# Examine treatment type distributions
# Count unique values in the Trt_T_S column
subtype_counts <- table(LTDL_Data$Trt_T_S)
print(subtype_counts)

# Count unique values in the Trtmn_T column
main_treatment_counts <- table(LTDL_Data$Trtmn_T)
print(main_treatment_counts)

# Summarize counts by treatment categories and subtypes
counts_table <- LTDL_Data %>%
  group_by(Trt_T_M, Trt_T_S, Trtmn_T) %>%
  summarise(count = n()) %>%
  arrange(Trt_T_S, Trtmn_T)

# Display the counts table
print(counts_table)


### Step 2: Subset data and start assigning master names ###

# Creating a sorted table by Trt_ID and displaying values from Objects
LTDL_filtered_all <- LTDL_Data %>%
  select(Prj_ID, Trt_ID, Objctvs, Trtmn_T, Trt_T_S, Trt_T_M, Objctvs, Plnnd_I, Actl_Im, Trt_Cnc, Trt_C_D) %>%
  arrange(Trtmn_T)

print(LTDL_filtered_all)

write_xlsx(LTDL_filtered_all, "LTDL_filtered_all.xlsx")

view(Trt_ID$"20741")

# Filter rows where Trt_ID is 20741
filtered_rows <- LTDL_filtered_all %>%
  filter(Trt_ID == 20741)

# View the resulting rows
print(filtered_rows)

#### STEP 2: Examining vegetation treatments and what they mean ####

# Define the set of values you want to keep in Trt_T_S
values_to_keep <- c(
  "Tree/Brush Control: Lop and Scatter",
  "Tree/Brush Control: Manual",
  "Tree/Brush Control: Mechanical",
  "Tree/Brush Control: Pinyon Juniper Removal",
  "Tree/Brush Control: Pinyon Juniper Removal - Hand Cutting",
  "Tree/Brush Control: Pinyon Juniper Removal - Mechanical Cutting",
  "Tree/Brush Control: Thin and Pile",
  "Vegetation Disturbance: Chipping",
  "Vegetation Disturbance: Mastication",
  "Vegetation Disturbance: Mechanical Thinning",
  "Vegetation Disturbance: Mowing",
  "Vegetation Disturbance: Bulldozing"
)

# Subset the data to keep only the rows where Trt_T_S is in the specified values
LTDL_Data_veg <- LTDL_Data %>%
  filter(Trtmn_T %in% values_to_keep)

# View the subsetted data
view(LTDL_Data_veg)

# Create a sorted table by Trtmn_T and displaying values from Objctvs
sorted_table <- LTDL_Data_veg %>%
  select(Prj_ID, Objctvs, Trtmn_T) %>%
  arrange(Trtmn_T)

# View the sorted table
head(sorted_table)

# Export the table to an Excel file
write_xlsx(sorted_table, "sorted_table.xlsx")

filtered_data <- LTDL_Data %>% 
  filter(Prj_ID == "3671")

# View the filtered data
write_xlsx(filtered_data, "data.xlsx")

soil_values <- c("Soil Disturbance: Chaining",
"Soil Disturbance: Discing",
"Soil Disturbance: Harrowing",
"Soil Disturbance: Plowing",
"Soil Disturbance: Rollerchopping"
)

# Subset the data to keep only the rows where Trtmn_T is in the specified values
LTDL_Data_soil <- LTDL_Data %>%
  filter(Trtmn_T %in% soil_values)

# Create a sorted table by Trtmn_T and displaying values from Objctvs
sorted_soil_table <- LTDL_Data_soil%>%
  select(Prj_ID, Objctvs, Trtmn_T) %>%
  arrange(Trtmn_T)

# View the filtered data
write_xlsx(sorted_soil_table, "soil.xlsx")

seed_values <- c("Ground Seeding"
)

# Subset the data to keep only the rows where Trtmn_T is in the specified values
seed_data <- LTDL_Data %>%
  filter(Trtmn_T %in% seed_values)

# Create a sorted table by Trtmn_T and displaying values from Objctvs
seed_data_table <- seed_data%>%
  select(Prj_ID, Objctvs, Trtmn_T) %>%
  arrange(Trtmn_T)

# View the filtered data
write_xlsx(seed_data_table, "seed.xlsx")


burn_values <- c("Prescribed Burn")

# Subset the data to keep only the rows where Trtmn_T is in the specified values
burn_data <- LTDL_Data %>%
  filter(Trtmn_T %in% burn_values)

# Create a sorted table by Trtmn_T and displaying values from Objctvs
burn_data_table <- burn_data%>%
  select(Prj_ID, Objctvs, Trtmn_T) %>%
  arrange(Trtmn_T)

# View the filtered data
write_xlsx(burn_data_table, "burn.xlsx")


Herbicide Application
Herbicide Application: Brush/Tree Control
Noxious Weeds: Weed Control - Herbicide Application

# HERBICIDES
herb_values <- c("Herbicide Application",
                 "Herbicide Application: Brush/Tree Control",
                 "Noxious Weeds: Weed Control - Herbicide Application"
)
# Subset the data to keep only the rows where Trtmn_T is in the specified values
herb_data <- LTDL_Data %>%
  filter(Trtmn_T %in% herb_values)

# Create a sorted table by Trtmn_T and displaying values from Objctvs
herb_data_table <- herb_data %>%
  select(Prj_ID, Objctvs, Trtmn_T) %>%
  arrange(Trtmn_T)

# View the filtered data
write_xlsx(herb_data_table, "herb.xlsx")

# Slash Treatment: Slash piling
slash_values <- c("Slash Treatment: Slash piling"
)
# Subset the data to keep only the rows where Trtmn_T is in the specified values
slash_data <- LTDL_Data %>%
  filter(Trtmn_T %in%slash_values)

# Create a sorted table by Trtmn_T and displaying values from Objctvs
slash_data_table <- slash_data %>%
  select(Prj_ID, Objctvs, Trtmn_T) %>%
  arrange(Trtmn_T)

# View the filtered data
write_xlsx(herb_data_table, "herb.xlsx")

# ground seeding
ground_values <- c("Ground Seeding: Drill",
"Ground Seeding: ATV Broadcast",
"Ground Seeding: Broadcast",
"Ground Seeding: Dribble",
"Ground Seeding: Hand Broadcast",
"Ground Seeding: Imprinting"
)

# Subset the data to keep only the rows where Trtmn_T is in the specified values
ground_data <- LTDL_Data %>%
  filter(Trtmn_T %in% ground_values)

# Create a sorted table by Trtmn_T and displaying values from Objctvs
ground_data_table <- ground_data %>%
  select(Prj_ID, Objctvs, Trtmn_T) %>%
  arrange(Trtmn_T)

# View the filteredd data
write_xlsx(ground_data_table, "ground.xlsx")


# Aerial Seeding

aerial_values <- c("Aerial Seeding",
"Aerial Seeding: Fixed Wing",
"Aerial Seeding: Rotor Wing"
)

# Subset the data to keep only the rows where Trtmn_T is in the specified values
aerial_data <- LTDL_Data %>%
  filter(Trtmn_T %in% aerial_values)

# Create a sorted table by Trtmn_T and displaying values from Objctvs
aerial_data_table <- aerial_data %>%
  select(Prj_ID, Objctvs, Trtmn_T) %>%
  arrange(Trtmn_T)

# View the filteredd data
write_xlsx(aerial_data_table, "aerial_data.xlsx")

# Greenstrip

green_values <- c("Greenstrip: Ground Seeding"
)

# Subset the data to keep only the rows where Trtmn_T is in the specified values
green_data <- LTDL_Data %>%
  filter(Trtmn_T %in% green_values)

# Create a sorted table by Trtmn_T and displaying values from Objctvs
green_data_table <- green_data %>%
  select(Prj_ID, Objctvs, Trtmn_T) %>%
  arrange(Trtmn_T)

# View the filteredd data
write_xlsx(green_data_table, "green_data.xlsx")




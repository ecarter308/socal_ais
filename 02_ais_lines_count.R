####

# 2. AIS Lines Count #

# The AIS Lines Count script was run after "1. Intersecting AIS Features" which extracted 2023 West Coast AIS lines that intersected the BOEM aliquot grid for Southern California. This script was created by Isaac Keohane and modified by Eliza Carter to count the number of AIS transects per grid cell for Southern California for every month of 2023. The script separates the transectc counts by vessel type, which is determined in the "ais_processing_functions" script. Vessel codes can be found here: https://coast.noaa.gov/data/marinecadastre/ais/VesselTypeCodes2018.pdf

# Date created: December 2024

####

library(sf)
library(stringr)
library(matrixStats)
library(dplyr)
library(units)
library(lubridate)
library(terra)
library(tidyterra)
source("C:/Users/Eliza.Carter/Documents/Projects/California/socal_ais/code/ais_processing_functions.R") # make sure path is correct
fname <- basename(rstudioapi::getSourceEditorContext()$path) #get the name of the current file
start0 <- Sys.time()


###########

###
# output directory
out_dir = "C:/Users/Eliza.Carter/Documents/Projects/California/socal_ais/data/c_transect_data"


###########

file_month = "202312"

# dsn of merged lines for the relevant year
dsn_lines = file.path(stringr::str_glue("C:/Users/Eliza.Carter/Documents/Projects/California/socal_ais/data/b_intermediate_data/wc_{file_month}_transects_socal.shp"))  # make sure this is right

########
# pull the relevant lines datasets
lines_month = sf::st_read(dsn_lines, quiet = T) 

#project
lines_month = st_transform(lines_month, "EPSG:32611")

lines_month = st_make_valid(lines_month)

lines_month = lines_month[st_geometry_type(lines_month)=="LINESTRING",]

lines_month$ShipTypeCat <- categorize_ship_types(lines_month$vessel_typ) # from ais_processing_functions.R
########


#######

# read in the hex grid
fishnet_polygon <- st_read("C:/Users/Eliza.Carter/Documents/Projects/California/socal_ais/data/b_intermediate_data/socal_boem_grid.shp")


fishnet_polygon <- st_zm(fishnet_polygon, drop = T, what = "ZM") #drop Z and M fields

fishnet_polygon = st_transform(fishnet_polygon, "EPSG:32611")

fishnet_polygon  = st_make_valid(fishnet_polygon)

fishnet_polygon$fn_id = seq(nrow(fishnet_polygon)) # add an id field
# create a version of the fishnet that is a line for each grid instead of polygons
fishnet_boundary_lines = st_cast(fishnet_polygon, "MULTILINESTRING")

#######

# Extract the intersecting points between the lines and each grid cell in the fishnet 
# Do this separately for each ship type
ship_types = c("Cargo", "Tanker", "Tug_Tow", "Fishing", "Military", "Passenger", "Pleasure", "Other")

print(paste("start loop"))

#fishnet_polygon$n_unique_total = 0

for(j in seq(length(ship_types))){
  # get just the lines for this ship (vessel) type
  vessel_lines <- lines_month %>%
    filter(ShipTypeCat == ship_types[j])
  
  if (nrow(vessel_lines) > 0) {
    ###
    # generate points for all of the intersections between ais lines and the grid cell boundaries
    line_intersect_points = st_intersection(fishnet_boundary_lines, vessel_lines) %>%
      st_cast(., "MULTIPOINT") %>%
      st_cast(., "POINT")
    
    # generate endpoint intersections
    line_end_pts <- st_coordinates(st_cast(vessel_lines, "MULTILINESTRING")) %>% # get coordinates of line vertices
      as.data.frame(.) %>% # convert from matrix to data frame
      group_by(c(L2)) %>%  # group by the origninal line it came from
      slice(c(1,n())) %>%  # get the first and last point of the linestring
      st_as_sf(., coords=c("X", "Y"), crs=st_crs(vessel_lines))
    
    end_pt_intscts = st_join(fishnet_polygon, line_end_pts, join=st_contains, left=FALSE)
    
    # combine end point intersections and wall intersections
    intsct_data = rbind(st_drop_geometry(line_intersect_points) %>% select(fn_id), 
                        st_drop_geometry(end_pt_intscts) %>% select(fn_id))
    
    # # count unique IDs per grid cell
    # unique_counts = st_drop_geometry(line_intersect_points) %>% select(fn_id, doc)
    # 
    # new_colname=paste0("doc_", ship_types[j])
    # 
    # unique_counts = unique_counts %>%
    #   group_by(fn_id) %>%
    #   summarise(!!new_colname := n_distinct(doc))
    # 
    # fishnet_polygon = fishnet_polygon %>% left_join(unique_counts, by="fn_id")
  
    # count the number of point intersections for each fishnet grid (using the fishnet id)
    intsct_data = intsct_data %>%
      group_by(fn_id) %>%
      summarise(!!ship_types[j] := n()) # set column name to ship type
    
    # join the counts for this ship type back to the fishnet polygon
    fishnet_polygon = fishnet_polygon %>%
      left_join(intsct_data, by=c("fn_id"))
  } else {
    fishnet_polygon = fishnet_polygon %>% mutate(!!ship_types[j] := 0)
  }
  
  print(paste(j, Sys.time()))
}
######

# replace NA with 0, divide by 2
fishnet_polygon = fishnet_polygon %>%
  mutate(across(all_of(ship_types), 
                ~ as.numeric(ifelse(is.na(.x), 0, .x/2)) )) %>%
  mutate(month = file_month)

#write fishnet_polygon table to shapefile
st_write(fishnet_polygon, stringr::str_glue("C:/Users/Eliza.Carter/Documents/Projects/California/socal_ais/data/c_transect_data/AIS_transects_{file_month}.shp"), delete = T, overwrite = T)



#############

end <- Sys.time()
print(end - start) # print runtime for this iteration


print("full loop ended")
print(Sys.time() - start0)







#############

# join all shapefiles together

# Set the directory where shapefiles are stored
shapefile_dir <- "C:/Users/Eliza.Carter/Documents/Projects/California/socal_ais/data/c_transect_data"

# List all shapefiles (modify pattern if needed)
shapefiles <- list.files(shapefile_dir, pattern = "^AIS_transects_.*\\.shp$", full.names = TRUE)

# Read the first shapefile (this will keep its geometry)
first_shp <- st_read(shapefiles[1])

# Extract geometry and ID column
geometry <- first_shp %>% select(fn_id, geometry)

# Read the remaining shapefiles and left join them
shp_data <- first_shp %>% st_drop_geometry()  # Drop geometry
for (i in 2:length(shapefiles)) {
  temp_shp <- st_read(shapefiles[i]) %>% st_drop_geometry()
  shp_data <- left_join(shp_data, temp_shp, by = "fn_id")  # Left join on ID
}

# Sum all columns for each ship type
shp_data <- shp_data %>%
  mutate(Cargo_sum = rowSums(select(., contains("Cargo")), na.rm = TRUE))

shp_data <- shp_data %>%
  mutate(Tanker_sum = rowSums(select(., contains("Tanker")), na.rm = TRUE))

shp_data <- shp_data %>%
  mutate(Tug_Tow_sum = rowSums(select(., contains("Tug_Tow")), na.rm = TRUE))

shp_data <- shp_data %>%
  mutate(Fishing_sum = rowSums(select(., contains("Fishing")), na.rm = TRUE))

shp_data <- shp_data %>%
  mutate(Military_sum = rowSums(select(., contains("Military")), na.rm = TRUE))

shp_data <- shp_data %>%
  mutate(Passenger_sum = rowSums(select(., contains("Passenger")), na.rm = TRUE))

shp_data <- shp_data %>%
  mutate(Pleasure_sum = rowSums(select(., contains("Pleasure")), na.rm = TRUE))

shp_data <- shp_data %>%
  mutate(Other_sum = rowSums(select(., contains("Other")), na.rm = TRUE))


# Remove the old columns
shp_data <- shp_data %>%
  select(Index.x , Protractio.x , Lease_Blk.x , Aliquot.x , fn_id, Cargo_sum, Tanker_sum, Tug_Tow_sum, Fishing_sum, Military_sum, Passenger_sum, Pleasure_sum, Other_sum)

shp_data <- shp_data %>%
  rename(Index = Index.x, Protractio = Protractio.x, Lease_Blk = Lease_Blk.x, Aliquot = Aliquot.x, Cargo = Cargo_sum, Tanker = Tanker_sum,Tug_Tow = Tug_Tow_sum, Fishing = Fishing_sum, Military = Military_sum, Passenger = Passenger_sum, Pleasure = Pleasure_sum, Other = Other_sum)

# Merge back the geometry from the first shapefile
final_sf <- left_join(geometry, shp_data, by = "fn_id")

# Save the final output
st_write(final_sf, "C:/Users/Eliza.Carter/Documents/Projects/California/socal_ais/data/c_transect_data/AIS_transects_2023_all.shp")




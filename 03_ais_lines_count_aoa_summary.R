#### 

# socal AOA area

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

# dsn of merged lines for the relevant month
dsn_lines = file.path(stringr::str_glue("C:/Users/Eliza.Carter/Documents/Projects/California/socal_ais/data/b_intermediate_data/wc_{file_month}_transects_buffer_int.shp")) 

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

# read in buffered AOA polygon
fishnet_polygon <- st_read("C:/Users/Eliza.Carter/Documents/Projects/California/socal_ais/data/b_intermediate_data/AOAOptionN2D_buffer.shp")

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
                ~ as.numeric(ifelse(is.na(.x), 0, .x/2)) ))

st_write(fishnet_polygon, stringr::str_glue("C:/Users/Eliza.Carter/Documents/Projects/California/socal_ais/data/c_transect_data/AIS_AOA_{file_month}.shp"), delete = T, overwrite = T)


end <- Sys.time()
print(end - start) # print runtime for this iteration


print("full loop ended")
print(Sys.time() - start0)


############################

############################


# create summary document for monthly information


fishnet_data <- st_drop_geometry(fishnet_polygon)

fishnet_data <- fishnet_data %>%
  mutate(month = file_month)

# Define path to save the summary document
summary_doc_path <- "AOAN2D_monthly_summary_doc.rds"

# Check if the file exists
if (file.exists(summary_doc_path)) {
  # Load previous data
  summary_doc_final <- readRDS(summary_doc_path)
} else {
  # If first run, initialize with the current dataset
  summary_doc_final <- fishnet_data
}

# Append new data
summary_doc_final <- bind_rows(summary_doc_final, fishnet_data)

# Save the updated dataset
saveRDS(summary_doc_final, summary_doc_path)

# write to CSV for external use
write.csv(summary_doc_final, "C:/Users/Eliza.Carter/Documents/Projects/California/socal_ais/data/c_transect_data/AOAN2D_monthly_summary_doc.csv", row.names = FALSE)



############################

############################


# create summary document for annual ship information (sog, length, width, unique mmsi)


ais_lines_intersected <- rbind(st_read("C:/Users/Eliza.Carter/Documents/Projects/California/socal_ais/data/b_intermediate_data/wc_202301_transects_buffer_int.shp"),
                               st_read("C:/Users/Eliza.Carter/Documents/Projects/California/socal_ais/data/b_intermediate_data/wc_202302_transects_buffer_int.shp"),
                               st_read("C:/Users/Eliza.Carter/Documents/Projects/California/socal_ais/data/b_intermediate_data/wc_202303_transects_buffer_int.shp"),
                               st_read("C:/Users/Eliza.Carter/Documents/Projects/California/socal_ais/data/b_intermediate_data/wc_202304_transects_buffer_int.shp"),
                               st_read("C:/Users/Eliza.Carter/Documents/Projects/California/socal_ais/data/b_intermediate_data/wc_202305_transects_buffer_int.shp"),
                               st_read("C:/Users/Eliza.Carter/Documents/Projects/California/socal_ais/data/b_intermediate_data/wc_202306_transects_buffer_int.shp"),
                               st_read("C:/Users/Eliza.Carter/Documents/Projects/California/socal_ais/data/b_intermediate_data/wc_202307_transects_buffer_int.shp"),
                               st_read("C:/Users/Eliza.Carter/Documents/Projects/California/socal_ais/data/b_intermediate_data/wc_202308_transects_buffer_int.shp"),
                               st_read("C:/Users/Eliza.Carter/Documents/Projects/California/socal_ais/data/b_intermediate_data/wc_202309_transects_buffer_int.shp"),
                               st_read("C:/Users/Eliza.Carter/Documents/Projects/California/socal_ais/data/b_intermediate_data/wc_202310_transects_buffer_int.shp"),
                               st_read("C:/Users/Eliza.Carter/Documents/Projects/California/socal_ais/data/b_intermediate_data/wc_202311_transects_buffer_int.shp"),
                               st_read("C:/Users/Eliza.Carter/Documents/Projects/California/socal_ais/data/b_intermediate_data/wc_202312_transects_buffer_int.shp") )


ais_lines_intersected$ShipTypeCat <- categorize_ship_types(ais_lines_intersected$vessel_typ) # from ais_processing_functions.R

ship_types = c("Cargo", "Tanker", "Tug_Tow", "Fishing", "Military", "Passenger", "Pleasure", "Other")


my_data = st_drop_geometry(ais_lines_intersected)

summary_cols = c("sog_mean", "length", "width")
my_summary = my_data %>%
  group_by(ShipTypeCat) %>%
  summarise(across(all_of(summary_cols),
                   list(mean=mean, sd=sd, min=min, max=max),
                   .names= "{.col}_{.fn}"),
            n_mmsi = n_distinct(MMSI)
  )

write.csv(my_summary, "C:/Users/Eliza.Carter/Documents/Projects/California/socal_ais/data/c_transect_data/2023_ship_stat_summary.csv", row.names = FALSE)

##########################




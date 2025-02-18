# socal_ais

AIS and VMS Processing Steps for Southern California
11/22/2024 – Eliza Carter


## VMS
Data received from OLE
Cleaned raw data files so they would be correctly read as CSVs into R
Some lines were not reading geometry correctly
Renamed cleaned files as “NOAAA_24_006_year_month_clean”
Combined the three csvs for each month into one (00 vms combine)
Clean up land data (01 land data cleaning)
Contains files for continents, big islands, small islands, very small islands, 1 mile buffered ports, 1 mile buffered harbors
Clip points to the SoCal polygon (02 vms point clip to polygon)
Clip points to only land, filter for points <= 4 for fishing speeds, and export points for each month (03 vms ocean fishing)
Merge all month point files into a single year using Arc tools
Reconstruct Tracks in ArcPro for each year
Use a distance of 4 international nautical miles and 90 minutes to split lines
Merge all track years into one file using Arc tools
Use Isaac’s script to count the number of intersections for each declaration code per BOEM aliquot grid cell per year (vms rasterize lines)
Project grid and VMS lines to UTM Zone 11N for the west coast
Count vessel transits per cell (end points inside and end points on the edges of each cell divided by 2)
Make a count field for each declaration code for the unique vessel doc numbers per cell (ex. Average # vessels per year per declaration code = 269, unique vessels per declaration code = doc_269)
Export to a shapefile
Ran Isaac’s script on the shapefile with all transits and divided by 9 for a yearly average

## AIS
Download AIS tracks that Brian created for all west coast EEZ
Select all AIS lines at intersect the study area/grid (01 Combine Intersecting AIS Features.py)
Use the Vessel Type Codes (2018) file to classify the AIS codes into ship types in Isaac’s processing script (https://coast.noaa.gov/data/marinecadastre/ais/VesselTypeCodes2018.pdf)
Use Isaac’s script to count the number of intersections for each vessel type per BOEM aliquot grid cell per year (02 ais lines count)
Project grid and VMS lines to UTM Zone 11N for the west coast
Count vessel transits per cell (end points inside and end points on the edges of each cell divided by 2)
Export to a shapefile
Create summary files for ORI AOA by using only lines that intersect the AOA

####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Collect Data from Census API
## 
## Notes
## From Chris's email:
## a map (likely using amcharts jquery mapping tool) that lists top employment
## sectors for each jurisdiction. Each state and each county would have its own
## dedicated page that listed the top employment sectors in the jurisdiction. 
## The Census Bureau compiles this data every 5 years:
## http://www.census.gov/econ/susb/index.html
## 
## API key: fb9a5df29c5d7510df67ef32fe7f311f9a0eb1dc
##
## SUSB MSA codes: http://www2.census.gov/econ/susb/data/msa_codes_2007_to_2011.txt
## State, county, NAICS data: http://www.census.gov/econ/susb/data/susb2011.html
##
## Creator: Eric Stone (ericstone@me.com) 
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(data.table)
library(stringr)
library(RColorBrewer)
library(maptools)
library(rgdal)
library(plyr)

#source the function to merge a dt w/ a shapefile
source("merge_sp_dt.R")

#load census NAICS data
naics <- data.table(read.csv("data/county_naicssectors_2011.csv", stringsAsFactors = FALSE))

#load shapefiles
county.shp <- readShapePoly("shapefiles/counties_clipped_wgs.shp")
state.shp <- readShapePoly(("shapefiles/state_reduced_wgs.shp"))

#get rid of US totals
naics <- naics[ !description == "State Total" & !is.na(naics_code)]

#get the counts of each industry by state
naics.counts <- naics[county_code == 999, list(N = sum(as.numeric(establishments_2011), na.rm = TRUE)), by="naics_description,state_code"]
setkey(naics.counts, state_code, N)
naics.counts[, rank := .N:1, by = state_code]
unique(naics.counts[rank == 1]$naics_description)

naics.counts.cnty <- naics[county_code != 999, list(N = sum(as.numeric(establishments_2011))), by="naics_description,state_code,county_code"]
setkey(naics.counts.cnty, county_code, N)
naics.counts.cnty[, rank := .N:1, by = "state_code,county_code"]
unique(naics.counts.cnty[rank == 1]$naics_description)


#define colors for each industry code
cnty.colors <- data.table(color = c(brewer.pal(n = 12, "Set3"), brewer.pal(n = 9, "Set1")[1:7]),
                          naics_description = arrange(naics.counts.cnty[rank == 1, .N, by = "naics_description"], N, decreasing = TRUE)$naics_description)

cnty.most.pop <- merge(naics.counts.cnty[rank == 1], cnty.colors, by = "naics_description")


#isolate the non-geographic datat from the county shapefile
#edit the data to make it merge-able
cnty.shp.data <- data.table(county.shp@data)
cnty.shp.data[, state_code := as.numeric(as.character(STATEFP))]
cnty.shp.data[, county_code := as.numeric(as.character(COUNTYFP))]

#merge shapefile data into naics data and add colors
cnty.most.pop <- merge(cnty.most.pop, 
                       cnty.shp.data[, list(state_code, county_code)], 
                       by = c("state_code", "county_code"), 
                       all = TRUE)
cnty.most.pop[is.na(naics_description), naics_description := "No Data"]
cnty.most.pop[is.na(color), color := "grey50"]
cnty.most.pop[, GEOID := paste0(sprintf("%02.0f", state_code), sprintf("%03.0f", county_code))]

#merge in state and county names! duh duh duhhh
cnty.most.pop <- merge(cnty.most.pop, 
                       unique(naics[, list(state_code, state_name, county_code, geo_name = description)]), 
                       by = c("state_code", "county_code"))

#merge the data into the shapefile
cnty.shp.ann <- JoinSpDf(x = county.shp, 
                         y = cnty.most.pop[, list(GEOID, state_code, county_code, state_name, geo_name, naics_description, color)], 
                         xcol = "GEOID", ycol = "GEOID")

#write the data as geojson
#WARNING: if the file name already exists, ORG will fail w/ no explanation
writeOGR(cnty.shp.ann, "county_data", "county_data", driver = "GeoJSON")

#------------------------------------
#now repeat at the state level

#make the state file by isolating the top industry from each state
state.most.pop <- naics.counts[rank == 1]

#define colors
state.colors <- merge(data.table(naics_description = unique(state.most.pop$naics_description)),
                      cnty.colors,
                      by = "naics_description", all.x = T, all.y = F)

state.most.pop <- merge(state.most.pop, state.colors, by = "naics_description")

#cross check with shapefile data to assign grey color to missing
state.shp.data <- data.table(state.shp@data)
state.shp.data[, state_code := as.numeric(as.character(STATEFP))]

state.most.pop <- merge(state.most.pop, state.shp.data[, list(state_code)], by = c("state_code"), all = TRUE)
state.most.pop[is.na(naics_description), naics_description := "No Data"]
state.most.pop[is.na(color), color := "grey50"]

#define geoid to make merge work
state.most.pop[, GEOID := paste0(sprintf("%02.0f", state_code))]

#merge in state name
state.most.pop <- merge(state.most.pop, 
                        unique(naics[, list(state_code, state_name)]), 
                        by = c("state_code"))

#add data to shapefile
state.shp.ann <- JoinSpDf(x = state.shp, 
                          y = state.most.pop[, list(GEOID, state_code, geo_name = state_name, naics_description, color)], 
                          xcol = "GEOID", ycol = "GEOID")

#write to geojson
writeOGR(state.shp.ann, "state_data", "state_data", driver = "GeoJSON")

#the end


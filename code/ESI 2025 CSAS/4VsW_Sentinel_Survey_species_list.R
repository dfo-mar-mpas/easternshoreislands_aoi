library(Mar.datawrangling)
library(magrittr)
library(leaflet)
library(sf)
library(MarConsNetData)
library(dplyr)
library(writexl)
#get_data('isdb', data.dir="C:/Users/HarbinJ/Documents/data/isdb") # Note: Make path be your own path.

lists <- NULL
for (i in seq_along(ls()[which(grepl("IS", ls()))])) {
  lists[[i]] <- sort(names(eval(parse(text=ls()[which(grepl("IS", ls()))][i]))))
}
names(lists) <- ls()[which(grepl("IS", ls()))]

iscatches <- ISCATCHES[,c("FISHSET_ID", "CATCH_ID", "SPECCD_ID")]
iscatches$common <- NA
iscatches$scientific <- NA

species <- unique(iscatches$SPECCD_ID)

# STEP 1: Get SCIENTIFIC and COMMON names for SPECCD_ID

for (i in seq_along(species)) {
  message(i)
  keep <- which(ISSPECIESCODES$SPECCD_ID == species[i])
  iscatches$common[which(iscatches$SPECCD_ID == species[i])] <- ISSPECIESCODES$COMMON[keep]
  iscatches$scientific[which(iscatches$SPECCD_ID == species[i])] <- ISSPECIESCODES$SCIENTIFIC[keep]
}

# STEP 2: DETERMINE WHICH 'TRIPCD_ID' was 4VSW SENTINEL PROGRAM. ANSWER: 7050
ISTRIPTYPECODES[which(ISTRIPTYPECODES$TRIP_TYPE == "4VSW SENTINEL PROGRAM"),] # THIS TELLS US THE TRIP TRIPCD_ID IS 7050. We then apply that below.

#lists[grepl("TRIPCD_ID", lists)]

istrip <- ISTRIPS[which(ISTRIPS$TRIPCD_ID == ISTRIPTYPECODES$TRIPCD_ID[which(ISTRIPTYPECODES$TRIP_TYPE == "4VSW SENTINEL PROGRAM")]),]

# STEP 3: FIND THE ASSOCIATED FISHSET_ID ('tows') FROM THE TRIP_IDs ABOVE ASSOCIATED WITH 4VSW SENTINEL PROGRAM

fishset <- ISFISHSETS[which(ISFISHSETS$TRIP_ID %in% istrip$TRIP_ID),]

# STEP 4: SUBSET iscatches TO ONLY INCLUDE FISHSET_ID FROM the TRIP_IDs associated with 4VSW SENTINEL PROGRAM'S TRIPCD_ID
iscatches <- iscatches[which(iscatches$FISHSET_ID %in% fishset$FISHSET_ID),] # SPECCD_ID is what they caught, FISHSET_ID is what they were fishing for

# STEP 5: DETERMINE LATITUDE AND LONGITUDE OF FISHSET_ID
iscatches$latitude <- NA
iscatches$longitude <- NA
iscatches$date <- NA
fs <- unique(iscatches$FISHSET_ID)
for (i in seq_along(fs)) {
  message(i)
  keep <- which(ISSETPROFILE_WIDE$FISHSET_ID == fs[i])
  iscatches$latitude[which(iscatches$FISHSET_ID == fs[i])] <- ISSETPROFILE_WIDE$LATITUDE[keep]
  iscatches$longitude[which(iscatches$FISHSET_ID == fs[i])] <- ISSETPROFILE_WIDE$LONGITUDE[keep]
  iscatches$date[which(iscatches$FISHSET_ID == fs[i])] <- as.numeric(ISSETPROFILE_WIDE$YEAR[keep])
}

# STEP 6: Subset for years 2000 onwards since there was a change in methods (after conversation with Nick)

iscatches <- iscatches[which(as.numeric(iscatches$date) > 1999),]


# STEP 7: DETERMINE WHICH LAT/LON COORDINATES ARE WITHIN ESI

# Find which are within ESI
d <- data_draft_areas(bioregion = "Scotian Shelf")
geom <- d$geoms[which(d$SiteName_E ==  "Eastern Shore Islands")]

geom <- st_make_valid(geom)

# STEP 8: Subset for a 10 km butter for 'outside' data (after conversation with Nick)

geom_extended <- st_buffer(geom, dist = 10000) # 10 km buffer
geom_extended_exclusive <- sf::st_difference(geom_extended, geom)


iscatches_sf <- st_as_sf(iscatches, coords = c("longitude", "latitude"), crs = 4326)
within_esi <- st_intersects(iscatches_sf, geom, sparse = FALSE)
iscatches$ESI <- apply(within_esi, 1, function(x) ifelse(any(x), TRUE, FALSE))

within_outer <- st_intersects(iscatches_sf, geom_extended_exclusive, sparse = FALSE)
iscatches$outer <- apply(within_outer, 1, function(x) ifelse(any(x), TRUE, FALSE))


esi <- iscatches[which(iscatches$ESI),]
outer <- iscatches[which(iscatches$outer),]

map <- leaflet() %>%
  addTiles() %>%
  addPolygons(data=geom, color="#00BFFF", fillColor="gray") %>%
  addPolygons(data=geom_extended_exclusive, color="#6a3d9a", fillColor="#6a3d9a", weight=0.5) %>%
  addCircleMarkers(lat=esi$latitude, lng=esi$longitude, color="red", radius=3) %>%
  addCircleMarkers(lat=outer$latitude, lng=outer$longitude, color="red", radius=3)


map

# Step 9: Create dataframe indicating species and years

esiSpecies <- unique(esi$scientific)
ES <- list()
common <- NULL
for (i in seq_along(esiSpecies)) {
  ES[[i]] <- paste0(sort(unique(esi$date[which(esi$scientific == esiSpecies[i])])), collapse=",")
  common[i] <- unique(esi$common[which(esi$scientific == esiSpecies[i])])
}

ESI <- data.frame(scientific=esiSpecies, common=common, year=unlist(ES))
ESI <- ESI %>%
  arrange(scientific)


## Outer
outerSpecies <- unique(outer$scientific)
OU <- list()
commonOuter <- NULL
for (i in seq_along(outerSpecies)) {
  OU[[i]] <- paste0(sort(unique(outer$date[which(outer$scientific == outerSpecies[i])])), collapse=",")
  commonOuter[i] <- unique(outer$common[which(outer$scientific == outerSpecies[i])])
}

OUTER <- data.frame(scientific=outerSpecies, common=commonOuter, year=unlist(OU))
OUTER <- OUTER %>%
  arrange(OUTER$scientific)

#write_xlsx(ESI, "4VsW_Sentinel_Survey_esi.xlsx")
#write_xlsx(OUTER, "4VsW_Sentinel_Survey_outer_boundary.xlsx")


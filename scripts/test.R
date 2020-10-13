
library(leaflet)

load("C:/HANDmodels/scripts/result.Rdata")
path_sf_zones <- "C:/ap/data/huc12_uc.shp"
s <- 1

map_results_leaflet <- function(result, path_sf_zones, s){
  # attach data to sf object
  s <- as.numeric(s)
  nad16n <- "+proj=utm +zone=16 +ellps=GRS80 +datum=NAD83 +units=m +no_defs" #temporary
  field_zone <- result[["settings"]]$field_zone
  main <- names(result[["summary"]])[s]
  ranks <- result[["summary"]][1:(s+1)]
  huc12 <- sf::st_read(path_sf_zones, quiet = TRUE) #temporary
  huc12 <- st_transform(huc12, crs = st_crs(4326)) #temporary
  huc12_result <- merge(huc12, ranks, by = field_zone, all.x = TRUE)

  # define plot attr
  rank_name <- names(result[["summary"]])[s+1]
  binpal <- colorBin("YlOrRd", huc12_result[[rank_name]], bins = 10, pretty = FALSE, reverse = TRUE)
  label <- paste(
    "Name:", huc12_result$Name, "\n",
    "Rank:", huc12_result[[rank_name]])
  
  # plot
  leaflet(huc12_result) %>% 
    addProviderTiles("OpenStreetMap.Mapnik") %>% 
    addPolygons(stroke = NA, fillColor = binpal(huc12_result[[rank_name]]), fillOpacity = 0.6,
                highlight = highlightOptions(color = "white", weight = 1, bringToFront = TRUE), label = label)
}

map_results_leaflet(result, path_sf_zones, s)

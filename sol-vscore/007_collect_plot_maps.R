library(sf)
library(htmltools)
library(leaflet)

# Source all Files ----

world <- read_sf("world-shapefile/")
world_map = (world[ ,"name"])

source("002_ecosystem_metrics.R")
source("004_validator_stake_metrics.R")
source("006_validator_vote_metrics.R")

voter_coordinate <- ecoappdata[ , c("epoch","voter_pubkey", "longitude", "latitude")]
voter_country <- ecoappdata[ , c("epoch","voter_pubkey", "country")]

# Add Coordinate Geometry ----

# Filter to Desired Epoch note ecoappdata may have differing max epoch than other data.
add_coordinate_to_validator <- function(validator_lvl_data, voter_coordinate, select_epoch = NULL){
 
  vcll <- merge(validator_lvl_data, voter_coordinate, 
                all.x = TRUE,
                all.y = FALSE, 
                by = c("epoch","voter_pubkey"))
  
  if( !is.null(select_epoch) ){
    vcll <- vcll %>% dplyr::filter(epoch == select_epoch)
  }
  
  # infill missing as 0,0 "unknown"
  vcll$latitude[is.na(vcll$latitude)] <- -150
  vcll$longitude[is.na(vcll$longitude)] <- 10
  
  vcll <- st_as_sf(vcll, coords = c("longitude", "latitude"), crs = 4326, agr = "constant")
  return(vcll)
}

# Add Country Geometry ----

# Filter to Desired Epoch note ecoappdata may have differing max epoch than other data.

add_country_to_validator <- function(validator_lvl_data, voter_country, world_map, select_epoch = NULL){
   
  if(!is.null(select_epoch)){
     validator_lvl_data <- validator_lvl_data %>% filter(epoch == select_epoch)
   }
  
 vctry <- merge(validator_lvl_data, voter_country, all.x = TRUE, all.y = FALSE, by = c("epoch","voter_pubkey"))
 vctry <- merge(vctry, world_map, all.x = TRUE, by.x = "country", by.y = "name")
 
 # Reactangle in the Pacific for Unknown
 coords <- matrix(c(-150, 10,
                    -130, 10,
                    -130, -10,
                    -150, -10,
                    -150, 10), 
                  ncol = 2, byrow = TRUE)
 
 # Create the polygon
 polygon <- st_polygon(list(coords))
 
 # Create a simple features geometry column
 sfc <- st_sfc(polygon, crs = 4326)
 
 # Replace the geometry of unknown countries with the rectangle
 vctry$geometry[vctry$country == 'Unknown' | is.na(vctry$country)] <- sfc
 
 return(vctry)
}

# Probably the function to use, aggregate to Country then add country geometry. 
add_country_to_countrylvl <- function(country_lvl_data, world_map, select_epoch = NULL){
  
  cl <- merge(country_lvl_data, world_map, all.x = TRUE, by.x = "country", by.y = "name")
  
  if(!is.null(select_epoch)){
    cl <- cl %>% filter(epoch == select_epoch)
  }
  
  # Reactangle in the Pacific for Unknown
  coords <- matrix(c(-150, 10,
                     -130, 10,
                     -130, -10,
                     -150, -10,
                     -150, 10), 
                   ncol = 2, byrow = TRUE)
  
  # Create the polygon
  polygon <- st_polygon(list(coords))
  
  # Create a simple features geometry column
  sfc <- st_sfc(polygon, crs = 4326)
  
  # Replace the geometry of unknown countries with the rectangle
  cl$geometry[cl$country == 'Unknown' | is.na(cl$country)] <- sfc
  
  
  return(cl)
}


# Variables ----

# probably need to aggregate up to unique coordinates to make any useful plot. But available.
validator_stake_coords_463 <- add_coordinate_to_validator(validator_lvl_data = validator_stake, 
                                                          voter_coordinate, 
                                                          select_epoch = 463)

# Still needs to be aggregated up to country level to make sense. 
validator_country_463 <- add_country_to_validator(validator_lvl_data = validator_stake, 
                                              voter_country,
                                              world_map, 
                                              select_epoch = 463)

country_463 <- add_country_to_countrylvl(ecosystem_sol_stake_stats_by_epoch_country, world_map, 463)

# Map ---- 

# HTML Compliant 
country_463$label <- paste0("Country: ",country_463$country, 
                            "<br>Total Stake: ",
                            format(floor(country_463$total_stake), big.mark = ",")
                            )


plot_country <- function(country_geo_data, color_col, hover_text_col){
  
  colorpal <- colorNumeric(palette = c("#301855",
                                       "#a84089",
                                       "#ab7dbe",
                                       "#dd6aa8",
                                       "#ff98fe"), domain = country_geo_data[[color_col]])
  
  df <- st_as_sf(country_geo_data)
  df[["clr"]] <- country_geo_data[[color_col]]
  df[["txt"]] <- country_geo_data[[hover_text_col]]
  
  leaflet(df) %>% 
    addTiles() %>% 
    addPolygons(
      fillColor = ~colorpal(clr),
      fillOpacity = 0.8,
      weight = 1,
      color = "white",
      label = lapply(df$txt, HTML),
      highlightOptions = highlightOptions(
        color = "black",
        weight = 2,
        bringToFront = TRUE
      )
    )  %>%
    addLegend(
      "bottomright", 
      pal = colorpal, 
      values = ~clr,
      title = color_col,
      opacity = 1
    )
  
}

plot_country(country_463, "n_validators", "label")



# Coordinates ----


validator_stake_coords_463$label <- paste0("Country: ",validator_stake_coords_463$country, 
                            "<br>Total Stake: ",
                            format(floor(validator_stake_coords_463$total_stake), big.mark = ",")
)

coords_geo_data <- validator_stake_coords_463 


plot_coords <- function(coords_geo_data, color_col, 
                        size_col, marker_text){
  
  colorpal <- colorNumeric(palette = c("#301855",
                                       "#a84089",
                                       "#ab7dbe",
                                       "#dd6aa8",
                                       "#ff98fe"), domain = country_geo_data[[color_col]])
  
  df <- st_as_sf(country_geo_data)
  df[["clr"]] <- country_geo_data[[color_col]]
  df[["txt"]] <- country_geo_data[[hover_text_col]]
  
  library(leaflet)
  
  # Create a synthetic data frame
  df <- data.frame(
    latitude = c(40.7128, 34.0522, 51.5074, 48.8566, 37.7749, 55.7558),
    longitude = c(-74.0060, -118.2437, -0.1278, 2.3522, -122.4194, 37.6176),
    id = c(1, 2, 3, 4, 5, 6),
    value1 = c(10, 20, 30, 40, 50, 60),
    value2 = c(100, 200, 300, 400, 500, 600),
    label = c("Marker 1", "Marker 2", "Marker 3", "Marker 4", "Marker 5", "Marker 6")
  )
  
  
  map <- leaflet()
  
  map <- map %>% addTiles()
  
  cluster_options <- markerClusterOptions(
    disableClusteringAtZoom = 8
  )
  
  map <- map %>%
    addCircleMarkers(
      data = df,
      lat = ~latitude,
      lng = ~longitude,
      group = ~id,
      clusterOptions = cluster_options,
      popup = ~label
    )
  
  leaflet(df) %>% 
    addTiles() %>% 
    addPolygons(
      fillColor = ~colorpal(clr),
      fillOpacity = 0.8,
      weight = 1,
      color = "white",
      label = lapply(df$txt, HTML),
      highlightOptions = highlightOptions(
        color = "black",
        weight = 2,
        bringToFront = TRUE
      )
    )  %>%
    addLegend(
      "bottomright", 
      pal = colorpal, 
      values = ~clr,
      title = color_col,
      opacity = 1
    )
  
}



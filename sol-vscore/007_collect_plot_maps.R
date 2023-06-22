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
  vcll$latitude[is.na(vcll$latitude)] <- 10
  vcll$longitude[is.na(vcll$longitude)] <- -150
  
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

# Will jitter coordinates
validator_stake_coords_463 <- add_coordinate_to_validator(validator_lvl_data = validator_stake, 
                                                          voter_coordinate, 
                                                          select_epoch = 463)

# Still needs to be aggregated up to country level to make sense. 
validator_country_463 <- add_country_to_validator(validator_lvl_data = validator_stake, 
                                              voter_country,
                                              world_map, 
                                              select_epoch = 463)

country_463 <- add_country_to_countrylvl(ecosystem_sol_stake_stats_by_epoch_country, world_map, 463)

# HTML Compliant Labels ----

validator_stake_coords_463$label <- paste0("<b>Validator ...", 
                                           substr(validator_stake_coords_463$voter_pubkey, start = 40, stop = 44), 
                                           "</b><br># Stakers: ",
                                           validator_stake_coords_463$nstakers, 
                                           "<br>Total Stake: ",
                                           format(floor(validator_stake_coords_463$sol_staked),
                                                  big.mark = ","),
                                           
                                           '<br><a href="',
                                           'https://solanacompass.com/validators/',
                                           validator_stake_coords_463$voter_pubkey,
                                           '">', 
                                           'Compass Link',
                                           '</a>'
                                           )

country_463$label <- paste0("Country: ",country_463$country, 
                            "<br>Total Stake: ",
                            format(floor(country_463$total_stake), big.mark = ",")
)

# Map ---- 

plot_country <- function(country_geo_data, color_col, hover_text_col){
  
  colorpal <- colorNumeric(palette = c("#a6d96a", "#66bd63", "#1a9850", "#006837", "#004529"), 
                           domain = country_geo_data[[color_col]])
  
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

plot_coords <- function(coords_geo_data, color_col, marker_text){
  
  colorpal <- colorNumeric(palette = rev(c("#a6d96a", "#66bd63", "#1a9850", "#006837", "#004529")), 
                           domain = coords_geo_data[[color_col]])
  
  df <- coords_geo_data
  df[["clr"]] <- coords_geo_data[[color_col]]
  df[["txt"]] <- coords_geo_data[[marker_text]]
 
  
  
  set.seed(4)
  df$latitude <- df$latitude + rnorm(length(df$latitude), mean = 0, sd = 0.1)
  df$longitude <- df$longitude + rnorm(length(df$longitude), mean = 0, sd = 0.1)
  
  cluster_options <- markerClusterOptions(
    disableClusteringAtZoom = 4
  )
 
  leaflet(df) %>% 
    addTiles() %>% 
    addCircleMarkers(
      lat = ~latitude,
      lng = ~longitude,
      color = ~colorpal(df$clr),
      group = ~voter_pubkey,
      clusterOptions = cluster_options,
      popup = ~lapply(df$txt, HTML)
    )  %>%
    addLegend(
      "bottomright", 
      pal = colorpal, 
      values = ~clr,
      title = color_col,
      opacity = 1
    )
  
}

plot_coords(validator_stake_coords_463, color_col = "sol_staked", marker_text = "label")

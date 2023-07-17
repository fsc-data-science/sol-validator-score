
# Functions 002 ---- 

num_sol_staked <- function(ecoappdata){
  ecoappdata %>% 
    group_by(epoch) %>% 
    summarise(
      total_stake = sum(sol_stake), 
      n_validators = length(unique(voter_pubkey)),
      current_stake = sum( sol_stake * (delinquent == FALSE) ),
      n_current_validators = sum(delinquent == FALSE),
      delinquent_stake = sum( sol_stake * (delinquent == TRUE) ),
      n_delinquent_validators = sum(delinquent == TRUE)
    )
}

# If you loosen current to active at least 1 epoch in last 10 epochs counts as current.
num_sol_staked_last10 <- function(ecoappdata){
  
  ecoappdata %>% 
    group_by(epoch) %>% 
    summarise(
      total_stake = sum(sol_stake), 
      n_validators = length(unique(voter_pubkey)),
      recently_active_stake = sum( sol_stake * (count_active_last10 > 0) ),
      n_recent_active_validators = sum(count_active_last10 > 0),
      delinquent10_stake = sum( sol_stake * (count_active_last10 == 0) ),
      n_delinquent10_validators = sum(count_active_last10 == 0)
    )
  
}

num_sol_staked_by_country <- function(ecoappdata){
  ecoappdata %>% 
    group_by(epoch, country) %>% 
    summarise(
      total_stake = sum(sol_stake), 
      n_validators = length(unique(voter_pubkey)),
      current_stake = sum( sol_stake * (delinquent == FALSE) ),
      n_current_validators = sum(delinquent == FALSE),
      delinquent_stake = sum( sol_stake * (delinquent == TRUE) ),
      n_delinquent_validators = sum(delinquent == TRUE)
    )
}

ecosystem_10epoch_churn <- function(ecoappdata){
  eco_stats <- num_sol_staked(ecoappdata)
  
  churn_rate <- rep(0, length(eco_stats$current_stake))
  
  for(i in 10:length(eco_stats$current_stake)){
    temp <- sum(abs(diff(eco_stats$current_stake[(i-9):i])))/
      (eco_stats$current_stake[i])
    
    if(!is.na(temp) & !is.infinite(temp) & !is.nan(temp)){
      churn_rate[i] <- temp
    }
  }
  
  eco_stats$churn_rate <- churn_rate*100
  return(eco_stats)
  
}

get_nakamoto <- function(values){
  vec <- sort(values,decreasing = TRUE)
  cumv <- cumsum(vec)
  return(
    which(cumv > (sum(vec) * 0.5))[1]
  )
}

get_nakamoto_currents <- function(ecoappdata){
  ecoappdata %>% 
    group_by(epoch) %>% 
    filter(delinquent == FALSE) %>% 
    summarise(
      nakamoto = get_nakamoto(sol_stake)
    )
}

get_current_nakamoto_by_country <- function(ecoappdata){
  ecoappdata %>% 
    dplyr::filter(delinquent == FALSE) %>% 
    group_by(epoch, country) %>% 
    summarise(stake = sum(sol_stake)) %>% 
    group_by(epoch) %>% 
    summarise(
      country_nakamoto = get_nakamoto(stake)
    )
}


get_gini <- function(values){
  mv <- mean(values)
  abs_diff <- abs(values-mv)
  mad <- mean(abs_diff)
  return(mad/mv/2)
}

get_gini_currents <- function(ecoappdata){
  
  ecoappdata %>% 
    group_by(epoch) %>% 
    filter(delinquent == FALSE) %>% 
    summarise(
      gini = get_gini(sol_stake)
    )
}


get_gini_recent <- function(ecoappdata){
  ecoappdata %>% 
    group_by(epoch) %>% 
    filter(count_active_last10 > 0) %>% 
    summarise(
      gini = get_gini(sol_stake)
    )
}

get_current_gini_by_country <- function(ecoappdata){
  ecoappdata %>% 
    dplyr::filter(delinquent == FALSE) %>% 
    group_by(epoch, country) %>% 
    summarise(stake = sum(sol_stake)) %>% 
    group_by(epoch) %>% 
    summarise(
      country_gini = get_gini(stake)
    )
}

get_current_cdf_stats <- function(ecoappdata){
  current <- ecoappdata %>% dplyr::filter(delinquent == FALSE)
  
  # Gini defined as 1/2 the relative mean absolute difference 
  # this is better for discrete values; slightly different than integral approach
  # e.g., ineq::Gini but similar.
  
  # cdf 
  cdf_intervals <- seq(from = 50000, to = 10000000, by = 10000) 
  
  data.frame(
    reference_sol_stake = cdf_intervals, 
    cdf_value = ecdf(current$sol_stake)(cdf_intervals)
  )
}

plot_current_cdf <- function(ecoappdata, title = "~65% of Validators have between 70K-80K Sol Staked"){
  
  plotly::plot_ly(data = get_current_cdf_stats(ecoappdata),
                  x = ~reference_sol_stake, 
                  y = ~cdf_value, 
                  type = 'scatter', mode = 'lines+markers') %>% 
    layout(xaxis = list(title = "# SOL Staked", 
                        titlefont = list(size = 18)),
           yaxis = list(title = "Cumulative % of Voter Pubkey",
                        titlefont = list(size = 18)),
           title = list(
             text = title, 
             font = list(size = 18),
             y = 0.95)
    )
}

get_current_software_stats <- function(ecoappdata){
  ecoappdata %>% filter(delinquent == FALSE) %>%  
    group_by(epoch) %>% 
    mutate(
      maxsoftware = max(modified_software_version)) %>% 
    group_by(epoch, modified_software_version) %>% 
    summarise(n = n(), 
              ismax = (modified_software_version == maxsoftware)) %>% 
    unique()
  
}


# Functions 004 ---- 

rolling_absum <- function(v, k = 10){
  d = rep(0, length(v))
  
  if(length(v) < (k+1)){
    return(d)  
  }
  
  for(i in k:length(v)){
    d[i] <- sum( abs(v[ (i-k+1):i ]))
  }
  return(d)
}

get_10epoch_staker_churn <- function(validator_stake){
  validator_stake %>% group_by(voter_pubkey) %>% 
    dplyr::arrange(epoch) %>% 
    mutate(
      ten_epoch_staker_churn_percent = 100*rolling_absum(num_new_stakers, k = 10)/nstakers
    )
}

get_10epoch_sol_churn <- function(validator_stake){
  validator_stake %>% group_by(voter_pubkey) %>% 
    dplyr::arrange(epoch) %>% 
    mutate(
      ten_epoch_churn_percent = 100*rolling_absum(num_new_sol_staked, k = 10)/sol_staked
    )
}



# Functions 006 ---- 
rolling_avg <- function(v, k = 10, default = 100){
  d = rep(default, length(v))
  
  v[is.na(v)] <- default
  
  if(length(v) < (k+1)){
    return(d)  
  }
  
  for(i in k:length(v)){
    d[i] <- mean(v[ (i-k+1):i ])
  }
  return(d)
}

get_vote_avg10_attendance <- function(validator_vote){
  validator_vote %>% 
    group_by(voter_pubkey) %>%
    arrange(epoch) %>% 
    mutate(
      avg_10epoch_attendance = 100 - rolling_avg(percent_on_chosen_fork, k = 10)
    )
}

get_vote_profile <- function(validator_vote){
  validator_vote %>% 
    group_by(voter_pubkey) %>%
    arrange(epoch) %>% 
    summarise(
      earliest_epoch = min(last_active_epoch),
      latest_active_epoch = max(last_active_epoch),
      age = max(last_active_epoch) - min(last_active_epoch),
      experience = sum(active_category == "active")
    )
  
}

# Functions (Maps) 007 ----

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

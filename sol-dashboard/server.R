function(input, output, session) {
  
  output$bpvalcounts <- renderPlotly({
    nval.by.epoch.plot
  })
  
  output$solstaked <- renderPlotly({
    print(input$solstakedradio)
    if(input$solstakedradio == "Total Stake") {
      total.stake.plot
    } else {
      avg.stake.plot
    }
  })
  
  
  output$ageplot <- renderPlotly({
    if(input$valageradio == "# New Validators") {
      new.vals.plot
    } else if(input$valageradio == "Validator Age") {
      avg.age.plot
    }
    
  })
  
    output$corrplot <- renderPlotly({
    xvar <- corr.plot.choices[selection_name == input$corr_x]$table_name
    yvar <- corr.plot.choices[selection_name == input$corr_y]$table_name
    sizevar <- corr.plot.choices[selection_name == input$corr_size]$table_name
    
    dolog.x <- corr.plot.choices[selection_name == input$corr_x]$log_axis
    dolog.y <- corr.plot.choices[selection_name == input$corr_y]$log_axis
   
    # Create scatter plot
    plot_ly(overview.vals, 
            x = ~overview.vals[[xvar]], 
            y = ~overview.vals[[yvar]], 
            type = "scatter", mode = "markers",
            hovertext = ~paste0(overview.vals[["display_name"]]),
            marker = list(
              color = "rgba(153, 69, 255, 0.75)",
                          line = list(
                            color = 'rgba(153, 69, 255, 1)',  # border
                            width = 1  # border width
                          ) )) %>%
      layout(xaxis = list(title = input$corr_x, type = ifelse(dolog.x, "log", "linear")),
             yaxis = list(title = input$corr_y, type = ifelse(dolog.y, "log", "linear")),
             showlegend = FALSE,
             plot_bgcolor = "transparent",
             paper_bgcolor = "transparent",
             font = list(family = "Roboto Mono", size = 12))
    
  })
  
  observe({
  mms <<- input$min_max_stake  
  mmn <<- input$min_max_nstakers
  cc <<- input$countries
  matt <<- input$min_attendance
  })
  
  filter_map_info <- reactive({

   list(input$countries, input$min_max_nstakers, input$min_attendance, input$min_max_stake)
    
    map_info <- overview.vals
    
    map_info <- map_info %>% dplyr::filter(
      country %in% input$countries & 
      sol_staked >= (1e3 * input$min_max_stake[1]) &  
      sol_staked <= (1e3 * input$min_max_stake[2]) &  
      count_active_last10 >= input$min_attendance  
    )
    
    map_info$label <- paste0("<b>Validator ...", 
                                           substr(map_info$voter_pubkey, start = 40, stop = 44), 
                             "</b><br># Stakers: ",
                             map_info$nstakers, 
                             "<br>Total Stake: ",
                             format(floor(map_info$sol_staked),
                                    big.mark = ","),
                             "<br>Country: ",
                             map_info$country,
                             "<br>Software Version: ",
                             map_info$modified_software_version,
                             '<br><a href="',
                             'https://solanacompass.com/validators/',
                             map_info$voter_pubkey,
                             '">', 
                             'Compass Link',
                             '</a>'
    )
    
   map_info
  })
  
  output$select_validators <- renderReactable({
    
    tbl_ <- filter_map_info()[ , c("voter_pubkey", "display_name", "age", 
                                   "sol_staked", "nstakers", 
                                   "avg_stake_size", "country",
                                   "count_active_last10",
                                   "modified_software_version")]
   
     reactable(tbl_[, 2:9],
              columns = list(
                display_name = colDef(
                  name = "Voter", 
                  cell = function(value, index) {
                    # Render as a link
                    url <- sprintf("https://solanacompass.com/validators/%s", tbl_[index, "voter_pubkey"], value)
                    htmltools::tags$a(href = url, target = "_blank", as.character(value))
                  }
                ),
                modified_software_version = colDef(
                  name = "Software Version"
                ),
                count_active_last10 = colDef(
                  name = "# Epochs Active (of last 10)"
                ),
                sol_staked = colDef(
                  name = "Sol Staked",
                  cell = function(value){
                    format(round(value,2), big.mark   = ",")
                  }
                ),
                nstakers = colDef(
                  name = "Stakers",
                  cell = function(value){
                    format(round(value,2), big.mark   = ",")
                  }
                ),
                avg_stake_size = colDef(
                  name = "Avg Stake Size",
                  cell = function(value){
                    format(round(value,2), big.mark   = ",")
                  }
                ),
                country = colDef(
                  name = "Country"
                )
              )
    )
    
  })
  
  output$gini_naka_plot <- renderPlotly({
    if(input$gininakaradio == "Gini") {
      gini.time.plot
    } else {
      nakamoto.time.plot
    }
  })
  
  output$gini_naka_text <- renderText({
    
    if(input$gininakaradio == "Gini") {
      "Validator Gini Coefficient by SOL Staked and Location"
    } else {
      "Validator Nakamoto Coefficient by SOL Staked and Location"
    }
  })
  
  output$val_staker_gini_token <- renderPlotly({
    if(input$stakerginiradio == "Staker Gini vs Stake Share") {
      staker.gini.vs.prop.plot
    } else {
      staker.val.gini.distr
    }
    })
  
  output$staker_gini_desc <- renderText({
    if(input$stakerginiradio == "Staker Gini vs Stake Share") {
      "Within Validator Staker Gini Coefficient vs. Validator % of Total Staked"
    } else {
      "Distribution of Staker Gini Coefficient Across Validators"
    }
  })
  
  output$themap <- renderLeaflet({
    plot_coords(validator_stake_coords, color_col = "sol_staked", marker_text = "label")
  })
  
  output$decentralized_map <- renderLeaflet({
    plot_coords(filter_map_info(), color_col = "sol_staked", marker_text = "label")
    
    })
  
  
}




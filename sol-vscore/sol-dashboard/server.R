function(input, output, session) {
  
  # do it this way when choices is very long:
  # updateSelectizeInput(session, inputId = 'validator', 
  #                      choices = validator.pubkeys,
  #                      selected = "AAZdEa1krazg48bTnydEsDzNmFrQPt6T7XxhfdQ65D2r", 
  #                      server = TRUE)
  
  
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
    #c("# New Validators", "Validator Age", "Retention by Activity", "Retention by SOL"
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
    #dolog.size <- corr.plot.choices[selection_name == input$corr_size]$log_axis
    
    # # normalize sizes:
    # if(sizevar == "equal") {
    #   size.var <- 1  
    # } else {
    #   size.var <- (overview.vals[[sizevar]] - min(overview.vals[[sizevar]])) / 
    #     (max(overview.vals[[sizevar]]) - min(overview.vals[[sizevar]]))*10
    # }
    
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
                          #size = ~log(overview.vals[[sizevar]]))) %>%
      layout(xaxis = list(title = input$corr_x, type = ifelse(dolog.x, "log", "linear")),
             yaxis = list(title = input$corr_y, type = ifelse(dolog.y, "log", "linear")),
             showlegend = FALSE,
             plot_bgcolor = "transparent",
             paper_bgcolor = "transparent",
             font = list(family = "Roboto Mono", size = 12))
    
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
    plot_coords(validator_stake_coords_463, color_col = "sol_staked", marker_text = "label")
  })
  
  
}




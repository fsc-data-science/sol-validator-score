fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "shiny.css"),
    tags$link(rel = "icon", href = "flipside.svg")
  ),
  
  fluidRow(id = "maintabs-row",
           div(class = "flex-row",
               div(class = "left-section",
                   img(src = "solana.svg"),
                   "Solana Validators"
               ),
               
               div(class = "right-section",
                   span(id = "beta", "BETA!  "), " EPOCH: ", current.epoch
               )
           ),
           column(
             width = 12,
             div(
               class = "centered-tabs", 
               tabsetPanel(
                 id = "maintabs", type = "pills", selected = "overview",
                 
                 # Overview ----
                 tabPanel(
                   title = "OVERVIEW", 
                   value = "overview",
                   
                   fluidRow(
                     column(5, 
                            div(class = "box",
                                radioGroupButtons(
                                  inputId = "numvalidatorsradio",
                                  label = NULL, 
                                  choices = c("# Validators")
                                ),
                                div(class = "box-text",
                                    paste0("Epoch ", current.epoch, ": ", n.active.vals, " Active Validators, ", n.inactive.vals, " Inactive Validators")),
                                plotlyOutput("bpvalcounts", height = plotly.height, width = plotly.width)
                            ),
                            
                            div(class = "box",
                                radioGroupButtons(
                                  inputId = "valageradio",
                                  label = NULL, 
                                  choices = c("# New Validators", "Validator Age") 
                                  #"Retention by Activity", "Retention by SOL")
                                ),
                                plotlyOutput("ageplot", height = plotly.height, width = plotly.width)
                            ),
                            
                     ),
                     column(5,
                            div(class = "box",
                                       radioGroupButtons(
                                         inputId = "solstakedradio",
                                         label = NULL, 
                                         choices = c("Total Stake", "Avg Stake")
                                       ),
                                       div(class = "box-text", 
                                           paste0("Epoch ", current.epoch, ": ", format(round(total.stake), big.mark = ","), 
                                                  " SOL Staked (", format(round(avg.stake), big.mark = ","), " avg stake)")
                                       ),
                                       plotlyOutput("solstaked", height = plotly.height, width = plotly.width)
                            ), # close box
                            
                            div(class = "box",
                                radioGroupButtons(
                                  inputId = "corrradio",
                                  label = NULL, 
                                  choices = c("Correlations")
                                ),
                                fluidRow(column(4, selectInput(inputId = "corr_x", label = "X Axis", 
                                                               choices = c("First Epoch", "# Stakers"), 
                                                               selected = "# Stakers", width = "100%")),
                                         column(4, selectInput(inputId = "corr_y", label = "Y Axis", 
                                                               choices = c("Avg Stake Size", "SOL Staked"), 
                                                               selected = "SOL Staked", width = "100%")),
                                         column(4, selectInput(inputId = "corr_size", label = "Size", 
                                                               choices = c("Coming Soon"), selected = "Coming Soon",
                                                               #choices = c("Equal", "Avg Stake Size", "SOL Staked", "# Stakers", "Age in Epochs"), 
                                                               #selected = "Equal", 
                                                               width = "100%"))),
                                
                                plotlyOutput("corrplot", height = plotly.height, width = plotly.width)
                                
                            ) # close box
                            
                            
                     ), # close column 2 size 5
                     
                     column(2,
                            br(),br(),
                            div(class = "sidemetric",
                                div(class = "largemetric", met.total.stake),
                                div(class = "metricdesc", "Total Staked")),
                            
                            div(class = "sidemetric",
                                div(class = "largemetric", met.active.rate),
                                div(class = "metricdesc", "Validator Active Rate")),
                            
                            div(class = "sidemetric",
                                div(class = "largemetric", met.active10.rate),
                                div(class = "metricdesc", "10 Epoch Active Rate")),
                            
                            div(class = "sidemetric",
                                div(class = "largemetric", met.net.sol),
                                div(class = "metricdesc", "Net SOL Staked")),
                            
                            div(class = "sidemetric",
                                div(class = "largemetric", met.net.validators),
                                div(class = "metricdesc", "Net Validators")),
                            
                            div(class = "sidemetric",
                                div(class = "largemetric", "TBD"),
                                div(class = "metricdesc", "# Leader Slots"))
                     ) # close column 2
                     
                   ) # close boxes row
                   
                 ), # close overview panel
                 
                 # Decentralization ----
                 tabPanel(title = "DECENTRALIZATION",
                          value = "decentralization",
                          fluidRow(
                            column(5,
                                   div(class = "box",
                                       radioGroupButtons(
                                         inputId = "gininakaradio",
                                         label = NULL, 
                                         choices = c("Gini", "Nakamoto")
                                       ),
                                       div(class = "box-text", 
                                           textOutput("gini_naka_text")),
                                       plotlyOutput("gini_naka_plot", height = plotly.height, width = plotly.width)
                                   ), # close box
                                   
                                   div(class = "box",
                                       radioGroupButtons(
                                         inputId = "stakerginiradio",
                                         label = NULL, 
                                         choices = c("Staker Gini vs Stake Share", "Staker Gini Distribution")
                                       ),
                                       div(class = "box-text", 
                                           textOutput("staker_gini_desc"),
                                           plotlyOutput("val_staker_gini_token", height = plotly.height, width = plotly.width)
                                       ),
                                   )   
                            ), #close column
                            
                            column(5,
                                   div(class = "box",
                                       radioGroupButtons(
                                         inputId = "mapradio",
                                         label = NULL, 
                                         choices = c("Validator Map")
                                       ),
                                       div(class = "box-text", "Zoom in to See More"),
                                       leafletOutput("themap", height = plotly.height, width = plotly.width)
                                   ),
                                   
                                   div(class = "box",
                                       radioGroupButtons(
                                         inputId = "stakercountradio",
                                         label = NULL, 
                                         choices = c("TBD")
                                       ),
                                       div(class = "box-text", 
                                           "TBD"),
                                       #plotlyOutput("across_val_gini", height = plotly.height, width = plotly.width)
                                   )
                            ), #close column
                            

                            column(2,
                                   br(),br(),
                                   div(class = "sidemetric",
                                       div(class = "largemetric", met.nakamoto.token),
                                       div(class = "metricdesc", "Nakamoto (Token)")),
                                   
                                   div(class = "sidemetric",
                                       div(class = "largemetric", met.nakamoto.country),
                                       div(class = "metricdesc", "Nakamoto (Country)")),
                                   
                                   div(class = "sidemetric",
                                       div(class = "largemetric", met.gini.token),
                                       div(class = "metricdesc", "Gini (Token)")),
                                   
                                   div(class = "sidemetric",
                                       div(class = "largemetric", met.gini.country),
                                       div(class = "metricdesc", "Gini (Country)")),
                                   
                                   div(class = "sidemetric",
                                       div(class = "largemetric", met.staker.gini),
                                       div(class = "metricdesc", "Avg Staker Gini"))
                                   
                            ) # close column 2
                            
                            
                          ) # close row
                 ),
                 # Find Validator ----
                 tabPanel(title = "FIND A VALIDATOR",
                          value = "find",
                          fluidRow(
                            column(4, 
                                   div(class = "box",
                                       
                                       div(class = "box-text",
                                           radioGroupButtons(
                                             inputId = "solstakefilterradio",
                                             label = NULL, 
                                             choices = c("Sol Stake (1,000s)")
                                           ),
                                           
                                           numericRangeInput(inputId = "min_max_stake", 
                                                             label = NULL,
                                                             value = c(0, 10000),
                                                             min = 0,
                                                             max = 100000, 
                                                             step = 10
                                           )                                    
                                       ),
                                       
                                      div(class = "box-text",
                                          radioGroupButtons(
                                            inputId = "nstakersfilterradio",
                                            label = NULL, 
                                            choices = c("# Stakers")
                                          ),
                                          
                                          numericRangeInput(inputId = "min_max_nstakers", 
                                                            label = NULL,
                                                            value = c(1, 100000),
                                                            min = 0,
                                                            max = 200000, 
                                                            step = 100
                                          )                                    
                                      ),
                                      div(class = "box-text",
                                          radioGroupButtons(
                                            inputId = "countriesradio",
                                            label = NULL, 
                                            choices = c("Countries")
                                          ),
                                         
                                          pickerInput(
                                            inputId = "countries",
                                            label = NULL,
                                            choices = country_choices,
                                            options = list(
                                              `actions-box` = TRUE,
                                              `selected-text-format` = "count > 1",
                                              `count-selected-text` = "{0} options selected"
                                            ), selected = country_choices,
                                            multiple = TRUE
                                          )
                                      ),
                                      div(class = "box-text",
                                          radioGroupButtons(
                                            inputId = "minattendanceradio",
                                            label = NULL, 
                                            choices = c("10 Epoch Attendance")
                                          ),
                                          
                                          sliderInput(inputId = "min_attendance",
                                                      label = NULL,
                                                      min = 0,
                                                      max = 10,
                                                      value = 1,
                                                      step = 1
                                                      )
                                      )
                                          
                                       ) # end box 
                                   ),
                            column(6,
                                   div(class = "box",
                                       radioGroupButtons(
                                         inputId = "mapradio",
                                         label = NULL, 
                                         choices = c("Validator Map")
                                       ),
                                       div(class = "box-text", "Zoom in to See More"),
                                       leafletOutput("decentralized_map", height = plotly.height, width = plotly.width)
                                   ), # close box
                                   
                                   div(class = "box",
                                       radioGroupButtons(
                                         inputId = "selectvalidatorsradio",
                                         label = NULL, 
                                         choices = c("Select Validators")
                                       ),
                                       reactableOutput("select_validators")
                                                                                                       
                                   ) # close box
                                   
                                   
                            ), # close column 2 size 5
                            
                            column(2,
                                   br(),br(),
                                   div(class = "sidemetric",
                                       div(class = "largemetric", met.total.stake),
                                       div(class = "metricdesc", "Qualified Stake")),
                                   
                                   div(class = "sidemetric",
                                       div(class = "largemetric", met.active.rate),
                                       div(class = "metricdesc", "# Qualifying Validators")),
                                   
                                   div(class = "sidemetric",
                                       div(class = "largemetric", met.active10.rate),
                                       div(class = "metricdesc", "Avg Stake")),
                                   
                                   div(class = "sidemetric",
                                       div(class = "largemetric", met.net.sol),
                                       div(class = "metricdesc", "# Stakers"))
                                   
                            ) # close column 2
                            
                          ) # close boxes row
                          
                          ), # close overview panel
                 
                 tabPanel(title = "BY VALIDATOR",
                          value= "by_validator",
                          fluidRow(column(3),
                                   column(6, 
                                          br(), br(), br(),
                                          div(class = "tbd", "Coming Soon: All info for a single validator")
                                   ),
                                   column(3)
                          )
                 ),
                 
               ), # close tab panel
               
             ) # close centered-tabs div
             
           ) # close column 12
  ) # close tabs fluid row
)










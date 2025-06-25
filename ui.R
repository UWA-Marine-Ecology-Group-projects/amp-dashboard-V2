ui <- page_navbar(
  id = "navbar_id", # Add an ID to track the active panel
  title = div(
    HTML(paste0(
      "Australian Marine Parks Dashboard"
    )),
  ),
  nav_spacer(),
  useShinyalert(),  # Set up shinyalert
  
  # Sidebar with radio button and dropdowns
  sidebar = sidebar(
    id = "main_sidebar", # Add an ID to the sidebar
    width = 300,
    
    # radioButtons(
    #   inputId = "toggle",
    #   label = "Investigate:",
    #   choices = c("Network/Bioregion" = "Network", 
    #               "Marine Park" = "Marine Park"),
    #   selected = "Marine Park"
    # ),
    
    #     # First radio button for selecting network
    radioButtons("network", "Choose a Network/Bioregion:",
                 choices = c(unique(all_data$metadata$network)
                 ),
                 
                 selected = "South-west"),
    #     # Conditionally display the park input based on filterpark selection
    # conditionalPanel(
    #   condition = "input.toggle == 'Marine Park'",
    #   
    # uiOutput("dynamic_marine_park")
    radioButtons("marine_park", 
                 "Marine Park/Sentinel Area:", 
                 choices = c(initial_parks))
    # ),
    
  ),
  
  useShinyjs(), # Enable shinyjs
  
  theme = bs_theme("bslib_spacer" = "1rem"),
  
  # Main panel with conditional content
  nav_panel(
    title = "Dashboard",
    fluidRow(
      
      div(
        # Conditional panels for name of the view ----
        # conditionalPanel(
        #   condition = "input.toggle == 'Network'",
        # h3(htmlOutput("network_name"))
        
        # conditionalPanel(
        # condition = "input.toggle == 'Marine Park'",
        
        h3(htmlOutput("marinepark_name"))

      ),
      
      layout_column_wrap(
        
        shinyWidgets::pickerInput(
          inputId = "metric",
          label = "Ecosystem component:",
          width = "100%",
          choices = unique(all_data$dropdown_data$metric),
          multiple = FALSE,
          selected = unique(all_data$dropdown_data$metric)[1],
          options = list(`actions-box` = TRUE, `live-search` = FALSE, `dropup-auto` = FALSE)
        ),
        
        uiOutput("dynamic_ecosystem_subcomponent"
        ),
      ),
      
      h4(htmlOutput("ecosystem_subcomponent_name")),
      # div(uiOutput("ecosystem_subcomponent_name")),
      
      div(
        # card(
        layout_column_wrap(
          
          div(
            layout_column_wrap(height = 200,
                               fill = FALSE,
                               
                               value_box(
                                 title = textOutput("individuals_counted_title"),
                                 theme = "primary",
                                 value = textOutput("individuals_counted"),
                                 showcase = uiOutput("dynamic_icon1")# icon("fish")
                               ),
                               
                               value_box(
                                 title = textOutput("number_species_title"),
                                 theme = "primary",
                                 value = textOutput("number_species"),
                                 showcase = uiOutput("dynamic_icon2")#icon("fish")
                               )
            )),
          
          # Marine park images
          div(style = "align-items: center; justify-content: center;",
              # conditionalPanel(
              #   condition = "input.toggle == 'Network'",
              #   uiOutput("ui_network",
              #            width = "100%")
              # ),
              # conditionalPanel(
              #   condition = "input.toggle == 'Marine Park'",
              uiOutput("ui_marine_park",
                       width = "100%")
              # )
          )
        ),
        
      ),
      div(navset_card_pill(
        nav_panel(title = h4("Observations:"),
                  
                  div(
                    page_fillable(layout_column_wrap(
                      width = NULL,
                      fill = FALSE,
                      style = css(grid_template_columns = "1fr 1fr"),
                      
                      # SUMMARY CARD ----
                      div(
                        
                        card(
                          height = 500,
                          
                          card_header(
                            "Summary"
                          ),
                          
                          layout_column_wrap(
                            width = 1/3, height = 225,
                            
                            value_box(
                              title = "Depths surveyed",
                              theme = "primary",
                              value = textOutput("depth_range")
                            ),
                            
                            value_box(
                              title = "Average depth surveyed",
                              theme = "primary",
                              value = textOutput("average_depth")
                            ),
                            
                            value_box(
                              title = textOutput("samples_deployed_title"),
                              theme = "primary",
                              value = textOutput("samples_deployed"),
                              showcase = uiOutput("dynamic_icon3")#icon("fish")
                            ),
                          ),
                          
                          conditionalPanel(
                            condition = "input.metric == 'Natural Values'",
                            
                            h5("Method(s) used for data collection:"),
                            uiOutput("ui_method_button"),
                          ),
                          uiOutput("ui_open_ga_button", width = "100%")
                        ),
                        
                        card(
                          
                          max_height = 150,
                          full_screen = TRUE,
                          
                          card_header(
                            "Observations"
                          ),
                          
                          uiOutput("dynamic_text")
                        )
                      ),
                      
                      div(
                        page_fillable(
                          card(
                            full_screen = TRUE,
                            max_height = 665,
                            
                            card_header(
                              "Map"
                            ),
                            
                            uiOutput("map_year_slider"),
                            
  #                           tags$head(
  #                             tags$style(HTML("
  #   #map-container {
  #     height: 100vh !important;
  #   }
  #   #map {
  #     height: 100% !important;
  #     width: 100% !important;
  #   }
  #   .bslib-card-fullscreen {
  #     height: 100vh !important;
  #   }
  # "))),
                            
                            layout_column_wrap(width = 1,
                                               card(full_screen = FALSE,
                                                    min_height = 500,
                                                    # max_height = 600,
                                                    width = "50%",
                                                    id = "map-container",
                                                    withSpinner(leafletOutput("map", #height = "100%"
                                                                              height = "50vh"
                                                                              #, height = 550
                                                                              ))
                                               )
                            )
                            
                            
                          )
                        ))
                    )
                    )),
                  
                  tags$head(
                    tags$style(HTML("
               .fishnclips-legend-aus {
                 background-color: rgba(255, 255, 255, 0.8); /* white background with transparency */
                 padding: 10px;
                 border-radius: 5px;
                 box-shadow: 0 0 10px rgba(0, 0, 0, 0.2); /* optional shadow */
               font: 14px / 16px Arial, Helvetica, sans-serif;
               }

                       .fishnclips-legend-map {
                 background-color: rgba(255, 255, 255, 0.8); /* white background with transparency */
                 padding: 10px;
                 border-radius: 5px;
                 box-shadow: 0 0 10px rgba(0, 0, 0, 0.2); /* optional shadow */
               font: 14px / 16px Arial, Helvetica, sans-serif;
               }

               .leaflet-container {z-index:0}

               .leaflet-top, .leaflet-bottom {
    z-index: unset !important;
}

.leaflet-touch .leaflet-control-layers, .leaflet-touch .leaflet-bar {
    z-index: 10000000000 !important;
}"))
                  ),

div(
  layout_column_wrap(
    width = NULL,
    fill = FALSE,
    style = css(grid_template_columns = "1fr 1fr"),
    
    # MOST ABUNDANT SPECIES ----
    div(
      card(
        height = 715,
        card_header("Most abundant species"),
        
        # h5(HTML(paste0("Most common species:"))),
        withSpinner(
          plotOutput("top_ten_plot", height = 620
          )
        )
      )
    ),
    
    
    # ASSEMBLAGE SPATIAL ----
    div(
      card(
        full_screen = TRUE,
        height = 715,
        
        card_header("Spatial distribution of assemblage metrics:"),
        
        shinyWidgets::pickerInput(
          inputId = "assemblage",
          label = "Choose an assemblage metric:",
          width = "100%",
          choices = c("Total abundance", "Species richness"),
          multiple = FALSE,
          selected = "Total abundance",
          options = list(`actions-box` = TRUE, `live-search` = FALSE, `dropup-auto` = FALSE)
        ),
        
        uiOutput("assemblage_year_slider"),
        
        card(full_screen = FALSE,
             max_height = "100%",
             id = "map-container",
             style = "height: 85vh;",
             withSpinner(leafletOutput("assemblage_map", height = 400))
        ))
    ))),

tags$head(
  tags$script(HTML("
      function resizeIframe(obj) {
        obj.style.height = obj.contentWindow.document.documentElement.scrollHeight + 'px';
      }
    "))),

# SPECIES DISTRIBUTION MAP ----
div(
  page_fillable(
  card(
    full_screen = TRUE,
    max_height = 700,
    
    card_header("Investigate a species abundance data"),
    
    fluidRow( column(width = 6,
                     htmlOutput("ui_species")),
              
              column(width = 6,
                     uiOutput("species_year_slider"))),#),#)

      layout_columns(width = 1/2,
                       card(full_screen = FALSE,
                            max_height = 500, #500, #"100%",
                            width = "100%",
                            id = "map-container",
                            
                            withSpinner(leafletOutput("species_map", height = 460
                                                      ))
                       ),
                       
                       div(
                         class = "iframe-container",
                         card(
                           # min_height = "450",
                           # max_height = 430,
                           max_height = 500, #500, #"100%",
                           max_width = "100%",
                           withSpinner(htmlOutput("iframe"#, height = "100%"
                                                  )))
                       )))#,
    # ),
  ),
  
  
  
  layout_column_wrap(#width = 1/2,
    
    card(full_screen = TRUE,
         # max_height = 700,
         
         card_header("Investigate a species length data"),
         
         fluidRow( column(width = 4,
                          htmlOutput("ui_species_length")),
                   
                   column(width = 4,
                          uiOutput("species_year_slider_length")),
                   
                   column(width = 4,
                          numericInput("binwidth", 
                                       label = "Bin width for histograms (mm)", 
                                       min = 0, 
                                       max = 1000, 
                                       value = 10,
                                       width = "100%")),
                   
                   ),
         
         card(full_screen = FALSE,
              max_height = "100%",
              width = "100%",
              id = "map-container",
              
              card_header("Length frequency histogram"),
              
              withSpinner(plotOutput("length_histogram"#, height = 450
                                     ))
              ),
         
         
         card(full_screen = FALSE,
              max_height = "100%",
              width = "100%",
              id = "map-container",
              
              card_header("Normalised length frequency histogram"),
              
              withSpinner(plotOutput("length_histogram_density"#, height = 400
                                     ))
         )
  ),
  
)
),

tags$head(
  tags$style(HTML("


      .custom-button {
      background-color: #black !important;
      width: 100%;
      }

      .custom-button:hover {
        background-color: #007BC2 !important;
        color: white !important;
      }
    ")) # 263F6B navy hover colour
),
        ),


nav_panel(title = h4("Status & Trends:"),
          
          
          div(
            layout_column_wrap(
              width = NULL,
              fill = FALSE,
              style = css(grid_template_columns = "1fr 1fr"),
              
              card(
                
                max_height = 400,
                full_screen = TRUE,
                
                card_header("Trend"),
                
                uiOutput("dynamic_text1")
              ),
              
              card(
                
                card_header("Condition"),
                
                max_height = 400,
                uiOutput("condition_plot_ui")
              ))),
          
          div(
            card(
              max_height = 700,
              card_header("Modelled outputs"),
              
              uiOutput("dynamic_options", width = "100%"),
              
              page_fillable(layout_column_wrap(
                width = NULL,
                fill = FALSE,
                style = css(grid_template_columns = "1fr 1fr"),
                
                div(
                  card(full_screen = TRUE,
                       uiOutput("temporal_plot_ui"))
                ),
                
                div(
                  card(
                    full_screen = TRUE,
                    max_height = 700,
                    id = "map-container",
                    leafletOutput("australia_map", height = 500
                    ))),
              )))),
          
),
      ),
      )

    )
  ),
nav_panel(
  title = "FishNClips",
  leafletOutput("fishnclips", height = "85%")),

nav_item(input_dark_mode()),
nav_item(tags$img(src = "https://marineecology.io/images/meg_logo_and_title.png", height = "30px", style = "float: right;")),
nav_item(tags$img(src = "https://www.dcceew.gov.au/sites/default/files/env/pages/2f5bf626-4f90-43f9-a27a-7c701c032f71/images/nesp-marine-coastal-hub.jpg", height = "60px", style = "float: right;")),
nav_item(tags$img(src = "https://australianmarineparks.gov.au/static/eaef8f7794ae4c58fb7d5814d8e9841e/amp-inline-image-australian-marine-parks-7d165e03e1950d809f8a8071ea899a2e2d49eb45e3364feb2563bc03a2d82a86.svg", height = "60px", style = "float: right;"))
)

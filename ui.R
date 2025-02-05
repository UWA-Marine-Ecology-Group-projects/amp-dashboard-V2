ui <- page_navbar(
  id = "navbar_id", # Add an ID to track the active panel
  title = div(
    HTML(paste0(
      "Australian Marine Parks Dashboard <i>(this is a draft and contains fake data DO NOT USE FOR INTERPRETATION)</i>"
    )),
  ),
  nav_spacer(),
  
  # Sidebar with radio button and dropdowns
  sidebar = sidebar(
    id = "main_sidebar", # Add an ID to the sidebar
    width = 300,
    
    radioButtons(
      inputId = "toggle",
      label = "Investigate:",
      choices = c("Network", "Marine Park"),
      selected = "Marine Park"
    ),
    
    #     # First radio button for selecting network
    radioButtons("network", "Choose a Network:",
                 choices = c(unique(all_data$file_info$network)
                 ),
                 
                 selected = "South-west"),
    #     # Conditionally display the park input based on filterpark selection
    conditionalPanel(
      condition = "input.toggle == 'Marine Park'",
      
      uiOutput("dynamic_marine_park")
    ),
    
  ),
  
  useShinyjs(), # Enable shinyjs
  
  theme = bs_theme("bslib_spacer" = "1rem"),
  
  # Main panel with conditional content
  nav_panel(
    title = "Dashboard",
    fluidRow(
      
      div(
        # Conditional panels for name of the view ----
        conditionalPanel(
          condition = "input.toggle == 'Network'",
          uiOutput("network_name_1")),
        
        conditionalPanel(
          condition = "input.toggle == 'Marine Park'",
          
          uiOutput("marinepark_name_1")),
        
        layout_column_wrap(height = 175, fill = FALSE,
                           value_box(
                             title = "Fish counted",
                             theme = "primary",
                             value = textOutput("fish_counted"),
                             showcase = icon("fish")
                           ),
                           
                           value_box(
                             title = "Fish species identified",
                             theme = "primary",
                             value = textOutput("fish_species"),
                             showcase = icon("fish")
                           ),
                           
                           value_box(
                             title = "stereo-BRUVs deployed",
                             theme = "primary",
                             value = textOutput("bruvs_deployed"),
                             showcase = img(src = "stereo-BRUV_filled_transparent.png",
                                            height = "80px",
                                            style = "margin-left: 15px;" # Adjust the value as needed)
                             )
                           ),
                           
                           value_box(
                             title = "stereo-BOSS deployed",
                             theme = "primary",
                             value = textOutput("boss_deployed"),
                             showcase = img(src = "frame_transparent_white.png",
                                            height = "80px",
                                            style = "margin-left: 15px;" # Adjust the value as needed)
                             )
                           )
        ),
        
        # Marine park images
        # div(style = "align-items: center; justify-content: center;",
        #     conditionalPanel(
        #       condition = "input.toggle == 'Network'",
        #       uiOutput("ui_network", width = "100%")
        #     ),
        #     conditionalPanel(
        #       condition = "input.toggle == 'Marine Park'",
        #       uiOutput("ui_marine_park", width = "100%")
        #     )
        # ),
        
      ),
      
      layout_column_wrap(
        
        selectInput(
          inputId = "metric",
          label = "Ecosystem component:",
          choices = unique(all_data$dropdown_data$metric),
          selected = unique(all_data$dropdown_data$metric)[1]
        ),
        
        uiOutput("dynamic_ecosystem_subcomponent"
        ),
      ),
      
      div(uiOutput("ecosystem_subcomponent_name")),
      
      div(
        # card(
        layout_column_wrap(
          width = NULL, 
          # height = 300, 
          fill = FALSE,
          style = css(grid_template_columns = "1fr 1fr"),
          
          # SUMMARY CARD ----
          div(
            
            card(
              height = 450,
              
              card_header(
                "Summary"
              ),
              
              # h5(HTML(paste0("Summary:"))),
              
              layout_column_wrap(
                width = 1/3, height = 110,
                
                value_box(
                  title = "Park Area",
                  theme = "primary",
                  value = "2,435 km²"
                ),
                
                value_box(
                  title = "Depth Range",
                  theme = "primary",
                  value = "30 - 500 m"
                ),
                
                value_box(
                  title = "Average Depth",
                  theme = "primary",
                  value = "177 m"
                )
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
                "Trend"
              ),
              
              # h5(HTML(paste0("Trends:"))),
              uiOutput("dynamic_text")
            )
          ),
          
          # MOST ABUNDANT SPECIES ----
          div(
            card(
              height = 615,
              
              card_header(
                "Most abundant species"
              ),
              
              # h5(HTML(paste0("Most common species:"))),
              withSpinner(
                plotOutput("top_ten_plot", height = 520
                )
              )
            )
          )
        )
      ),
      
      tags$head(
        tags$style(HTML("
               .fishnclips-legend {
                 background-color: rgba(255, 255, 255, 0.8); /* white background with transparency */
                 padding: 10px;
                 border-radius: 5px;
                 box-shadow: 0 0 10px rgba(0, 0, 0, 0.2); /* optional shadow */
               }
               
               .leaflet-container {z-index:0}
               
               .leaflet-top, .leaflet-bottom {
    z-index: unset !important;
}

.leaflet-touch .leaflet-control-layers, .leaflet-touch .leaflet-bar {
    z-index: 10000000000 !important;
}"))
      ),

# SPECIES DISTRIBUTION MAP ----
div(
  # card(
  layout_column_wrap(
    width = NULL, 
    # height = 300, 
    fill = FALSE,
    style = css(grid_template_columns = "1fr 1fr"),
    
    # SUMMARY CARD ----
    div(
      card(
        full_screen = TRUE, 
        height = 600,
        
        card_header(
          "Spatial distribution of assemblage metrics:"
        ),
        
        selectInput(
          inputId = "assemblage",
          label = "Choose an assemblage metric",
          choices = c("Total abundance", "Species richness", "Community Temperature Index"),
          selected = "Total abundance",
          width = "100%"
        ),
        
        card(full_screen = FALSE, 
             max_height = "100%",
             id = "map-container",
             # style = "position: sticky; top: 0; height: 100vh;",
             style = "height: 85vh;",
             leafletOutput("assemblage_map"
             )
        ))
    ),
    
    
    div(
      
      card(
        full_screen = TRUE, 
        height = 600,
        
        card_header(
          "Spatial distributions of species"
        ),
        
        htmlOutput("ui_species"),
        
        # selectInput(
        #   inputId = "species",
        #   label = "Choose a species:",
        #   choices = unique(all_data$bubble_data$display_name),
        #   selected = unique(all_data$bubble_data$display_name)[1],
        #   width = "100%"
        # ),
        
        card(full_screen = FALSE, 
             max_height = "100%",
             id = "map-container",
             # style = "position: sticky; top: 0; height: 100vh;",
             style = "height: 85vh;",
             leafletOutput("species_map"
             )
        ))
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


# uiOutput("condition_plot_ui"),




div(
  card(
    
    card_header(
      "Modelled outputs"
    ),
    
    uiOutput("dynamic_options", width = "100%"),
    
    layout_column_wrap(
      width = NULL, 
      # height = 300, 
      fill = FALSE,
      style = css(grid_template_columns = "1fr 1fr"),
      
      div(
        card(full_screen = TRUE, 
             
             # uiOutput("metric_name"),
             uiOutput("temporal_plot_ui"))
        
      ),
      
      
      div(
        card(
          full_screen = TRUE, 
          max_height = "100%",
          id = "map-container",
          # style = "position: sticky; top: 0; height: 100vh;",
          style = "height: 85vh;",
          leafletOutput("australia_map"
          ))),
    ))),


# )
# ),

    )
  ),
nav_panel(
  title = "FishNClips",
  leafletOutput("fishnclips", height = "85%")),

# nav_panel(
#   title = "Summary Statistics",
#   
#   # Conditional panels for name of the view ----
#   conditionalPanel(
#     condition = "input.toggle == 'Network'",
#     
#     uiOutput("network_name_2")#,
#     # uiOutput("ui_network")
#   ),
#   
#   conditionalPanel(
#     condition = "input.toggle == 'Marine Park'",
#     
#     uiOutput("marinepark_name_2")#,
#     # uiOutput("ui_marine_park")
#   ),
#   
# ),

nav_item(input_dark_mode()),
nav_item(tags$img(src = "https://marineecology.io/images/meg_logo_and_title.png", height = "30px", style = "float: right;"))
)

ui <- page_navbar(
  id = "navbar_id",
  title = div(HTML("Australian Marine Parks Dashboard")),
  nav_spacer(),
  
  theme = theme,
  
  header = tagList(
    tags$head(
      includeCSS("www/custom.css"),
      
      # Update method legend HTML safely (no duplicates ever)
      tags$script(HTML("
        Shiny.addCustomMessageHandler('set_method_legend_html', function(msg) {
          var el = document.getElementById('method-legend');
          if(el) el.innerHTML = msg.html;
        });
      "))
    )
  ),
  
  sidebar = sidebar(
    id = "main_sidebar",
    width = 300,
    
    tags$h5("Marine Park/Sentinel Area:"),
    
    selectizeInput(
      "marine_park",
      label = NULL,
      choices = split(df$marine_park, df$network),
      selected = "Geographe Marine Park", #df$marine_park[[1]],   # <- pick the first park as default
      options = list(
        placeholder = "Start typing a park name...",
        dropdownHeight = "800px",
        maxOptions = 5000
      )
    )
  ),
  
  nav_panel(
    title = "Dashboard",
    fluidRow(
      div(
        style = "align-items: center; justify-content: center;",
        uiOutput("ui_marine_park", width = "100%")
      ),
      
      div(h6("")),
      
      div(class = "vb-grid-tight", uiOutput("ui_method_cards")),
      
      div(
        navset_card_pill(
          # ---- TAB 1: Observations ----
          nav_panel(
            title = "Observations:",
            navset_card_tab(
              id = "obs_tabs",
              
              nav_panel(
                title = "Sampling effort",
                layout_column_wrap(
                  min_height = 650,
                  max_height = 800,
                  width = 1/2,
                  card(
                    min_height = 500,
                    full_screen = TRUE,
                    card_header("Map of Sampling Effort"),
                    leafletOutput("map_methods", height = 450)
                  ),
                  div(
                    spinnerPlotOutput("plot_effort_year"),
                    spinnerPlotOutput("plot_effort_depth")
                  )
                )
              ),
              
              nav_panel(
                title = "Top species",
                spinnerPlotOutput("top_species_plot", height = 700)
              ),
              
              nav_panel(
                title = "Total abundance",
                uiOutput("assemblage_maps_ui")
              ),
              
              nav_panel(
                title = "Species Richness",
                uiOutput("richness_maps_ui")
              ),
              
              nav_panel(
                title = "Investigate a species abundance data",
                # layout_column_wrap(
                  # width = 1/2,
                  # min_height = 650,
                  # max_height = 800,
                  # card(
                    # full_screen = TRUE,
                    # card_header("Map (dummy – hook up later)"),
                selectizeInput(
                  "species",
                  label = NULL,
                  width = "100%",
                  choices = "Chrysophrys auratus (Pink snapper)",
                  selected = "Chrysophrys auratus (Pink snapper)", 
                  options = list(
                    placeholder = "Choose a species...",
                    dropdownHeight = "800px",
                    maxOptions = 5000
                  )
                ),
                    uiOutput("species_abundance_maps_ui")
                  # )#,
                  # card(
                  #   full_screen = TRUE,
                  #   card_header("Species abundance plot"),
                  #   spinnerPlotOutput("species_plot", height = 450)
                  # )
                # )
              ),
              
              nav_panel(
                title = "Length",
                # layout_column_wrap(
                #   width = 1/2,
                #   min_height = 650,
                #   max_height = 800,
                #   card(
                #     full_screen = TRUE,
                #     card_header("Length plot"),
                #     spinnerPlotOutput("length_plot", height = 450)
                #   ),
                  card(
                    full_screen = TRUE,
                    card_header("Length histogram"),
                    spinnerPlotOutput("length_hist", height = 450)
                  )
                # )
              )
              
            ) # <-- closes navset_card_tab(id="obs_tabs")
          ),  # <-- closes nav_panel(title="Observations:")
          
          # ---- TAB 2: State & Trend ----
          nav_panel(
            title = "State & Trend:",
            navset_card_tab(
              id = "trend_metric_tab",
              nav_panel(
                title = "Large Reef Fish Index* (B20*)",
                uiOutput("trend_plots_b20_ui"),
                uiOutput("b20_blocks_ui")
              ),
              nav_panel(
                title = "Reef Fish Thermal Index",
                uiOutput("trend_plots_rfti_ui"),
                uiOutput("rfti_blocks_ui")
              )
            )
          )
        )
  ))),
  
  nav_panel(
    title = "FishNClips",
    leafletOutput("fishnclips", height = "85%")
  ),
  
  nav_item(input_dark_mode()),
  nav_item(tags$img(src = "https://marineecology.io/images/meg_logo_and_title.png", height = "30px", style = "float: right;")),
  nav_item(tags$img(src = "https://www.dcceew.gov.au/sites/default/files/env/pages/2f5bf626-4f90-43f9-a27a-7c701c032f71/images/nesp-marine-coastal-hub.jpg", height = "60px", style = "float: right;")),
  nav_item(tags$img(src = "https://australianmarineparks.gov.au/static/eaef8f7794ae4c58fb7d5814d8e9841e/amp-inline-image-australian-marine-parks-7d165e03e1950d809f8a8071ea899a2e2d49eb45e3364feb2563bc03a2d82a86.svg", height = "60px", style = "float: right;"))
)

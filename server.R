server <- function(input, output, session) {
  
  # bs_themer() # Turn this on if want to see real-time theming
  
  observe({
    active_panel <- input$navbar_id %>% glimpse
    
    # if (active_panel == "Summary Statistics") {
    #   shinyjs::hide("main_sidebar")
    # } else {
    #   shinyjs::show("main_sidebar")
    # }
  })
  
  # Dynamic dropdown for Ecosystem Component
  output$dynamic_ecosystem_subcomponent <- renderUI({
    req(input$metric)
    components <- unique(all_data$dropdown_data$ecosystem_component[all_data$dropdown_data$metric == input$metric])
    
    shinyWidgets::pickerInput(
      inputId = "ecosystemsubcomponent",
      label = "Ecosystem sub-component:",
      width = "100%",
      choices = components,
      multiple = FALSE,
      selected = components[1],
      options = list(`actions-box` = TRUE, `live-search` = FALSE, `dropup-auto` = FALSE)
    )
    
    # selectInput(
    #   inputId = "ecosystemsubcomponent",
    #   label = "Ecosystem sub-component:",
    #   choices = components,
    #   selected = components[1],
    #   width = "100%"
    # )
  })
  
  output$dynamic_options <- renderUI({
    req(input$metric, input$ecosystemsubcomponent)
    options <- all_data$dropdown_data$options[all_data$dropdown_data$metric == input$metric & all_data$dropdown_data$ecosystem_component == input$ecosystemsubcomponent]
    if (length(options) > 0) {
      options_list <- strsplit(options, "\\|")[[1]]
      selectInput(
        inputId = "options",
        label = "Choose an indicator metric:",#paste(input$ecosystemcomponent, "Options:"),
        choices = options_list,
        selected = options_list[1],
        width = "100%"
      )
    }
  })
  
  render_marinepark_name <- function() {
    req(input$toggle, input$network, input$marine_park)
    h3(HTML(paste0("<b>", input$marine_park)))
  }
  
  output$marinepark_name_1 <- renderUI(render_marinepark_name())
  output$marinepark_name_2 <- renderUI(render_marinepark_name())
  
  render_network_name <- function() {
    req(input$toggle, input$network, input$marine_park)
    h3(HTML(paste0("<b>", input$network, " Network")))
  }
  
  output$network_name_1 <- renderUI(render_network_name())
  output$network_name_2 <- renderUI(render_network_name())
  
  output$ecosystem_subcomponent_name <- renderUI({
    req(input$ecosystemsubcomponent)
    
    h4(HTML(paste0("<i>", input$ecosystemsubcomponent)))
    
  })
  
  output$metric_name <- renderUI({
    req(input$options)
    
    h5(HTML(paste0("<i>", input$options)))
    
  })
  
  # Reset marine park input when switching toggle
  observeEvent(input$toggle, {
    if (input$toggle == "Network") {
      updateSelectInput(session, "marine_park", selected = NULL)
    }
  })
  
  
  output$dynamic_marine_park <- renderUI({
    req(input$toggle, input$network)
    
    selected_network <- input$network
    
    parks <- all_data$file_info %>%
      dplyr::filter(network == selected_network) %>%
      dplyr::distinct(marine_park) %>%
      dplyr::filter(!marine_park %in% c("South-west Network", "North-west Network")) %>%
      dplyr::pull(marine_park)
    
    radioButtons("marine_park",
                 "Marine Park/Sentinel Area:",
                 choices = parks)
  })
  
  observeEvent(input$network, {
    selected_network <- input$network
    
    parks <- all_data$file_info %>%
      dplyr::filter(network == selected_network) %>%
      dplyr::distinct(marine_park) %>%
      dplyr::filter(!marine_park %in% c("South-west Network", "North-west Network")) %>%
      dplyr::pull(marine_park)
    
    updateRadioButtons(session, "marine_park", choices = c(parks), selected = parks[1])
  })
  
  # Reactive to handle the condition filtered dataset
  condition_filtered_data <- reactive({
    req(input$toggle, input$network)
    
    plot_list <- all_data$file_info
    
    if (input$toggle == "Marine Park") {
      req(input$marine_park)  # Ensure marine_park input is selected
      
      # message("view conditional data marine park")
      
      plot_list %>%
        dplyr::filter(network %in% input$network) %>%
        dplyr::filter(marine_park %in% input$marine_park) %>%
        dplyr::filter(metric %in% input$ecosystemsubcomponent) #%>% glimpse
    } else {
      
      # message("view conditional data network")
      
      plot_list %>%
        dplyr::filter(network %in% input$network) %>%
        dplyr::filter(marine_park %in% paste(input$network, "Network")) %>%
        dplyr::filter(metric %in% input$ecosystemsubcomponent) #%>% glimpse
    }
  })
  
  output$condition_plot_ui <- renderUI({
    req(condition_filtered_data())
    
    chosen_plot <- condition_filtered_data()
    
    # Check if data is valid
    # if (is.null(chosen_plot) || nrow(chosen_plot) == 0 || !"file" %in% colnames(chosen_plot)) {
    # Fallback to plain text
    # return(tags$p("No condition data available for the selected filters.", style = "font-size: 18px; color: gray; text-align: center;"))
    # } else {
    # Render the plot
    
    
    validate(need(nrow(chosen_plot) > 0, "No condition data available for the selected filters."))
    plotOutput("condition_plot", height = condition_plot_height())
    # }
  })
  
  output$condition_plot <- renderPlot({
    
    req(condition_filtered_data())
    
    chosen_plot <- condition_filtered_data()
    
    # Debug chosen_plot
    # message("Chosen plot: ", ifelse(is.null(chosen_plot), "NULL", paste(nrow(chosen_plot), "rows")))
    
    # Safely read and plot
    file_path <- here::here(unique(chosen_plot$file))
    if (!file.exists(file_path)) {
      stop("File does not exist: ", file_path)
    }
    chosen_file <- readRDS(file_path)
    plot(chosen_file)
    
  })
  
  # Reactive height for the plot
  condition_plot_height <- reactive({
    req(condition_filtered_data())
    
    chosen_plot <- condition_filtered_data()
    
    if (!is.null(chosen_plot) && nrow(chosen_plot) > 0) {
      num_years <- as.numeric(chosen_plot$years)
      
      if (num_years == 1) {
        height <- 200
      } else {
        height <- num_years * 175  # Adjust the calculation as needed
      }
    } else {
      height <- 100  # Default height if no data
    }
    
    # message("Dynamic plot height: ", height)
    return(height)
  })
  
  output$condition_plot_ui <- renderUI({
    
    req(input$toggle, input$network)
    plotOutput("condition_plot", height = paste0(condition_plot_height(), "px"))
    
  })
  
  output$dynamic_text <- renderUI({
    req(input$toggle, input$network)
    
    if (input$toggle == "Marine Park") {
      req(input$marine_park)  # Ensure marine_park input is selected
      text <- all_data$text_data %>%
        dplyr::filter(network %in% input$network) %>%
        dplyr::filter(marine_park %in% input$marine_park) %>%
        dplyr::filter(ecosystem_condition %in% input$ecosystemsubcomponent)
    } else {
      text <- all_data$text_data %>%
        dplyr::filter(network %in% input$network) %>%
        dplyr::filter(marine_park %in% paste(input$network, "Network")) %>%
        dplyr::filter(ecosystem_condition %in% input$ecosystemsubcomponent)
    }
    
    h6(unique(text$text))
  })
  
  # Temporal plot filtered data ----
  temporal_filtered_data <- reactive({
    req(input$toggle, input$network)
    
    plot_list <- all_data$temporal_file_info
    
    if(input$toggle %in% "Marine Park"){
      req(input$marine_park) # Ensure marine_park input is available
      chosen_plot <- plot_list %>%
        dplyr::filter(network %in% input$network) %>%
        dplyr::filter(marine_park %in% input$marine_park) %>%
        dplyr::filter(metric %in% input$options)
      
    } else {
      
      chosen_plot <- plot_list %>%
        dplyr::filter(network %in% input$network) %>%
        dplyr::filter(marine_park %in% paste(input$network, "Network")) %>%
        dplyr::filter(metric %in% input$options)
    }
  })
  
  
  output$temporal_plot <- renderPlot({
    
    req(temporal_filtered_data())
    
    chosen_plot <- temporal_filtered_data()
    # validate(need(nrow(chosen_plot) > 0, "No temporal data available for the selected filters."))
    
    chosen_file <- readRDS(here::here(unique(chosen_plot$file)))
    plot(chosen_file)
    
  })
  
  # Reactive height for the plot
  temporal_plot_height <- reactive({
    req(temporal_filtered_data())
    
    chosen_plot <- temporal_filtered_data()
    
    if (nrow(chosen_plot) > 0) {
      
      num_depths <- as.numeric(unique(chosen_plot$depth_classes))
      
      if(num_depths == 1){
        
        height <- 250
        
      } else {
        
        height <- num_depths * 250
        
      }
      
    } else {
      height <- 50  # Default height if no data
    }
    
    return(height)
  })
  
  # UI for temporal plot ----
  output$temporal_plot_ui <- renderUI({
    
    req(input$toggle, input$network)
    plotOutput("temporal_plot", height = paste0(temporal_plot_height(), "px"))
    
  })
  
  # Create filtered metadata ----
  metadata_filtered_data <- reactive({
    req(input$toggle, input$network)
    
    metadata <- all_data$metadata
    
    if (input$toggle == "Marine Park") {
      req(input$marine_park)  # Ensure marine_park input is selected
      metadata %>%
        dplyr::filter(network %in% input$network) %>%
        dplyr::filter(marine_park %in% input$marine_park)
    } else {
      metadata %>%
        dplyr::filter(network %in% input$network)
    }
  })
  
  # Create filtered predicted rasters ----
  raster_predicted_data <- reactive({
    req(input$toggle, input$network, input$options)
    
    raster_list <- all_data$raster_data %>%
      dplyr::filter(estimate %in% c("Probability", "Mean"))
    
    # message("view chosen raster dataset")
    
    if (input$toggle == "Marine Park") {
      req(input$marine_park)  # Ensure marine_park input is selected
      raster_list %>%
        dplyr::filter(network %in% input$network) %>%
        dplyr::filter(marine_park %in% input$marine_park) %>%
        dplyr::filter(metric %in% input$options) #%>% glimpse()
    } else {
      raster_list %>%
        dplyr::filter(network %in% input$network) %>%
        dplyr::filter(marine_park %in% paste(input$network, "Network")) %>%
        dplyr::filter(metric %in% input$options) #%>% glimpse()
    }
  })
  
  # Create filtered error rasters ----
  raster_error_data <- reactive({
    req(input$toggle, input$network, input$options)
    
    raster_list <- all_data$raster_data %>%
      dplyr::filter(estimate %in% c("Error"))
    
    if (input$toggle == "Marine Park") {
      req(input$marine_park)  # Ensure marine_park input is selected
      raster_list %>%
        dplyr::filter(network %in% input$network) %>%
        dplyr::filter(marine_park %in% input$marine_park) %>%
        dplyr::filter(metric %in% input$options)
    } else {
      raster_list %>%
        dplyr::filter(network %in% input$network) %>%
        dplyr::filter(marine_park %in% paste(input$network, "Network")) %>%
        dplyr::filter(metric %in% input$options)
    }
  })
  
  # Create map for Dashboard ----
  output$australia_map <- renderLeaflet({
    req(input$toggle, input$network, input$options)
    
    points <- metadata_filtered_data()
    
    if (nrow(points) == 0) {
      points <- tibble(
        latitude_dd = c(-25.0, -25.1),
        longitude_dd = c(133.0, 133.1)
      )
    }
    
    metric_title <- unique(raster_predicted_data()$metric)
    
    icon <- iconList(blue = makeIcon("images/marker_blue.png", iconWidth = 40, iconHeight =40))
    
    map.dat <- dat
    
    boss.habitat.highlights.popups <- filter(map.dat, source %in% c("boss.habitat.highlights"))
    bruv.habitat.highlights.popups <- filter(map.dat, source %in% c("bruv.habitat.highlights"))
    fish.highlights.popups <- filter(map.dat, source %in% c("fish.highlights"))
    threed.model.popups <- filter(map.dat, source %in% c("3d.model"))
    image.popups <- filter(map.dat, source %in% c('image'))
    
    # Having this in the global.R script breaks now - make icons on server side
    icon.bruv.habitat <- iconList(blue = makeIcon("images/marker_green.png", iconWidth = 40, iconHeight =40))
    icon.boss.habitat <- iconList(blue = makeIcon("images/marker_pink.png", iconWidth = 40, iconHeight =40))
    icon.fish <- iconList(blue = makeIcon("images/marker_yellow.png", iconWidth = 40, iconHeight =40))
    icon.models <- iconList(blue = makeIcon("images/marker_purple.png", iconWidth = 40, iconHeight =40))
    
    # Initial Leaflet map ----
    map <- leaflet(points) %>%
      addTiles() %>%
      # addProviderTiles(providers$Esri.WorldImagery, group = "World Imagery (satellite)") %>%
      #addMarkers(~longitude_dd, ~latitude_dd, group = "Sampling locations") %>%
      
      addMarkers(data = points, ~longitude_dd, ~latitude_dd,
                 icon = icon,
                 # popup = bruv.habitat.highlights.popups$popup,
                 clusterOptions = markerClusterOptions(iconCreateFunction =
                                                         JS("
                                          function(cluster) {
                                             return new L.DivIcon({
                                               html: '<div style=\"background-color:rgba(0, 123, 255, 0.9)\"><span>' + cluster.getChildCount() + '</div><span>',
                                               className: 'marker-cluster'
                                             });
                                           }")),
                 group = "Sampling locations"#,
                 #popupOptions=c(closeButton = TRUE,minWidth = 0,maxWidth = 700)
      )%>%
      
      
      fitBounds(
        lng1 = min(points$longitude_dd), lat1 = min(points$latitude_dd),
        lng2 = max(points$longitude_dd), lat2 = max(points$latitude_dd)
      ) %>%
      
      # Ngari Capes Marine Parks
      addPolygons(data = ngari.mp, weight = 1, color = "black",
                  fillOpacity = 0.8, fillColor = "#7bbc63",
                  group = "State Marine Parks", label=ngari.mp$Name) %>%
      
      # State Marine Parks
      addPolygons(data = state.mp, weight = 1, color = "black",
                  fillOpacity = 0.8, fillColor = ~state.pal(zone),
                  group = "State Marine Parks", label=state.mp$COMMENTS) %>%
      
      # Add a legend
      addLegend(pal = state.pal, values = state.mp$zone, opacity = 1,
                title="State Zones",
                position = "bottomright", group = "State Marine Parks") %>%
      
      # Commonwealth Marine Parks
      addPolygons(data = commonwealth.mp, weight = 1, color = "black",
                  fillOpacity = 0.8, fillColor = ~commonwealth.pal(zone),
                  group = "Australian Marine Parks", label=commonwealth.mp$ZoneName) %>%
      
      # Add a legend
      addLegend(pal = commonwealth.pal, values = commonwealth.mp$zone, opacity = 1,
                title="Australian Marine Park Zones",
                position = "bottomright", group = "Australian Marine Parks") %>%
      
      # stereo-BRUV habitat videos
      addMarkers(data=bruv.habitat.highlights.popups,
                 icon = icon.bruv.habitat,
                 popup = bruv.habitat.highlights.popups$popup,
                 #label = bruv.habitat.highlights.popups$sample,
                 clusterOptions = markerClusterOptions(iconCreateFunction =
                                                         JS("
                                          function(cluster) {
                                             return new L.DivIcon({
                                               html: '<div style=\"background-color:rgba(124, 248, 193, 0.9)\"><span>' + cluster.getChildCount() + '</div><span>',
                                               className: 'marker-cluster'
                                             });
                                           }")),
                 group = "FishNClips",
                 popupOptions=c(closeButton = TRUE,minWidth = 0,maxWidth = 700))%>%
      
      # BOSS habitat videos
      addMarkers(data=boss.habitat.highlights.popups,
                 icon = icon.boss.habitat,
                 popup = boss.habitat.highlights.popups$popup,
                 #label = boss.habitat.highlights.popups$sample,
                 clusterOptions = markerClusterOptions(iconCreateFunction =
                                                         JS("
                                          function(cluster) {
                                             return new L.DivIcon({
                                               html: '<div style=\"background-color:rgba(248, 124, 179, 0.9)\"><span>' + cluster.getChildCount() + '</div><span>',
                                               className: 'marker-cluster'
                                             });
                                           }")),
                 group = "FishNClips",
                 popupOptions=c(closeButton = TRUE,minWidth = 0,maxWidth = 700))%>%
      
      # stereo-BRUV fish videos
      addMarkers(data=fish.highlights.popups,
                 icon = icon.fish,
                 popup = fish.highlights.popups$popup,
                 clusterOptions = markerClusterOptions(iconCreateFunction =
                                                         JS("
                                          function(cluster) {
                                             return new L.DivIcon({
                                               html: '<div style=\"background-color:rgba(241, 248, 124,0.9)\"><span>' + cluster.getChildCount() + '</div><span>',
                                               className: 'marker-cluster'
                                             });
                                           }")),
                 group = "FishNClips",
                 popupOptions=c(closeButton = TRUE,minWidth = 0,maxWidth = 700))%>%
      
      # 3D models
      addMarkers(data=threed.model.popups,
                 icon = icon.models,
                 popup = threed.model.popups$popup,
                 clusterOptions = markerClusterOptions(iconCreateFunction =
                                                         JS("
                                          function(cluster) {
                                             return new L.DivIcon({
                                               html: '<div style=\"background-color:rgba(131, 124, 248,0.9)\"><span>' + cluster.getChildCount() + '</div><span>',
                                               className: 'marker-cluster'
                                             });
                                           }")),
                 group = "FishNClips",
                 popupOptions=c(closeButton = TRUE, minWidth = 0,maxWidth = 700)
      )%>%
      
      addControl(html = html_legend, position = "bottomleft", className = "fishnclips-legend-aus") %>%
      
      addLayersControl(
        # baseGroups = c("OSM (default)", "World Imagery (satellite)"),
        overlayGroups = c("Australian Marine Parks",
                          "State Marine Parks",
                          "Sampling locations",
                          "FishNClips"),
        options = layersControlOptions(collapsed = FALSE),
        position = "bottomright"
      )  %>% # Ensure "Predicted" is hidden initially
      hideGroup("State Marine Parks") %>%
      hideGroup("Australian Marine Parks")%>%
      hideGroup("FishNClips")
    
    
    # Add tiles only if raster_predicted_data() has valid data ----
    if (!is.null(raster_predicted_data()) && nrow(raster_predicted_data()) > 0) {
      
      # message(paste0("raster available:", unique(raster_predicted_data()$tile_service_url)))
      # Blue = low, yellow = high
      
      map <- map %>%
        addTiles(
          urlTemplate = paste(unique(raster_predicted_data()$tile_service_url)),
          attribution = "© GlobalArchive",
          group = "Predicted"
        ) %>%
        addLegend(
          position = "bottomright",
          pal = colorNumeric(palette = viridisLite::turbo(256, direction = -1),  #(reverse here)
                             domain = c(raster_predicted_data()$min, raster_predicted_data()$max)
          ),
          values = seq(
            from = raster_predicted_data()$min,
            to = raster_predicted_data()$max,
            length.out = 5
          ),  # Define 5 fixed values for the legend
          title = "Predicted",
          labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE)),
          opacity = 1,
          group = "Predicted"
        )
    }
    
    # Add tiles only if raster_error_data() has valid data ----
    if (!is.null(raster_error_data()) && nrow(raster_error_data()) > 0) {
      map <- map %>%
        addTiles(
          urlTemplate = paste(unique(raster_error_data()$tile_service_url)),
          attribution = "© GlobalArchive",
          group = "Error"
        ) %>%
        hideGroup("Error")  # Ensure "Error" is hidden initially
    }
    
    # Add custom radio buttons with title as a control ----
    map %>%
      htmlwidgets::onRender(
        glue::glue(
          "function(el, x) {{
           var map = this;
           var customControl = L.control({{position: 'topright'}});  // Position of the control

           customControl.onAdd = function(map) {{
             var div = L.DomUtil.create('div', 'leaflet-bar');
             div.innerHTML = `
               <div style='text-align: center; margin-bottom: 8px; font-weight: bold;'>
                 {metric_title}
               </div>
               <form>
                 <label><input type='radio' name='layer' value='Predicted' checked> Predicted</label><br>
                 <label><input type='radio' name='layer' value='Error'> Error</label>
               </form>`;
             div.style.backgroundColor = 'white';
             div.style.padding = '10px';
             div.style.border = '2px solid gray';
             return div;
           }};

           customControl.addTo(map);

           // Listen for changes in the radio buttons
           var radioButtons = document.querySelectorAll('input[name=\"layer\"]');
           radioButtons.forEach(function(rb) {{
             rb.addEventListener('change', function(e) {{
               Shiny.setInputValue('layer_toggle', e.target.value, {{priority: 'event'}});  // Send selected value to Shiny
             }});
           }});
         }}"
        )
      )
  })
  
  observe({
    input$australia_map_groups
    shinyjs::runjs(sprintf("
      var isVisible = %s.includes('FishNClips');
      var legend = document.querySelector('.fishnclips-legend-aus');
      if (legend) {
        legend.style.display = isVisible ? 'block' : 'none';
      }
    ", jsonlite::toJSON(input$australia_map_groups)))
  })
  
  observe({
    input$map_groups
    shinyjs::runjs(sprintf("
      var isVisible = %s.includes('FishNClips');
      var legend = document.querySelector('.fishnclips-legend-map');
      if (legend) {
        legend.style.display = isVisible ? 'block' : 'none';
      }
    ", jsonlite::toJSON(input$map_groups)))
  })
  
  
  # Observe the radio button input and update the map ----
  observe({
    req(input$layer_toggle)  # Ensure toggle input is available
    
    map_proxy <- leafletProxy("australia_map")
    
    # Show/hide layers based on the selected radio button
    if (input$layer_toggle == "Predicted") {
      map_proxy %>%
        showGroup("Predicted") %>%
        hideGroup("Error")%>%
        clearControls() %>%  # Clear all existing controls
        addLegend(
          position = "bottomright",
          pal = colorNumeric(palette = viridisLite::turbo(256, direction = -1),
                             domain = c(raster_predicted_data()$min, raster_predicted_data()$max)),
          values = seq(
            from = raster_predicted_data()$min,
            to = raster_predicted_data()$max,
            length.out = 5
          ),  # Define 5 fixed values for the legend
          title = "Predicted",
          labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE)),
          opacity = 1
        )
    } else if (input$layer_toggle == "Error") {
      map_proxy %>%
        showGroup("Error") %>%
        hideGroup("Predicted")%>%
        clearControls() %>%  # Clear all existing controls
        addLegend(
          position = "bottomright",
          pal = colorNumeric(palette = viridisLite::plasma(256, direction = -1),
                             domain = c(raster_error_data()$min, raster_error_data()$max)),
          # values = c(, ),
          values = seq(
            from = raster_error_data()$min,
            to = raster_error_data()$max,
            length.out = 5
          ),  # Define 5 fixed values for the legend
          title = "Error",
          labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE)),
          opacity = 1
        )
    }
  })
  
  
  
  output$fishnclips <- renderLeaflet({
    
    # map.dat <- map.dat() # call in filtered data
    map.dat <- dat
    
    points <- metadata_filtered_data()
    
    boss.habitat.highlights.popups <- filter(map.dat, source %in% c("boss.habitat.highlights"))
    bruv.habitat.highlights.popups <- filter(map.dat, source %in% c("bruv.habitat.highlights"))
    fish.highlights.popups <- filter(map.dat, source %in% c("fish.highlights"))
    threed.model.popups <- filter(map.dat, source %in% c("3d.model"))
    image.popups <- filter(map.dat, source %in% c('image'))
    
    # Having this in the global.R script breaks now - make icons on server side
    icon.bruv.habitat <- iconList(blue = makeIcon("images/marker_green.png", iconWidth = 40, iconHeight =40))
    icon.boss.habitat <- iconList(blue = makeIcon("images/marker_pink.png", iconWidth = 40, iconHeight =40))
    icon.fish <- iconList(blue = makeIcon("images/marker_yellow.png", iconWidth = 40, iconHeight =40))
    icon.models <- iconList(blue = makeIcon("images/marker_purple.png", iconWidth = 40, iconHeight =40))
    
    # lng1 <- min(map.dat$longitude)
    # lat1 <- min(map.dat$latitude)
    # lng2 <- max(map.dat$longitude)
    # lat2 <- max(map.dat$latitude)
    
    leaflet <- leaflet() %>%
      addProviderTiles('Esri.WorldImagery', group = "World Imagery") %>%
      addTiles(group = "Open Street Map")%>%
      addControl(html = html_legend, position = "bottomleft") %>%
      # flyToBounds(lng1, lat1, lng2, lat2)%>%
      fitBounds(
        lng1 = min(points$longitude_dd), lat1 = min(points$latitude_dd),
        lng2 = max(points$longitude_dd), lat2 = max(points$latitude_dd)
      ) %>%
      
      # stereo-BRUV habitat videos
      addMarkers(data=bruv.habitat.highlights.popups,
                 icon = icon.bruv.habitat,
                 popup = bruv.habitat.highlights.popups$popup,
                 #label = bruv.habitat.highlights.popups$sample,
                 clusterOptions = markerClusterOptions(iconCreateFunction =
                                                         JS("
                                          function(cluster) {
                                             return new L.DivIcon({
                                               html: '<div style=\"background-color:rgba(124, 248, 193, 0.9)\"><span>' + cluster.getChildCount() + '</div><span>',
                                               className: 'marker-cluster'
                                             });
                                           }")),
                 group="BRUV Habitat imagery",
                 popupOptions=c(closeButton = TRUE,minWidth = 0,maxWidth = 700))%>%
      
      # BOSS habitat videos
      addMarkers(data=boss.habitat.highlights.popups,
                 icon = icon.boss.habitat,
                 popup = boss.habitat.highlights.popups$popup,
                 #label = boss.habitat.highlights.popups$sample,
                 clusterOptions = markerClusterOptions(iconCreateFunction =
                                                         JS("
                                          function(cluster) {
                                             return new L.DivIcon({
                                               html: '<div style=\"background-color:rgba(248, 124, 179, 0.9)\"><span>' + cluster.getChildCount() + '</div><span>',
                                               className: 'marker-cluster'
                                             });
                                           }")),
                 group="BOSS Habitat imagery",
                 popupOptions=c(closeButton = TRUE,minWidth = 0,maxWidth = 700))%>%
      
      # stereo-BRUV fish videos
      addMarkers(data=fish.highlights.popups,
                 icon = icon.fish,
                 popup = fish.highlights.popups$popup,
                 clusterOptions = markerClusterOptions(iconCreateFunction =
                                                         JS("
                                          function(cluster) {
                                             return new L.DivIcon({
                                               html: '<div style=\"background-color:rgba(241, 248, 124,0.9)\"><span>' + cluster.getChildCount() + '</div><span>',
                                               className: 'marker-cluster'
                                             });
                                           }")),
                 group="Fish highlights",
                 popupOptions=c(closeButton = TRUE,minWidth = 0,maxWidth = 700))%>%
      
      # 3D models
      addMarkers(data=threed.model.popups,
                 icon = icon.models,
                 popup = threed.model.popups$popup,
                 clusterOptions = markerClusterOptions(iconCreateFunction =
                                                         JS("
                                          function(cluster) {
                                             return new L.DivIcon({
                                               html: '<div style=\"background-color:rgba(131, 124, 248,0.9)\"><span>' + cluster.getChildCount() + '</div><span>',
                                               className: 'marker-cluster'
                                             });
                                           }")),
                 group="3D models",
                 popupOptions=c(closeButton = TRUE, minWidth = 0,maxWidth = 700)
      )%>%
      
      
      # Ngari Capes Marine Parks
      addPolygons(data = ngari.mp, weight = 1, color = "black",
                  fillOpacity = 0.8, fillColor = "#7bbc63",
                  group = "State Marine Parks", label=ngari.mp$Name)%>%
      
      # State Marine Parks
      addPolygons(data = state.mp, weight = 1, color = "black",
                  fillOpacity = 0.8, fillColor = ~state.pal(zone),
                  group = "State Marine Parks", label=state.mp$COMMENTS)%>%
      
      # Add a legend
      addLegend(pal = state.pal, values = state.mp$zone, opacity = 1,
                title="State Zones",
                position = "bottomright", group = "State Marine Parks")%>%
      
      # Commonwealth Marine Parks
      addPolygons(data = commonwealth.mp, weight = 1, color = "black",
                  fillOpacity = 0.8, fillColor = ~commonwealth.pal(zone),
                  group = "Australian Marine Parks", label=commonwealth.mp$ZoneName)%>%
      
      # Add a legend
      addLegend(pal = commonwealth.pal, values = commonwealth.mp$zone, opacity = 1,
                title="Australian Marine Park Zones",
                position = "bottomright", group = "Australian Marine Parks")%>%
      
      addLayersControl(
        baseGroups = c("World Imagery","Open Street Map"),
        overlayGroups = c("Fish highlights",
                          "BRUV Habitat imagery","BOSS Habitat imagery",
                          "3D models",
                          "State Marine Parks",
                          "Australian Marine Parks"), options = layersControlOptions(collapsed = FALSE)) #%>%
    
    # hideGroup("Australian Marine Parks") %>%
    # hideGroup("State Marine Parks")
    
    return(leaflet)
    
  })
  
  
  # Fish images ----
  # Network image
  output$ui_network <- renderUI({
    
    req(input$network)
    network <- stringr::str_replace_all(tolower(input$network), c(" " = ".", "-" = "."))
    
    
    img(src = paste0("networks/", network, ".jpg"), align = "left", width = "100%", style = "margin-bottom: 10px;")
  })
  
  # Park image
  output$ui_marine_park <- renderUI({
    
    req(input$marine_park)
    
    park <- stringr::str_replace_all(tolower(input$marine_park), c(" marine park" = "", " " = ".", "-" = ".")) %>%
      glimpse
    
    img(src = paste0("parks/", park, ".jpg"), align = "left", width = "100%", style = "margin-bottom: 10px;")
  })
  
  # Valuebox text
  
  summary_data <- reactive({
    req(input$toggle, input$network)
    
    stats <- all_data$stats
    
    if (input$toggle == "Marine Park") {
      req(input$marine_park)  # Ensure marine_park input is selected
      stats %>%
        dplyr::filter(network %in% input$network) %>%
        dplyr::filter(marine_park %in% input$marine_park)
    } else {
      stats %>%
        dplyr::filter(network %in% input$network) %>%
        dplyr::filter(marine_park %in% paste(input$network, "Network"))
    }
  })
  
  output$fish_counted <- renderText({
    
    data <- summary_data() %>% filter(metric %in% "fish_counted")
    unique(data$value)
    
  })
  
  output$fish_species <- renderText({
    data <- summary_data() %>% filter(metric %in% "fish_species")
    unique(data$value)
  })
  
  # output$hours_watched <- renderText({
  #   data <- summary_data() %>% filter(metric %in% "hours_watched")
  #   unique(data$value)
  # })
  
  output$bruvs_deployed <- renderText({
    data <- summary_data() %>% filter(metric %in% "bruvs_deployed")
    unique(data$value)
  })
  
  # TODO link this with GA when Nik has created links
  output$ui_open_ga_button <- renderUI({
    shiny::a(
      h4(#icon("th"),
        icon("globe"), # Changed icon to "globe"
        paste0("View synthesis dataset on GlobalArchive"),
        class = "custom-button btn btn-default action-button",
        style = "font-weight:600"),
      target = "_blank",
      href = paste0("https://dev.globalarchive.org/ui/main/syntheses/"),
                    style = "width: 100%; display: block; text-align: center; background-color: #f8f9fa; padding: 10px; border-radius: 5px;"
                    # ,input$slider # could put synthesis ID here
      
    )
  })
  
  output$ui_method_button <- renderUI({
    
    data <- all_data$method_data
    
    if (input$toggle == "Marine Park") {
      
      req(input$marine_park)  # Ensure marine_park input is selected
      
      data <- data %>%
        dplyr::filter(network %in% input$network) %>%
        dplyr::filter(marine_park_or_area %in% input$marine_park) %>%
        dplyr::filter(ecosystem_condition %in% input$ecosystemsubcomponent) %>%
        glimpse
      
    } else {
      
      data <- data %>%
        dplyr::filter(network %in% input$network) %>%
        dplyr::filter(ecosystem_condition %in% input$ecosystemsubcomponent) %>%
        dplyr::filter(marine_park_or_area %in% paste(input$network, "Network")) %>%
        glimpse
      
    }
    
    # If data is empty or method is NA, return NULL
    if (nrow(data) == 0 || is.na(data$method)) {
      return(NULL)
    }
    
    # Extract methods
    methods <- unique(unlist(strsplit(data$method, ", "))) #%>% glimpse()
    
    # Dynamically create buttons for each method
    buttons <- list()
    
    if ("stereo-BRUV" %in% methods) {
      
      bruv_button <- shiny::a(
        h2(img(src = "stereo-BRUV_filled_transparent_colour.png",
               height = "80px"#,
               #style = "margin-left: 15px;" # Adjust the value as needed)
        ),
        "stereo-BRUVs",
        class = "custom-button btn btn-default action-button", # use primary for blue
        style = "font-weight:600; width: 280px; text-align: center;"),
        href = paste0("https://benthic-bruvs-field-manual.github.io/")
      )
    } else{
      
      bruv_button <- ""
    }
    
    if ("stereo-BOSS" %in% methods) {
      boss_button <- shiny::a(
        h2(img(src = "frame_transparent.png",
               height = "80px"#,
               #style = "margin-left: 15px;" # Adjust the value as needed)
        ),
        "stereo-BOSS",
        class = "custom-button btn btn-default action-button",
        style = "font-weight:600; width: 280px; text-align: center;"),
        target = "_blank",
        href = paste0("https://drop-camera-field-manual.github.io/")
      )
    } else {
      
      boss_button <- ""
      
    }
    
    addition <- NULL
    
    print(length(methods))
    
    if (length(methods) > 1) {
      
      message("includes both")
      addition <- h1("+")
      
    }
    
    # Wrap buttons in a div for proper alignment
    tagList(div(width = "100%", style = "display: flex; gap: 25px; justify-content: center; align-items: center;", bruv_button, addition, boss_button))
    
  })
  
  ## Top ten most common species ----
  output$top_ten_plot <- renderPlot({
    
    data <- all_data$top_species
    
    if (input$toggle == "Marine Park") {
      
      req(input$marine_park)  # Ensure marine_park input is selected
      
      data_filtered <- data %>%
        dplyr::filter(network %in% input$network) %>%
        dplyr::filter(marine_park %in% input$marine_park) %>%
        glimpse
      
    } else {
      
      data_filtered <- data %>%
        dplyr::filter(network %in% input$network) %>%
        dplyr::filter(marine_park %in% paste(input$network, "Network")) %>%
        glimpse
      
    }
    
    p <-   ggplot2::ggplot(data_filtered, ggplot2::aes(x = reorder(display_name, total_number), y = total_number)) +
      ggplot2::geom_bar(stat = "identity", position = ggplot2::position_dodge()) +
      ggplot2::coord_flip() +
      ggplot2::xlab("Species") +
      ggplot2::ylab("Overall abundance") +
      # ggplot_mpatheme() +
      ggplot2::theme(axis.text.y = ggplot2::element_text(face = "italic")) +
      ggplot2::scale_y_continuous(expand = ggplot2::expand_scale(mult = c(0, .1))) +
      ggplot_theme
    
    p
  })
  
  # Create species dropdown ----
  output$ui_species <- renderUI({
    
    data <- all_data$bubble_data
    
    if (input$toggle == "Marine Park") {
      
      req(input$marine_park)  # Ensure marine_park input is selected
      
      data <- data %>%
        dplyr::filter(network %in% input$network) %>%
        dplyr::filter(marine_park %in% input$marine_park) 
      
    } else {
      
      data <- data %>%
        dplyr::filter(network %in% input$network) %>%
        dplyr::filter(marine_park %in% paste(input$network, "Network")) 
      
    }
    
    choices <- data %>%
      dplyr::group_by(display_name) %>%
      dplyr::summarise(total_number = sum(count)) %>%
      dplyr::arrange(desc(total_number)) %>%
      dplyr::distinct(display_name) %>%
      dplyr::pull("display_name")
    
    shinyWidgets::pickerInput(
      inputId = "species",
      label = "Choose a species:",
      width = "100%",
      choices = choices,
      multiple = FALSE,
      selected = choices[1],
      options = list(`actions-box` = TRUE, `live-search` = TRUE, `dropup-auto` = FALSE)
    )
  })
  
  # Create species bubble plot ----
  output$species_map <- renderLeaflet({
    req(input$toggle, input$network, input$options)
    
    points <- metadata_filtered_data()
    
    data <- all_data$bubble_data %>%
      dplyr::filter(display_name %in% input$species) 
    
    metadata <- all_data$synthesis_metadata
    
    if (input$toggle == "Marine Park") {
      
      req(input$marine_park)  # Ensure marine_park input is selected
      
      data <- data %>%
        dplyr::filter(network %in% input$network) %>%
        dplyr::filter(marine_park %in% input$marine_park) 
      
      metadata <- metadata %>%
        dplyr::filter(network %in% input$network) %>%
        dplyr::filter(marine_park %in% input$marine_park) 
      
    } else {
      
      data <- data %>%
        dplyr::filter(network %in% input$network) %>%
        dplyr::filter(marine_park %in% paste(input$network, "Network")) 
      
      metadata <- metadata %>%
        dplyr::filter(network %in% input$network) %>%
        dplyr::filter(marine_park %in% paste(input$network, "Network")) 
      
    }
    
    message("combined data")
    data <- full_join(data, metadata) %>%
      replace_na(list(count = 0)) #%>%
      #glimpse()
    
    overzero <- filter(data, count > 0)
    
    equalzero <- filter(data, count == 0) 
    max_ab <- max(data$count)
    
    
    if (nrow(points) == 0) {
      points <- tibble(
        latitude_dd = c(-25.0, -25.1),
        longitude_dd = c(133.0, 133.1)
      )
    }
    
    map.dat <- dat
    
    # Initial Leaflet map ----
    map <- leaflet(points) %>%
      addTiles() %>%
      # addProviderTiles(providers$Esri.WorldImagery, group = "World Imagery (satellite)") %>%
      
      fitBounds(
        lng1 = min(points$longitude_dd), lat1 = min(points$latitude_dd),
        lng2 = max(points$longitude_dd), lat2 = max(points$latitude_dd)
      ) %>%
      
      # Ngari Capes Marine Parks
      addPolygons(data = ngari.mp, weight = 1, color = "black",
                  fillOpacity = 0.8, fillColor = "#7bbc63",
                  group = "State Marine Parks", label=ngari.mp$Name) %>%
      
      # State Marine Parks
      addPolygons(data = state.mp, weight = 1, color = "black",
                  fillOpacity = 0.8, fillColor = ~state.pal(zone),
                  group = "State Marine Parks", label=state.mp$COMMENTS) %>%
      
      # Add a legend
      addLegend(pal = state.pal, values = state.mp$zone, opacity = 1,
                title="State Zones",
                position = "bottomright", group = "State Marine Parks") %>%
      
      # Commonwealth Marine Parks
      addPolygons(data = commonwealth.mp, weight = 1, color = "black",
                  fillOpacity = 0.8, fillColor = ~commonwealth.pal(zone),
                  group = "Australian Marine Parks", label=commonwealth.mp$ZoneName) %>%
      
      # Add a legend
      addLegend(pal = commonwealth.pal, values = commonwealth.mp$zone, opacity = 1,
                title="Australian Marine Park Zones",
                position = "bottomright", group = "Australian Marine Parks") %>%
      
      add_legend(colors = c("white", "green", "green"),
                 labels = c(0, round(max_ab / 2), max_ab),
                 sizes = c(5, 20, 40),
                 title = "Abundance", group = "abundance"
      ) %>%
    # add_legend_ta(
    #         colors = c("black", "green", "green"),
    #         labels = c(0, round(max.ta / 2), max.ta),
    #         sizes = c(5, 20, 40), group = "Total abundance"
    #       ) %>%
    
    addLayersControl(
      overlayGroups = c("Australian Marine Parks",
                        "State Marine Parks"),
      options = layersControlOptions(collapsed = FALSE),
      position = "bottomright"
    )  %>% 
      hideGroup("State Marine Parks") %>%
      hideGroup("Australian Marine Parks")%>%
      hideGroup("FishNClips")
    
    if (nrow(overzero)) {
      map <- map %>%
        addCircleMarkers(
          data = overzero, lat = ~latitude_dd, lng = ~longitude_dd,
          radius = ~ (((count / max(count)) * 20)), fillOpacity = 0.5, stroke = FALSE,
          label = ~ as.character(count), color = "green"
        )
    }
    
    if (nrow(equalzero)) {
      map <- map %>%
        addCircleMarkers(
          data = equalzero, lat = ~latitude_dd, lng = ~longitude_dd,
          radius = 2, fillOpacity = 0.5, color = "white", stroke = FALSE,
          label = ~ as.character(count)
        )
    }
    
    map
    
  })
  
  # Create assemblage bubble plot ----
  output$assemblage_map <- renderLeaflet({
    req(input$toggle, input$network, input$options)
    
    points <- metadata_filtered_data()
    
    #message("view chosen assemblage metric")
    
    assemblage_metric <- tolower(str_replace_all(input$assemblage, " ", "_")) #%>%
      #glimpse()
    
    data <- all_data$metric_bubble_data %>%
      dplyr::filter(metric %in% assemblage_metric)
    
    if (input$toggle == "Marine Park") {
      
      req(input$marine_park)  # Ensure marine_park input is selected
      
      data <- data %>%
        dplyr::filter(network %in% input$network) %>%
        dplyr::filter(marine_park %in% input$marine_park) 
      
    } else {
      
      data <- data %>%
        dplyr::filter(network %in% input$network) %>%
        dplyr::filter(marine_park %in% paste(input$network, "Network")) 
      
    }
    
    overzero <- filter(data, value > 0)
    
    equalzero <- filter(data, value == 0)
    max_ab <- max(data$value)
    
    
    # Initial Leaflet map ----
    map <- leaflet(points) %>%
      addTiles() %>%
      # addProviderTiles(providers$Esri.WorldImagery, group = "World Imagery (satellite)") %>%
      
      fitBounds(
        lng1 = min(points$longitude_dd), lat1 = min(points$latitude_dd),
        lng2 = max(points$longitude_dd), lat2 = max(points$latitude_dd)
      ) %>%
      
      # Ngari Capes Marine Parks
      addPolygons(data = ngari.mp, weight = 1, color = "black",
                  fillOpacity = 0.8, fillColor = "#7bbc63",
                  group = "State Marine Parks", label=ngari.mp$Name) %>%
      
      # State Marine Parks
      addPolygons(data = state.mp, weight = 1, color = "black",
                  fillOpacity = 0.8, fillColor = ~state.pal(zone),
                  group = "State Marine Parks", label=state.mp$COMMENTS) %>%
      
      # Add a legend
      addLegend(pal = state.pal, values = state.mp$zone, opacity = 1,
                title="State Zones",
                position = "bottomright", group = "State Marine Parks") %>%
      
      # Commonwealth Marine Parks
      addPolygons(data = commonwealth.mp, weight = 1, color = "black",
                  fillOpacity = 0.8, fillColor = ~commonwealth.pal(zone),
                  group = "Australian Marine Parks", label=commonwealth.mp$ZoneName) %>%
      
      # Add a legend
      addLegend(pal = commonwealth.pal, values = commonwealth.mp$zone, opacity = 1,
                title="Australian Marine Park Zones",
                position = "bottomright", group = "Australian Marine Parks") %>%
      
      add_legend(colors = c("white", "green", "green"),
                 labels = c(0, round(max_ab / 2), max_ab),
                 sizes = c(5, 20, 40),
                 title = input$assemblage, 
                 group = "abundance"
      ) %>%
      
      
      addLayersControl(
        overlayGroups = c("Australian Marine Parks",
                          "State Marine Parks"),
        options = layersControlOptions(collapsed = FALSE),
        position = "bottomright"
      )  %>% 
      hideGroup("State Marine Parks") %>%
      hideGroup("Australian Marine Parks")%>%
      hideGroup("FishNClips")
    
    if (nrow(overzero)) {
      map <- map %>%
        addCircleMarkers(
          data = overzero, lat = ~latitude_dd, lng = ~longitude_dd,
          radius = ~ (((value / max(value)) * 20)), fillOpacity = 0.5, stroke = FALSE,
          label = ~ as.character(value), color = "green"
        )
    }
    
    if (nrow(equalzero)) {
      map <- map %>%
        addCircleMarkers(
          data = equalzero, lat = ~latitude_dd, lng = ~longitude_dd,
          radius = 2, fillOpacity = 0.5, color = "white", stroke = FALSE,
          label = ~ as.character(value)
        )
    }
    
    map
    
  })
  
  
  # Species specific temporal plots ----
  output$species_temporal <- renderPlot({
    req(input$toggle, input$network, input$options)
    
    data <- all_data$temporal_data %>%
      dplyr::filter(display_name %in% input$species) %>%
      dplyr::mutate(year = substr(date_midpoint, 1, 4)) %>%
      dplyr::mutate(year = as.numeric(year))
    
    if (input$toggle == "Marine Park") {
      
      req(input$marine_park)  # Ensure marine_park input is selected
      
      data <- data %>%
        dplyr::filter(network %in% input$network) %>%
        dplyr::filter(marine_park %in% input$marine_park) 
      
    } else {
      
      data <- data %>%
        dplyr::filter(network %in% input$network) %>%
        dplyr::filter(marine_park %in% paste(input$network, "Network")) 
      
    }
  
  
  # Get unique depth classes
  depth_classes <- unique(data$depth_class)
  
  # Create a list to store plots for each depth class
  depth_plots <- list()
  
  # Loop through each depth class
  for (depth in depth_classes) {
    # Filter data for the current depth class
    depth_data <- data %>% filter(depth_class == depth)
    
    # Create the plot for the current depth class
    p <- ggplot2::ggplot(depth_data, aes(x = year, 
                                y = average_abundance, 
                                fill = zone, 
                                group = zone, 
                                shape = zone, 
                                col = zone)) +
      # geom_errorbar(aes(ymin = average_abundance - se, ymax = average_abundance + se), width = 0.02) +
      geom_point(size = 3,
                 stroke = 0.2, 
                 color = "black", 
                 alpha = 0.8, 
                 shape = 21) +
      geom_line() +
      geom_vline(xintercept = 2018, linetype = "dashed", color = "black") +
      scale_color_manual(values = c(
        "Multiple Use Zone" = "#b9e6fb",
        "Habitat Protection Zone" = "#fff8a3",
        "National Park Zone" = "#7bbc63",
        "Special Purpose Zone" = "#6BB1E5",
        "Sanctuary Zone" = "#bfd054",
        "Other Zones" = "#bddde1"
      ), name = "Australian Marine Parks") +
      scale_fill_manual(values = c(
        "Multiple Use Zone" = "#b9e6fb",
        "Habitat Protection Zone" = "#fff8a3",
        "National Park Zone" = "#7bbc63",
        "Special Purpose Zone" = "#6BB1E5",
        "Sanctuary Zone" = "#bfd054",
        "Other Zones" = "#bddde1"
      ), name = "Australian Marine Parks") +
      labs(
        x = "Year",
        y = "Abundance",
        color = "Australian Marine Parks",
        title = paste("Depth:", depth)
      ) +
      labs(x = "Year", y = str_wrap(unique(depth_data$metric), 30)) +
      theme_bw() +
      theme(#axis.title.y = element_blank(), # Remove y-axis labels for individual plots
        legend.position = "top",      # Suppress individual legends
        axis.title = element_text(size = 16), # Larger axis titles
        axis.text = element_text(size = 14), # Larger axis text
        legend.title = element_text(size = 16), # Larger legend title
        legend.text = element_text(size = 14), # Larger legend text
        plot.title = element_text(size = 18, face = "italic"), # Larger plot title
        strip.text = element_text(size = 16), # Larger facet strip text
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())
    
    depth_plots[[depth]] <- p
  }
  
  
  # Combine all depth plots into a single stacked plot
  combined_plot <- wrap_plots(depth_plots, ncol = 1)
  
  final_plot <- combined_plot #+
    #plot_layout(guides = "collect") #+ # Collect legends into one
    # theme(
    #   legend.position = "top", # Position legend at the bottom
    #   legend.title = element_text(size = 16),
    #   legend.text = element_text(size = 14)#,
    #   # axis.title.y = element_text(size = 16)
    # )
  
  final_plot
  })
  
  
  # Create first map for Dashboard ----
  output$map <- renderLeaflet({
    req(input$toggle, input$network, input$options)
    
    points <- metadata_filtered_data()
    
    if (nrow(points) == 0) {
      points <- tibble(
        latitude_dd = c(-25.0, -25.1),
        longitude_dd = c(133.0, 133.1)
      )
    }
  
    icon <- iconList(blue = makeIcon("images/marker_blue.png", iconWidth = 40, iconHeight =40))
    
    map.dat <- dat
    
    boss.habitat.highlights.popups <- filter(map.dat, source %in% c("boss.habitat.highlights"))
    bruv.habitat.highlights.popups <- filter(map.dat, source %in% c("bruv.habitat.highlights"))
    fish.highlights.popups <- filter(map.dat, source %in% c("fish.highlights"))
    threed.model.popups <- filter(map.dat, source %in% c("3d.model"))
    image.popups <- filter(map.dat, source %in% c('image'))
    
    # Having this in the global.R script breaks now - make icons on server side
    icon.bruv.habitat <- iconList(blue = makeIcon("images/marker_green.png", iconWidth = 40, iconHeight =40))
    icon.boss.habitat <- iconList(blue = makeIcon("images/marker_pink.png", iconWidth = 40, iconHeight =40))
    icon.fish <- iconList(blue = makeIcon("images/marker_yellow.png", iconWidth = 40, iconHeight =40))
    icon.models <- iconList(blue = makeIcon("images/marker_purple.png", iconWidth = 40, iconHeight =40))
    
    # Initial Leaflet map ----
    map <- leaflet(points) %>%
      addTiles() %>%
      # addProviderTiles(providers$Esri.WorldImagery, group = "World Imagery (satellite)") %>%
      #addMarkers(~longitude_dd, ~latitude_dd, group = "Sampling locations") %>%
      
      addMarkers(data = points, ~longitude_dd, ~latitude_dd,
                 icon = icon,
                 # popup = bruv.habitat.highlights.popups$popup,
                 clusterOptions = markerClusterOptions(iconCreateFunction =
                                                         JS("
                                          function(cluster) {
                                             return new L.DivIcon({
                                               html: '<div style=\"background-color:rgba(0, 123, 255, 0.9)\"><span>' + cluster.getChildCount() + '</div><span>',
                                               className: 'marker-cluster'
                                             });
                                           }")),
                 group = "Sampling locations"#,
                 #popupOptions=c(closeButton = TRUE,minWidth = 0,maxWidth = 700)
      )%>%
      
      
      fitBounds(
        lng1 = min(points$longitude_dd), lat1 = min(points$latitude_dd),
        lng2 = max(points$longitude_dd), lat2 = max(points$latitude_dd)
      ) %>%
      
      # Ngari Capes Marine Parks
      addPolygons(data = ngari.mp, weight = 1, color = "black",
                  fillOpacity = 0.8, fillColor = "#7bbc63",
                  group = "State Marine Parks", label=ngari.mp$Name) %>%
      
      # State Marine Parks
      addPolygons(data = state.mp, weight = 1, color = "black",
                  fillOpacity = 0.8, fillColor = ~state.pal(zone),
                  group = "State Marine Parks", label=state.mp$COMMENTS) %>%
      
      # Add a legend
      addLegend(pal = state.pal, values = state.mp$zone, opacity = 1,
                title="State Zones",
                position = "bottomright", group = "State Marine Parks") %>%
      
      # Commonwealth Marine Parks
      addPolygons(data = commonwealth.mp, weight = 1, color = "black",
                  fillOpacity = 0.8, fillColor = ~commonwealth.pal(zone),
                  group = "Australian Marine Parks", label=commonwealth.mp$ZoneName) %>%
      
      # Add a legend
      addLegend(pal = commonwealth.pal, values = commonwealth.mp$zone, opacity = 1,
                title="Australian Marine Park Zones",
                position = "bottomright", group = "Australian Marine Parks") %>%
      
      # stereo-BRUV habitat videos
      addMarkers(data=bruv.habitat.highlights.popups,
                 icon = icon.bruv.habitat,
                 popup = bruv.habitat.highlights.popups$popup,
                 #label = bruv.habitat.highlights.popups$sample,
                 clusterOptions = markerClusterOptions(iconCreateFunction =
                                                         JS("
                                          function(cluster) {
                                             return new L.DivIcon({
                                               html: '<div style=\"background-color:rgba(124, 248, 193, 0.9)\"><span>' + cluster.getChildCount() + '</div><span>',
                                               className: 'marker-cluster'
                                             });
                                           }")),
                 group = "FishNClips",
                 popupOptions=c(closeButton = TRUE,minWidth = 0,maxWidth = 700))%>%
      
      # BOSS habitat videos
      addMarkers(data=boss.habitat.highlights.popups,
                 icon = icon.boss.habitat,
                 popup = boss.habitat.highlights.popups$popup,
                 #label = boss.habitat.highlights.popups$sample,
                 clusterOptions = markerClusterOptions(iconCreateFunction =
                                                         JS("
                                          function(cluster) {
                                             return new L.DivIcon({
                                               html: '<div style=\"background-color:rgba(248, 124, 179, 0.9)\"><span>' + cluster.getChildCount() + '</div><span>',
                                               className: 'marker-cluster'
                                             });
                                           }")),
                 group = "FishNClips",
                 popupOptions=c(closeButton = TRUE,minWidth = 0,maxWidth = 700))%>%
      
      # stereo-BRUV fish videos
      addMarkers(data=fish.highlights.popups,
                 icon = icon.fish,
                 popup = fish.highlights.popups$popup,
                 clusterOptions = markerClusterOptions(iconCreateFunction =
                                                         JS("
                                          function(cluster) {
                                             return new L.DivIcon({
                                               html: '<div style=\"background-color:rgba(241, 248, 124,0.9)\"><span>' + cluster.getChildCount() + '</div><span>',
                                               className: 'marker-cluster'
                                             });
                                           }")),
                 group = "FishNClips",
                 popupOptions=c(closeButton = TRUE,minWidth = 0,maxWidth = 700))%>%
      
      # 3D models
      addMarkers(data=threed.model.popups,
                 icon = icon.models,
                 popup = threed.model.popups$popup,
                 clusterOptions = markerClusterOptions(iconCreateFunction =
                                                         JS("
                                          function(cluster) {
                                             return new L.DivIcon({
                                               html: '<div style=\"background-color:rgba(131, 124, 248,0.9)\"><span>' + cluster.getChildCount() + '</div><span>',
                                               className: 'marker-cluster'
                                             });
                                           }")),
                 group = "FishNClips",
                 popupOptions=c(closeButton = TRUE, minWidth = 0,maxWidth = 700)
      )%>%
      
      addControl(html = html_legend, position = "bottomleft", className = "fishnclips-legend-map") %>%
      
      addLayersControl(
        # baseGroups = c("OSM (default)", "World Imagery (satellite)"),
        overlayGroups = c("Australian Marine Parks",
                          "State Marine Parks",
                          "Sampling locations",
                          "FishNClips"),
        options = layersControlOptions(collapsed = FALSE),
        position = "bottomright"
      )  %>% # Ensure "Predicted" is hidden initially
      hideGroup("State Marine Parks") %>%
      hideGroup("Australian Marine Parks")%>%
      hideGroup("FishNClips")
  })
  
  # Create species iframe
  output$iframe <- renderUI({
    
    dat <- all_data$foa_codes[display_name %in% c(input$species)] %>%
      dplyr::distinct(url) %>%
      dplyr::pull("url")
    
    frame <- tags$iframe(src = paste0(dat),
                         style = "width: 100%; height: 100vh; border: none;",
                         onload = "resizeIframe(this)"
    )
    frame
    
  })
  
  # End of server ----
}

server <- function(input, output, session) {

  ## Show Shinyalert for draft data ----
  shinyalert(
    title = "Draft Product Disclaimer",
    text = "By proceeding, you agree that this draft product is not to be used for decision-making, communication, or publication purposes.",
    type = "warning",
    confirmButtonText = "I Agree",
    html = FALSE,
    closeOnEsc = FALSE,
    closeOnClickOutside = FALSE
  )
  
  # Create Marine Park Name Title ----
  debounced_park <- debounce(reactive(input$marine_park), 300)
  
  output$marinepark_name <- renderUI({
    req(debounced_park())
    HTML(paste0("<b>", debounced_park(), "</b>"))
  })
  
  # # Create Network Name Title (not used in server atm) ----
  ## Turned off because we are not showing network level atm
  # debounced_network <- debounce(reactive(input$network), 200)
  # 
  # output$network_name <- renderUI({
  #   req(debounced_network())
  #   HTML(paste0("<b>", debounced_network(), "</b>"))
  # })
  
  # Create Ecosystem Component name (e.g. Demersal Fish, or Mobile macro inverts) ----
  debounced_ecosystem_subcomponent <- debounce(reactive(input$ecosystemsubcomponent), 300)
  
  output$ecosystem_subcomponent_name <- renderUI({
    req(debounced_ecosystem_subcomponent())
    HTML(paste0("<i>", debounced_ecosystem_subcomponent(), "</i>"))
  })

  # # Indicator metric name (not used in server) ----
  # output$metric_name <- renderUI({
  #   req(input$options)
  #   h5(HTML(paste0("<i>", input$options)))
  # })
  
  
  
  
  
  
  
  
 
  # Dynamic Marine Park/Sentinel Area Options ----
  # 1. Debounce network
  debounced_network <- debounce(reactive(input$network), 200)
  
  # 2. Update radiobuttons with observeEvent using filtered dataframe
  observeEvent(debounced_network(), {

    parks <- all_data$synthesis_metadata %>%
      dplyr::filter(network == debounced_network()) %>%
      dplyr::distinct(marine_park) %>%
      dplyr::filter(!marine_park %in% c("South-west Network", "North-west Network")) %>%
      dplyr::pull(marine_park) %>%
      sort()
    
    if (length(parks) > 0) {
      updateRadioButtons(
        session,
        inputId = "marine_park",
        choices = parks,
        selected = parks[1]
      )
    } else {
      # Optional fallback — you can hide, disable, or warn instead
      updateRadioButtons(
        session,
        inputId = "marine_park",
        choices = c("No available parks"),
        selected = "No available parks"
      )
    }
  })
  
  # Dynamic drop down for Ecosystem Component ----
  # 1.  Filter the ecosystem components to the ones that are possible in that marine_park
  filtered_components <- reactive({
    req(input$metric, input$marine_park)
    
    all_data$synthesis_metadata %>%
      distinct(network, marine_park, ecosystem_component) %>%
      left_join(all_data$dropdown_data) %>%
      filter(metric %in% input$metric, marine_park %in% input$marine_park) %>%
      pull(ecosystem_component) %>%
      unique()
  })
  
  # 2. Debounce components if the reactive input changes too quickly
  debounced_components <- debounce(filtered_components, millis = 200)
  
  # 3. Create UI output using debounced components
  output$dynamic_ecosystem_subcomponent <- renderUI({
    components <- filtered_components()
    req(length(components) > 0)
    
    shinyWidgets::pickerInput(
      inputId = "ecosystemsubcomponent",
      label = "Ecosystem sub-component:",
      width = "100%",
      choices = components,
      multiple = FALSE,
      selected = components[1],
      options = list(`actions-box` = TRUE, `live-search` = FALSE, `dropup-auto` = FALSE)
    )
  })
  
  # Dynamic dropdown for Indicator Metric  ----
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
  

  

  # # Reset marine park input when switching toggle
  # observeEvent(input$toggle, {
  #   if (input$toggle == "Network") {
  #     updateSelectInput(session, "marine_park", selected = NULL)
  #   }
  # })
  
  # # Reactive to filter parks based on selected network
  # available_parks <- reactive({
  #   req(input$network)
  #   
  #   all_data$synthesis_metadata %>%
  #     dplyr::filter(network == input$network) %>%
  #     dplyr::distinct(marine_park) %>%
  #     dplyr::filter(!marine_park %in% c("South-west Network", "North-west Network")) %>%
  #     dplyr::pull(marine_park)
  # })
  # 
  # # Dynamic UI for Marine Park/Sentinel Area
  # output$dynamic_marine_park <- renderUI({
  #   parks <- available_parks()
  #   
  #   # Default to "None" if no parks available
  #   radioButtons("marine_park",
  #                "Marine Park/Sentinel Area:",
  #                choices = if (length(parks) > 0) parks else "None",
  #                selected = if (length(parks) > 0) parks[1] else "None"
  #   )
  # })
  # 
  # # Observe network selection and update radio buttons
  # observeEvent(input$network, {
  #   parks <- available_parks()
  #   
  #   updateRadioButtons(session, "marine_park",
  #                      choices = if (length(parks) > 0) parks else "None",
  #                      selected = if (length(parks) > 0) parks[1] else "None")
  # })
  

  
  # Filter the Condition dataset ----
  condition_filtered_data <- reactive({
    req(#input$toggle, 
        input$network)
    
    plot_list <- all_data$file_info
    
    # if (input$toggle == "Marine Park") {
      req(input$marine_park)  # Ensure marine_park input is selected
      
      message("view conditional data marine park")
      
      plot_list %>%
        dplyr::filter(network %in% input$network) %>%
        dplyr::filter(marine_park %in% debounced_park()) %>%
        dplyr::filter(metric %in% input$ecosystemsubcomponent) %>% glimpse
    # } else {
    #   
    #   # message("view conditional data network")
    #   
    #   plot_list %>%
    #     dplyr::filter(network %in% input$network) %>%
    #     dplyr::filter(marine_park %in% paste(input$network, "Network")) %>%
    #     dplyr::filter(metric %in% input$ecosystemsubcomponent) #%>% glimpse
    # }
  })
  
  # # Condition plot UI ----
  output$condition_plot_ui <- renderUI({
    req(condition_filtered_data())
    
    message("view chosen codntion plot")
    
    chosen_plot <- condition_filtered_data() %>% glimpse
    # validate(need(nrow(chosen_plot) > 0, "No condition data available for the selected filters."))
    plotOutput("condition_plot", height = condition_plot_height())
  })
  
  # Output - condition plot ----
  output$condition_plot <- renderPlot({
    
    req(condition_filtered_data())
    chosen_plot <- condition_filtered_data()
    
    file_path <- here::here(unique(chosen_plot$file))
    if (!file.exists(file_path)) {
      stop("File does not exist: ", file_path)
    }
    
    chosen_file <- readRDS(file_path)
    plot(chosen_file)
    
  })
  
  # Reactive height for the condition plot ----
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
    
    return(height)
  })
  
  # output$condition_plot_ui <- renderUI({
  #
  #   req(input$toggle, input$network)
  #   plotOutput("condition_plot", height = paste0(condition_plot_height(), "px"))
  #
  # })
  
  # Dynamic trend text for Observations tab ----
  output$dynamic_text <- renderUI({
    req(#input$toggle, 
        input$network)
    
    # if (input$toggle == "Marine Park") {
      req(debounced_park)  # Ensure marine_park input is selected
      text <- all_data$text_data %>%
        dplyr::filter(network %in% input$network) %>%
        dplyr::filter(marine_park %in% debounced_park()) %>%
        dplyr::filter(ecosystem_condition %in% input$ecosystemsubcomponent)
    # } else {
    #   text <- all_data$text_data %>%
    #     dplyr::filter(network %in% input$network) %>%
    #     dplyr::filter(marine_park %in% paste(input$network, "Network")) %>%
    #     dplyr::filter(ecosystem_condition %in% input$ecosystemsubcomponent)
    # }
    
    h6(unique(text$text))
  })
  
  # Dynamic trend text for Status and trends tab ----
  output$dynamic_text1 <- renderUI({
    req(#input$toggle, 
        input$network)
    
    # if (input$toggle == "Marine Park") {
      req(input$marine_park)  # Ensure marine_park input is selected
      text <- all_data$text_data %>%
        dplyr::filter(network %in% input$network) %>%
        dplyr::filter(marine_park %in% debounced_park()) %>%
        dplyr::filter(ecosystem_condition %in% input$ecosystemsubcomponent)
    # } else {
    #   text <- all_data$text_data %>%
    #     dplyr::filter(network %in% input$network) %>%
    #     dplyr::filter(marine_park %in% paste(input$network, "Network")) %>%
    #     dplyr::filter(ecosystem_condition %in% input$ecosystemsubcomponent)
    # }
    
    h6(unique(text$text))
  })
  
  # Temporal plot filtered data ----
  temporal_filtered_data <- reactive({
    req(#input$toggle, 
        input$network)
    
    plot_list <- all_data$temporal_file_info
    
    # if(input$toggle %in% "Marine Park"){
      req(input$marine_park) # Ensure marine_park input is available
      chosen_plot <- plot_list %>%
        dplyr::filter(network %in% input$network) %>%
        dplyr::filter(marine_park %in% debounced_park()) %>%
        dplyr::filter(metric %in% input$options)
      
    # } else {
    #   
    #   chosen_plot <- plot_list %>%
    #     dplyr::filter(network %in% input$network) %>%
    #     dplyr::filter(marine_park %in% paste(input$network, "Network")) %>%
    #     dplyr::filter(metric %in% input$options)
    # }
  })
  
  # Ouput for temporal plot ----
  output$temporal_plot <- renderPlot({
    
    req(temporal_filtered_data())
    
    chosen_plot <- temporal_filtered_data()
    # validate(need(nrow(chosen_plot) > 0, "No temporal data available for the selected filters."))
    
    chosen_file <- readRDS(here::here(unique(chosen_plot$file)))
    plot(chosen_file)
    
  })
  
  # Reactive height for the temporal plot ----
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
    
    req(#input$toggle, 
        input$network)
    plotOutput("temporal_plot", height = paste0(temporal_plot_height(), "px"))
    
  })
  
  # Create filtered metadata ----
  metadata_filtered_data <- reactive({
    req(#input$toggle, 
        input$network)
    
    metadata <- all_data$metadata
    
    # if (input$toggle == "Marine Park") {
      req(input$marine_park, input$ecosystemsubcomponent)  # Ensure marine_park input is selected
      
      metadata %>%
        dplyr::filter(network %in% input$network) %>%
        dplyr::filter(marine_park %in% debounced_park()) %>%
        dplyr::filter(ecosystem_component %in% input$ecosystemsubcomponent)
    # } else {
    #   metadata %>%
    #     dplyr::filter(network %in% input$network)
    # }
  })
  
  # Create filtered predicted rasters ----
  raster_predicted_data <- reactive({
    req(#input$toggle, 
        input$network, input$options)
    
    raster_list <- all_data$raster_data %>%
      dplyr::filter(estimate %in% c("Probability", "Mean"))
    
    # message("view chosen raster dataset")
    
    # if (input$toggle == "Marine Park") {
      req(input$marine_park)  # Ensure marine_park input is selected
      raster_list %>%
        dplyr::filter(network %in% input$network) %>%
        dplyr::filter(marine_park %in% debounced_park()) %>%
        dplyr::filter(metric %in% input$options) #%>% glimpse()
    # } else {
    #   raster_list %>%
    #     dplyr::filter(network %in% input$network) %>%
    #     dplyr::filter(marine_park %in% paste(input$network, "Network")) %>%
    #     dplyr::filter(metric %in% input$options) #%>% glimpse()
    # }
  })
  
  # Create filtered error rasters ----
  raster_error_data <- reactive({
    req(#input$toggle, 
        input$network, input$options)
    
    raster_list <- all_data$raster_data %>%
      dplyr::filter(estimate %in% c("Error"))
    
    # if (input$toggle == "Marine Park") {
      req(input$marine_park)  # Ensure marine_park input is selected
      raster_list %>%
        dplyr::filter(network %in% input$network) %>%
        dplyr::filter(marine_park %in% debounced_park()) %>%
        dplyr::filter(metric %in% input$options)
    # } else {
    #   raster_list %>%
    #     dplyr::filter(network %in% input$network) %>%
    #     dplyr::filter(marine_park %in% paste(input$network, "Network")) %>%
    #     dplyr::filter(metric %in% input$options)
    # }
  })
  
  output$fishnclips <- renderLeaflet({

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
                          "Australian Marine Parks"), options = layersControlOptions(collapsed = FALSE))

    return(leaflet)

  })

  # # Fish images ----
  # # Network image ----
  # output$ui_network <- renderUI({
  #   
  #   req(input$network)
  #   network <- stringr::str_replace_all(tolower(input$network), c(" " = ".", "-" = "."))
  #   
  #   img(src = paste0("networks/", network, ".jpg"),
  #       # height = 250,
  #       align = "left",
  #       width = "100%",
  #       style = "margin-bottom: 10px;")
  # })
  # 
  # Park image ----
  output$ui_marine_park <- renderUI({
    
    req(input$marine_park)
    
    park <- stringr::str_replace_all(tolower(input$marine_park), c(" marine park" = "", " " = ".", "-" = ".")) #%>%
      #glimpse
    
    div(
      style = "width: 100%; height: 200px; overflow: hidden; position: relative;",
      img(
        src = paste0("parks/", park, ".jpg"),
        style = "width: 100%; height: 100%; object-fit: cover; position: absolute; top: 0; left: 0;"
      )
    )
    
  })
  
  # Summary data for the Valuebox text ----
  summary_data <- reactive({
    req(#input$toggle, 
        input$network)
    
    message("view summary data")
    stats <- all_data$stats
    
    # if (input$toggle == "Marine Park") {
      req(input$ecosystemsubcomponent)  # Ensure marine_park input is selected
      stats %>%
        dplyr::filter(network %in% input$network) %>%
        dplyr::filter(marine_park %in% debounced_park()) %>%
        dplyr::filter(ecosystem_component %in% input$ecosystemsubcomponent) #%>%
        #glimpse()
      
    # } else {
    #   stats %>%
    #     dplyr::filter(network %in% input$network) %>%
    #     dplyr::filter(marine_park %in% paste(input$network, "Network"))
    # }
  })
  
  # Number of individuals counted valuebox ----
  output$individuals_counted <- renderText({
    
    message("view individuals counted")
    
    data <- summary_data() %>% 
      filter(metric %in% "individuals_counted") #%>%
      #glimpse()
    
    unique(data$value)
    
  })
  
  # Number of individuals counted title ----
  output$individuals_counted_title <- renderText({
    
    if(input$ecosystemsubcomponent %in% "Demersal fish"){
      title <- "Fish counted"
    } else if(input$ecosystemsubcomponent %in% "Mobile macro invertebrates"){
      title <- "Mobile macro invertebrates observed"
    }
    
    title
  })
  
  # Individuals counted icon ----
  output$dynamic_icon1 <- renderUI({
    if (input$ecosystemsubcomponent %in% "Demersal fish") {
      icon("fish", class = "fa-2x")  # Font Awesome icon
    } else if (input$ecosystemsubcomponent %in% "Mobile macro invertebrates") {
      tags$img(src = "lobster-icon.svg", height = "80px")
    }
  })
  
  # Number of species valuebox ----
  output$number_species <- renderText({
    data <- summary_data() %>% 
      filter(metric %in% "number_species")
    
    unique(data$value)
  })
  
  # Number of species title ----
  output$number_species_title <- renderText({
    
    if(input$ecosystemsubcomponent %in% "Demersal fish"){
      title <- "Number of fish species identified"
    } else if(input$ecosystemsubcomponent %in% "Mobile macro invertebrates"){
      title <- "Number of mobile macro invertebrates species identified"
    }
    
    title
  })
  
  # Number of species icon ----
  output$dynamic_icon2 <- renderUI({
    if (input$ecosystemsubcomponent %in% "Demersal fish") {
      icon("fish", class = "fa-2x")  # Font Awesome icon
    } else if (input$ecosystemsubcomponent %in% "Mobile macro invertebrates") {
      tags$img(src = "lobster-icon.svg", height = "80px")
    }
  })
  
  
  # Number of samples----
  output$samples_deployed <- renderText({
    
    data <- summary_data() %>% 
      filter(metric %in% "samples_deployed")
    
    unique(data$value)
  })
  
  # Number of samples title ----
  output$samples_deployed_title <- renderText({
    
    if(input$ecosystemsubcomponent %in% "Demersal fish"){
      title <- "stereo-BRUVs deployed"
    } else if(input$ecosystemsubcomponent %in% "Mobile macro invertebrates"){
      title <- "Lobster pots deployed"
    }
    
    title
  })
  
  # Samples icon ----
  output$dynamic_icon3 <- renderUI({
    if (input$ecosystemsubcomponent %in% "Demersal fish") {
      tags$img(src = "stereo-BRUV_filled_transparent.png",
          height = "80px",
          style = "margin-left: 15px;") # Adjust the value as needed)
    } else if (input$ecosystemsubcomponent %in% "Mobile macro invertebrates") {
      tags$img(src = "lobster-pot.png", height = "80px"#,
               #style = "margin-left: 15px;"
               )
    }
  })
  
  
  # Depth data for summary ----
  depth_data <- reactive({
    req(#input$toggle, 
        input$network)
    
    data <- all_data$synthesis_metadata
    
    # if (input$toggle == "Marine Park") {
      
      req(input$marine_park)  # Ensure marine_park input is selected
      
      data <- data %>%
        dplyr::filter(network %in% input$network) %>%
        dplyr::filter(marine_park %in% debounced_park()) %>%
        dplyr::filter(ecosystem_component %in% input$ecosystemsubcomponent)
      
    # } else {
    #   
    #   data <- data %>%
    #     dplyr::filter(network %in% input$network) %>%
    #     dplyr::filter(marine_park %in% paste(input$network, "Network"))
    #   
    # }
    
    data <- data %>%
      dplyr::group_by(network, marine_park) %>%
      dplyr::summarise(average_depth = mean(depth_m), min_depth = min(depth_m), max_depth = max(depth_m))#%>%
      #glimpse()
    
    data
  })
  
  # Avergae depth surveyed ----
  output$average_depth <- renderText({
    depth <- paste(round(unique(depth_data()$average_depth)), "m")
    return(depth)
  })
  
  # Average depth range surveyed
  output$depth_range <- renderText({
    depth <- paste(round(unique(depth_data()$min_depth)), "-", round(unique(depth_data()$max_depth)), "m")
    return(depth)
  })
  
  # Link to Global Archive Synthesis ----
  # TODO link this with GA when Nik has created links
  output$ui_open_ga_button <- renderUI({
    
    data <- all_data$synthesis_metadata
    
    # if (input$toggle == "Marine Park") {
      req(input$marine_park)  # Ensure marine_park input is selected
      
      # message("view conditional data marine park")
      
      data <- data %>%
        dplyr::filter(network %in% input$network) %>%
        dplyr::filter(marine_park %in% debounced_park()) %>%
        dplyr::filter(ecosystem_component %in% input$ecosystemsubcomponent)
    # } else {
    #   
    #   # message("view conditional data network")
    #   
    #   data <- data %>%
    #     dplyr::filter(network %in% input$network) %>%
    #     dplyr::filter(marine_park %in% paste(input$network, "Network"))
    # }
    
    synthesis_id <- unique(data$synthesis_id)
    
    
    shiny::a(
      h4(#icon("th"),
        icon("globe"), # Changed icon to "globe"
        paste0("View synthesis dataset on GlobalArchive"),
        class = "custom-button btn btn-default action-button",
        style = "font-weight:600"),
      target = "_blank",
      href = paste0("https://dev.globalarchive.org/ui/main/syntheses/", synthesis_id),
      style = "width: 100%; display: block; text-align: center; background-color: #f8f9fa; padding: 10px; border-radius: 5px;"
      # ,input$slider # could put synthesis ID here
      
    )
  })
  
  # Method Buttons ----
  output$ui_method_button <- renderUI({
    
    data <- all_data$method_data
    
    # if (input$toggle == "Marine Park") {
      
      req(input$marine_park)  # Ensure marine_park input is selected
      
      message("method data")
      
      data <- data %>%
        dplyr::filter(network %in% input$network) %>%
        dplyr::filter(marine_park_or_area %in% debounced_park()) %>%
        dplyr::filter(ecosystem_condition %in% input$ecosystemsubcomponent) #%>%
        #glimpse()
    #   
    # } else {
    #   
    #   data <- data %>%
    #     dplyr::filter(network %in% input$network) %>%
    #     dplyr::filter(ecosystem_condition %in% input$ecosystemsubcomponent) %>%
    #     dplyr::filter(marine_park_or_area %in% paste(input$network, "Network")) %>%
    #     glimpse
    #   
    # }
    # 
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
    
    if ("Lobster-pot" %in% methods) {
      lobster_button <- shiny::a(
        h2(img(src = "lobster-pot-colour.png",
               height = "80px"#,
               #style = "margin-left: 15px;" # Adjust the value as needed)
        ),
        "Lobster pots",
        class = "custom-button btn btn-default action-button",
        style = "font-weight:600; width: 280px; text-align: center;"),
        target = "_blank",
        href = paste0("https://drop-camera-field-manual.github.io/")
      )
    } else {
      
      lobster_button <- ""
      
    }
    
    addition <- NULL
    
    print(length(methods))
    
    if (length(methods) > 1) {
      
      message("includes both")
      addition <- h1("+")
      
    }
    
    # Wrap buttons in a div for proper alignment
    tagList(div(width = "100%", style = "display: flex; gap: 25px; justify-content: center; align-items: center;", lobster_button, bruv_button, addition, boss_button))
    
  })
  
  ## Top ten most common species ----
  output$top_ten_plot <- renderPlot({
    
    data <- all_data$top_species
    
    # if (input$toggle == "Marine Park") {
      
      req(input$marine_park, input$ecosystemsubcomponent)  # Ensure marine_park input is selected
      
      data_filtered <- data %>%
        dplyr::filter(network %in% input$network) %>%
        dplyr::filter(marine_park %in% debounced_park()) %>%
        dplyr::filter(ecosystem_component %in% input$ecosystemsubcomponent)# %>%
        #glimpse
    #   
    # } else {
    #   
    #   data_filtered <- data %>%
    #     dplyr::filter(network %in% input$network) %>%
    #     dplyr::filter(marine_park %in% paste(input$network, "Network")) %>%
    #     glimpse
    #   
    # }
    
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

    # if (input$toggle == "Marine Park") {

      req(input$marine_park)  # Ensure marine_park input is selected

      data <- data %>%
        dplyr::filter(network %in% input$network) %>%
        dplyr::filter(marine_park %in% debounced_park()) %>%
        dplyr::filter(ecosystem_component %in% input$ecosystemsubcomponent)

    # } else {
    #
    #   data <- data %>%
    #     dplyr::filter(network %in% input$network) %>%
    #     dplyr::filter(marine_park %in% paste(input$network, "Network"))
    #
    # }

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

  
  # Create species dropdown ----
  output$ui_species_length <- renderUI({
    
    data <- all_data$length_combined
    
    # if (input$toggle == "Marine Park") {
    
    req(input$marine_park)  # Ensure marine_park input is selected
    
    data <- data %>%
      dplyr::filter(network %in% input$network) %>%
      dplyr::filter(marine_park %in% debounced_park()) %>%
      dplyr::filter(ecosystem_component %in% input$ecosystemsubcomponent)
    
    # } else {
    #
    #   data <- data %>%
    #     dplyr::filter(network %in% input$network) %>%
    #     dplyr::filter(marine_park %in% paste(input$network, "Network"))
    #
    # }
    
    choices <- data %>%
      dplyr::group_by(display_name) %>%
      dplyr::summarise(total_number = sum(count)) %>%
      dplyr::arrange(desc(total_number)) %>%
      dplyr::distinct(display_name) %>%
      dplyr::pull("display_name")
    
    shinyWidgets::pickerInput(
      inputId = "specieslength",
      label = "Choose a species:",
      width = "100%",
      choices = choices,
      multiple = FALSE,
      selected = choices[1],
      options = list(`actions-box` = TRUE, `live-search` = TRUE, `dropup-auto` = FALSE)
    )
  })
  

# Create filtered error rasters ----
init_species_map <- reactive({
  req(#input$toggle,
      input$network, input$marine_park, input$ecosystemsubcomponent)

  data <- all_data$bubble_data %>%
    dplyr::filter(display_name %in% input$species)

  metadata <- all_data$synthesis_metadata
  # #
  # # if (input$toggle == "Marine Park") {
  #   req(input$marine_park)  # Ensure marine_park input is selected
    data <- data %>%
      # dplyr::filter(network %in% input$network) %>%
      dplyr::filter(marine_park %in% debounced_park()) %>%
      dplyr::filter(ecosystem_component %in% input$ecosystemsubcomponent)

    metadata <- metadata %>%
      # dplyr::filter(network %in% input$network) %>%
      dplyr::filter(marine_park %in% debounced_park()) %>%
      dplyr::filter(ecosystem_component %in% input$ecosystemsubcomponent)
  # #
  # # } else {
  # #   data <- data %>%
  # #     dplyr::filter(network %in% input$network) %>%
  # #     dplyr::filter(marine_park %in% paste(input$network, "Network"))
  # #
  # #   metadata <- metadata %>%
  # #     dplyr::filter(network %in% input$network) %>%
  # #     dplyr::filter(marine_park %in% paste(input$network, "Network"))
  # # }

  data <- full_join(data, metadata) %>%
    replace_na(list(count = 0)) %>%
    mutate(count = as.numeric(count), year = str_sub(date_time, 1, 4))

  return(data)

})

output$species_map <- renderLeaflet({

  data <- init_species_map()

  message("data for species map")

  points_og <- metadata_filtered_data() %>%
    dplyr::mutate(year = str_sub(date_time, 1, 4)) #%>%
    #glimpse()


  map <- leaflet() %>%
    addTiles() %>%
    fitBounds(
      lng1 = min(points_og$longitude_dd), lat1 = min(points_og$latitude_dd),
      lng2 = max(points_og$longitude_dd), lat2 = max(points_og$latitude_dd)
    ) %>%
    addPolygons(data = ngari.mp, weight = 1, color = "black",
                fillOpacity = 0.8, fillColor = "#7bbc63",
                group = "State Marine Parks", label = ngari.mp$Name) %>%

    addPolygons(data = state.mp, weight = 1, color = "black",
                fillOpacity = 0.8, fillColor = ~state.pal(zone),
                group = "State Marine Parks", label = state.mp$COMMENTS) %>%

    addPolygons(data = commonwealth.mp, weight = 1, color = "black",
                fillOpacity = 0.8, fillColor = ~commonwealth.pal(zone),
                group = "Australian Marine Parks", label = commonwealth.mp$ZoneName) %>%

    addLayersControl(
      overlayGroups = c("Australian Marine Parks", "State Marine Parks"),
      options = layersControlOptions(collapsed = FALSE),
      position = "bottomright"
    ) %>%
    hideGroup("State Marine Parks") %>%
    hideGroup("Australian Marine Parks")

  return(map)

})

output$species_year_slider <- renderUI({

  # Extract unique years from the dataset
  available_years <- sort(unique(as.numeric(year_data()$year)))

  sliderTextInput(
    inputId = "year",
    label = "Choose a year:",
    choices = available_years,
    grid = TRUE,
    width = "100%"
  )
})

output$species_year_slider_length <- renderUI({
  
  # Extract unique years from the dataset
  available_years <- sort(unique(as.numeric(year_data()$year)))
  
  sliderTextInput(
    inputId = "yearlength",
    label = "Choose a year:",
    choices = available_years,
    grid = TRUE,
    width = "100%"
  )
})
observeEvent(input$species, {
  observeEvent(input$year, {
    # req(input$year)  # Ensure year is selected

    data <- all_data$bubble_data %>%
      dplyr::filter(display_name %in% input$species)

    metadata <- all_data$synthesis_metadata

    # if (input$toggle == "Marine Park") {

      req(input$marine_park)  # Ensure marine_park input is selected

      data <- data %>%
        dplyr::filter(network %in% input$network) %>%
        dplyr::filter(marine_park %in% debounced_park()) %>%
        dplyr::filter(ecosystem_component %in% input$ecosystemsubcomponent)

      metadata <- metadata %>%
        dplyr::filter(network %in% input$network) %>%
        dplyr::filter(marine_park %in% debounced_park()) %>%
        dplyr::filter(ecosystem_component %in% input$ecosystemsubcomponent)

    # } else {
    #
    #   data <- data %>%
    #     dplyr::filter(network %in% input$network) %>%
    #     dplyr::filter(marine_park %in% paste(input$network, "Network"))
    #
    #   metadata <- metadata %>%
    #     dplyr::filter(network %in% input$network) %>%
    #     dplyr::filter(marine_park %in% paste(input$network, "Network"))
    #
    # }

    max_ab <- ifelse(nrow(data) > 0, max(as.numeric(data$count), na.rm = TRUE), 1)

    # message("chosen year")
    # message(input$year)

    message("combined species filtered data")

    data <- full_join(data, metadata) %>%
      replace_na(list(count = 0)) %>%
      dplyr::mutate(count = as.numeric(count)) %>%
      dplyr::mutate(year = str_sub(date_time, 1, 4)) #%>%
      # dplyr::filter(year %in% as.numeric(input$year)) %>%
      #glimpse()

    #message("chosen year")
    chosen_year <- as.numeric(input$year) #%>% glimpse()

    message(unique(data$year))

    #message("data")
    year_dat <- data %>%
      dplyr::mutate(year = as.numeric(year)) %>%
      dplyr::filter(year %in% chosen_year) #%>%
      #glimpse()

    # year_dat <- year_dat[year_dat$year == chosen_year,]

    print(unique(year_dat$year))

    message("overzero")
    overzero <- filter(year_dat, count > 0) #%>% glimpse()

    message("underzero")
    equalzero <- filter(year_dat, count %in% 0)




      leafletProxy("species_map") %>%
      clearMarkers() %>%
        {
          if (nrow(overzero) > 0) {
            addCircleMarkers(.,
              data = overzero, lat = ~latitude_dd, lng = ~longitude_dd,
              radius = ~ (((count / max_ab) * 20)), fillOpacity = 0.5, stroke = FALSE,
              label = ~ as.character(count), color = "green"
            )
          } else {.}
        } %>%
        {
          if (nrow(equalzero) > 0) {
            addCircleMarkers(.,
              data = equalzero, lat = ~latitude_dd, lng = ~longitude_dd,
              radius = 2, fillOpacity = 0.5, color = "white", stroke = FALSE,
              label = ~ as.character(count)
            )
          } else {.}
        }

  })
})

year_data <- reactive({

  metadata <- all_data$synthesis_metadata

  # if (input$toggle == "Marine Park") {

    req(input$marine_park)  # Ensure marine_park input is selected

    metadata <- metadata %>%
      dplyr::filter(network %in% input$network) %>%
      dplyr::filter(marine_park %in% debounced_park()) %>%
      dplyr::filter(ecosystem_component %in% input$ecosystemsubcomponent)

  # } else {
  #
  #   metadata <- metadata %>%
  #     dplyr::filter(network %in% input$network) %>%
  #     dplyr::filter(marine_park %in% paste(input$network, "Network"))
  #
  # }

  data <- metadata %>%
    dplyr::mutate(year = str_sub(date_time, 1, 4))

})

  output$map_year_slider <- renderUI({

    # Extract unique years from the dataset
    available_years <- sort(unique(as.numeric(year_data()$year)))

    sliderTextInput(
      inputId = "map_year",
      label = "Choose a year:",
      choices = available_years,
      grid = TRUE,
      width = "100%"
    )
  })

  output$assemblage_year_slider <- renderUI({

    # Extract unique years from the dataset
    available_years <- sort(unique(as.numeric(year_data()$year)))

    sliderTextInput(
      inputId = "assemblage_year",
      label = "Choose a year:",
      choices = available_years,
      grid = TRUE,
      width = "100%"
    )
  })

  # Create assemblage bubble plot ----
  output$assemblage_map <- renderLeaflet({

    # points <- metadata_filtered_data()

    points_og <- metadata_filtered_data() %>%
      dplyr::mutate(year = str_sub(date_time, 1, 4))

    assemblage_metric <- tolower(str_replace_all(input$assemblage, " ", "_"))

    data <- all_data$metric_bubble_data %>%
      dplyr::filter(metric %in% assemblage_metric)

    # if (input$toggle == "Marine Park") {

      req(input$marine_park)  # Ensure marine_park input is selected

      data <- data %>%
        # dplyr::filter(network %in% input$network) %>%
        dplyr::filter(marine_park %in% debounced_park()) %>%
        dplyr::filter(ecosystem_component %in% input$ecosystemsubcomponent)

    # } else {
#
#       data <- data %>%
#         dplyr::filter(network %in% input$network) %>%
#         dplyr::filter(marine_park %in% paste(input$network, "Network"))
#
#     }

    max_ab <- ifelse(nrow(data) > 0, max(data$value, na.rm = TRUE), 1)  # Avoid errors

    min_year <- min(data$year)

    # message("glimpse data")

    data <- data %>%
      dplyr::filter(year %in% as.numeric(min_year)) #%>%
    #glimpse()

    overzero <- filter(data, value  > 0)
    equalzero <- filter(data, value  %in% 0) #%>%
      # glimpse()

    # Initial Leaflet map ----
    map <- leaflet(data) %>%
      addTiles() %>%

      fitBounds(
        lng1 = min(points_og$longitude_dd), lat1 = min(points_og$latitude_dd),
        lng2 = max(points_og$longitude_dd), lat2 = max(points_og$latitude_dd)
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

      addLayersControl(
        overlayGroups = c("Australian Marine Parks",
                          "State Marine Parks"),
        options = layersControlOptions(collapsed = FALSE),
        position = "bottomright"
      )  %>%
      hideGroup("State Marine Parks") %>%
      hideGroup("Australian Marine Parks")%>%
      hideGroup("FishNClips" )%>%
      add_legend(colors = c("white", "green", "green"),
                 labels = c(0, round(max_ab / 2), max_ab),
                 sizes = c(5, 20, 40),
                 title = input$assemblage,
                 group = "abundance"
      )




    if (nrow(overzero)) {
      map <- map %>%
        addCircleMarkers(
          data = overzero, lat = ~latitude_dd, lng = ~longitude_dd,
          radius = ~ (((value / max_ab) * 20)), fillOpacity = 0.5, stroke = FALSE,
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

  observeEvent(input$assemblage, {
    observeEvent(input$assemblage_year, {

      # points <- metadata_filtered_data()

      assemblage_metric <- tolower(str_replace_all(input$assemblage, " ", "_"))

      data <- all_data$metric_bubble_data %>%
        dplyr::filter(metric %in% assemblage_metric)

      # if (input$toggle == "Marine Park") {

        req(input$marine_park)  # Ensure marine_park input is selected

        data <- data %>%
          # dplyr::filter(network %in% input$network) %>%
          dplyr::filter(marine_park %in% debounced_park()) %>%
          dplyr::filter(ecosystem_component %in% input$ecosystemsubcomponent)

      # } else {
      #
      #   data <- data %>%
      #     dplyr::filter(network %in% input$network) %>%
      #     dplyr::filter(marine_park %in% paste(input$network, "Network"))
      #
      # }

      max_ab <- ifelse(nrow(data) > 0, max(data$value, na.rm = TRUE), 1)  # Avoid errors

      data <- data %>%
        dplyr::filter(year %in% input$assemblage_year)

      overzero <- filter(data, value > 0)
      equalzero <- filter(data, value == 0)

      map <- leafletProxy("assemblage_map") %>%
        clearMarkers()

      if (nrow(overzero)) {
        map <- map %>%
          addCircleMarkers(
            data = overzero, lat = ~latitude_dd, lng = ~longitude_dd,
            radius = ~ (((value / max_ab) * 20)), fillOpacity = 0.5, stroke = FALSE,
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

      # %>%
      #   clearMarkers() %>%
      #   addCircleMarkers(
      #     data = overzero, lat = ~latitude_dd, lng = ~longitude_dd,
      #     radius = ~ (((count / max_ab) * 20)), fillOpacity = 0.5, stroke = FALSE,
      #     label = ~ as.character(count), color = "green"
      #   ) %>%
      #   addCircleMarkers(
      #     data = equalzero, lat = ~latitude_dd, lng = ~longitude_dd,
      #     radius = 2, fillOpacity = 0.5, color = "white", stroke = FALSE,
      #     label = ~ as.character(count)
      #   )

    })
  })


  # Species specific temporal plots ----
  output$species_temporal <- renderPlot({
    req(#input$toggle,
        input$network, input$options)

    data <- all_data$temporal_data %>%
      dplyr::filter(display_name %in% input$species) %>%
      dplyr::mutate(year = substr(date_midpoint, 1, 4)) %>%
      dplyr::mutate(year = as.numeric(year))

    # if (input$toggle == "Marine Park") {

      req(input$marine_park)  # Ensure marine_park input is selected

      data <- data %>%
        dplyr::filter(network %in% input$network) %>%
        dplyr::filter(marine_park %in% debounced_park())

    # } else {
    #
    #   data <- data %>%
    #     dplyr::filter(network %in% input$network) %>%
    #     dplyr::filter(marine_park %in% paste(input$network, "Network"))
    #
    # }


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

    points <- metadata_filtered_data() %>%
      dplyr::mutate(year = str_sub(date_time, 1, 4))

    points_og <- metadata_filtered_data() %>%
      dplyr::mutate(year = str_sub(date_time, 1, 4))

    min_year <- min(points$year)

    points <- points %>%
      dplyr::filter(year %in% min_year)

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

      addMarkers(data = points, ~longitude_dd, ~latitude_dd,
                 icon = icon,
                 clusterOptions = markerClusterOptions(iconCreateFunction =
                                                         JS("
                                        function(cluster) {
                                           return new L.DivIcon({
                                             html: '<div style=\"background-color:rgba(0, 123, 255, 0.9)\"><span>' + cluster.getChildCount() + '</div><span>',
                                             className: 'marker-cluster'
                                           });
                                         }")),
                 group = "Sampling locations"
      )%>%


      fitBounds(
        lng1 = min(points_og$longitude_dd), lat1 = min(points_og$latitude_dd),
        lng2 = max(points_og$longitude_dd), lat2 = max(points_og$latitude_dd)
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
  
  observeEvent(input$map_year, {

    data <- all_data$synthesis_metadata %>%
      dplyr::mutate(year = as.numeric(str_sub(date_time, 1, 4))) %>%
      dplyr::filter(year %in% as.numeric(input$map_year)) %>%
      glimpse()

    icon <- iconList(blue = makeIcon("images/marker_blue.png", iconWidth = 40, iconHeight =40))

    map <- leafletProxy("map") %>%
      clearMarkers() %>%
      clearGroup(group = "Sampling locations") %>%
      addMarkers(data = data, ~longitude_dd, ~latitude_dd,
                 icon = icon,
                 clusterOptions = markerClusterOptions(iconCreateFunction =
                                                         JS("
                                        function(cluster) {
                                           return new L.DivIcon({
                                             html: '<div style=\"background-color:rgba(0, 123, 255, 0.9)\"><span>' + cluster.getChildCount() + '</div><span>',
                                             className: 'marker-cluster'
                                           });
                                         }")),
                 group = "Sampling locations")

    map

  })

  # Create species iframe
  output$iframe <- renderUI({
    
    if(input$ecosystemsubcomponent %in% "Demersal fish"){
      
      dat <- all_data$foa_codes[display_name %in% c(input$species)] %>%
        dplyr::distinct(url) %>%
        dplyr::pull("url")
      
    } else if(input$ecosystemsubcomponent %in% "Mobile macro invertebrates"){
      
      # dat <- "https://www.fish.wa.gov.au/Documents/recreational_fishing/fact_sheets/fact_sheet_western_rock_lobster.pdf"
      dat <- "https://panuliruscygnus.org/"
    }

    frame <- tags$iframe(src = paste0(dat),
                         style = "width: 100%; height: 100vh; border: none;",
                         onload = "resizeIframe(this)"
    )
    frame

  })
  
  
  
  output$length_histogram <- renderPlot({
    
    req(input$binwidth)
    
    message("length hist data")
    
    binwidth <- input$binwidth
    
    length <- all_data$length_combined %>%
      dplyr::filter(display_name %in% input$specieslength) %>%
      dplyr::filter(network %in% input$network) %>%
      dplyr::filter(marine_park %in% debounced_park()) %>%
      dplyr::filter(ecosystem_component %in% input$ecosystemsubcomponent) %>%
      dplyr::glimpse()
    
    metadata <- all_data$synthesis_metadata
    
    # if (input$toggle == "Marine Park") {
    
    req(input$marine_park)  # Ensure marine_park input is selected
    
    metadata <- metadata %>%
      dplyr::filter(network %in% input$network) %>%
      dplyr::filter(marine_park %in% debounced_park()) %>%
      dplyr::filter(ecosystem_component %in% input$ecosystemsubcomponent)
    
    message("combined length with year")
    
    length <- left_join(length, metadata) %>%
      dplyr::mutate(year = str_sub(date_time, 1, 4)) %>% glimpse()
    
    chosen_year <- as.numeric(input$yearlength) #%>% glimpse()
    
    message(unique(data$year))
    
    length <- length %>%
      dplyr::mutate(year = as.numeric(year)) %>%
      dplyr::filter(year %in% chosen_year) #%>%
    
    ggplot(length, aes(x = length_mm)) +
      geom_histogram(binwidth = binwidth, fill = "steelblue", color = "white") +
      facet_grid(jurisdiction ~ zone, scales = "free_y") +
      ggplot_theme +
      labs(
        x = "Length (mm)",
        y = "Frequency"
      )
    
  })

  
  output$length_histogram_density <- renderPlot({
    
    req(input$binwidth)
    
    message("length hist data")
    
    length <- all_data$length_combined %>%
      dplyr::filter(display_name %in% input$specieslength) %>%
      dplyr::filter(network %in% input$network) %>%
      dplyr::filter(marine_park %in% debounced_park()) %>%
      dplyr::filter(ecosystem_component %in% input$ecosystemsubcomponent) %>%
      ungroup() %>%
      dplyr::select(-geometry) %>%
      as.data.frame() %>%
      dplyr::glimpse()
    
    
    metadata <- all_data$synthesis_metadata
    
    metadata <- metadata %>%
      dplyr::filter(network %in% input$network) %>%
      dplyr::filter(marine_park %in% debounced_park()) %>%
      dplyr::filter(ecosystem_component %in% input$ecosystemsubcomponent)
    
    message("combined length with year")
    
    length <- left_join(length, metadata) %>%
      dplyr::mutate(year = str_sub(date_time, 1, 4)) %>% glimpse()
    
    chosen_year <- as.numeric(input$yearlength) #%>% glimpse()
    
    message(unique(data$yearlength))
    
    length <- length %>%
      dplyr::mutate(year = as.numeric(year)) %>%
      dplyr::filter(year %in% chosen_year) #%>%
    
    
    # binwidth <- input$binwidth
    # length_df <- length %>%
    #   mutate(
    #     length_bin = cut(length_mm, breaks = seq(0, max(length_mm, na.rm = TRUE) + binwidth, by = binwidth), right = FALSE)
    #   )
    # 
    # # Count per bin, then calculate proportions per group
    # hist_summary <- length_df %>%
    #   group_by(jurisdiction, zone, length_bin) %>%
    #   summarise(n = n(), .groups = "drop") %>%
    #   group_by(jurisdiction, zone) %>%
    #   mutate(prop = n / sum(n))
    # 
    # # ggplot(hist_summary, aes(x = length_bin, y = prop, fill = zone)) +
    # #   geom_col(position = "dodge") +
    # #   facet_grid(jurisdiction ~ .) +
    # #   labs(
    # #     title = "Proportional Histogram of Fish Length",
    # #     x = "Length Bin (mm)",
    # #     y = "Proportion"
    # #   )
    
    ggplot(length, aes(x = length_mm, fill = zone)) +
      geom_histogram(aes(y = ..density..), binwidth = input$binwidth, fill = "steelblue", color = "white", position = "identity") +
      facet_grid(jurisdiction ~ zone, scales = "free_y") +
      labs(
        # title = "Normalized Histograms of Fish Length",
        x = "Length (mm)",
        y = "Proportion (Density)"
      ) +
      ggplot_theme
    
  })

  # # Create map for Dashboard ----
  output$australia_map <- renderLeaflet({
    # req(input$toggle, input$network, input$options)

    points_og <- metadata_filtered_data() %>%
      dplyr::mutate(year = str_sub(date_time, 1, 4))

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
      addMarkers(data = points, ~longitude_dd, ~latitude_dd,
                 icon = icon,
                 clusterOptions = markerClusterOptions(iconCreateFunction =
                                                         JS("
                                        function(cluster) {
                                           return new L.DivIcon({
                                             html: '<div style=\"background-color:rgba(0, 123, 255, 0.9)\"><span>' + cluster.getChildCount() + '</div><span>',
                                             className: 'marker-cluster'
                                           });
                                         }")),
                 group = "Sampling locations"
      )%>%

      fitBounds(
        lng1 = min(points_og$longitude_dd), lat1 = min(points_og$latitude_dd),
        lng2 = max(points_og$longitude_dd), lat2 = max(points_og$latitude_dd)
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
  
  # End of server ----
}

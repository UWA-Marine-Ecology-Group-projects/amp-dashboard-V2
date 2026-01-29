base_map <- function(max_zoom = 18, current_zoom = 6) {
  leaflet() |>
    addTiles(options = tileOptions(minZoom = 4, maxZoom = max_zoom)) |>
    addMapPane("polys",  zIndex = 410) |>
    addMapPane("points", zIndex = 420) |>
    addPolygons(
      data = commonwealth.mp,
      color = "black", weight = 1,
      fillColor = ~commonwealth.pal(zone), fillOpacity = 0.8,
      popup = ~ZoneName,
      options = pathOptions(pane = "polys"),
      group = "Australian Marine Parks"
    ) |>
    addLegend(
      pal = commonwealth.pal,
      values = commonwealth.mp$zone,
      opacity = 1,
      title = "Australian Marine Park Zones",
      position = "bottomleft",
      group = "Australian Marine Parks"
    ) |>
    addLayersControl(
      baseGroups = c("World Imagery", "Open Street Map"),
      overlayGroups = c("Australian Marine Parks", "Sampling locations"),
      options = layersControlOptions(collapsed = FALSE),
      position = "topright"
    )
}

method_legend_control <- function(pal, values, title = "Method") {
  # Build the legend HTML with an id we can remove reliably
  cols <- pal(values)
  labs <- as.character(values)
  
  items <- paste0(
    "<div style='display:flex; align-items:center; margin:2px 0;'>",
    "<span style='width:12px; height:12px; background:", cols,
    "; display:inline-block; margin-right:6px; border:1px solid rgba(255,255,255,0.6)'></span>",
    labs,
    "</div>",
    collapse = ""
  )
  
  html <- paste0(
    "<div id='method-legend' class='leaflet-control leaflet-bar' ",
    "style='background:white; padding:8px 10px; border-radius:6px;'>",
    "<div style='font-weight:700; margin-bottom:6px;'>", title, "</div>",
    items,
    "</div>"
  )
  
  htmltools::HTML(html)
}

make_trend_plot <- function(df, y_label, management_change_year = 2018) {
  ggplot(df, aes(x = year, y = mean, colour = zone, group = zone)) +
    geom_vline(xintercept = management_change_year, linetype = "dashed", linewidth = 0.5) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 2) +
    geom_linerange(aes(ymin = mean - se, ymax = mean + se), linewidth = 0.5) +
    facet_wrap(
      ~ depth_class,
      ncol = 1,
      scales = "free_y",
      labeller = labeller(depth_class = function(x) paste0("Depth: ", x))
    ) +
    scale_colour_manual(values = zone_cols, name = "Australian Marine Parks") +
    scale_x_continuous(
      breaks = c(2016, 2019, 2022),
      expand = expansion(mult = c(0.03, 0.06))
    ) +
    labs(
      x = "Year",
      y = y_label
    ) +
    theme_classic(base_size = 11) +
    theme(
      legend.position = "top",
      legend.direction = "horizontal",
      strip.background = element_blank(),
      strip.text = element_text(face = "italic"),
      axis.title.y = element_text(margin = margin(r = 10))
    )
}



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
  
  ## Create Marine Park Name Title ----
  debounced_park <- debounce(reactive(input$marine_park), 300)
  
  output$marinepark_name <- renderUI({
    req(input$marine_park)
    HTML(paste0("<b>", input$marine_park, "</b>")) # changed from debounced
  })
  
  ## Park banner ----
  output$ui_marine_park <- renderUI({
    
    req(input$marine_park)
    
    park <- stringr::str_replace_all(tolower(input$marine_park), c(" marine park" = "", " " = ".", "-" = "."))
    
    div(
      class = "park-hero",
      img(
        src   = paste0("parks/", park, ".jpg"),
        class = "park-hero-img"
      ),
      div(class = "park-hero-overlay"),
      div(class = "park-hero-text", input$marine_park)
    )
  })
  
  method_meta <- function(method) {
    # method-specific heading + icon + link
    switch(
      method,
      "stereo-BRUV" = list(
        title = "Baited Remote Underwater Stereo-video\n(stereo-BRUV)",
        img   = "stereo-BRUV_filled_transparent.png",
        href  = "https://benthic-bruvs-field-manual.github.io/"
      ),
      "UVC" = list(
        title = "Underwater Visual Census\n(UVC)",
        img   = "uvc_new.png",
        href  = "https://benthic-bruvs-field-manual.github.io/"
      ),
      "stereo-ROV" = list(
        title = "Stereo Remotely Operated Vehicle\n(stereo-ROV)",
        img   = "rov.png", # put a file in www/rov.png (or change)
        href  = "https://benthic-bruvs-field-manual.github.io/"
      ),
      # fallback
      list(title = method, img = NULL, href = "#")
    )
  }
  
  method_card_ui <- function(row) {
    meta <- method_meta(row$method)
    
    card(
      min_height = 725,
      max_height = 825,
      full_screen = TRUE,
      
      shiny::a(
        h4(
          if (!is.null(meta$img)) img(src = meta$img, height = "120px", style = "display:block; margin:0 auto;"),
          meta$title,
          style = "margin: 0; line-height: 1.2;"
        ),
        class = "custom-button btn btn-default action-button",
        style = "font-weight:600; width:100%; text-align:center; font-size: 20px; color: white;",
        href = meta$href,
        target = "_blank"
      ),
      
      layout_column_wrap(
        width = 1/2,
        
        value_box("Deployments", scales::label_comma()(row$deployments),
                  theme_color = "primary", showcase = icon("ship", class = "fa-xl")),
        
        value_box("Fish Counted", scales::label_comma()(row$fish_counted),
                  theme_color = "primary", showcase = icon("fish-fins", class = "fa-xl")),
        
        value_box("Fish Species", scales::label_comma()(row$fish_species),
                  theme_color = "primary", showcase = icon("fish-fins", class = "fa-xl")),
        
        value_box("Other species", scales::label_comma()(row$other_species),
                  theme_color = "primary", showcase = icon("shrimp", class = "fa-xl")),
        
        value_box("Length Measurements", scales::label_comma()(row$length_measurements),
                  theme_color = "primary", showcase = icon("ruler-vertical", class = "fa-xl")),
        
        value_box("Years Included", paste0(row$years_min, " - ", row$years_max),
                  theme_color = "primary", showcase = icon("calendar", class = "fa-xl")),
        
        value_box("Depths Surveyed", paste0(row$depth_min_m, " - ", row$depth_max_m, " m"),
                  theme_color = "primary", showcase = icon("arrow-down-up-across-line", class = "fa-xl")),
        
        value_box("Average Depth", paste0(row$avg_depth_m, " m"),
                  theme_color = "primary", showcase = icon("wave-square", class = "fa-xl")),
        
        value_box("Deployments with benthos", scales::label_comma()(row$deployments_with_benthos),
                  theme_color = "primary", showcase = icon("seedling", class = "fa-xl")),
        
        value_box("Deployments with relief", scales::label_comma()(row$deployments_with_relief),
                  theme_color = "primary", showcase = icon("mound", class = "fa-xl"))
      ),
      
      # method-specific GA link (optional)
      shiny::a(
        h4(icon("globe"), "View synthesis dataset on GlobalArchive", class = "custom-button btn btn-default action-button",
           style = "font-weight:600"),
        target = "_blank",
        href = paste0("https://dev.globalarchive.org/ui/main/syntheses/", row$synthesis_id),
        style = "width: 100%; display: block; text-align: center; background-color: #f8f9fa; padding: 10px; border-radius: 5px;"
      )
    )
  }
  
  park_rows <- reactive({
    req(input$marine_park)
    # tiny lookup only (fast). replace with data.table keyed lookup later if needed.
    park_method_summary %>%
      dplyr::filter(marine_park == input$marine_park) %>%
      dplyr::arrange(factor(method, levels = c("stereo-BRUV", "UVC", "stereo-ROV")))
  })
  
  output$ui_method_cards <- renderUI({
    rows <- park_rows()
    req(nrow(rows) > 0)
    
    n <- nrow(rows)
    
    card_width <- dplyr::case_when(
      n == 1 ~ 1,
      TRUE   ~ 1/2
    )
    
    layout_column_wrap(
      width = card_width,
      min_height = 825,
      !!!lapply(seq_len(n), function(i) method_card_ui(rows[i, ]))
    )
  })
  
  # ---- Map points reactive ----
  map_points <- reactive({
    # req(input$marine_park)
    
    metadata <- all_data$metadata
    
    # choose a method column robustly
    method_col <- dplyr::case_when(
      "method" %in% names(metadata) ~ "method",
      "Method" %in% names(metadata) ~ "Method",
      TRUE ~ NA_character_
    )
    
    req(!is.na(method_col))
    
    pts <- metadata %>%
      dplyr::filter(marine_park == input$marine_park) %>%
      dplyr::transmute(
        longitude = longitude_dd,
        latitude  = latitude_dd,
        method_raw = .data[[method_col]],
        popup = paste0(
          "<b>", marine_park, "</b><br/>",
          "Method: ", method_raw
        )
      ) %>%
      dplyr::filter(!is.na(longitude), !is.na(latitude))
    
    pts %>%
      dplyr::mutate(
        method = dplyr::case_when(
          stringr::str_detect(tolower(method_raw), "bruv") ~ "stereo-BRUV",
          stringr::str_detect(tolower(method_raw), "uvc")  ~ "UVC",
          stringr::str_detect(tolower(method_raw), "rov")  ~ "stereo-ROV",
          TRUE ~ "Other"
        )
      )
  })
  
  # ---- Initial map ----
  output$map_methods <- renderLeaflet({
    pts <- map_points()
    req(nrow(pts) > 0)
    
    # bounds safety
    lng1 <- min(pts$longitude, na.rm = TRUE)
    lat1 <- min(pts$latitude,  na.rm = TRUE)
    lng2 <- max(pts$longitude, na.rm = TRUE)
    lat2 <- max(pts$latitude,  na.rm = TRUE)
    req(is.finite(lng1), is.finite(lat1), is.finite(lng2), is.finite(lat2))
    
    base_map() %>%
      addProviderTiles("Esri.WorldImagery", group = "World Imagery") %>%
      addTiles(group = "Open Street Map") %>%
      # single legend container (never duplicated)
      fitBounds(lng1, lat1, lng2, lat2) 
  })
  
  # ---- Update markers + zoom + legend whenever the POINTS change ----
  observeEvent(map_points(), {
    pts <- map_points()
    req(nrow(pts) > 0)
    
    lng1 <- min(pts$longitude, na.rm = TRUE)
    lat1 <- min(pts$latitude,  na.rm = TRUE)
    lng2 <- max(pts$longitude, na.rm = TRUE)
    lat2 <- max(pts$latitude,  na.rm = TRUE)
    req(is.finite(lng1), is.finite(lat1), is.finite(lng2), is.finite(lat2))
    
    methods_present <- sort(unique(pts$method))
    methods_present <- methods_present[methods_present %in% names(method_cols)]
    req(length(methods_present) > 0)
    
    pal <- leaflet::colorFactor(
      palette = unname(method_cols[methods_present]),
      domain  = methods_present
    )
    
    proxy <- leafletProxy("map_methods", data = pts) %>%
      clearGroup("Sampling locations") %>%
      addCircleMarkers(
        lng = ~longitude, lat = ~latitude,
        radius = 6, stroke = TRUE, weight = 1,
        fillOpacity = 0.9, color = "white",
        fillColor = ~pal(method),
        popup = ~popup,
        group = "Sampling locations",
        options = pathOptions(pane = "points")
      ) %>%
      fitBounds(lng1, lat1, lng2, lat2)
    
    # 🔒 Always replace (never stack) the method legend
    proxy %>%
      removeControl(layerId = "method_legend") %>%   # safe even if missing
      addLegend(
        pal = pal,
        values = methods_present,
        title = "Method",
        opacity = 1,
        position = "bottomright",                    # avoids clashing with layers control (topright)
        layerId = "method_legend",
        group = "Sampling locations"                 # optional: hide when sampling layer is off
      )
  }, ignoreInit = FALSE)
  
  
  effort_df_all <- reactive({
    prep_effort_df(all_data$metadata)   # <-- swap to all_data$metadata if needed
  })
  
  effort_df_park <- reactive({
    req(input$marine_park)
    effort_df_all() %>%
      filter(marine_park == input$marine_park)
  })
  
  output$plot_effort_year <- renderPlot({
    df <- effort_df_park()
    req(nrow(df) > 0)
    
    ggplot(df %>% filter(!is.na(year)), aes(x = year, fill = method)) +
      geom_bar(color = "black", linewidth = 0.2) +
      scale_fill_manual(values = method_cols, drop = TRUE) +
      labs(
        title = "Sampling Effort by Year",
        x = "Year",
        y = "Number of deployments",
        fill = NULL
      ) +
      theme_minimal(base_size = 12) +
      theme(panel.grid.minor = element_blank(),
            legend.position = "right")
  })
  
  output$plot_effort_depth <- renderPlot({
    df <- effort_df_park()
    req(nrow(df) > 0)
    
    ggplot(df %>% filter(!is.na(depth_m), depth_m >= 0), aes(x = depth_m, fill = method)) +
      geom_histogram(binwidth = 5, color = "black", linewidth = 0.2) +
      scale_fill_manual(values = method_cols, drop = TRUE) +
      labs(
        title = "Sampling Effort by Depth",
        x = "Depth (m)",
        y = "Number of surveys",
        fill = NULL
      ) +
      theme_minimal(base_size = 12) +
      theme(panel.grid.minor = element_blank(),
            legend.position = "right")
  })
  
  top_species_data <- reactive({
    req(input$marine_park)
    
    counts_all <- all_data$top_species  # <-- CHANGE if needed
    
    # standardise method labels to match your map + method_cols
    df <- counts_all %>%
      filter(marine_park == input$marine_park) %>%
      mutate(
        method = case_when(
          str_detect(tolower(method), "bruv") ~ "stereo-BRUV",
          str_detect(tolower(method), "uvc")  ~ "UVC",
          str_detect(tolower(method), "rov")  ~ "stereo-ROV",
          TRUE ~ "Other"
        )
      ) %>%
      filter(method %in% c("stereo-BRUV", "UVC", "stereo-ROV"))
    
    # methods present *in this park*
    methods_present <- df %>% distinct(method) %>% pull(method)
    
    # top N species *per method* (change n = 20 if you want)
    df_top <- df %>%
      dplyr::group_by(method) %>%
      slice_max(order_by = total_number, n = 10, with_ties = FALSE) %>%
      ungroup()
    
    # italic scientific name + normal common name
    df_top %>%
      tidyr::extract(
        display_name, into = c("sci", "common"),
        regex = "^(.*?)\\s*\\((.*?)\\)$", remove = FALSE
      ) %>%
      mutate(
        sci = if_else(is.na(sci), display_name, sci),
        common = if_else(is.na(common), "NA", common),
        label = paste0("<i>", sci, "</i><span> (", common, ")</span>"),
        method = factor(method, levels = c("stereo-BRUV", "UVC", "stereo-ROV"))
      )
  })
  
  output$top_species_plot <- renderPlot({
    df <- top_species_data()
    req(nrow(df) > 0)
    
    n_methods <- df %>% distinct(method) %>% nrow()
    
    ggplot(df, aes(
      x = tidytext::reorder_within(label, total_number, method),
      y = total_number,
      fill = method
    )) +
      geom_col(colour = "black") +
      coord_flip() +
      tidytext::scale_x_reordered() +
      scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
      scale_fill_manual(values = method_cols, drop = TRUE) +
      facet_wrap(vars(method), scales = "free", ncol = n_methods) +
      labs(
        x = "Species",
        y = "Total number",
        fill = NULL
      ) +
      ggplot_theme_md() +
      theme(
        axis.text.y = ggtext::element_markdown(size = 12),
        axis.text.x = element_text(vjust = 0.5)
      )
  })
  
  ## Fish And Clips ----
  metadata_filtered_data <- reactive({
    req(input$marine_park)
    metadata <- all_data$metadata
    
    metadata %>%
      dplyr::filter(marine_park %in% input$marine_park) %>%
      dplyr::filter(ecosystem_component %in% "Demersal fish")
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
  
  # Assemblage maps ----
  # 1) Methods present for selected park
  methods_present <- reactive({
    df_all <- all_data$metric_bubble_data
    
    df_all <- df_all %>%
      dplyr::mutate(method = "stereo-BRUV") %>%
      distinct(method)
    
    req(nrow(df_all) > 0)
    
    m <- as.character(df_all$method)
    m <- m[m %in% names(method_cols)]
    unique(m)
  })
  
  # 2) One big card UI, 1–3 columns
  output$assemblage_maps_ui <- renderUI({
    methods <- methods_present()
    req(length(methods) > 0)
    
    n <- length(methods)
    cw <- dplyr::case_when(n == 1 ~ 12, n == 2 ~ 6, TRUE ~ 4)
    
    ids_ui <- vapply(methods, method_to_id, character(1))
    message("[assemblage UI] leafletOutput ids: ", paste(ids_ui, collapse = ", "))
    
    bslib::layout_columns(
      col_widths = rep(cw, n),
      !!!lapply(methods, function(m) {
        div(
          style = "padding: 0.5rem;",
          h4(m, style = "margin: 0 0 0.5rem 0;"),
          leafletOutput(method_to_id(m), height = "68vh")
        )
      })
    )
  })
  
  
  total_abundance_points <- reactive({
    req(input$marine_park)
    
    all_data$metric_bubble_data %>%
      dplyr::filter(marine_park %in% input$marine_park) %>%
      dplyr::filter(metric %in% "total_abundance") %>%
      dplyr::rename(latitude = latitude_dd, longitude = longitude_dd) %>%
      dplyr::mutate(method = "stereo-BRUV")
    
  })
  
  # 3) Render base maps for each method when tab is active / park changes / methods change
  observeEvent(
    list(input$obs_tabs, input$marine_park, methods_present()),
    {
      if (!identical(input$obs_tabs, "Total abundance")) return()
      req(input$marine_park)
      
      methods <- methods_present()
      req(length(methods) > 0)

      pts <- total_abundance_points()
      req(nrow(pts) > 0)
      
      # bounds safety
      lng1 <- min(pts$longitude, na.rm = TRUE)
      lat1 <- min(pts$latitude,  na.rm = TRUE)
      lng2 <- max(pts$longitude, na.rm = TRUE)
      lat2 <- max(pts$latitude,  na.rm = TRUE)
      
      req(is.finite(lng1), is.finite(lat1), is.finite(lng2), is.finite(lat2))
      
      # overzero <- dplyr::filter(data, value > 0) %>%
      #   sf::st_as_sf(coords = c("longitude_dd", "latitude_dd"))
      # 
      # equalzero <- dplyr::filter(data, value == 0) %>%
      #   sf::st_as_sf(coords = c("longitude_dd", "latitude_dd"))
    
      for (m in methods) {
        id <- method_to_id(m)
        
        output[[id]] <- renderLeaflet({
          base_map() %>%
            addProviderTiles("Esri.WorldImagery", group = "World Imagery") %>%
            addTiles(group = "Open Street Map")  %>%
            fitBounds(lng1, lat1, lng2, lat2) %>%
            clearGroup("Sampling locations") %>%
            addCircleMarkers(
              data = pts,
              lng = ~longitude, lat = ~latitude,
              radius = 6, stroke = TRUE, weight = 1,
              fillOpacity = 0.9, color = "white",
              group = "Sampling locations",
              options = pathOptions(pane = "points")
            )
        })
        
        outputOptions(output, id, suspendWhenHidden = FALSE)
      }
    },
    ignoreInit = TRUE
  )

  observeEvent(
    list(input$obs_tabs, input$marine_park, methods_present(), total_abundance_points()),
    {
      if (!identical(input$obs_tabs, "Total abundance")) return()
      req(input$marine_park)
      
      pts <- total_abundance_points()
      req(nrow(pts) > 0)
      
      methods <- sort(unique(pts$method))
      req(length(methods) > 0)
      
      session$onFlushed(once = TRUE, function() {
        for (m in methods) {
          id <- method_to_id(m)
          
          dat_m <- dplyr::filter(pts, method == m)
          
          leafletProxy(id, session = session) %>%
            clearGroup("Sampling locations") %>%
            addCircleMarkers(
              data = dat_m,
              lng = ~longitude, lat = ~latitude,
              radius = 6, stroke = TRUE, weight = 1,
              fillOpacity = 0.9, color = "white",
              group = "Sampling locations",
              options = pathOptions(pane = "points")
            )
        }
      })
    },
    ignoreInit = FALSE
  )
  
  # Trends ---
  trend_metric_code <- reactive({
    req(input$trend_metric)
    
    dplyr::case_when(
      input$trend_metric == "Large Reef Fish Index* (B20*)" ~ "B20",
      input$trend_metric == "Reef Fish Thermal Index"       ~ "RFTI",  # <-- change if your sheet uses a different code
      TRUE ~ NA_character_
    )
  })
  
  trend_df_park <- reactive({
    req(input$marine_park)
    req(trend_metric_code())
    
    df <- all_data$trend_data  
    
    df %>%
      dplyr::filter(
        marine_park == input$marine_park,
        metric == trend_metric_code()
      ) %>%
      dplyr::mutate(
        date = as.Date(date),
        year = lubridate::year(date),
        zone = factor(zone, levels = names(zone_cols)),
        depth_class = factor(depth_class, levels = c("0-30 m", "30-70 m"))
      ) %>%
      dplyr::filter(!is.na(zone))
  })
  
  trend_methods_present <- reactive({
    df <- trend_df_park()
    req(nrow(df) > 0)
    sort(unique(df$method))
  })
  
  method_to_trend_id <- function(method) {
    paste0("trend_plot_", gsub("[^A-Za-z0-9]+", "_", method))
  }
  
  output$trend_plots_ui <- renderUI({
    df <- trend_df_park()
    
    validate(
      need(nrow(df) > 0, "No trend data available for this Marine Park + Metric.")
    )
    
    methods <- sort(unique(df$method))
    n <- length(methods)
    
    # 1 method → full width, 2–3 methods → 2 per row
    card_width <- if (n == 1) 1 else 1/2
    
    layout_column_wrap(
      width = card_width,
      !!!lapply(methods, function(m) {
        id <- method_to_trend_id(m)
        
        card(
          full_screen = TRUE,
          card_header(paste("Method:", m)),
          plotOutput(id, height = 420)
        )
      })
    )
  })
  
  
  observe({
    df <- trend_df_park()
    req(nrow(df) > 0)
    
    methods <- trend_methods_present()
    req(length(methods) > 0)
    
    for (m in methods) {
      local({
        method <- m
        id <- method_to_trend_id(method)
        
        output[[id]] <- renderPlot({
          d <- df %>% dplyr::filter(method == !!method)
          req(nrow(d) > 0)
          make_trend_plot(d, y_label = input$trend_metric, management_change_year = 2018)
        })
        
        outputOptions(output, id, suspendWhenHidden = FALSE)
      })
    }
  })
  
  
  # End of server ----
}

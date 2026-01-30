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
      overlayGroups = c("Australian Marine Parks", "Sampling locations", "Probability", "Error"),
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

# make_trend_plot <- function(df, y_label, management_change_year = 2018) {
#   ggplot(df, aes(x = year, y = mean, colour = zone, group = zone)) +
#     geom_vline(xintercept = management_change_year, linetype = "dashed", linewidth = 0.5) +
#     geom_line(linewidth = 0.8) +
#     geom_point(size = 2) +
#     geom_linerange(aes(ymin = mean - se, ymax = mean + se), linewidth = 0.5) +
#     facet_wrap(
#       ~ depth_class,
#       ncol = 1,
#       scales = "free_y",
#       labeller = labeller(depth_class = function(x) paste0("Depth: ", x))
#     ) +
#     scale_colour_manual(values = zone_cols, name = "Australian Marine Parks") +
#     scale_x_continuous(
#       breaks = c(2016, 2019, 2022),
#       expand = expansion(mult = c(0.03, 0.06))
#     ) +
#     labs(
#       x = "Year",
#       y = y_label
#     ) +
#     theme_classic(base_size = 11) +
#     theme(
#       legend.position = "top",
#       legend.direction = "horizontal",
#       strip.background = element_blank(),
#       strip.text = element_text(face = "italic"),
#       axis.title.y = element_text(margin = margin(r = 10))
#     )
# }



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
        
        # outputOptions(output, id, suspendWhenHidden = FALSE)
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
  # trend_metric_code <- reactive({
  #   req(input$trend_metric)
  #   
  #   dplyr::case_when(
  #     input$trend_metric == "Large Reef Fish Index* (B20*)" ~ "B20",
  #     input$trend_metric == "Reef Fish Thermal Index"       ~ "RFTI",  # <-- change if your sheet uses a different code
  #     TRUE ~ NA_character_
  #   )
  # })
  # 
  # trend_df_park <- reactive({
  #   req(input$marine_park)
  #   req(trend_metric_code())
  #   
  #   df <- all_data$trend_data  
  #   
  #   df %>%
  #     dplyr::filter(
  #       marine_park == input$marine_park,
  #       metric == trend_metric_code()
  #     ) %>%
  #     dplyr::mutate(
  #       date = as.Date(date),
  #       year = lubridate::year(date),
  #       zone = factor(zone, levels = names(zone_cols)),
  #       depth_class = factor(depth_class, levels = c("0-30 m", "30-70 m"))
  #     ) %>%
  #     dplyr::filter(!is.na(zone))
  # })
  # 
  # trend_methods_present <- reactive({
  #   df <- trend_df_park()
  #   req(nrow(df) > 0)
  #   sort(unique(df$method))
  # })
  # 
  # method_to_trend_id <- function(method) {
  #   paste0("trend_plot_", gsub("[^A-Za-z0-9]+", "_", method))
  # }
  # 
  # output$trend_plots_ui <- renderUI({
  #   df <- trend_df_park()
  #   
  #   validate(
  #     need(nrow(df) > 0, "No trend data available for this Marine Park + Metric.")
  #   )
  #   
  #   methods <- sort(unique(df$method))
  #   n <- length(methods)
  #   
  #   # 1 method → full width, 2–3 methods → 2 per row
  #   card_width <- if (n == 1) 1 else 1/2
  #   
  #   layout_column_wrap(
  #     width = card_width,
  #     !!!lapply(methods, function(m) {
  #       id <- method_to_trend_id(m)
  #       
  #       card(
  #         full_screen = TRUE,
  #         card_header(paste("Method:", m)),
  #         plotOutput(id, height = 420)
  #       )
  #     })
  #   )
  # })
  # 
  # 
  # observe({
  #   df <- trend_df_park()
  #   req(nrow(df) > 0)
  #   
  #   methods <- trend_methods_present()
  #   req(length(methods) > 0)
  #   
  #   for (m in methods) {
  #     local({
  #       method <- m
  #       id <- method_to_trend_id(method)
  #       
  #       output[[id]] <- renderPlot({
  #         d <- df %>% dplyr::filter(method == !!method)
  #         req(nrow(d) > 0)
  #         make_trend_plot(d, y_label = input$trend_metric, management_change_year = 2018)
  #       })
  #       
  #       outputOptions(output, id, suspendWhenHidden = FALSE)
  #     })
  #   }
  # })
  # 
  # trend_df_park_b20 <- reactive({
  #   req(input$marine_park)
  #   
  #   all_data$trend_data %>%
  #     dplyr::filter(marine_park == input$marine_park, metric == "B20") %>%
  #     dplyr::mutate(
  #       date = as.Date(date),
  #       year = lubridate::year(date),
  #       zone = factor(zone, levels = names(zone_cols)),
  #       depth_class = factor(depth_class, levels = c("0-30 m", "30-70 m"))
  #     ) %>%
  #     dplyr::filter(!is.na(zone))
  # })
  

  # # -----------------------------
  # # STATE & TREND (Tabbed: B20 / RFTI)
  # # -----------------------------
  # 
  # # ---- Helpers: safe ids per method ----
  # method_safe <- function(x) gsub("[^A-Za-z0-9]+", "_", x)
  # 
  # # Trend-line plot output ids
  # method_to_trend_id <- function(metric, method) {
  #   paste0("trend_", metric, "_", method_safe(method))
  # }
  # 
  # # Block ids (year/map/plot) per metric+method
  # year_id_for_method <- function(metric, m) paste0("year_", metric, "_", method_safe(m))
  # map_id_for_method  <- function(metric, m) paste0("map_",  metric, "_", method_safe(m))
  # plot_id_for_method <- function(metric, m) paste0("plot_", metric, "_", method_safe(m))
  # 
  # # ---- Plot makers (DO NOT reference input inside these) ----
  # make_trend_plot <- function(df, y_label, management_change_year = 2018) {
  #   ggplot(df, aes(x = year, y = mean, colour = zone, group = zone)) +
  #     geom_vline(xintercept = management_change_year, linetype = "dashed", linewidth = 0.5) +
  #     geom_line(linewidth = 0.8) +
  #     geom_point(size = 2) +
  #     geom_linerange(aes(ymin = mean - se, ymax = mean + se), linewidth = 0.5) +
  #     facet_wrap(
  #       ~ depth_class,
  #       ncol = 1,
  #       scales = "free_y",
  #       labeller = labeller(depth_class = function(x) paste0("Depth: ", x))
  #     ) +
  #     scale_colour_manual(values = zone_cols, name = "Australian Marine Parks") +
  #     scale_x_continuous(
  #       breaks = c(2016, 2019, 2022),
  #       expand = expansion(mult = c(0.03, 0.06))
  #     ) +
  #     labs(x = "Year", y = y_label) +
  #     theme_classic(base_size = 11) +
  #     theme(
  #       legend.position = "top",
  #       legend.direction = "horizontal",
  #       strip.background = element_blank(),
  #       strip.text = element_text(face = "italic"),
  #       axis.title.y = element_text(margin = margin(r = 10))
  #     )
  # }
  # 
  # # B20 species facet plot:
  # # bar height = abundance (value_a), error bars = SE (value_b)
  # make_b20_species_plot <- function(df) {
  #   ggplot(df, aes(x = reorder(taxa, value_a), y = value_a)) +
  #     geom_col() +
  #     geom_errorbar(aes(ymin = pmax(0, value_a - value_b), ymax = value_a + value_b),
  #                   width = 0.2, linewidth = 0.4) +
  #     coord_flip() +
  #     facet_grid(depth_class ~ zone, scales = "free_y") +
  #     scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  #     labs(x = NULL, y = "Abundance") +
  #     theme_minimal(base_size = 11) +
  #     theme(panel.grid.minor = element_blank(),
  #           strip.text = element_text(face = "bold"))
  # }
  # 
  # # RFTI species facet plot:
  # # bar height = Log Abundance (value_b), label = Temp (value_a)
  # make_rfti_species_plot <- function(df) {
  #   ggplot(df, aes(x = reorder(taxa, value_b), y = value_b)) +
  #     geom_col() +
  #     coord_flip() +
  #     geom_text(aes(label = sprintf("%.1f\u00B0C", value_a)),
  #               hjust = -0.05, size = 3) +
  #     facet_grid(depth_class ~ zone, scales = "free_y") +
  #     scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  #     labs(x = NULL, y = "Log Abundance") +
  #     theme_minimal(base_size = 11) +
  #     theme(panel.grid.minor = element_blank(),
  #           strip.text = element_text(face = "bold"))
  # }
  # 
  # # -----------------------------
  # # 1) Trend-line data (from all_data$trend_data) for both metrics
  # # -----------------------------
  # 
  # trend_df_park_metric <- function(metric_code) {
  #   reactive({
  #     req(input$marine_park)
  #     
  #     all_data$trend_data %>%
  #       dplyr::filter(marine_park == input$marine_park, metric == metric_code) %>%
  #       dplyr::mutate(
  #         date = as.Date(date),
  #         year = lubridate::year(date),
  #         zone = factor(zone, levels = names(zone_cols)),
  #         depth_class = factor(depth_class, levels = c("0-30 m", "30-70 m"))
  #       ) %>%
  #       dplyr::filter(!is.na(zone))
  #   })
  # }
  # 
  # trend_df_b20  <- trend_df_park_metric("B20")
  # trend_df_rfti <- trend_df_park_metric("RFTI")
  # 
  # trend_methods_present <- function(trend_df_reactive) {
  #   reactive({
  #     df <- trend_df_reactive()
  #     req(nrow(df) > 0)
  #     sort(unique(df$method))
  #   })
  # }
  # 
  # trend_methods_b20  <- trend_methods_present(trend_df_b20)
  # trend_methods_rfti <- trend_methods_present(trend_df_rfti)
  # 
  # # -----------------------------
  # # 2) Trend plots UI + renderers (B20 tab)
  # # -----------------------------
  # 
  # output$trend_plots_b20_ui <- renderUI({
  #   # req(input$trend_metric_tab == "B20")
  #   
  #   df <- trend_df_b20()
  #   validate(need(nrow(df) > 0, "No B20 trend data available for this Marine Park."))
  #   
  #   methods <- trend_methods_b20()
  #   n <- length(methods)
  #   card_width <- if (n == 1) 1 else 1/2
  #   
  #   bslib::layout_column_wrap(
  #     width = card_width,
  #     !!!lapply(methods, function(m) {
  #       id <- method_to_trend_id("B20", m)
  #       bslib::card(
  #         full_screen = TRUE,
  #         bslib::card_header(paste("Method:", m)),
  #         plotOutput(id, height = 420)
  #       )
  #     })
  #   )
  # })
  # 
  # observe({
  #   # req(input$trend_metric_tab == "B20")
  #   
  #   df <- trend_df_b20()
  #   req(nrow(df) > 0)
  #   
  #   methods <- trend_methods_b20()
  #   req(length(methods) > 0)
  #   
  #   for (m in methods) {
  #     local({
  #       method <- m
  #       id <- method_to_trend_id("B20", method)
  #       
  #       output[[id]] <- renderPlot({
  #         d <- df %>% dplyr::filter(method == !!method)
  #         validate(need(nrow(d) > 0, "No data for this method."))
  #         make_trend_plot(d, y_label = "Large Reef Fish Index* (B20*)", management_change_year = 2018)
  #       })
  #       
  #       # outputOptions(output, id, suspendWhenHidden = FALSE)
  #     })
  #   }
  # })
  # 
  # # -----------------------------
  # # 3) Trend plots UI + renderers (RFTI tab)
  # # -----------------------------
  # 
  # output$trend_plots_rfti_ui <- renderUI({
  #   # req(input$trend_metric_tab == "RFTI")
  #   
  #   df <- trend_df_rfti()
  #   validate(need(nrow(df) > 0, "No RFTI trend data available for this Marine Park."))
  #   
  #   methods <- trend_methods_rfti()
  #   n <- length(methods)
  #   card_width <- if (n == 1) 1 else 1/2
  #   
  #   bslib::layout_column_wrap(
  #     width = card_width,
  #     !!!lapply(methods, function(m) {
  #       id <- method_to_trend_id("RFTI", m)
  #       bslib::card(
  #         full_screen = TRUE,
  #         bslib::card_header(paste("Method:", m)),
  #         plotOutput(id, height = 420)
  #       )
  #     })
  #   )
  # })
  # 
  # observe({
  #   # req(input$trend_metric_tab == "RFTI")
  #   
  #   df <- trend_df_rfti()
  #   req(nrow(df) > 0)
  #   
  #   methods <- trend_methods_rfti()
  #   req(length(methods) > 0)
  #   
  #   for (m in methods) {
  #     local({
  #       method <- m
  #       id <- method_to_trend_id("RFTI", method)
  #       
  #       output[[id]] <- renderPlot({
  #         d <- df %>% dplyr::filter(method == !!method)
  #         validate(need(nrow(d) > 0, "No data for this method."))
  #         make_trend_plot(d, y_label = "Reef Fish Thermal Index", management_change_year = 2018)
  #       })
  #       
  #       # outputOptions(output, id, suspendWhenHidden = FALSE)
  #     })
  #   }
  # })
  # 
  # # -----------------------------
  # # 4) Species block data (from all_data$trend_data_fish) for both metrics
  # # -----------------------------
  # 
  # fish_df_park_metric <- function(metric_code) {
  #   reactive({
  #     req(input$marine_park)
  #     
  #     all_data$trend_data_fish %>%
  #       dplyr::filter(marine_park == input$marine_park, metric == metric_code) %>%
  #       dplyr::mutate(
  #         date = as.Date(date),
  #         year = lubridate::year(date),
  #         zone = as.factor(zone),
  #         depth_class = as.factor(depth_class)
  #       )
  #   })
  # }
  # 
  # fish_df_b20  <- fish_df_park_metric("B20")
  # fish_df_rfti <- fish_df_park_metric("RFTI")
  # 
  # fish_methods_present <- function(fish_df_reactive) {
  #   reactive({
  #     df <- fish_df_reactive()
  #     req(nrow(df) > 0)
  #     sort(unique(df$method))
  #   })
  # }
  # 
  # fish_methods_b20  <- fish_methods_present(fish_df_b20)
  # fish_methods_rfti <- fish_methods_present(fish_df_rfti)
  # 
  # # -----------------------------
  # # 5) Blocks UI + renderers (B20 tab: year dropdown + map + abundance bars)
  # # -----------------------------
  # 
  # output$b20_blocks_ui <- renderUI({
  #   # req(input$trend_metric_tab == "B20")
  #   
  #   df <- fish_df_b20()
  #   validate(need(nrow(df) > 0, "No B20 species data available for this Marine Park."))
  #   
  #   methods <- fish_methods_b20()
  #   req(length(methods) > 0)
  #   
  #   card_width <- if (length(methods) == 1) 1 else 1/2
  #   
  #   bslib::layout_column_wrap(
  #     width = card_width,
  #     !!!lapply(methods, function(m) {
  #       yrs <- sort(unique(df$year[df$method == m]))
  #       req(length(yrs) > 0)
  #       
  #       bslib::card(
  #         full_screen = TRUE,
  #         bslib::card_header(paste("Method:", m)),
  #         
  #         tags$div(
  #           tags$strong("Choose year:"),
  #           selectInput(
  #             inputId = year_id_for_method("B20", m),
  #             label = NULL,
  #             choices = yrs,
  #             selected = max(yrs),
  #             width = "100%"
  #           )
  #         ),
  #         
  #         leafletOutput(map_id_for_method("B20", m), height = 320),
  #         plotOutput(plot_id_for_method("B20", m), height = 380)
  #       )
  #     })
  #   )
  # })
  # 
  # observe({
  #   # req(input$trend_metric_tab == "B20")
  #   
  #   df_all <- fish_df_b20()
  #   req(nrow(df_all) > 0)
  #   
  #   methods <- fish_methods_b20()
  #   req(length(methods) > 0)
  #   
  #   for (m in methods) {
  #     local({
  #       method <- m
  #       year_input_id <- year_id_for_method("B20", method)
  #       map_id <- map_id_for_method("B20", method)
  #       plot_id <- plot_id_for_method("B20", method)
  #       
  #       output[[plot_id]] <- renderPlot({
  #         req(input[[year_input_id]])
  #         d <- df_all %>% dplyr::filter(method == !!method, year == as.integer(input[[year_input_id]]))
  #         validate(need(nrow(d) > 0, "No B20 species data for this method/year."))
  #         make_b20_species_plot(d)
  #       })
  #       # outputOptions(output, plot_id, suspendWhenHidden = FALSE)
  #       
  #       output[[map_id]] <- renderLeaflet({
  #         base_map() %>%
  #           addProviderTiles("Esri.WorldImagery", group = "World Imagery") %>%
  #           addTiles(group = "Open Street Map")
  #       })
  #       # outputOptions(output, map_id, suspendWhenHidden = FALSE)
  #       
  #       # observeEvent(input[[year_input_id]], {
  #       #   # Hook for your B20 map layer update
  #       #   # d <- df_all %>% filter(method == method, year == as.integer(input[[year_input_id]]))
  #       #   # leafletProxy(map_id) %>% ...
  #       # }, ignoreInit = TRUE)
  #       
  #       observeEvent(
  #         list(input$marine_park, input[[year_input_id]]),
  #         {
  #           yr_raw <- input[[year_input_id]]
  #           req(!is.null(yr_raw), length(yr_raw) == 1, nzchar(as.character(yr_raw)))
  #           yr <- as.integer(yr_raw)
  #           
  #           update_tiles_on_map(
  #             map_id = map_id,
  #             marine_park = input$marine_park,
  #             method = method,
  #             metric = "B20",
  #             year = yr,
  #             metric_title = "B20"
  #           )
  #         },
  #         ignoreInit = FALSE
  #       )
  #       
  #     })
  #   }
  # })
  # 
  # # -----------------------------
  # # 6) Blocks UI + renderers (RFTI tab: year dropdown + map + log abundance bars + temp labels)
  # # -----------------------------
  # 
  # output$rfti_blocks_ui <- renderUI({
  #   # req(input$trend_metric_tab == "RFTI")
  #   
  #   df <- fish_df_rfti()
  #   validate(need(nrow(df) > 0, "No RFTI species data available for this Marine Park."))
  #   
  #   methods <- fish_methods_rfti()
  #   req(length(methods) > 0)
  #   
  #   card_width <- if (length(methods) == 1) 1 else 1/2
  #   
  #   bslib::layout_column_wrap(
  #     width = card_width,
  #     !!!lapply(methods, function(m) {
  #       yrs <- sort(unique(df$year[df$method == m]))
  #       req(length(yrs) > 0)
  #       
  #       bslib::card(
  #         full_screen = TRUE,
  #         bslib::card_header(paste("Method:", m)),
  #         
  #         tags$div(
  #           tags$strong("Choose year:"),
  #           selectInput(
  #             inputId = year_id_for_method("RFTI", m),
  #             label = NULL,
  #             choices = yrs,
  #             selected = max(yrs),
  #             width = "100%"
  #           )
  #         ),
  #         
  #         leafletOutput(map_id_for_method("RFTI", m), height = 320),
  #         plotOutput(plot_id_for_method("RFTI", m), height = 380)
  #       )
  #     })
  #   )
  # })
  # 
  # observe({
  #   # req(input$trend_metric_tab == "RFTI")
  #   
  #   df_all <- fish_df_rfti()
  #   req(nrow(df_all) > 0)
  #   
  #   methods <- fish_methods_rfti()
  #   req(length(methods) > 0)
  #   
  #   for (m in methods) {
  #     local({
  #       method <- m
  #       year_input_id <- year_id_for_method("RFTI", method)
  #       map_id <- map_id_for_method("RFTI", method)
  #       plot_id <- plot_id_for_method("RFTI", method)
  #       
  #       output[[plot_id]] <- renderPlot({
  #         req(input[[year_input_id]])
  #         d <- df_all %>% dplyr::filter(method == !!method, year == as.integer(input[[year_input_id]]))
  #         validate(need(nrow(d) > 0, "No RFTI species data for this method/year."))
  #         make_rfti_species_plot(d)
  #       })
  #       # outputOptions(output, plot_id, suspendWhenHidden = FALSE)
  #       
  #       output[[map_id]] <- renderLeaflet({
  #         base_map() %>%
  #           addProviderTiles("Esri.WorldImagery", group = "World Imagery") %>%
  #           addTiles(group = "Open Street Map")
  #       })
  #       # outputOptions(output, map_id, suspendWhenHidden = FALSE)
  #       
  #       # observeEvent(input[[year_input_id]], {
  #       #   # Hook for your RFTI map layer update
  #       #   # d <- df_all %>% filter(method == method, year == as.integer(input[[year_input_id]]))
  #       #   # leafletProxy(map_id) %>% ...
  #       # }, ignoreInit = TRUE)
  #       
  #       observeEvent(
  #         list(input$marine_park, input[[year_input_id]]),
  #         {
  #           yr_raw <- input[[year_input_id]]
  #           req(!is.null(yr_raw), length(yr_raw) == 1, nzchar(as.character(yr_raw)))
  #           yr <- as.integer(yr_raw)
  #           
  #           update_tiles_on_map(
  #             map_id = map_id,
  #             marine_park = input$marine_park,
  #             method = method,
  #             metric = "RFTI",
  #             year = yr,
  #             metric_title = "RFTI"
  #           )
  #         },
  #         ignoreInit = FALSE
  #       )
  #       
  #       
  #     })
  #   }
  # })
  # 
  # 
  # 
  # # First get rasters 
  # get_tiles_for <- function(marine_park, method, metric, year = NULL) {
  #   
  #   df <- all_data$raster_tags %>%
  #     dplyr::filter(
  #       marine_park == !!marine_park,
  #       method == !!method,
  #       metric == !!metric
  #     )
  #   
  #   # only filter by year if provided and length 1
  #   if (!is.null(year) && length(year) == 1 && !is.na(year)) {
  #     df <- df %>% dplyr::filter(year == !!year)
  #   }
  #   
  #   df
  # }
  # 
  # update_tiles_on_map <- function(map_id, marine_park, method, metric, year = NULL, metric_title = NULL) {
  #   
  #   tiles <- get_tiles_for(marine_park, method, metric, year)
  #   
  #   message("tiles n = ", nrow(tiles))
  #   if (nrow(tiles) > 0) message("estimates: ", paste(unique(tiles$estimate), collapse = ", "))
  #   if (nrow(tiles) > 0) message("url: ", unique(tiles$tile_service_url)[1])
  #   
  #   proxy <- leaflet::leafletProxy(map_id) %>%
  #     leaflet::clearGroup("Probability") %>%
  #     leaflet::clearGroup("Error") %>%
  #     leaflet::removeControl(layerId = "prob_leg") %>%
  #     leaflet::removeControl(layerId = "err_leg")
  #   
  #   if (is.null(tiles) || nrow(tiles) == 0) return(invisible(NULL))
  #   
  #   # --- Probability ---
  #   prob <- tiles %>% dplyr::filter(tolower(estimate) == "probability")
  #   if (nrow(prob) > 0) {
  #     url <- unique(prob$tile_service_url)[1]
  #     
  #     proxy <- proxy %>%
  #       leaflet::addTiles(
  #         urlTemplate = url,
  #         attribution = "© GlobalArchive",
  #         group = "Probability"
  #       ) %>%
  #       leaflet::showGroup("Probability")
  #   }
  #   
  #   # --- Error ---
  #   err <- tiles %>% dplyr::filter(tolower(estimate) == "error")
  #   if (nrow(err) > 0) {
  #     url <- unique(err$tile_service_url)[1]
  #     
  #     proxy <- proxy %>%
  #       leaflet::addTiles(
  #         urlTemplate = url,
  #         attribution = "© GlobalArchive",
  #         group = "Error"
  #       ) %>%
  #       leaflet::hideGroup("Error")
  #   }
  #   
  #   invisible(NULL)
  # }
  
  
  # -----------------------------
  # STATE & TREND (Tabbed: B20 / RFTI)  ✅ drop-in replacement chunk
  # -----------------------------
  
  # ---- Helpers: safe ids per method ----
  method_safe <- function(x) gsub("[^A-Za-z0-9]+", "_", x)
  
  # Trend-line plot output ids
  method_to_trend_id <- function(metric, method) {
    paste0("trend_", metric, "_", method_safe(method))
  }
  
  # ---- helper: scale point radius by abundance ----
  radius_from_abundance <- function(x,
                                    min_radius = 4,
                                    max_radius = 14,
                                    clamp_quantile = 0.95) {
    
    x <- suppressWarnings(as.numeric(x))
    x[!is.finite(x)] <- NA_real_
    
    # avoid single crazy outliers
    cap <- stats::quantile(x, clamp_quantile, na.rm = TRUE)
    x <- pmin(x, cap)
    
    scales::rescale(
      x,
      to = c(min_radius, max_radius),
      from = range(x, na.rm = TRUE)
    )
  }
  
  # Block ids (year/map/plot) per metric+method
  year_id_for_method <- function(metric, m) paste0("year_", metric, "_", method_safe(m))
  map_id_for_method  <- function(metric, m) paste0("map_",  metric, "_", method_safe(m))
  plot_id_for_method <- function(metric, m) paste0("plot_", metric, "_", method_safe(m))
  
  # ---- Plot makers (DO NOT reference input inside these) ----
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
      labs(x = "Year", y = y_label) +
      theme_classic(base_size = 11) +
      theme(
        legend.position = "top",
        legend.direction = "horizontal",
        strip.background = element_blank(),
        strip.text = element_text(face = "italic"),
        axis.title.y = element_text(margin = margin(r = 10))
      )
  }
  
  # B20 species facet plot:
  # bar height = abundance (value_a), error bars = SE (value_b)
  make_b20_species_plot <- function(df) {
    ggplot(df, aes(x = reorder(taxa, value_a), y = value_a)) +
      geom_col() +
      geom_errorbar(
        aes(ymin = pmax(0, value_a - value_b), ymax = value_a + value_b),
        width = 0.2, linewidth = 0.4
      ) +
      coord_flip() +
      facet_grid(depth_class ~ zone, scales = "free_y") +
      scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
      labs(x = NULL, y = "Abundance") +
      theme_minimal(base_size = 11) +
      theme(panel.grid.minor = element_blank(),
            strip.text = element_text(face = "bold"))
  }
  
  # RFTI species facet plot:
  # bar height = Log Abundance (value_b), label = Temp (value_a)
  make_rfti_species_plot <- function(df) {
    ggplot(df, aes(x = reorder(taxa, value_b), y = value_b)) +
      geom_col() +
      coord_flip() +
      geom_text(aes(label = sprintf("%.1f\u00B0C", value_a)),
                hjust = -0.05, size = 3) +
      facet_grid(depth_class ~ zone, scales = "free_y") +
      scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
      labs(x = NULL, y = "Log Abundance") +
      theme_minimal(base_size = 11) +
      theme(panel.grid.minor = element_blank(),
            strip.text = element_text(face = "bold"))
  }
  
  # -----------------------------
  # 1) Trend-line data (from all_data$trend_data) for both metrics
  # -----------------------------
  
  trend_df_park_metric <- function(metric_code) {
    reactive({
      req(input$marine_park)
      
      all_data$trend_data %>%
        dplyr::filter(marine_park == input$marine_park, metric == metric_code) %>%
        dplyr::mutate(
          date = as.Date(date),
          year = lubridate::year(date),
          zone = factor(zone, levels = names(zone_cols)),
          depth_class = factor(depth_class, levels = c("0-30 m", "30-70 m"))
        ) %>%
        dplyr::filter(!is.na(zone))
    })
  }
  
  trend_df_b20  <- trend_df_park_metric("B20")
  trend_df_rfti <- trend_df_park_metric("RFTI")
  
  trend_methods_present <- function(trend_df_reactive) {
    reactive({
      df <- trend_df_reactive()
      req(nrow(df) > 0)
      sort(unique(df$method))
    })
  }
  
  trend_methods_b20  <- trend_methods_present(trend_df_b20)
  trend_methods_rfti <- trend_methods_present(trend_df_rfti)
  
  # -----------------------------
  # 2) Trend plots UI + renderers (B20 tab)
  # -----------------------------
  
  output$trend_plots_b20_ui <- renderUI({
    df <- trend_df_b20()
    validate(need(nrow(df) > 0, "No B20 trend data available for this Marine Park."))
    
    methods <- trend_methods_b20()
    card_width <- if (length(methods) == 1) 1 else 1/2
    
    bslib::layout_column_wrap(
      width = card_width,
      !!!lapply(methods, function(m) {
        id <- method_to_trend_id("B20", m)
        bslib::card(
          full_screen = TRUE,
          bslib::card_header(paste("Method:", m)),
          plotOutput(id, height = 420)
        )
      })
    )
  })
  
  observe({
    df <- trend_df_b20()
    req(nrow(df) > 0)
    
    methods <- trend_methods_b20()
    req(length(methods) > 0)
    
    for (m in methods) {
      local({
        method <- m
        id <- method_to_trend_id("B20", method)
        
        output[[id]] <- renderPlot({
          d <- df %>% dplyr::filter(method == !!method)
          validate(need(nrow(d) > 0, "No data for this method."))
          make_trend_plot(d, y_label = "Large Reef Fish Index* (B20*)", management_change_year = 2018)
        })
      })
    }
  })
  
  # -----------------------------
  # 3) Trend plots UI + renderers (RFTI tab)
  # -----------------------------
  
  output$trend_plots_rfti_ui <- renderUI({
    df <- trend_df_rfti()
    validate(need(nrow(df) > 0, "No RFTI trend data available for this Marine Park."))
    
    methods <- trend_methods_rfti()
    card_width <- if (length(methods) == 1) 1 else 1/2
    
    bslib::layout_column_wrap(
      width = card_width,
      !!!lapply(methods, function(m) {
        id <- method_to_trend_id("RFTI", m)
        bslib::card(
          full_screen = TRUE,
          bslib::card_header(paste("Method:", m)),
          plotOutput(id, height = 420)
        )
      })
    )
  })
  
  observe({
    df <- trend_df_rfti()
    req(nrow(df) > 0)
    
    methods <- trend_methods_rfti()
    req(length(methods) > 0)
    
    for (m in methods) {
      local({
        method <- m
        id <- method_to_trend_id("RFTI", method)
        
        output[[id]] <- renderPlot({
          d <- df %>% dplyr::filter(method == !!method)
          validate(need(nrow(d) > 0, "No data for this method."))
          make_trend_plot(d, y_label = "Reef Fish Thermal Index", management_change_year = 2018)
        })
      })
    }
  })
  
  # -----------------------------
  # 4) Species block data (from all_data$trend_data_fish) for both metrics
  # -----------------------------
  
  fish_df_park_metric <- function(metric_code) {
    reactive({
      req(input$marine_park)
      
      all_data$trend_data_fish %>%
        dplyr::filter(marine_park == input$marine_park, metric == metric_code) %>%
        dplyr::mutate(
          date = as.Date(date),
          year = lubridate::year(date),
          zone = as.factor(zone),
          depth_class = as.factor(depth_class)
        )
    })
  }
  
  fish_df_b20  <- fish_df_park_metric("B20")
  fish_df_rfti <- fish_df_park_metric("RFTI")
  
  fish_methods_present <- function(fish_df_reactive) {
    reactive({
      df <- fish_df_reactive()
      req(nrow(df) > 0)
      sort(unique(df$method))
    })
  }
  
  fish_methods_b20  <- fish_methods_present(fish_df_b20)
  fish_methods_rfti <- fish_methods_present(fish_df_rfti)
  
  # -----------------------------
  # 5) Blocks UI + renderers (B20 tab: year dropdown + map + abundance bars)
  # -----------------------------
  
  output$b20_blocks_ui <- renderUI({
    df <- fish_df_b20()
    validate(need(nrow(df) > 0, "No B20 species data available for this Marine Park."))
    
    methods <- fish_methods_b20()
    req(length(methods) > 0)
    
    card_width <- if (length(methods) == 1) 1 else 1/2
    
    bslib::layout_column_wrap(
      width = card_width,
      !!!lapply(methods, function(m) {
        yrs <- sort(unique(df$year[df$method == m]))
        req(length(yrs) > 0)
        
        bslib::card(
          full_screen = TRUE,
          bslib::card_header(paste("Method:", m)),
          tags$div(
            tags$strong("Choose year:"),
            selectInput(
              inputId = year_id_for_method("B20", m),
              label = NULL,
              choices = yrs,
              selected = max(yrs),
              width = "100%"
            )
          ),
          leafletOutput(map_id_for_method("B20", m), height = 320),
          plotOutput(plot_id_for_method("B20", m), height = 380)
        )
      })
    )
  })
  
  observe({
    df_all <- fish_df_b20()
    req(nrow(df_all) > 0)
    
    methods <- fish_methods_b20()
    req(length(methods) > 0)
    
    for (m in methods) {
      local({
        method <- m
        year_input_id <- year_id_for_method("B20", method)
        map_id <- map_id_for_method("B20", method)
        plot_id <- plot_id_for_method("B20", method)
        
        output[[plot_id]] <- renderPlot({
          yr_raw <- input[[year_input_id]]
          req(!is.null(yr_raw), length(yr_raw) == 1, nzchar(as.character(yr_raw)))
          yr <- as.integer(yr_raw)
          
          d <- df_all %>% dplyr::filter(method == !!method, year == !!yr)
          validate(need(nrow(d) > 0, "No B20 species data for this method/year."))
          make_b20_species_plot(d)
        })
        
        output[[map_id]] <- renderLeaflet({
          base_map() %>%
            addProviderTiles("Esri.WorldImagery", group = "World Imagery") %>%
            addTiles(group = "Open Street Map")
        })
        
        observeEvent(
          list(input$marine_park, input[[year_input_id]]),
          {
            yr_raw <- input[[year_input_id]]
            req(!is.null(yr_raw), length(yr_raw) == 1, nzchar(as.character(yr_raw)))
            yr <- as.integer(yr_raw)
            
            update_tiles_on_map(
              map_id = map_id,
              marine_park = input$marine_park,
              method = method,
              metric = "B20",
              year = yr,
              metric_title = "B20"
            )
          },
          ignoreInit = FALSE
        )
      })
    }
  })
  
  # -----------------------------
  # 6) Blocks UI + renderers (RFTI tab: year dropdown + map + log abundance bars + temp labels)
  # -----------------------------
  
  output$rfti_blocks_ui <- renderUI({
    df <- fish_df_rfti()
    validate(need(nrow(df) > 0, "No RFTI species data available for this Marine Park."))
    
    methods <- fish_methods_rfti()
    req(length(methods) > 0)
    
    card_width <- if (length(methods) == 1) 1 else 1/2
    
    bslib::layout_column_wrap(
      width = card_width,
      !!!lapply(methods, function(m) {
        yrs <- sort(unique(df$year[df$method == m]))
        req(length(yrs) > 0)
        
        bslib::card(
          full_screen = TRUE,
          bslib::card_header(paste("Method:", m)),
          tags$div(
            tags$strong("Choose year:"),
            selectInput(
              inputId = year_id_for_method("RFTI", m),
              label = NULL,
              choices = yrs,
              selected = max(yrs),
              width = "100%"
            )
          ),
          leafletOutput(map_id_for_method("RFTI", m), height = 320),
          plotOutput(plot_id_for_method("RFTI", m), height = 380)
        )
      })
    )
  })
  
  observe({
    df_all <- fish_df_rfti()
    req(nrow(df_all) > 0)
    
    methods <- fish_methods_rfti()
    req(length(methods) > 0)
    
    for (m in methods) {
      local({
        method <- m
        year_input_id <- year_id_for_method("RFTI", method)
        map_id <- map_id_for_method("RFTI", method)
        plot_id <- plot_id_for_method("RFTI", method)
        
        output[[plot_id]] <- renderPlot({
          yr_raw <- input[[year_input_id]]
          req(!is.null(yr_raw), length(yr_raw) == 1, nzchar(as.character(yr_raw)))
          yr <- as.integer(yr_raw)
          
          d <- df_all %>% dplyr::filter(method == !!method, year == !!yr)
          validate(need(nrow(d) > 0, "No RFTI species data for this method/year."))
          make_rfti_species_plot(d)
        })
        
        output[[map_id]] <- renderLeaflet({
          base_map() %>%
            addProviderTiles("Esri.WorldImagery", group = "World Imagery") %>%
            addTiles(group = "Open Street Map")
        })
        
        observeEvent(
          list(input$marine_park, input[[year_input_id]]),
          {
            yr_raw <- input[[year_input_id]]
            req(!is.null(yr_raw), length(yr_raw) == 1, nzchar(as.character(yr_raw)))
            yr <- as.integer(yr_raw)
            
            update_tiles_on_map(
              map_id = map_id,
              marine_park = input$marine_park,
              method = method,
              metric = "RFTI",
              year = yr,
              metric_title = "RFTI"
            )
          },
          ignoreInit = FALSE
        )
      })
    }
  })
  
  # -----------------------------
  # 7) Raster tiles: lookup + update (Probability/Error)
  # -----------------------------
  
  get_tiles_for <- function(marine_park, method, metric, year = NULL) {
    df <- all_data$raster_tags %>%
      dplyr::filter(
        marine_park == !!marine_park,
        method == !!method,
        metric == !!metric
      )
    
    if (!is.null(year) && length(year) == 1 && !is.na(year)) {
      df <- df %>% dplyr::filter(year == !!year)
    }
    
    df
  }
  
  update_tiles_on_map <- function(map_id, marine_park, method, metric, year = NULL, metric_title = NULL) {
    
    tiles <- get_tiles_for(marine_park, method, metric, year)
    
    message("CALL update_tiles_on_map -> map_id=", map_id,
            " park=", marine_park,
            " method=", method,
            " metric=", metric,
            " year=", year)
    
    message("tiles n = ", nrow(tiles))
    if (nrow(tiles) > 0) message("estimates: ", paste(unique(tiles$estimate), collapse = ", "))
    if (nrow(tiles) > 0) message("url: ", unique(tiles$tile_service_url)[1])
    
    proxy <- leaflet::leafletProxy(map_id, session = session) %>%
      leaflet::clearGroup("Probability") %>%
      leaflet::clearGroup("Error") %>%
      leaflet::removeControl(layerId = "tile_url_debug")
    
    if (is.null(tiles) || nrow(tiles) == 0) return(invisible(NULL))
    
    # --- Probability ---
    prob <- tiles %>% dplyr::filter(tolower(estimate) == "probability")
    if (nrow(prob) > 0) {
      url <- unique(prob$tile_service_url)[1]
      
      proxy <- proxy %>%
        leaflet::addTiles(
          urlTemplate = url,
          attribution = "© GlobalArchive",
          group = "Probability"
        ) %>%
        leaflet::showGroup("Probability") %>%
        leaflet::addControl(
          html = htmltools::HTML(paste0(
            "<div style='background:white; padding:6px 8px; border:1px solid #999; font-size:12px;'>",
            "<b>Tile URL</b><br/>",
            htmltools::htmlEscape(url),
            "</div>"
          )),
          position = "bottomleft",
          layerId = "tile_url_debug"
        )
    }
    
    # --- Error ---
    err <- tiles %>% dplyr::filter(tolower(estimate) == "error")
    if (nrow(err) > 0) {
      url <- unique(err$tile_service_url)[1]
      
      proxy <- proxy %>%
        leaflet::addTiles(
          urlTemplate = url,
          attribution = "© GlobalArchive",
          group = "Error"
        ) %>%
        leaflet::hideGroup("Error")  # hidden by default
    }
    
    invisible(NULL)
  }
  
  # # =========================
  # # DUMMY ADD-ONS FOR:
  # #  - Species Richness maps (richness_maps_ui)
  # #  - Species abundance dummy map (species_abundance_map)
  # #  - Length histogram (length_hist)
  # # =========================
  # 
  # # --- helper: safe output id per metric+method (doesn't collide with your existing method_to_id) ---
  # metric_method_to_id <- function(metric, method) {
  #   paste0(metric, "_", gsub("[^A-Za-z0-9]+", "_", method))
  # }
  # 
  # # -------------------------
  # # 1) SPECIES RICHNESS MAPS (dummy, same layout pattern as Total abundance)
  # # -------------------------
  # 
  # species_richness_points <- reactive({
  #   req(input$marine_park)
  #   
  #   df <- all_data$metric_bubble_data %>%
  #     dplyr::filter(marine_park %in% input$marine_park)
  #   
  #   # Try common richness metric names; otherwise fall back to total_abundance
  #   metric_candidates <- c("species_richness", "richness", "sr", "Species Richness", "S")
  #   metric_use <- NULL
  #   
  #   if ("metric" %in% names(df)) {
  #     metric_use <- metric_candidates[metric_candidates %in% unique(df$metric)][1]
  #     if (is.na(metric_use) || is.null(metric_use)) metric_use <- NULL
  #   }
  #   
  #   if (is.null(metric_use)) {
  #     # fallback so dummy always draws
  #     metric_use <- "total_abundance"
  #   }
  #   
  #   df %>%
  #     dplyr::filter(metric %in% metric_use) %>%
  #     dplyr::rename(latitude = latitude_dd, longitude = longitude_dd) %>%
  #     # dummy method so you get at least one map
  #     dplyr::mutate(method = "stereo-BRUV") %>%
  #     dplyr::filter(!is.na(longitude), !is.na(latitude))
  # })
  # 
  # methods_present_richness <- reactive({
  #   pts <- species_richness_points()
  #   req(nrow(pts) > 0)
  #   
  #   m <- sort(unique(as.character(pts$method)))
  #   m <- m[m %in% names(method_cols)]
  #   if (length(m) == 0) m <- "stereo-BRUV"
  #   m
  # })
  # 
  # output$richness_maps_ui <- renderUI({
  #   methods <- methods_present_richness()
  #   req(length(methods) > 0)
  #   
  #   n <- length(methods)
  #   cw <- dplyr::case_when(n == 1 ~ 12, n == 2 ~ 6, TRUE ~ 4)
  #   
  #   bslib::layout_columns(
  #     col_widths = rep(cw, n),
  #     !!!lapply(methods, function(m) {
  #       div(
  #         style = "padding: 0.5rem;",
  #         h4(m, style = "margin: 0 0 0.5rem 0;"),
  #         leafletOutput(metric_method_to_id("richness", m), height = "68vh")
  #       )
  #     })
  #   )
  # })
  # 
  # # render richness maps when tab is active
  # observeEvent(
  #   list(input$obs_tabs, input$marine_park, methods_present_richness()),
  #   {
  #     if (!identical(input$obs_tabs, "Species Richness")) return()
  #     req(input$marine_park)
  #     
  #     pts <- species_richness_points()
  #     req(nrow(pts) > 0)
  #     
  #     lng1 <- min(pts$longitude, na.rm = TRUE)
  #     lat1 <- min(pts$latitude,  na.rm = TRUE)
  #     lng2 <- max(pts$longitude, na.rm = TRUE)
  #     lat2 <- max(pts$latitude,  na.rm = TRUE)
  #     req(is.finite(lng1), is.finite(lat1), is.finite(lng2), is.finite(lat2))
  #     
  #     methods <- methods_present_richness()
  #     
  #     for (m in methods) {
  #       id <- metric_method_to_id("richness", m)
  #       
  #       output[[id]] <- renderLeaflet({
  #         base_map() %>%
  #           addProviderTiles("Esri.WorldImagery", group = "World Imagery") %>%
  #           addTiles(group = "Open Street Map") %>%
  #           fitBounds(lng1, lat1, lng2, lat2) %>%
  #           clearGroup("Sampling locations") %>%
  #           addCircleMarkers(
  #             data = pts,
  #             lng = ~longitude, lat = ~latitude,
  #             radius = 6, stroke = TRUE, weight = 1,
  #             fillOpacity = 0.9, color = "white",
  #             group = "Sampling locations",
  #             options = pathOptions(pane = "points")
  #           )
  #       })
  #     }
  #   },
  #   ignoreInit = TRUE
  # )
  # 
  # # -------------------------
  # # 2) SPECIES ABUNDANCE TAB: DUMMY MAP (uses your existing map_points())
  # # -------------------------
  # 
  # species_abundance_map_points <- reactive({
  #   pts <- map_points()     # your existing reactive in server
  #   req(nrow(pts) > 0)
  #   pts
  # })
  # 
  # output$species_abundance_map <- renderLeaflet({
  #   pts <- species_abundance_map_points()
  #   req(nrow(pts) > 0)
  #   
  #   lng1 <- min(pts$longitude, na.rm = TRUE)
  #   lat1 <- min(pts$latitude,  na.rm = TRUE)
  #   lng2 <- max(pts$longitude, na.rm = TRUE)
  #   lat2 <- max(pts$latitude,  na.rm = TRUE)
  #   req(is.finite(lng1), is.finite(lat1), is.finite(lng2), is.finite(lat2))
  #   
  #   base_map() %>%
  #     addProviderTiles("Esri.WorldImagery", group = "World Imagery") %>%
  #     addTiles(group = "Open Street Map") %>%
  #     fitBounds(lng1, lat1, lng2, lat2)
  # })
  # 
  # observeEvent(species_abundance_map_points(), {
  #   pts <- species_abundance_map_points()
  #   req(nrow(pts) > 0)
  #   
  #   lng1 <- min(pts$longitude, na.rm = TRUE)
  #   lat1 <- min(pts$latitude,  na.rm = TRUE)
  #   lng2 <- max(pts$longitude, na.rm = TRUE)
  #   lat2 <- max(pts$latitude,  na.rm = TRUE)
  #   req(is.finite(lng1), is.finite(lat1), is.finite(lng2), is.finite(lat2))
  #   
  #   # if method_cols doesn't include "Other", keep it safe
  #   pts <- pts %>% dplyr::mutate(method = ifelse(method %in% names(method_cols), method, names(method_cols)[1]))
  #   
  #   leafletProxy("species_abundance_map", data = pts) %>%
  #     clearGroup("Sampling locations") %>%
  #     addCircleMarkers(
  #       lng = ~longitude, lat = ~latitude,
  #       radius = 6, stroke = TRUE, weight = 1,
  #       fillOpacity = 0.9, color = "white",
  #       fillColor = ~method_cols[method],
  #       popup = ~popup,
  #       group = "Sampling locations",
  #       options = pathOptions(pane = "points")
  #     ) %>%
  #     fitBounds(lng1, lat1, lng2, lat2)
  # }, ignoreInit = FALSE)
  # 
  # # -------------------------
  # # 3) LENGTH TAB: DUMMY HISTOGRAM (length_hist)
  # # -------------------------
  # 
  # length_values_dummy <- reactive({
  #   # Try likely places for lengths, else dummy random values.
  #   candidates <- list(
  #     all_data$bruv_length,
  #     all_data$dov_length,
  #     all_data$length,
  #     all_data$length_data
  #   )
  #   
  #   df <- NULL
  #   for (x in candidates) {
  #     if (!is.null(x) && inherits(x, "data.frame") && nrow(x) > 0) { df <- x; break }
  #   }
  #   
  #   if (is.null(df)) {
  #     return(stats::rnorm(500, mean = 300, sd = 80))
  #   }
  #   
  #   length_col <- dplyr::case_when(
  #     "length_mm" %in% names(df) ~ "length_mm",
  #     "Length_mm" %in% names(df) ~ "Length_mm",
  #     "length"    %in% names(df) ~ "length",
  #     "Length"    %in% names(df) ~ "Length",
  #     TRUE ~ NA_character_
  #   )
  #   
  #   if (is.na(length_col)) {
  #     return(stats::rnorm(500, mean = 300, sd = 80))
  #   }
  #   
  #   v <- suppressWarnings(as.numeric(df[[length_col]]))
  #   v <- v[is.finite(v)]
  #   
  #   if (length(v) < 10) stats::rnorm(500, mean = 300, sd = 80) else v
  # })
  # 
  # output$length_hist <- renderPlot({
  #   v <- length_values_dummy()
  #   req(length(v) > 0)
  #   
  #   ggplot(data.frame(length_mm = v), aes(x = length_mm)) +
  #     geom_histogram(bins = 30, colour = "black", linewidth = 0.2) +
  #     labs(
  #       title = "Length distribution",
  #       x = "Length (mm)",
  #       y = "Count"
  #     ) +
  #     theme_minimal(base_size = 12) +
  #     theme(panel.grid.minor = element_blank())
  # })
  
  # =========================================================
  # ASSEMBLAGE + RICHNESS + SPECIES (TWO MAPS EACH)  + LENGTH HIST
  # =========================================================
  
  # ---- helpers ----
  safe_id <- function(x) gsub("[^A-Za-z0-9]+", "_", x)
  
  pick_two_methods <- function(pts = NULL) {
    # prefer methods found in the data; otherwise default to first two in method_cols
    defaults <- intersect(c("stereo-BRUV", "UVC", "stereo-ROV"), names(method_cols))
    if (length(defaults) < 2) defaults <- head(names(method_cols), 2)
    
    if (!is.null(pts) && "method" %in% names(pts)) {
      present <- pts %>% dplyr::distinct(method) %>% dplyr::pull(method)
      present <- intersect(present, names(method_cols))
      if (length(present) >= 2) return(present[1:2])
      if (length(present) == 1) return(unique(c(present, defaults))[1:2])
    }
    defaults[1:2]
  }
  
  metric_method_to_id <- function(metric, method) paste0(metric, "_", safe_id(method))
  
  # ---- function to build 2-map UI ----
  two_map_ui <- function(metric_key, methods) {
    bslib::layout_columns(
      col_widths = c(6, 6),
      !!!lapply(methods, function(m) {
        bslib::card(
          bslib::card_header(m),
          leafletOutput(metric_method_to_id(metric_key, m), height = "68vh")
        )
      })
    )
  }
  
  # ---- function to render + update 2 maps ----
  render_two_maps <- function(metric_key,
                              methods,
                              pts,
                              bounds = NULL,
                              popup_col = NULL,
                              abundance_col = NULL) {
    
    req(length(methods) == 2)
    
    if (!is.null(abundance_col) && abundance_col %in% names(pts)) {
      pts <- pts %>%
        dplyr::mutate(
          radius = radius_from_abundance(.data[[abundance_col]])
        )
    } else {
      pts <- pts %>% dplyr::mutate(radius = 6)
    }
    
    lng1 <- min(pts$longitude, na.rm = TRUE)
    lat1 <- min(pts$latitude,  na.rm = TRUE)
    lng2 <- max(pts$longitude, na.rm = TRUE)
    lat2 <- max(pts$latitude,  na.rm = TRUE)
    req(is.finite(lng1), is.finite(lat1), is.finite(lng2), is.finite(lat2))
    
    for (m in methods) {
      id <- metric_method_to_id(metric_key, m)
      
      output[[id]] <- renderLeaflet({
        base_map() %>%
          addProviderTiles("Esri.WorldImagery", group = "World Imagery") %>%
          addTiles(group = "Open Street Map") %>%
          fitBounds(lng1, lat1, lng2, lat2)
      })
      
      dat_m <- pts %>% dplyr::filter(method == m)
      
      leafletProxy(id, data = dat_m) %>%
        clearGroup("Sampling locations") %>%
        addCircleMarkers(
          lng = ~longitude,
          lat = ~latitude,
          radius = ~radius,     # ⭐ abundance-scaled
          stroke = TRUE,
          weight = 1,
          fillOpacity = 0.85,
          color = "white",
          fillColor = method_cols[[m]],
          popup = if (!is.null(popup_col) && popup_col %in% names(dat_m))
            dat_m[[popup_col]] else NULL,
          group = "Sampling locations",
          options = pathOptions(pane = "points")
        )
    }
  }
  
  # =========================================================
  # 1) TOTAL ABUNDANCE (TWO MAPS)
  # =========================================================
  
  total_abundance_points_2 <- reactive({
    req(input$marine_park)
    
    df <- all_data$metric_bubble_data %>%
      dplyr::filter(marine_park %in% input$marine_park, metric %in% "total_abundance") %>%
      dplyr::rename(latitude = latitude_dd, longitude = longitude_dd) %>%
      dplyr::filter(!is.na(longitude), !is.na(latitude))
    
    # If method isn't present in this dataset, duplicate into 2 methods
    if (!"method" %in% names(df)) {
      methods <- pick_two_methods()
      df <- tidyr::crossing(df, method = methods)
    } else {
      # standardise to your method_cols names if needed (optional)
      df <- df %>%
        dplyr::mutate(
          method = dplyr::case_when(
            stringr::str_detect(tolower(method), "bruv") ~ "stereo-BRUV",
            stringr::str_detect(tolower(method), "uvc")  ~ "UVC",
            stringr::str_detect(tolower(method), "rov")  ~ "stereo-ROV",
            TRUE ~ method
          )
        )
    }
    
    df %>%
      dplyr::mutate(method = ifelse(method %in% names(method_cols), method, pick_two_methods()[1]))
  })
  
  methods_total_abundance_2 <- reactive({
    pts <- total_abundance_points_2()
    req(nrow(pts) > 0)
    pick_two_methods(pts)
  })
  
  output$assemblage_maps_ui <- renderUI({
    methods <- methods_total_abundance_2()
    req(length(methods) == 2)
    two_map_ui("abundance", methods)
  })
  
  observeEvent(
    list(input$obs_tabs, input$marine_park, total_abundance_points_2()),
    {
      if (!identical(input$obs_tabs, "Total abundance")) return()
      pts <- total_abundance_points_2()
      req(nrow(pts) > 0)
      
      methods <- methods_total_abundance_2()
      render_two_maps(
        metric_key = "abundance",
        methods    = methods,
        pts        = pts,
        abundance_col = "value"   # or "total_abundance"
      )
    },
    ignoreInit = TRUE
  )
  
  # =========================================================
  # 2) SPECIES RICHNESS (TWO MAPS)
  # =========================================================
  
  species_richness_points_2 <- reactive({
    req(input$marine_park)
    
    df_all <- all_data$metric_bubble_data %>%
      dplyr::filter(marine_park %in% input$marine_park)
    
    # find a richness metric if present; fallback to total_abundance so it always draws
    metric_use <- c("species_richness", "richness", "Species Richness")[
      c("species_richness", "richness", "Species Richness") %in% unique(df_all$metric)
    ][1]
    if (is.na(metric_use) || is.null(metric_use)) metric_use <- "total_abundance"
    
    df <- df_all %>%
      dplyr::filter(metric %in% metric_use) %>%
      dplyr::rename(latitude = latitude_dd, longitude = longitude_dd) %>%
      dplyr::filter(!is.na(longitude), !is.na(latitude))
    
    if (!"method" %in% names(df)) {
      methods <- pick_two_methods()
      df <- tidyr::crossing(df, method = methods)
    }
    
    df %>%
      dplyr::mutate(method = ifelse(method %in% names(method_cols), method, pick_two_methods()[1]))
  })
  
  methods_richness_2 <- reactive({
    pts <- species_richness_points_2()
    req(nrow(pts) > 0)
    pick_two_methods(pts)
  })
  
  output$richness_maps_ui <- renderUI({
    methods <- methods_richness_2()
    req(length(methods) == 2)
    two_map_ui("richness", methods)
  })
  
  observeEvent(
    list(input$obs_tabs, input$marine_park, species_richness_points_2()),
    {
      if (!identical(input$obs_tabs, "Species Richness")) return()
      pts <- species_richness_points_2()
      req(nrow(pts) > 0)
      
      methods <- methods_richness_2()
      render_two_maps(
        metric_key = "richness",
        methods    = methods,
        pts        = pts,
        abundance_col = "value"   # richness value
      )
    },
    ignoreInit = TRUE
  )
  
  # =========================================================
  # 3) SPECIES ABUNDANCE (TWO MAPS)  [requires UI change noted above]
  # =========================================================
  
  species_points_2 <- reactive({
    pts <- map_points()
    req(nrow(pts) > 0)
    
    # ensure method is valid + keep only the two methods we will plot
    methods <- pick_two_methods(pts)
    pts %>%
      dplyr::filter(method %in% methods)
  })
  
  methods_species_2 <- reactive({
    pts <- species_points_2()
    req(nrow(pts) > 0)
    pick_two_methods(pts)
  })
  
  output$species_abundance_maps_ui <- renderUI({
    methods <- methods_species_2()
    req(length(methods) == 2)
    two_map_ui("species", methods)
  })
  
  observeEvent(
    list(input$obs_tabs, input$marine_park, species_points_2()),
    {
      if (!identical(input$obs_tabs, "Investigate a species abundance data")) return()
      pts <- species_points_2()
      req(nrow(pts) > 0)
      
      pts <- pts %>% dplyr::mutate(abundance = runif(n(), 1, 50))
      
      methods <- methods_species_2()
      render_two_maps(
        metric_key = "species",
        methods    = methods,
        pts        = pts,
        popup_col  = "popup",
        abundance_col = "abundance"   # or "count", etc
      )
    },
    ignoreInit = TRUE
  )
  
  # =========================================================
  # 4) LENGTH HISTOGRAM (MATCH YOUR FACETTED EXAMPLE)
  #    Facets: columns = Waters (Coastal/Federal), rows = Management (Fished/No-Take)
  # =========================================================
  
  length_df_for_hist <- reactive({
    req(input$marine_park)
    
    # pick a length table you actually have
    df <- NULL
    for (x in list(all_data$bruv_length, all_data$dov_length, all_data$length, all_data$length_data)) {
      if (!is.null(x) && inherits(x, "data.frame") && nrow(x) > 0) { df <- x; break }
    }
    if (is.null(df)) {
      # fallback dummy
      return(
        tibble::tibble(
          marine_park = input$marine_park,
          length_mm = stats::rnorm(600, mean = 250, sd = 70),
          method = rep(pick_two_methods(), each = 300),
          waters = rep(c("Coastal Waters", "Federal Waters"), length.out = 600),
          management = rep(c("Fished", "No-Take"), each = 300)
        )
      )
    }
    
    # detect columns robustly
    len_col <- c("length_mm", "Length_mm", "length", "Length", "fish_length_mm", "fork_length_mm")
    len_col <- len_col[len_col %in% names(df)][1]
    
    method_col <- c("method", "Method", "sampling_method", "platform")
    method_col <- method_col[method_col %in% names(df)][1]
    
    waters_col <- c("waters", "Waters", "jurisdiction", "Jurisdiction", "realm", "water_type")
    waters_col <- waters_col[waters_col %in% names(df)][1]
    
    mgmt_col <- c("management", "Management", "take", "Take", "zone_type", "ZoneType", "status", "Status", "zone")
    mgmt_col <- mgmt_col[mgmt_col %in% names(df)][1]
    
    out <- df
    
    # optional park filter if column exists
    if ("marine_park" %in% names(out)) out <- out %>% dplyr::filter(marine_park %in% input$marine_park)
    
    # build standardised columns
    out <- out %>%
      dplyr::mutate(
        length_mm = suppressWarnings(as.numeric(.data[[len_col]])),
        method_raw = if (!is.na(method_col)) as.character(.data[[method_col]]) else "stereo-BRUV",
        waters_raw = if (!is.na(waters_col)) as.character(.data[[waters_col]]) else "Coastal Waters",
        management_raw = if (!is.na(mgmt_col)) as.character(.data[[mgmt_col]]) else "Fished"
      ) %>%
      dplyr::filter(is.finite(length_mm), length_mm > 0)
    
    # normalise labels to match your plot
    out %>%
      dplyr::mutate(
        method = dplyr::case_when(
          stringr::str_detect(tolower(method_raw), "bruv") ~ "stereo-BRUV",
          stringr::str_detect(tolower(method_raw), "uvc")  ~ "UVC",
          stringr::str_detect(tolower(method_raw), "rov")  ~ "stereo-ROV",
          TRUE ~ method_raw
        ),
        method = ifelse(method %in% names(method_cols), method, pick_two_methods()[1]),
        waters = dplyr::case_when(
          stringr::str_detect(tolower(waters_raw), "federal|commonwealth|offshore") ~ "Federal Waters",
          TRUE ~ "Coastal Waters"
        ),
        management = dplyr::case_when(
          stringr::str_detect(tolower(management_raw), "no[- ]?take|sanctuary|green") ~ "No-Take",
          TRUE ~ "Fished"
        ),
        waters = factor(waters, levels = c("Coastal Waters", "Federal Waters")),
        management = factor(management, levels = c("Fished", "No-Take"))
      )
  })
  
  output$length_hist <- renderPlot({
    df <- length_df_for_hist()
    req(nrow(df) > 0)
    
    # keep only the two methods you’re displaying elsewhere (for consistency)
    keep_methods <- pick_two_methods(df)
    df <- df %>% dplyr::filter(method %in% keep_methods)
    
    ggplot(df, aes(x = length_mm, fill = method)) +
      geom_histogram(
        bins = 18, colour = "black", linewidth = 0.3,
        position = "identity", alpha = 0.95
      ) +
      facet_grid(management ~ waters, scales = "free_y") +
      scale_fill_manual(values = method_cols, drop = TRUE) +
      labs(
        x = "Length (mm)",
        y = "Frequency",
        fill = NULL
      ) +
      theme_classic(base_size = 12) +
      theme(
        legend.position = "right",
        strip.background = element_blank(),
        strip.text = element_text(face = "plain"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
      )
  })
  
  
  
  # End of server ----
}

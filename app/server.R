# Data Explorer

# Define server logic
server <- function(input, output) {
  # Controls ----
  
  # Input data ----
  dataset <- reactive({
    corals
  })
  
  # UI updates ----
  ## Data summary
  output$counts <- renderUI({
    tagList(
      tags$body(paste0("Events: ", length(unique(data()$eventID)))),
      tags$br(),
      tags$body(paste0("Occurrences: ", length(unique(data()$occurrenceID)))),
      tags$br(),
      tags$body(paste0("Taxa: ", length(unique(data()$taxonID)))),
    )
  })
  ## Taxonomy
  output$taxon <- renderUI({
    selectInput("taxon", "Select taxon", 
                c("All", sort(unique(corals[, input$rank]))),
                selected = "All")
  })
  ## Time
  max_ma <- reactive({
    intervals[which(intervals$age == input$max_age), "max_ma"]
  })
  min_ma <- reactive({
    intervals[which(intervals$age == input$min_age), "min_ma"]
  })
  output$age <- renderUI({
    tagList(
      tags$body(paste0(max_ma(), "–", min_ma(), " Ma")))
  })
  ## Space
  output$country <- renderUI({
    selectInput("country", "Select country", 
                c("All", sort(unique(dataset()$country))),
                selected = "All")
  })
  ## Geological context
  ### Group
  output$geogroup <- renderUI({
    selectInput("geogroup", "Select group",
                c("All", sort(unique(dataset()$group))),
                selected = "All")
  })
  ### Formation
  output$formation <- renderUI({
    selectInput("formation", "Select formation",
                c("All", sort(unique(dataset()$formation))),
                selected = "All")
  })
  ### Member
  output$member <- renderUI({
    selectInput("member", "Select member",
                c("All", sort(unique(dataset()$member))),
                selected = "All")
  })
  
  # Filtering ----
  data <- reactive({
    tmp <- dataset()
    # Taxonomy
    if (!is.null(input$taxon) && input$taxon != "All") {
      tmp <- tmp[which(tmp[, input$rank] == input$taxon), ]
    }
    # Time
    tmp <- tmp |>
      subset(earliestChronometricAge <= max_ma()) |>
      subset(latestChronometricAge >= min_ma())
    # Geography
    if(!is.null(input$country) && input$country != "All") {
      tmp <- tmp |>
        subset(country == input$country)
    }
    tmp <- tmp |>
      subset(decimalLatitude >= input$min_lat) |>
      subset(decimalLatitude <= input$max_lat) |>
      subset(decimalLongitude >= input$min_lng) |>
      subset(decimalLongitude <= input$max_lng)
    # Geological context
    if(input$geogroup != "All") {
      tmp <- tmp |>
        subset(group == input$geogroup)
    }
    if(input$formation != "All") {
      tmp <- tmp |>
        subset(formation == input$formation)
    }
    if(input$member != "All") {
      tmp <- tmp |>
        subset(member == input$member)
    }
    # Return data
    tmp$ageColour <- intervals$colour[match(tmp$earliestAgeOrLowestStage, intervals$age)]
    tmp
  })
  
  # Outputs ----
  
  ## Map
  ####### DATA SHOULD BE SUMMARISED BY EVENT 
  ####### DATA SHOULD BE SUMMARISED BY EVENT 
  ####### DATA SHOULD BE SUMMARISED BY EVENT 
  ### Rendering map
  output$map <- renderLeaflet({
    leaflet() |>
      addProviderTiles(providers$CartoDB.Positron, 
                       options = list(noWrap = TRUE,
                                      minZoom = 2,
                                      maxZoom = 14)) |>
      setView(lng = (min(dataset()$decimalLongitude) + max(dataset()$decimalLongitude)) / 2, 
              lat = (min(dataset()$decimalLatitude) + max(dataset()$decimalLatitude)) / 2, 
              zoom = 4) |> 
      setMaxBounds(lng1 = -180, lat1 = 90, lng2 = 180, lat2 = -90) |>
      addRectangles(lng1 = input$min_lng, lng2 = input$max_lng, 
                    lat1 = input$min_lat, lat2 = input$max_lat, 
                    fill = FALSE,
                    group = "Bounding Box") |>
      addCircleMarkers(data = data(),
                       lng = ~decimalLongitude,
                       lat = ~decimalLatitude,
                       layerId = ~eventID,
                       popup = paste(data()$eventID, 
                                     data()$species,
                                     sep = "<br/>"),
                       radius = 5,
                       stroke = TRUE,
                       weight = 1,
                       opacity = 1,
                       color = "black",
                       fillColor = ~ageColour,
                       fillOpacity = 0.5)
  })
  
  ## Table
  output$table <- renderDataTable(data(), 
                                  extensions = c("Responsive", "Scroller"), 
                                  options = list(
                                    deferRender = TRUE,
                                    scrollY = 800,
                                    scroller = TRUE
                                  ))
  
  ## Taxonomy
  ### Reactive filtering and taxon counts
  taxon_data <- reactive({
    # Define fields
    tax_names <- c("scientificName", "kingdom", "phylum", 
                   "class", "order", "family", "genus", "species")
    # Subset data
    tax_tmp <- data()[, tax_names]
    # Summarise data
    tax_tmp <- aggregate(x = cbind(tax_tmp[0], occurrences = 1), 
                         by = tax_tmp, FUN = length)
    # Reformat columns
    tax_tmp <- tax_tmp[, c("occurrences", tax_names)]
    # Order by occurrence counts
    tax_tmp <- tax_tmp[order(tax_tmp$occurrences, decreasing = TRUE), ]
    # Remove row names
    row.names(tax_tmp) <- NULL
    # Return data
    tax_tmp
  })
  ### Rendering table
  output$taxonomy <- renderDataTable(taxon_data(), 
                                     extensions = c("Responsive", "Scroller"), 
                                     options = list(
                                       deferRender = TRUE,
                                       scrollY = 800,
                                       scroller = TRUE
                                     ))
  
  ## Temporal ranges
  ### Analyses
  range_time <- reactive({
    # Split by taxon
    range_time <- split(x = data(), f = data()[, input$rank])
    # Calculate maximum and minimin range
    range_time <- data.frame(
      taxon = names(range_time),
      earliestChronometricAge = unlist(lapply(range_time, function(x) max(x$earliestChronometricAge))),
      latestChronometricAge = unlist(lapply(range_time, function(x) min(x$latestChronometricAge))))
  })
  ### Rendering plot
  output$range <- renderPlot(
    ggplot(range_time(), 
           aes(xmin = earliestChronometricAge, 
               xmax = latestChronometricAge, 
               y = taxon,
               colour = taxon)) +
      # Range
      geom_linerange() +
      # Oldest point
      geom_point(aes(x = earliestChronometricAge, 
                     y = taxon)) +
      # Youngest point
      geom_point(aes(x = latestChronometricAge, 
                     y = taxon)) +
      # Tax label
      geom_text(aes(x = earliestChronometricAge, 
                    y = taxon, 
                    # Reactive labels based on number of labels
                    label = if (nrow(range_time()) < 150) taxon else (NA)),
                # Adjustments
                hjust = 1.1, check_overlap = TRUE) +
      # Reverse x axis (geological time) and expand scale for labels
      scale_x_reverse(expand = expansion(mult = 0.1)) +
      # Reverse scale and expand scale
      scale_y_discrete(limits = rev, 
                       expand = expansion(mult = 0.05)) +
      # Add labels
      labs(x = "Time (Ma)", 
           y = tools::toTitleCase(input$rank)) +
      # Set default theme and base size
      theme_bw(base_size = 16) +
      # Customise theme
      theme(legend.position = "none",
            axis.title.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.text.y = element_blank(),
            panel.grid = element_blank()) +
      # No clipping (important for labels)
      coord_cartesian(clip = "off")
  )
  
  ## Taxonomic richness
  
  ## Diversification rates
  
}
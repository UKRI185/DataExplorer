# Data Explorer

# Define server logic
server <- function(input, output) {
  # Controls ----
  observeEvent(input$restart, {
    session$reload()
  })
  # Input data ----
  dataset <- reactive({
    if (is.null(input$file)) {
      corals
    } else {
      file <- input$file
      ext <- tools::file_ext(file$datapath)
      
      req(file)
      validate(need(ext == "csv", "Please upload a csv file"))
      
      read.csv(file$datapath, header = TRUE)
    }
  })
  # UI updates ----
  ## space
  output$country <- renderUI({
    selectInput("country", "Select country", 
                c("All", sort(unique(dataset()$country))),
                selected = "All")
  })
  ## Counts
  output$counts <- renderUI({
    tagList(
      tags$h6("Data summary"),
      tags$body(paste0("Events: ", length(unique(data()$eventID)))),
      tags$br(),
      tags$body(paste0("Occurrences: ", length(unique(data()$occurrenceID)))),
      tags$br(),
      tags$body(paste0("Taxa: ", length(unique(data()$taxonID)))),
    )
  })
  ## Ages
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
  ## Taxa
  output$taxon <- renderUI({
    selectInput("taxon", "Select taxon", 
                c("All", sort(unique(corals[, input$rank]))),
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
      subset(maximumAgeMa <= max_ma()) |>
      subset(minimumAgeMa >= min_ma())
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
    tmp
  })
  
  # Analyses ----
  ## Temporal ranges
  range_time <- reactive({
    range_time <- split(x = data(), f = data()[, input$rank])
    range_time <- data.frame(
      taxon = names(range_time),
      maximumAgeMa = unlist(lapply(range_time, function(x) max(x$maximumAgeMa))),
      minimumAgeMa = unlist(lapply(range_time, function(x) min(x$minimumAgeMa))))
  })
  range_occ <- reactive({
    unique(data()[, c(input$rank, "maximumAgeMa", "minimumAgeMa", "family", "countryCode")])
  })
  
  # Rendering ----
  
  ## Map
  output$map <- renderLeaflet({
    leaflet() |>
      addProviderTiles(providers$CartoDB.Positron, 
                       options = list(noWrap = TRUE,
                                      minZoom = 2,
                                      maxZoom = 14)) |>
      setView(lng = (min(corals$decimalLongitude) + max(corals$decimalLongitude)) / 2, 
              lat = (min(corals$decimalLatitude) + max(corals$decimalLatitude)) / 2, 
              zoom = 4) |> 
      setMaxBounds(lng1 = -180, lat1 = 90, lng2 = 180, lat2 = -90) |>
      addRectangles(lng1 = input$min_lng, lng2 = input$max_lng, 
                    lat1 = input$min_lat, lat2 = input$max_lat, 
                    group = "Bounding Box") |>
      addCircleMarkers(data = data(),
                       lng = ~decimalLongitude,
                       lat = ~decimalLatitude,
                       layerId = ~eventID,
                       #popup = ~colour,
                       group = ~assignedAge,
                       radius = 5,
                       stroke = TRUE,
                       weight = 1,
                       opacity = 1,
                       color = "#36454F",
                       fillColor = ~ageColour,
                       fillOpacity = 0.5) |>
      addLayersControl(overlayGroups = c(data()$assignedAge))
  })

  ## Table
  output$table <- renderDataTable(data(), 
                                  extensions = c("Responsive", "Scroller"), 
                                  options = list(
                                    deferRender = TRUE,
                                    scrollY = 800,
                                    scroller = TRUE
                                  ))
  
  ## Temporal ranges
  output$range <- renderPlot(
    ggplot(range_time(), aes(xmin = minimumAgeMa, xmax = maximumAgeMa, y = taxon)) +
      geom_linerange(linetype = 2) +
      geom_linerange(data = range_occ(), 
                     aes(xmax = maximumAgeMa, xmin = minimumAgeMa,
                         y = range_occ()[, input$rank])) +
      geom_point(data = range_occ(), 
                 aes(x = (maximumAgeMa + minimumAgeMa) / 2, 
                     y = range_occ()[, input$rank]),
                 colour = "black", fill = "orange",
                 pch = 23) +
      scale_x_reverse() +
      scale_y_discrete(limits = rev, guide = guide_axis(check.overlap = TRUE)) +
      labs(x = "Time (Ma)", y = tools::toTitleCase(input$rank)) +
      theme_bw() +
      theme(legend.position = "none",
            axis.text = element_text(size = (12 - log(nrow(range_time())))),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.x = element_blank())
  )
  
  ## Taxonomic richness
  
  ## Diversification rates
  
}
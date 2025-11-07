# Data Explorer UI

ui <- page_navbar(
  theme = my_theme,
  fillable_mobile = TRUE,
  title = "Data Explorer",
  nav_panel("Map", 
            leafletOutput("map")),
  nav_panel("Table", 
            dataTableOutput("table")),
  nav_panel("Taxonomy"),
  nav_panel("Temporal ranges", 
            plotOutput("range")),
  nav_panel("Taxonomic richness"),
  nav_panel("Diversification rates"),
  nav_spacer(),
  nav_item(
    input_dark_mode(mode = "light")
  ),
  sidebar = sidebar(
    actionButton("restart", icon = icon("refresh"),
                 "Restart session", width = "100%", class = "btn-warning rounded-0"),
    accordion(
      open = TRUE,
      accordion_panel(
        "Data", icon = bs_icon("database"),
        uiOutput("counts"),
        tags$br(),
        tags$br(),
        fileInput(inputId = "file", 
                  label = h6("Upload own dataset"), 
                  placeholder = "Select '.csv'", 
                  accept = ".csv"),
        tags$h6("...or continue with in-built dataset")
      )
    ), 
    h5("Filter"),
    accordion(open = FALSE,
              accordion_panel(
                "Taxonomy", icon = bs_icon("folder"),
                # Input: Taxonomic rank ----
                selectInput("rank", "Select taxonomic rank", tax_rank, selected = "genus"),
                # Input: Taxon ----
                uiOutput("taxon")
              ),
              accordion_panel(
                "Time", icon = bs_icon("hourglass-bottom"),
                # Input: Time ----
                selectInput("max_age", "From", int_name,
                            selected = "Danian"),
                selectInput("min_age", "To", int_name, 
                            selected = "Meghalayan"),
                # Output: Age ----
                uiOutput("age")
              ),
              accordion_panel(
                "Space",  icon = bs_icon("geo-alt"),
                # Input: Georegion ----
                uiOutput("country"),
                # Input: Bounding box ----
                numericInput("min_lng", "Minimum longitude (º)", value = -180,
                             min = -180, max = 180),
                numericInput("max_lng", "Maximum longitude (º)", value = 180,
                             min = -180, max = 180),
                numericInput("min_lat", "Minimum latitude (º)", value = -90,
                             min = -90, max = 90),
                numericInput("max_lat", "Maximum latitude (º)", value = 90,
                             min = -90, max = 90)
              ),
              accordion_panel(
                "Geological context", icon = bs_icon("hammer"),
                # Input: Group ----
                selectInput("geogroup", "Select Group", group,
                            selected = "All"),
                # Input: Formation ----
                selectInput("formation", "Select Formation", formation,
                            selected = "All"),
                # Input: Member ----
                selectInput("member", "Select Member", member,
                            selected = "All")
              ),
              accordion_panel(
                "Metadata", icon = bs_icon("journal")
              )
    ),
    h5("Download")
    )
)
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
    accordion(
      open = TRUE,
      accordion_panel(
        "Data summary", icon = bs_icon("database"),
        uiOutput("counts")
      )
    ), 
    h5("Filter"),
    accordion(open = FALSE,
              accordion_panel(
                "Taxonomy", icon = bs_icon("folder"),
                # Input: Taxonomic rank ----
                selectInput("rank", "Select rank", tax_rank, selected = "genus"),
                # Input: Taxon ----
                uiOutput("taxon")
              ),
              accordion_panel(
                "Time", icon = bs_icon("hourglass-bottom"),
                # Input: Time ----
                selectInput("max_age", "Maximum age", int_name,
                            selected = "Danian"),
                selectInput("min_age", "Minimum age", int_name, 
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
                uiOutput("geogroup"),
                # Input: Formation ----
                uiOutput("formation"),
                # Input: Member ----
                uiOutput("member"),
              ),
              accordion_panel(
                "Metadata", icon = bs_icon("journal")
              )
    ),
    h5("Download")
    )
)
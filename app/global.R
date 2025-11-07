# Data Explorer

# UI
## Load required libraries
library(shiny)
library(bslib)
library(bsicons)
## Theme
my_theme <- bs_theme(version = 5, preset = "bootstrap")

# Server
## Load required libraries
### Maps
library(leaflet)
### Data tables
library(DT)
### Required to make ggplot work
library(munsell) 
### General plotting
library(ggplot2)

# Data
corals <- readRDS(file = "data/corals.RDS")
intervals <- readRDS(file = "data/intervals.RDS")
intervals <- subset(intervals, max_ma <= max(corals$maximumAgeMa))

# Parameters
## Taxonomic ranks
tax_rank <- c(Species = "species", Genus = "genus", Family = "family", 
              Order = "order", Class = "class", Phylum = "phylum", 
              Kingdom = "kingdom")
## Ages
int_name <- lapply(unique(intervals$period), function(x) {
  subset(intervals, period == x)$age
})
names(int_name) <- unique(intervals$period)
## Country
cc <- c("All", sort(unique(corals$countryCode)))
## Geological context
group <- c("All", sort(unique(corals$group)))
formation <- c("All", sort(unique(corals$formation)))
member <- c("All", sort(unique(corals$member)))


# Header ----------------------------------------------------------------
# Project: DataExplorer
# File name: fetch_wrangle_data.R
# Last updated: 2025-10-23
# Author: Lewis A. Jones
# Email: LewisA.Jones@outlook.com
# Repository: https://github.com/LewisAJones/DataExplorer

# Load libraries and options --------------------------------------------
library(RCurl)
library(httr)
library(palaeoverse)
library(tidygeocoder)
library(galaxias)
library(dplyr)
source("R/options.R")

# Data downloading from PBDB --------------------------------------------
if (params$download) {
  # Use for fresh downloads
  RCurl::curlSetOpt(3000)
  # Read data 
  url <- httr::modify_url(url = params$base_url, query = params$query)
  # Save raw data
  download.file(url = url, destfile = paste0("raw_data/", params$file_name))
  # Read only
  Sys.chmod(paste0("app/data/raw/", params$file_name), mode = "0444")
}
# Read data
occdf <- read.csv(paste0("raw_data/", params$file_name), skip = 20)

# Get time bins --------------------------------------------------------
# Get interval data
age <- time_bins(scale = "international ages")
epoch <- time_bins(scale = "international epochs")
period <- time_bins(scale = "international periods")
era <- time_bins(scale = "international eras")
eon <- time_bins(scale = "international eons")
# Add age column
age$age <- age$interval_name
# Create empty columns
age$epoch <- NA
age$period <- NA
age$era <- NA
age$eon <- NA
# Assign groupings
for (i in 1:nrow(age)) {
  # Epoch
  age$epoch[i] <- epoch[which(epoch$min_ma <= age$min_ma[i] & 
                                epoch$max_ma >= age$max_ma[i]), "interval_name"]
  # Period
  age$period[i] <- period[which(period$min_ma <= age$min_ma[i] & 
                                  period$max_ma >= age$max_ma[i]), "interval_name"]
  # Era
  age$era[i] <- era[which(era$min_ma <= age$min_ma[i] & 
                            era$max_ma >= age$max_ma[i]), "interval_name"]
  # Eon
  age$eon[i] <- eon[which(eon$min_ma <= age$min_ma[i] & 
                            eon$max_ma >= age$max_ma[i]), "interval_name"]
}
# Subset data
age <- age[, c("bin", "eon", "era", "period", "epoch", "age", 
               "max_ma", "min_ma", "colour")]
# Save data
saveRDS(age, "app/data/intervals.RDS")

# Filter data -----------------------------------------------------------
occdf <- occdf |>
  subset(lat >= params$ylim[1]) |>
  subset(lat <= params$ylim[2]) |>
  subset(lng >= params$xlim[1]) |>
  subset(lng <= params$xlim[2])

# Wrangle data ----------------------------------------------------------

# Bin data
occdf <- bin_time(occdf = occdf, bins = age, method = "majority")
# Join dataset
occdf <- left_join(x = occdf, y = age, by = c("bin_assignment" = "bin"))

# Taxonomy
occdf$species <- occdf$accepted_name
occdf$species[which(occdf$accepted_rank != "species")] <- ""
occdf$specificEpithet <- gsub(".*? ", "", occdf$species)

# Geography
occdf <- occdf |>
  reverse_geocode(lat = lat, long = lng, full_results = TRUE)
occdf$country[which(occdf$country == "Panamá")] <- "Panama"
occdf$country[which(occdf$country == "Ayiti")] <- "Haiti"
occdf$country[which(occdf$country == "Nederland")] <- "Netherlands, Kingdom of the"
occdf$country[which(occdf$country == "Sint Maarten")] <- "Sint Maarten (Dutch part)"
occdf$country[which(occdf$country == "The Bahamas")] <- "Bahamas"
occdf$country[which(occdf$country == "British Virgin Islands")] <- "Virgin Islands (British)"
occdf$country[which(occdf$country == "México")] <- "Mexico"
occdf$country[which(occdf$country == "Venezuela")] <- "Venezuela, Bolivarian Republic of"
occdf$country[which(occdf$country == "República Dominicana")] <- "Dominican Republic"


# Format to dwc ---------------------------------------------------------

# Standardise data
occdf <- occdf |>
  set_events(eventID = as.character(collection_no)) |>
  set_occurrences(occurrenceID = occurrence_no,
                  basisOfRecord = "humanObservation") |>
  set_datetime(year = ref_pubyr) |>
  set_coordinates(decimalLongitude = lng,
                  decimalLatitude = lat) |>
  set_locality(country = country, 
               countryCode = cc) |>
  set_taxonomy(kingdom = "Animalia", 
               phylum = phylum,
               class = `class...38`,
               order = order,
               family = family,
               genus = genus,
               specificEpithet = specificEpithet) |>
  set_scientific_name(scientificName = accepted_name,
                      scientificNameAuthorship = "",
                      taxonRank = accepted_rank) |>
  rename(taxonID = accepted_no,
         earliestAgeOrLowestStage = early_interval,
         latestAgeOrHighestStage = late_interval,
         latestChronometricAge = min_ma.y,
         earliestChronometricAge = max_ma.y,
         group = geological_group,
         formation = formation,
         member = member) |> 
  mutate(earliestChronometricAgeReferenceSystem = "Ma",
         latestChronometricAgeReferenceSystem = "Ma")

occdf <- occdf[, c("eventID", "occurrenceID", "basisOfRecord", "year",
                   "decimalLongitude", "decimalLatitude", "country", "countryCode",
                   "scientificName", "scientificNameAuthorship", 
                   "taxonID", "taxonRank",
                   "kingdom", "phylum", "class", "order", "family", "genus",
                   "species", "specificEpithet",
                   "earliestAgeOrLowestStage", "latestAgeOrHighestStage",
                   "latestChronometricAge", "latestChronometricAgeReferenceSystem",
                   "earliestChronometricAge", "earliestChronometricAgeReferenceSystem",
                   "group", "formation", "member")]

# Save data
occdf <- data.frame(occdf)
saveRDS(occdf, "app/data/corals.RDS")

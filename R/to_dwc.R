# Load library
library(galaxias)
# Load data
corals <- readRDS("app/data/corals.RDS")

# Standardise data
corals <- corals |>
  set_occurrences(occurrenceID = occurrence_no,
                  basisOfRecord = "humanObservation") |>
  set_coordinates(decimalLongitude = lng,
                  decimalLatitude = lat) |>
  set_locality(country = cc,
               locality = region) |>
  set_scientific_name(scientificName = species,
                      taxonRank = accepted_rank,
                      )
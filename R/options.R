# Header ----------------------------------------------------------------
# Project: dash
# File name: options.R
# Last updated: 2025-02-04
# Author: Lewis A. Jones
# Email: LewisA.Jones@outlook.com
# Repository: https://github.com/LewisAJones/dash

# Define options --------------------------------------------------------
params <- list(
  # Download updated dataset
  download = TRUE,
  # Occurrence file name
  file_name = "pbdb.csv",
  # PBDB base url
  base_url = "https://paleobiodb.org/data1.2/occs/list.csv?datainfo&",
  # PBDB Download options
  query = list(
    base_name = "Scleractinia",
    taxon_reso = "genus",
    ident = "latest",
    taxon_status = "valid",
    idqual = "genus_certain",
    pres = "regular",
    interval = "Danian,Holocene",
    envtype = "marine",
    show = "genus,pres,strat,coll,coords,loc,class,refattr"),
  # The geographic extent of study
  xlim = c(-100, -58), 
  ylim = c(6, 35)
)

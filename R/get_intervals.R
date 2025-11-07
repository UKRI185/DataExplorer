# Load library
library(palaeoverse)
# Get interval data
age <- time_bins(scale = "international ages")
epoch <- time_bins(scale = "international epochs")
period <- time_bins(scale = "international periods")
era <- time_bins(scale = "international eras")
eon <- time_bins(scale = "international eons")
# Create age column
age$age <- age$interval_name
# Create empty columns
age$epoch <- NA
age$period <- NA
age$era <- NA
age$eon <- NA
# Assign groupings
for (i in 1:nrow(age)) {
  # Epoch
  age$epoch[i] <- epoch[which(epoch$min_ma <= age$min_ma[i] & epoch$max_ma >= age$max_ma[i]), "interval_name"]
  # Period
  age$period[i] <- period[which(period$min_ma <= age$min_ma[i] & period$max_ma >= age$max_ma[i]), "interval_name"]
  # Era
  age$era[i] <- era[which(era$min_ma <= age$min_ma[i] & era$max_ma >= age$max_ma[i]), "interval_name"]
  # Eon
  age$eon[i] <- eon[which(eon$min_ma <= age$min_ma[i] & eon$max_ma >= age$max_ma[i]), "interval_name"]
}
# Subset data
age <- age[, c("bin", "eon", "era", "period", "epoch", "age", "max_ma", "min_ma")]
# Save data
write.csv(age, "data/intervals.csv", row.names = FALSE)
saveRDS(age, "data/intervals.RDS")

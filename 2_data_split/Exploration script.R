# === packages
#install.packages("sp")
library(sp)

# === load data
# cege_data <- "/cloud/project/r_processing/0_source_data/UJTWorkSpace.RData"
cege_data <- "./0_source_data/UJTWorkSpace.RData"
load(cege_data)

# === explore data
plot(UJT[1:100])

UJT

# === export data
# Write data out to handle in python

write.csv(UJT,"ujt.csv")




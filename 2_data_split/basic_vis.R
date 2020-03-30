# INFO --------------------------------------------------------------------

# Purpose: Create a heatmap of Road link x Time, byDay allows you to subset for each day 
# Inputs: traffic_speeds.csv
# Outputs: heatmap jpeg

# CREATE HEATMAP ----------------------------------------------------------

data <- read.csv('./2_data_split/traffic_speeds.csv') # read the csv
nt <- data[,1:22] # drop the time columns 
nt <- as.matrix(t(nt)) # convert to matrix
heatmap(nt, Colv = NA, Rowv = NA, scale='column', xlab='Time', ylab='Road Link') # create the heatmap

# HEATMAP FOR DAY ---------------------------------------------------------

# day input should range from 1-30 (reflecting number of days in the dataset)
byDay <- function(day){
  select <- data[(1+(180*day)-180):(180*day),] # select the data for that day
  nt <- select[,1:22] # drop the time columns 
  nt <- as.matrix(t(nt)) # convert to matrix
  
  jpeg(file = paste0('day',i, '.jpeg', sep = '')) # to save the heatmap
  heatmap(nt, Colv = NA, Rowv = NA, scale='column', xlab='Time', ylab='Road Link') # create the heatmap
  dev.off()
}

#for(i in 1:30){
#  byDay(i)
#}



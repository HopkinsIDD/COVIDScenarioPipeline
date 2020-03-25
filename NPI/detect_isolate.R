## NPI based on detection and isolation

library(dplyr)

county <- read.csv(paste0(foldername,'geodata.csv'))
dates <- seq.Date(as.Date(ti_str), as.Date(tf_str), 1)

NPI <- as.data.frame(matrix(0, dim(county)[1],length(dates)))
colnames(NPI) <- as.Date(dates)
rownames(NPI) <- county$geoid

## Introducing NPI: randomly assign a pc value to each county based on Bootsma paper values
NPI[ , colnames(NPI) >= as.Date("2020/03/19") & colnames(NPI) <= as.Date("2020/05/14") ] <- 1
county$pc <- replicate(dim(county)[1], runif(dim(county)[1], 0.31, 0.73))
NPI <- NPI * county$pc
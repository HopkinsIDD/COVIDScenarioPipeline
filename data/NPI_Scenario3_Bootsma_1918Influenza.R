## create matrix for counties starting social distancing based on Bootsma R0 reductions

library(dplyr)

R.reduce <- read.csv('data/BootsmaReductions.csv')
county.status <- read.csv(paste(c(foldername,'geodata.csv'))
dates <- seq.Date(as.Date("2020/1/1"), as.Date("2020/7/1"), 1)

NPI <- as.data.frame(matrix(0, dim(county.status)[1],length(dates)))
colnames(NPI) <- as.Date(dates)
rownames(NPI) <- county.status$geoid

## Introducing NPI: randomly assign a pc value to each county based on Bootsma paper values
NPI[ , colnames(NPI) > as.Date("2020/03/13")] <- 1
county.status$pc <- replicate(dim(county.status)[1], sample(R.reduce$Reduction, 1, rep = TRUE))
NPI <- NPI * county.status$pc

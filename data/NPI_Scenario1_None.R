## create matrix for no intervention

library(dplyr)

# West coast
county.status <- read.csv(paste(c(foldername,'geodata.csv'))
dates <- seq.Date(as.Date("2020/1/1"), as.Date("2020/7/1"), 1)

NPI <- as.data.frame(matrix(0, dim(county.status)[1],length(dates)))
colnames(NPI) <- as.Date(dates)
rownames(NPI) <- county.status$geoid

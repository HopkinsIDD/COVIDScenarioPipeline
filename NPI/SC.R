## create matrix for counties starting social distancing

library(dplyr)

county.status <- read.csv(paste0(foldername,'geodata.csv'))
dates <- seq.Date(as.Date(ti_str), as.Date(tf_str), 1)

NPI <- as.data.frame(matrix(0, dim(county.status)[1],length(dates)))
colnames(NPI) <- as.Date(dates)
rownames(NPI) <- county.status$geoid

## Introducing NPI
NPI[ , colnames(NPI) > as.Date("2020/03/13")] <- 1
county.status$pc2 <- truncnorm::rtruncnorm(n = dim(county.status)[1], a = 0.16, b = 0.30, mean = 0.18, sd = 0.05)
NPI <- NPI * county.status$pc


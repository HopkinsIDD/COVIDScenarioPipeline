## create matrix for no intervention

library(dplyr)

# East Coast
county.status <- read.csv('data/east-coast/eastcoast_April1_topAR.csv')
filterUSPS = c('MD', 'DC', 'VA', 'DE', 'PA', 'NJ')
foldername = '../../data/east-coast/'

# West coast
#filterUSPS = ['CA', 'OR', 'WA']
#foldername = '../../data/west-coast/'

# MD
# county.status <- read.csv('maryland_April1_topAR.csv')
# filterUSPS = c('MD')
# foldername = '../../data/maryland/'


dates <- seq.Date(as.Date("2020/1/1"), as.Date("2020/7/1"), 1)

NPI <- as.data.frame(matrix(0, dim(county.status)[1],length(dates)))
colnames(NPI) <- as.Date(dates)
rownames(NPI) <- county.status$geoid

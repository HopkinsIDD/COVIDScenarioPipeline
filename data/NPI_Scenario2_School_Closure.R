## create matrix for counties starting social distancing

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


dates <- seq.Date(as.Date("2020/3/1"), as.Date("2020/7/1"), 1)

NPI <- as.data.frame(matrix(0, dim(county.status)[1],length(dates)))
colnames(NPI) <- as.Date(dates)
rownames(NPI) <- county.status$geoid

## Introducing NPI
NPI[ , colnames(NPI) > as.Date("2020/03/13")] <- 1
county.status$pc2 <- truncnorm::rtruncnorm(n = dim(county.status)[1], a = 0.16, b = 0.30, mean = 0.18, sd = 0.05)
NPI <- NPI * county.status$pc

# write.csv(NPI, file = "../../data/east-coast/EastCoast_NPI.csv")


## for introducing NPI in distinct spatial/temporal phases
# NPI.3 <- NPI
# county.status <- county.status%>%
#   mutate(tertile = ntile(desc(mean), 3))
# phase1 <- county.status$geoid[county.status$tertile == 1]
# phase2 <- county.status$geoid[county.status$tertile == 2]
# phase3 <- county.status$geoid[county.status$tertile == 3]
# 
# NPI.3[row.names(NPI) %in% phase1, colnames(NPI)>= as.Date("2020/03/18")] <- 0.25
# NPI.3[row.names(NPI) %in% phase2, colnames(NPI)>= as.Date("2020/03/25")] <- 0.25
# NPI.3[row.names(NPI) %in% phase3, colnames(NPI)>= as.Date("2020/04/01")] <- 0.25

# write.csv(pc, file = "../../data/maryland/Maryland_3phase_pc.csv")
# write.csv(NPI, file = "../../data/east-coast/EastCoast_3phase_pc.csv")

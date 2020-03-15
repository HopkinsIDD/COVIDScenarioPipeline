## create matrix for counties starting social distancing based on Bootsma R0 reductions

library(dplyr)

# East Coast
county.status <- read.csv('data/east-coast/eastcoast_April1_topAR.csv')
R.reduce <- read.csv('data/east-coast/BootsmaReductions.csv')

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

## Introducing NPI: randomly assign a pc value to each county based on Bootsma paper values
NPI[ , colnames(NPI) > as.Date("2020/03/13")] <- 1
county.status$pc <- replicate(dim(county.status)[1], sample(R.reduce$Reduction, 1, rep = TRUE))
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

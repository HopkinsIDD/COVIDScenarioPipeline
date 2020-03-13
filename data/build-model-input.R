## create matrix for counties starting social distancing

library(dplyr)

# East Coast
county.status <- read.csv('eastcoast_April1_topAR.csv')
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

pc <- as.data.frame(matrix(0, dim(county.status)[1],length(dates)))
colnames(pc) <- as.Date(dates)
rownames(pc) <- county.status$geoid

county.status <- county.status%>%
  mutate(tertile = ntile(desc(mean), 3))

phase1 <- county.status$geoid[county.status$tertile == 1]
phase2 <- county.status$geoid[county.status$tertile == 2]
phase3 <- county.status$geoid[county.status$tertile == 3]

pc[row.names(pc) %in% phase1, colnames(pc)>= as.Date("2020/03/18")] <- 0.25
pc[row.names(pc) %in% phase2, colnames(pc)>= as.Date("2020/03/25")] <- 0.25
pc[row.names(pc) %in% phase3, colnames(pc)>= as.Date("2020/04/01")] <- 0.25

# write.csv(pc, file = "../../data/maryland/Maryland_3phase_pc.csv")
write.csv(pc, file = "../../data/east-coast/EastCoast_3phase_pc.csv")

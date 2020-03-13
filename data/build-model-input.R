## create matrix for counties starting social distancing

library(dplyr)

filterUSPS = c('MD')
# filterUSPS = c('MD', 'DC', 'VA', 'DE', 'PA', 'NJ')
#filterUSPS = ['CA', 'OR', 'WA']
foldername = '../../data/maryland/'
# foldername = '../../data/east-coast/'
#foldername = '../../data/west-coast/'


census_tracts = read.csv('united-states-commutes/census_tracts_2010.csv')
census_tracts = census_tracts[census_tracts$USPS %in% filterUSPS,]
county.status <- read.csv('maryland_April1_topAR.csv')


census_tracts$county <- substr(census_tracts$GEOID, 1,5)
census_tracts.county <- census_tracts %>%
  group_by(county)%>%
  mutate(county.pop = sum(POP10))%>%
  distinct(county, .keep_all = TRUE)

dates <- seq.Date(as.Date("2020/3/1"), as.Date("2020/7/1"), 1)

pc <- as.data.frame(matrix(0, dim(census_tracts.county)[1],length(dates)))
colnames(pc) <- as.Date(dates)
rownames(pc) <- census_tracts.county$county

phase1 <- county.status$geoid[1:8]
phase2 <- county.status$geoid[9:16]
phase3 <- county.status$geoid[17:24]

pc[row.names(pc) %in% phase1, colnames(pc)>= as.Date("2020/03/18")] <- 0.25
pc[row.names(pc) %in% phase2, colnames(pc)>= as.Date("2020/03/25")] <- 0.25
pc[row.names(pc) %in% phase3, colnames(pc)>= as.Date("2020/04/01")] <- 0.25

write.csv(pc, file = "../../data/maryland/Maryland_3phase_pc.csv")

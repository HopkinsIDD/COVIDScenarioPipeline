library(dplyr)

filterUSPS = c('CA', 'OR', 'WA', 'AZ', 'NV')
foldername = 'west-coast-AZ-NV_2011-15'


# Import census data and commuter data
census_tracts = read.csv('united-states-commutes 2010/census_tracts_2010.csv')
commute_data = read.csv('commute_data_2011-2015.csv')

# Select states of interest and aggregate pop density to county level
census_tracts = census_tracts[census_tracts$USPS %in% filterUSPS, ]
census_tracts$GEOID.county = substr(census_tracts$GEOID,1, nchar(census_tracts$GEOID)-6)  # remove last 6 digits referring to subcounties

census_tracts.county <- census_tracts %>%  
  group_by(GEOID.county)%>%
  mutate(POP10.county = sum(POP10))%>%
  distinct(GEOID.county, .keep_all = TRUE)

census_tracts.county <- census_tracts.county[, c('USPS', 'GEOID.county', 'POP10.county')]  
colnames(census_tracts.county) <- c('USPS', 'GEOID', 'POP10')

# The 2010 commutere data is reported to the subcounty level and GEOIDs need to be aggregated to the county level
if(commute_data$OFIPS > 10){
  commute_data$OFIPS.county <- substr(commute_data2$OFIPS,1, nchar(commute_data2$OFIPS)-6)
  commute_data$DFIPS.county <- substr(commute_data2$DFIPS,1, nchar(commute_data2$DFIPS)-6)
  
  commute_data.county <- commute_data2 %>%
    group_by(OFIPS.county, DFIPS.county) %>%
    mutate(FLOW.county = sum(FLOW)) %>%
    mutate(MOE.county = sum(MOE))%>%
    distinct(OFIPS.county, DFIPS.county, .keep_all = TRUE)
  
  commute_data.county <- commute_data2.county[ , c('OFIPS.county', 'DFIPS.county', 'FLOW.county', 'MOE.county')]
  colnames(commute_data2.county) <- c('OFIPS', 'DFIPS', 'FLOW', 'MOE')
}

# Select commuter data for states of interest
commute_data = commute_data[commute_data$OFIPS %in% census_tracts.county$GEOID,]
commute_data = commute_data[commute_data$DFIPS %in% census_tracts.county$GEOID,]

# Create mobility matrix 
counties.names = unique(census_tracts.county$GEOID)
n.counties = length(counties.names)
mobility = matrix(data = NA, nrow = n.counties, ncol = n.counties)

for (i in 1:n.counties){
  for (j in 1:n.counties){
    
   mobility[i,j] = ifelse(any(!is.na(commute_data$FLOW[commute_data$OFIPS == counties.names[i] & commute_data$DFIPS == counties.names[j]])), 
                          commute_data$FLOW[commute_data$OFIPS == counties.names[i] & commute_data$DFIPS == counties.names[j]], 0)
  }
}

rownames(mobility) <- colnames(mobility) <- counties.names
diag(mobility) <- 0

# Stash results in folder (create if not already)

if(!dir.exists(foldername)){
  dir.create(foldername)
}
write.table(mobility, file = paste0(foldername, "/mobility2011-15.txt"))
write.csv(census_tracts.county, paste0(foldername, "/geodata.csv"))


###### Can we delete below this point? I think we have all NPI stuff in other files ###################
## create matrix for counties starting social distancing

#library(dplyr)

# East Coast
#county.status <- read.csv('data/east-coast/eastcoast_April1_topAR.csv')
#filterUSPS = c('MD', 'DC', 'VA', 'DE', 'PA', 'NJ')
#foldername = '../../data/east-coast/'

# West coast
#filterUSPS = ['CA', 'OR', 'WA']
#foldername = '../../data/west-coast/'

# MD
# county.status <- read.csv('maryland_April1_topAR.csv')
# filterUSPS = c('MD')
# foldername = '../../data/maryland/'


#dates <- seq.Date(as.Date("2020/1/1"), as.Date("2020/7/1"), 1)

#NPI <- as.data.frame(matrix(0, dim(county.status)[1],length(dates)))
#colnames(NPI) <- as.Date(dates)
#rownames(NPI) <- county.status$geoid

## Introducing NPI
#NPI[ , colnames(NPI) > as.Date("2020/03/13")] <- 1
#county.status$pc <- truncnorm::rtruncnorm(n = dim(county.status)[1], a = 0.16, b = 0.30, mean = 0.18, sd = 0.05)
#NPI <- NPI * county.status$pc

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

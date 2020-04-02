library(dplyr)
library(tidyr)

outdir <- '../../data/'
filterUSPS <- c('MD', 'DC', 'VA', 'DE', 'PA', 'NJ')

commute_data <- readr::read_csv("united-states-commutes/commute_data.csv")
census_data <- readr::read_csv("united-states-commutes/census_tracts_2010.csv")

census_data <- census_data %>%
  filter(USPS %in% filterUSPS) %>%
  select(USPS,GEOID,POP10) %>%
  mutate(GEOID = substr(GEOID,1,5)) %>%
  group_by(GEOID) %>%
  summarize(USPS = unique(USPS),POP10 = sum(POP10)) %>%
  arrange(POP10)

commute_data <- commute_data %>%
  mutate(OFIPS = substr(OFIPS,1,5), DFIPS = substr(DFIPS,1,5)) %>%
  filter(OFIPS %in% census_data$GEOID, DFIPS %in% census_data$GEOID) %>%
  group_by(OFIPS,DFIPS) %>%
  summarize(FLOW = sum(FLOW)) %>%
  filter(OFIPS != DFIPS)

padding_table <- tibble(
  OFIPS = census_data$GEOID,
  DFIPS = census_data$GEOID,
  FLOW = 0
)

t_commute_table <- tibble(
  OFIPS = commute_data$DFIPS,
  DFIPS = commute_data$OFIPS,
  FLOW = commute_data$FLOW
)

rc <- padding_table %>% bind_rows(commute_data) %>% bind_rows(t_commute_table) %>%
  pivot_wider(OFIPS,names_from=DFIPS,values_from=FLOW, values_fill=c("FLOW"=0),values_fn = list(FLOW=sum))

if(!isTRUE(all(rc$OFIPS == census_data$GEOID))){
  stop("There was a problem generating the mobility matrix")
}

write.table(file = file.path(outdir,'mobility.txt'), as.matrix(rc[,-1]), row.names=FALSE, col.names = FALSE, sep = " ")
write.csv(file = file.path(outdir,'geodata.csv'), census_data)

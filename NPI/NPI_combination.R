## create matrix for counties starting social distancing

library(dplyr)

t_start = as.Date(ti_str)
t_NPI1 = as.Date("2020/3/13")
t_NPI2 = as.Date("2020/3/19")
t_NPI3 = as.Date("2020/5/15")
t_end = as.Date(tf_str)

scenarios <- c("None","KansasCity","Wuhan","None")
start_times <- lubridate::ymd(c(ti_str,"2020-03-13","2020-03-19","2020-05-15"))
end_times <- lubridate::ymd(c("2020-03-13","2020-03-19","2020-05-15",tf_str))

all_NPI = list()
for(scenario in scenarios){
  source(paste0("~/git/COVIDScenarioPipeline/",scenario,".R"))
  ## restrict based on above dates
  NPI[,t_start + seq_len(start_times[[scenario]] - t_start - 1)] <- 0 # Whatever the no intervention state is
  NPI[,end_times[[scenario]] + 1 + seq_len(t_end - end_times[[scenario]] - 1)] <- 0 # Whatever the no intervention state is
  all_NPI[[scenario]] <- NPI
}
inverse_NPI <- 1 - all_NPI[[1]]
for(idx in seq_len(length(NPI) - 1)){
  rc <- rc * ( 1 - all_NPI[[idx]])
}
NPI <- 1 - inverse_NPI
# West Coast
county <- read.csv('west-coast-AZ-NV/geodata.csv')

# Before interventions:
dates.pre <- seq.Date(t_start, t_NPI1 - 1, 1)#seq.Date(as.Date("2020/1/31"), as.Date("2020/3/12"), 1)
pre.NPI <- as.data.frame(matrix(0, dim(county)[1],length(dates.pre)))
colnames(pre.NPI) <- as.Date(dates.1pre)
rownames(pre.NPI) <- county$geoid

# School closure: 
dates.1 <- seq.Date(t_NPI1, t_NPI2 - 1, 1)#seq.Date(as.Date("2020/3/13"), as.Date("2020/3/18"), 1)
NPI.1 <- as.data.frame(matrix(1, dim(county)[1],length(dates.1)))
colnames(NPI.1) <- as.Date(dates.1)
rownames(NPI.1) <- county$geoid
county$NPI.1 <- truncnorm::rtruncnorm(n = dim(county)[1], a = 0.16, b = 0.30, mean = 0.18, sd = 0.05)
NPI.1 <- NPI.1 * county$NPI.1

# Wuhan shutdown: 
dates.2 <- seq.Date(t_NPI2, t_NPI3 - 1, 1)#seq.Date(as.Date("2020/3/19"), as.Date("2020/5/14"), 1)
NPI.2 <- as.data.frame(matrix(1, dim(county)[1],length(dates.2)))
colnames(NPI.2) <- as.Date(dates.2)
rownames(NPI.2) <- county$geoid
county$NPI.2 <- replicate(dim(county)[1], runif(dim(county)[1], 0.81, 0.89))
NPI.2 <- NPI.2 * county$NPI.2

# Detect and isolate:
dates.3 <- seq.Date(t_NPI3, t_end, 1)#seq.Date(as.Date("2020/5/15"), as.Date("2020/12/31"), 1)
NPI.3 <- as.data.frame(matrix(1, dim(county)[1],length(dates.3)))
colnames(NPI.3) <- as.Date(dates.3)
rownames(NPI.3) <- county$geoid
county$NPI.3 <- replicate(dim(county)[1], runif(dim(county)[1], 0.32, 0.88)) 
NPI.3 <- NPI.3 * county$NPI.3

# Stitch together

NPI <- cbind.data.frame(pre.NPI, NPI.1, NPI.2, NPI.3)

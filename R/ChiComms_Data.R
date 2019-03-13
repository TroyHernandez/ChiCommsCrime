
################################################################
chicommunities <- geojsonio::geojson_read("data/ChiComms.geojson",
                                          what = "sp")
# From here:
# https://data.cityofchicago.org/Facilities-Geographic-Boundaries/Boundaries-Community-Areas-current-/cauq-8yn6

################################################################
#   install.packages("geojsonio")
population <- read.csv("data/NhoodPopulation.csv", stringsAsFactors = F)
#  From here:
# https://datahub.cmap.illinois.gov/dataset/community-data-snapshots-raw-data

population$GEOG[56] <- "OHare"
population$GEOG[66] <- "Loop"
population <- population[order(population$GEOG), ]
population <- population[rank(as.character(chicommunities$community)), ]

growth.rate <- (exp(log(population$TOT_POP / population$X2000_POP) / 18) - 1)*100
growth <- (population$TOT_POP / population$X2000_POP - 1) * 100

################################################################
# as.character(chicommunities$community)[order(as.character(chicommunities$community))]
# Checking population
# data.frame(population$GEOG[rank(as.character(chicommunities$community))],
# chicommunities$community)

chicommunities$pop00 <- population$X2000_POP
chicommunities$pop02 <- population$X2000_POP * (1 + growth.rate / 100) ^ 2
chicommunities$pop18 <- population$TOT_POP
chicommunities$growth.rate <- growth.rate
chicommunities$growth <- growth

################################################################
# https://data.cityofchicago.org/Public-Safety/Crimes-2001-to-present/ijzp-q8t2
dat <- read.csv("data/Crimes_2001_to_present.csv")

total.crimes <- as.numeric(table(dat$Community.Area)[-1])[as.numeric(as.character(chicommunities$area_numbe))]
total.crimes.year <- table(dat$Community.Area, dat$Year)[-1, -c(1, 19)][as.numeric(as.character(chicommunities$area_numbe)), ]

total.crimes.02 <- total.crimes.year[, 1]
total.crimes.18 <- total.crimes.year[, ncol(total.crimes.year)]

# Citywide calculations
# sum(total.crimes.18) / sum(total.crimes.02)

# sum(total.crimes.02) / sum(chicommunities$pop02)
# sum(total.crimes.18) / sum(chicommunities$pop18)

# (sum(total.crimes.18) / sum(chicommunities$pop18)) / (sum(total.crimes.02) / sum(chicommunities$pop02)) - 1


crime.change <- total.crimes.year[, ncol(total.crimes.year)] /
                  total.crimes.year[, 1] - 1

per.cap.crime.change <- ((total.crimes.year[, ncol(total.crimes.year)] /
                            population$TOT_POP) /
                           (total.crimes.year[, 1] / chicommunities$pop02)) - 1

chicommunities$total.crimes.02 <- total.crimes.02
chicommunities$total.crimes.18 <- total.crimes.18
chicommunities$total.crimes <- total.crimes
chicommunities$crime.change <- crime.change
chicommunities$per.cap.crime.change <- per.cap.crime.change

saveRDS(chicommunities, "ChiComms.Rds")

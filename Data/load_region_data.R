library(cdcfluview)
library(data.table)
library(MMWRweek)
stopifnot(packageVersion("MMWRweek") >= "0.1.3")
## contains Sebastian's patch for non-English locales

usflu <- ilinet(region = "hhs", years = 1997:2019)
# load data date: 03.03.2020
usflu <- as.data.frame(usflu)
usflu <- subset(usflu, year <= 2019)  # discard incomplete 19/20 season

data <- usflu[c("region_type", "region", "year", "week", "weighted_ili")]

## CDC data source has zeros in early off-seasons that are actually missing data
data[data$weighted_ili == 0,]$weighted_ili <- NA

data <- as.data.table(data)
data[, week_number := max(week), by = year]
data[, InPeriod := week/week_number, by = year]

data[, time := MMWRweek2Date(MMWRyear = year, MMWRweek = week)]
data$time_index <- as.integer(data$time)

data <- as.data.frame(data)


save(data, file = here::here("Data", "Regionflu.RData"))

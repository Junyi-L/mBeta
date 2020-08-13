library(data.table)

load(here::here("Data", "usflu.RData"))

data$season <- ifelse(
  data$week <= 30,
  paste0(data$year - 1, "/", data$year),
  paste0(data$year, "/", data$year + 1)
)

## Season week column: week number within season (starts at week 31)
data$season_week <- sapply(seq_len(nrow(data)), function(row_ind) {
  sum(data$season == data$season[row_ind] & data$time_index <= data$time_index[row_ind])
})
data$season_week[data$season == "1997/1998"] <- data$season_week[data$season == "1997/1998"] + 9

## Sebastian's alternative implementation (validation)
stopifnot(all.equal(data$season_week,
  ifelse(data$week > 30,
         data$week - 30L,
         52 + (MMWRweek::MMWRweek(paste0(data$year,"-01-01"))$MMWRweek == 53) - 30 + data$week)
))

data$InSeason <- data$week %in% c(1:20, 40:53)


data <- as.data.table(data)

#Remove pandemic season 2008/2009 and 2009/2010
data[season %in% c("2008/2009","2009/2010")]$weighted_ili <- NA

table(data$season)
## 1997/1998 1998/1999 1999/2000 2000/2001 2001/2002 2002/2003 2003/2004 2004/2005
##        44        52        52        52        52        52        53        52
## 2005/2006 2006/2007 2007/2008 2008/2009 2009/2010 2010/2011 2011/2012 2012/2013
##        52        52        52        53        52        52        52        52
## 2013/2014 2014/2015 2015/2016 2016/2017 2017/2018 2018/2019 2019/2020
##        52        53        52        52        52        52        22

# remove season 1997/1998 and season 2019/2020 because they are incomplete.
data <- data[ !( season %in% c("1997/1998", "2019/2020")),]

# take season 2014/2015, 2015/2016, 2016/2017 and 2017/2018 as test period.
data[, train := !(season %in% c( "2015/2016",
                                 "2016/2017",
                                 "2017/2018",
                                 "2018/2019"))]

data[, week_number := max(week), by = season]
data[, InPeriod := season_week/week_number, by = year]

data[, sin_InPeriod1 := sin(2 * pi * InPeriod)]
data[, cos_InPeriod1 := cos(2 * pi * InPeriod)]
data[, sin_InPeriod2 := sin(4 * pi * InPeriod)]
data[, cos_InPeriod2 := cos(4 * pi * InPeriod)]
data[, sin_InPeriod3 := sin(6 * pi * InPeriod)]
data[, cos_InPeriod3 := cos(6 * pi * InPeriod)]
data[, cos_InPeriod4 := cos(8 * pi * InPeriod)]
data[, sin_InPeriod4 := sin(8 * pi * InPeriod)]
data[, cos_InPeriod5 := cos(10 * pi * InPeriod)]
data[, sin_InPeriod5 := cos(10 * pi * InPeriod)]

data[, x := (season_week == 22)]
data[, y := (season_week == 23)]

data[, weighted_ili_org := weighted_ili/100]
data[, seasonNr := as.numeric(as.factor(season))]
data[, prep := seasonNr < 12]
data[, SIndex := pmin(seasonNr, 13)]

data <- as.data.frame(data)
save(data, file = here::here("Data", "data_holidays.RData"))

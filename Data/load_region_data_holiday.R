library(data.table)

load(here::here("Data", "Regionflu.RData"))

data$season <- ifelse(
  data$week <= 30,
  paste0(data$year - 1, "/", data$year),
  paste0(data$year, "/", data$year + 1)
)

## Season week column: week number within season (starts at week 31)
season_list <- unique(data.frame(season = data$season, time_index = data$time_index))
data$season_week <- rep(sapply(seq_len(nrow(season_list)), function(row_ind) {
  sum(season_list$season == season_list$season[row_ind] &
        season_list$time_index <= season_list$time_index[row_ind])
}), each = 10)
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

# remove season 1997/1998 and season 2019/2020 because they are incomplete.
data <- data[ !( season %in% c("1997/1998", "2019/2020")),]

# take season 2014/2015, 2015/2016, 2016/2017 and 2017/2018 as test period.
data[, train := !(season %in% c( "2015/2016",
                                "2016/2017",
                                "2017/2018",
                                "2018/2019"))]

data[, week_number := max(week), by = season]
data[, InPeriod := season_week/week_number, by = year]

data[, x := (season_week == 22)]
data[, y := (season_week == 23)]

data[, weighted_ili_org := weighted_ili/100]
data[, seasonNr := as.numeric(as.factor(season))]
data[, SIndex := pmin(seasonNr, 13)]

data <- as.data.frame(data)
save(data, file = here::here("Data", "Region_data_holidays.RData"))


# create the adjacent matrix AM, a symmetric matrix
AM <- matrix(0, 10, 10)
i.upr <- which(lower.tri(AM, diag = FALSE), arr.ind=TRUE)
x <- c(1,0,0,0,0,0,0,0,0,
       1,0,0,0,0,0,0,0,
       1,1,0,0,0,0,0,
       1,1,1,0,0,0,
       0,1,1,0,0,
       1,1,1,0,
       1,0,0,
       1,1,
       1)
AM[i.upr] <- x
AM <- AM + t(AM)
colnames(AM) <- paste("Region", 1:10)
save(AM, file = here::here("Data", "adj_matrix.RData"))

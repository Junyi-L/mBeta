load(file = here::here("./Data/Regionflu.RData"))
library(data.table)
data$season <- ifelse(
  data$week <= 30,
  paste0(data$year - 1, "/", data$year),
  paste0(data$year, "/", data$year + 1)
)
## Season week column: week number within season
season_list <- unique(data.frame(season = data$season, time_index = data$time_index))
data$season_week <- rep(sapply(seq_len(nrow(season_list)), function(row_ind) {
  sum(season_list$season == season_list$season[row_ind] & 
        season_list$time_index <= season_list$time_index[row_ind])
}), each = 10)

data <- data.table(data)

data$season_week[data$season == "1997/1998"] <- data$season_week[data$season == "1997/1998"] + 9
data$InSeason <- data$week %in% c(1:20, 40:53)


#Remove pandemic season 2008/2009 and 2009/2010
data[season %in% c("2008/2009","2009/2010")]$weighted_ili <- NA

# > table(data$season)
# 
# 1997/1998 1998/1999 1999/2000 2000/2001 2001/2002 2002/2003 2003/2004 2004/2005 2005/2006 2006/2007 2007/2008 2008/2009 2009/2010 2010/2011 2011/2012 
# 44        52        52        52        52        52        53        52        52        52        52        53        52        52        52 
# 2012/2013 2013/2014 2014/2015 2015/2016 2016/2017 2017/2018 2018/2019 
# 52        52        53        52        52        52        22 

# remove season 1997/1998 and season 2019/2020 because they are incomplete.
data <- data[ !( season %in% c("1997/1998", "2019/2020")),]

# take season 2014/2015, 2015/2016, 2016/2017 and 2017/2018 as test period.
data[, train := !(season %in% c( "2015/2016", 
                                "2016/2017", 
                                "2017/2018", 
                                "2018/2019"))]

data[, week_number := max(week), by = season]
data[, InPeriod := season_week/week_number, by = year]

# data[, sin_InPeriod1 := sin(2 * pi * InPeriod)]
# data[, cos_InPeriod1 := cos(2 * pi * InPeriod)]
# data[, sin_InPeriod2 := sin(4 * pi * InPeriod)]
# data[, cos_InPeriod2 := cos(4 * pi * InPeriod)]
# data[, sin_InPeriod3 := sin(6 * pi * InPeriod)]
# data[, cos_InPeriod3 := cos(6 * pi * InPeriod)]
# data[, cos_InPeriod4 := cos(8 * pi * InPeriod)]
# data[, sin_InPeriod4 := sin(8 * pi * InPeriod)]
# data[, cos_InPeriod5 := cos(10 * pi * InPeriod)]
# data[, sin_InPeriod5 := cos(10 * pi * InPeriod)]


# data[, CH := (as.Date(paste(year,12,25,sep = "-")) < time + 7 & 
#                 as.Date(paste(year,12,25,sep = "-")) >= time)]
# data[, NY := (as.Date(paste((lubridate::year(time) + 1),"01","01",sep = "-")) < time + 7 & 
#                 as.Date(paste((lubridate::year(time) + 1),"01","01",sep = "-")) >= time) |
#        time == as.Date(paste((lubridate::year(time) ),"01","01",sep = "-")) ]
# 

# # Thanksgiving week from 1997 to 2018
# allyears <- 1997 : 2018
# TGdate <- sapply(allyears, FUN = function(year){
#   start <- as.Date(paste(year,"10","15",sep = "-"))
#   start_seq <- start + 0:6
#   starti <- start_seq[which(lubridate::wday(start_seq, week_start = getOption("lubridate.week.start", 7)) == 5)]
#   
#   seq4 <- starti + seq(0, 8*7,by = 7)
#   seq11 <- seq4[month(seq4) == 11]
#   return(seq11[4])
# })
# class(TGdate) <- "Date"
# 
# data[,TG := TGdate[lubridate::year(time) - allyears[1] + 1] < time + 7 & 
#        TGdate[lubridate::year(time) - allyears[1] + 1] >= time]
# 
# data[, TG_lag1 := shift(TG, n = 1L, type = "lag")]
# data[, NY_lag1 := shift(NY, n = 1L, type = "lag")]
# 
# data[is.na(TG_lag1) == 1,]$TG_lag1 = FALSE
# data[is.na(NY_lag1) == 1,]$NY_lag1 = FALSE
# 

data[, x := (season_week == 22)]
data[, y := (season_week == 23)]

# for beta model
# logit_FUN <- function(x){
#   qlogis(x/100)
# }


# data[, lastvalue := shift(weighted_ili,n = 1L, type = "lag")]
# data[, lastvalue := shift(weighted_ili,n = 1L, type = "lag")]
# data[, lastvalue2 := shift(weighted_ili,n = 2L, type = "lag")]
# 
# data[, lastvalue_log := log(lastvalue)]
# data[, lastvalue_logit := logit_FUN(lastvalue)]
# data[, lastvalue_log2 := log(lastvalue2)]
# data[, lastvalue_logit2 := logit_FUN(lastvalue2)]

data[, weighted_ili_org := weighted_ili/100]
data[, seasonNr := as.numeric(as.factor(season))]
data[, SIndex := pmin(seasonNr, 13)]
# data is data.frame
data <- data.frame(data)
save(data, file = here::here("./Data/Region_data_holidays.RData"))


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
save(AM, file = here::here("./Data/adj_matrix.RData"))

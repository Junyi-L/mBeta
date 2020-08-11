library(ggplot2)


load(file = here::here("./Data/data_holidays.RData"))
data <- data[is.na(data$weighted_ili) == FALSE, ]

pdf(file = here::here("./Plots/wILI.pdf"), width = 8, height = 4)
ggplot() +
  geom_vline(xintercept = 22, linetype="dashed", color = "gray", size = 1) +
  geom_line(data = data, aes(x=season_week, y=weighted_ili, group = season)) +
  scale_color_manual(values= "grey") +
  xlab("Season week") +
  ylab("wILI (%)") +
  theme_bw() +
  theme(legend.position = "none") 

dev.off()

library(ggplot2)
library(data.table)

load(file = here::here("./Data/usflu.RData"))
data$season <- ifelse(
  data$week <= 30,
  paste0(data$year - 1, "/", data$year),
  paste0(data$year, "/", data$year + 1)
)
## Season week column: week number within season
data$season_week <- sapply(seq_len(nrow(data)), function(row_ind) {
  sum(data$season == data$season[row_ind] & data$time_index <= data$time_index[row_ind])
})



data <- data.table(data)

data$season_week[data$season == "1997/1998"] <- data$season_week[data$season == "1997/1998"] + 9
data$InSeason <- data$week %in% c(1:20, 40:53)
# remove season 1997/1998 and season 2019/2020 because they are incomplete.
data <- data[ !( season %in% c("1997/1998", "2019/2020")),]
# take season 2014/2015, 2015/2016, 2016/2017 and 2017/2018 as test period.
data[, train := !(season %in% c( "2015/2016", 
                                 "2016/2017", 
                                 "2017/2018", 
                                 "2018/2019"))]

data[, group := ifelse(time <= "2010-07-25" & time >= "2008-07-27", 2,
                       ifelse(time > "2010-07-25", 3, 1) )]
data[, weighted_ili_org := weighted_ili/100]

data <- data.frame(data)

# shade_l <- as.Date(c("1999-05-21","2000-05-20","2001-05-21","2002-05-21"))
# shade_r <- as.Date(c("1999-09-24","2000-09-23","2001-09-24","2002-09-24"))
library(gridExtra)
pdf(file = here::here("./Plots/nwILI.pdf"), width = 8, height = 2)
data$group <- as.factor(data$group)
p1 <- ggplot(data = data, aes(x=time, y=weighted_ili, group = group)) +
  geom_line(aes(color = group)) +
  scale_color_manual(values=c("black", "grey", "black")) +
  xlab("Year") +
  ylab("wILI (%)") +
  ylim(0, 8) +
  theme_bw() +
  theme(legend.position = "none") +
  xlab("Year") +
  ylab("wILI (%)") 
p1
dev.off()



# ################# fanplots
library(data.table)
result4 <- readRDS(file = here::here("./Results/Forecast_mBeta4.rds"))
load(file = here::here("./Data/Region_data_holidays.RData"))
# General setting
library("surveillance")
library("HIDDA.forecasting")  # from https://HIDDA.github.io/forecasting/
library(ggplot2)
pal <- colorRampPalette(c("darkgreen", "gray93"))
width <- 5
height <- 2

library(data.table)
# prepare for PIT
data <- as.data.table(result4$result)
data[, PIT := pbeta(q = data_set.weighted_ili_org,
                    shape1 = shape1,
                    shape2 = shape2)]

for(r in 1:10){
  # Fan plot for region r
  quantilesm <- result4$quantile_matrix[result4$result$data_set.region ==
                                          paste("Region",r),]
  quantilesm <- as.matrix(quantilesm)
  ym <- result4$result$data_set.weighted_ili_org[result4$result$data_set.region ==
                                                   paste("Region",r)]
  LSm <- -result4$result$log_score[result4$result$data_set.region ==
                                     paste("Region",r)]
  DSSm <- result4$result$DS_score[result4$result$data_set.region ==
                                    paste("Region",r)]
  meanm <- result4$result$pt_pred[result4$result$data_set.region ==
                                    paste("Region",r)]
  upper_ylim <- max(quantilesm)
  ylimi <- c(0, upper_ylim)
  upper_score <- max(LSm,DSSm)
  lower_score <- min(LSm,DSSm)
  probs <- (1:99)/100

  wILI.sts <- sts(observed = ym,
                  epoch = result4$result$data_set.time[result4$result$data_set.region ==
                                                         paste("Region",r)],
                  epochAsDate = TRUE)

  pdf(file = here::here(paste0("./Plots/Fan/mBeta_Region", r,".pdf")),
      width = width,
      height = height)

  par(mar = c(1.5,2,0,0)+.5, cex.lab = 0.8, las = 1, mgp = c(1.5,0.5,0),
      lab = c(5,5,7), cex.axis = 0.8)

  osaplot(
    quantiles = quantilesm * 100, probs = 1:99/100,
    observed = ym * 100,
    means = meanm * 100,
    scores = cbind(LS = LSm,
                   DSS = DSSm),
    xlab = "",
    ylim = ylimi * 100,
    ylab = "wILI (%)",
    main = "",
    # cex.lab=0.6,
    # cex.main=0.6, cex.sub=0.6, cex.axis=0.6,
    fan.args = list(ln = c(0.1,0.9), rlab = NULL),
    observed.args = list(cex = 0.2),
    scores.args = list(ylim = c(-20, 20),
                       yaxs = "i",
                       xaxt = "n",
                       yaxt = "n",
                       col = gray(c(0.5, 0)),
                       panel.first = {
                         axis(2, c(-10,0,10), lwd = 0, lwd.ticks = 1)
                         # abline(h = c(15, 30), col = "grey92",
                         #        v = match(2016:2019, strftime(epoch(wILI.sts), "%Y")))
                         # box()
                       }),
    legend.args = if(r == 1) list(cex = 0.4,
                       inset = c(0.04,0)),
    key.args = if(r == 1) list(space = 1,
                     start = 198,
                     ylim = c(2,5),
                     rcex = 0.6),
    las = TRUE
  )
   addFormattedXAxis(wILI.sts, epochsAsDate = TRUE,
                    xaxis.tickFreq = list("%m"=atChange, "%Y"=atChange),
                    xaxis.labelFreq = list("%Y"=atMedian),
                    xaxis.labelFormat="%Y")
  dev.off()


  pdf(file = here::here(paste0("./Plots/Fan/PIT_Region", r,".pdf")),
      width = 1.94,
      height = 1.83)
  par(mar = c(1.5,2,0,0)+.5, cex.lab = 0.8, las = 1,
      cex.axis = 0.8, mgp = c(1.5,0.5,0))
  hist(data[data_set.region == paste("Region",r),]$PIT,
       freq = FALSE,
       ylab = "Relative Frequency",
       xlab = "",
       main = "")
  abline(h = 1, lty=2)
  dev.off()
}

# wili for all years-------------------------------------------
library(ggplot2)
library(data.table)

load(file = here::here("./Data/Regionflu.RData"))
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

data[, miss := sum(is.na(weighted_ili_org)) > 0, by = time]
data[miss == TRUE]$weighted_ili_org <- NA

data <- data.frame(data)

# shade_l <- as.Date(c("1999-05-21","2000-05-20","2001-05-21","2002-05-21"))
# shade_r <- as.Date(c("1999-09-24","2000-09-23","2001-09-24","2002-09-24"))
library(gridExtra)
pdf(file = here::here("./Plots/wILI1.pdf"), width = 8, height = 9)
data$group <- as.factor(data$group)
p1 <- ggplot(data = data[data$region %in% paste("Region", 1:5),], aes(x=time, y=weighted_ili, group = group)) +
  geom_line(aes(color = group)) +
  scale_color_manual(values=c("black", "grey", "black")) +
  xlab("Year") +
  ylab("wILI (%)") +
  ylim(0, 12.5) +
  geom_vline(xintercept = data$time[which(data$train == FALSE)[1]],
             linetype="dashed", color = "black", size = 1)  +
  theme_bw() +
  theme(legend.position = "none")

p1 + facet_grid(rows = vars(region),  scales="free_x" , as.table=TRUE) + xlab("Year") +
  ylab("wILI (%)")
dev.off()

pdf(file = here::here("./Plots/wILI2.pdf"), width = 8, height = 9)
data$group <- as.factor(data$group)
p1 <- ggplot(data = data[data$region %in% paste("Region", 6:10),],
             aes(x=time, y=weighted_ili, group = group)) +
  geom_line(aes(color = group)) +
  scale_color_manual(values=c("black", "grey", "black")) +
  xlab("Year") +
  ylab("wILI (%)") +
  ylim(0, 12.5) +
  geom_vline(xintercept = data$time[which(data$train == FALSE)[1]],
             linetype="dashed", color = "black", size = 1)  +
  theme_bw() +
  theme(legend.position = "none")

p1 + facet_grid(rows = vars(region),  scales="free_x" , as.table=TRUE) + xlab("Year") +
  ylab("wILI (%)")
dev.off()

library(betareg)
library(data.table)
library(ggplot2)
library(e1071)
library(logitnorm)

# prepare data
load(file = here::here("./Data/data_holidays.RData"))
missing <- is.na(data$weighted_ili)
firstnonmiss <- head(which(!missing), 1)
data <- data[firstnonmiss : dim(data)[1], ]
data <- data.table(data)



skewness(data$weighted_ili_org, na.rm = TRUE)
kurtosis(data$weighted_ili_org, na.rm = TRUE)

source(file = here::here("./Appl1/Beta.R"))
load(file = here::here("./Results/sarima_fit.RData"))


mean <- predict(Beta_lags_fit_a, data, type = "response")
# Pearson residuals
plot(mean, data_a$resid, ylim = c(-4,4), main = "beta")
plot(plogis(sarima_fit$fitted), sarima_fit$ARIMA_resid, ylim = c(-4,4), main = "sarima")

# Residuals on the original scale
Beta_resid <- residuals(Beta_lags_fit_a, type = "response")
Beta_resid_table <- data.frame(time_index = data1$time_index, resid = Beta_resid)
# residuals and mean are of different length due to missin values
data_a <- merge(data,Beta_resid_table, by = "time_index", all.x = TRUE)
plot(mean, data_a$resid, main = "beta", ylim = c(-0.02, 0.02))

moment_ARIMA <- function(mu, sigma, option = "mean"){
  if (is.na(mu) | is.na(sigma)) return(NA_real_)
  momentsLogitnorm(mu = mu, sigma = sigma)[[option]]
}

ARIMA_mean <- mapply(moment_ARIMA, mu = sarima_fit$fitted,
                     sigma = sqrt(sarima_fit$sigma2), MoreArgs = list(option = "mean"))

ARIMA_var <-  mapply(moment_ARIMA, mu = sarima_fit$fitted,
                     sigma = sqrt(sarima_fit$sigma2), MoreArgs = list(option = "var"))

ARIMA_resid <- data$weighted_ili_org - ARIMA_mean
sarima_fit$ARIMA_resid <- ARIMA_resid
plot(plogis(sarima_fit$fitted), sarima_fit$ARIMA_resid, 
     main = "sarima", ylim = c(-0.02, 0.02))

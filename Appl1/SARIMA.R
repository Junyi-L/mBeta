
library(forecast)
library(logitnorm)

load(file = here::here("Data", "data_holidays.RData"))

missing <- is.na(data$weighted_ili_org)
firstnonmiss <- head(which(!missing), 1)
data <- data[firstnonmiss : nrow(data), ]

wILi <- logit(data$weighted_ili_org)
wILi <- ts(wILi, frequency = 52)

sarima_fit <-
  auto.arima(wILi,trace = TRUE, allowdrift = TRUE, allowmean = TRUE, seasonal = TRUE)
## Best model: ARIMA(2,0,0)(1,1,0)[52] with drift


moment_ARIMA <- function(mu, sigma, option = "mean"){
  if (is.na(mu) | is.na(sigma)) return(NA_real_)
  momentsLogitnorm(mu = mu, sigma = sigma)[[option]]
}

ARIMA_mean <- mapply(moment_ARIMA, mu = sarima_fit$fitted,
                   sigma = sqrt(sarima_fit$sigma2), MoreArgs = list(option = "mean"))

ARIMA_var <-  mapply(moment_ARIMA, mu = sarima_fit$fitted,
                     sigma = sqrt(sarima_fit$sigma2), MoreArgs = list(option = "var"))

ARIMA_resid <- (data$weighted_ili_org - ARIMA_mean) / sqrt(ARIMA_var)

ARIMA_loglik <- mapply(dlogitnorm, data$weighted_ili_org, mu = sarima_fit$fitted,
                       sigma = sqrt(sarima_fit$sigma2), MoreArgs = list(log = TRUE))

sarima_fit$loglik_ad <- sum(ARIMA_loglik, na.rm = TRUE)
sarima_fit$LL <- ARIMA_loglik
sarima_fit$ARIMA_resid <- ARIMA_resid


save(sarima_fit, file = here::here("Results", "sarima_fit.RData"))

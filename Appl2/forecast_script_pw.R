## Beware that the initial fitting on the training data is relatively fast
## but the one-step-ahead (rolling) forecasts for all five models take hours

# prepare data
library(tsibble)
library(betareg)
library(data.table)
library(xtable)
library(surveillance)
load(file = here::here("./Data/Region_data_holidays.RData"))
load(file = here::here("./Data/adj_matrix.RData"))
source(file = here::here("./Appl2/fit_mBeta.R"))
source(file = here::here("./Appl2/forecast_mBeta.R"))

data <- as.data.table(data)
# if some regions have missing data at one time point,
# the all observations at this time point are taken as missing.
data[, miss := sum(is.na(weighted_ili_org)) > 0, by = time]
data[miss == TRUE]$weighted_ili_org <- NA

tsibData <- as_tsibble(data, key = region, index = time, regular = TRUE)

index_tsi <- index_var(tsibData)
key_tsi <- key_vars(tsibData)
nkey <- length(unique(tsibData[[key_tsi]]))
num_rows <- nrow(tsibData[tsibData$train == FALSE, ])
ntest <- num_rows/nkey
# tsibData is ordered first by region, then by time
test_index <- tsibData[tsibData$train == FALSE, ][[index_tsi]][1 : ntest]
train_data <- data[train == TRUE]
train_data <- as_tsibble(train_data, key = region, index = time, regular = TRUE)

# order matrix
OMat <- nbOrder(AM, maxlag = 10)
# negative profile log-likelihood
pll <- function(logd, OMat, beta_formula, train_data){
  d <- exp(logd)
  neW <- zetaweights(OMat, d)
  Beta_fit <- fit_mBeta(beta_formula,
                        tsiObj = train_data,
                        AM = neW)
  negll <- -logLik(Beta_fit)
  return(negll)
}

# model formulations
beta_formulas <- list(
  weighted_ili_org ~ region + x + y + Har(3,frac = InPeriod) +
    AR(4)+ NB(1) | region + (SIndex + Har(4, frac = InPeriod)),
  weighted_ili_org ~ region + x + y + Har(3,frac = InPeriod) +
    AR(4):region + NB(1) | region + (SIndex + Har(4, frac = InPeriod)),
  weighted_ili_org ~ region + x:region + y:region + Har(3,frac = InPeriod):region +
    AR(4):region | region + (SIndex + Har(4, frac = InPeriod)):region,
  weighted_ili_org ~ region + x:region + y:region + Har(3,frac = InPeriod):region +
    AR(4):region + NB(1) | region + (SIndex + Har(4, frac = InPeriod)):region,
  weighted_ili_org ~ region + x:region + y:region + Har(3,frac = InPeriod):region +
    AR(4):region + NB(1):region | region + (SIndex + Har(4, frac = InPeriod)):region
)



#-----------------------------------------------------
# compare AICc in training data
npar <- numeric()
AIC <- numeric()
AICc <- numeric()
BIC <- numeric()
logLik <- numeric()
d <- numeric()
hessian <- numeric()
conv <- numeric()
fit <- list()
for(i in 1 : length(beta_formulas)){
  cat("Fitting model", i, "... ")
  beta_formula <- beta_formulas[[i]]
  ptm <- proc.time()
  if (i == 3){
    # model 3 does not contain neighborhood part
    Update_mBeta <- fit_mBeta(beta_formula,
                              tsiObj = train_data)
    npar <- c(npar, dim(Update_mBeta$vcov)[1])
    fit[[i]] <- Update_mBeta
  }else{
    optim_beta <- optim(par = -0.69, fn = pll, method = "BFGS", OMat = OMat,
                        beta_formula = beta_formula,
                        train_data = train_data,
                        control = list(trace = 1, REPORT = 1), hessian = TRUE)
    hessian[i] <- unlist(optim_beta$hessian)
    conv[i] <- optim_beta$convergence
    di <- exp(optim_beta$par)
    d[i] <- di
    AM <- zetaweights(OMat, di)
    Update_mBeta <- fit_mBeta(beta_formula,
                              tsiObj = train_data,
                              AM = AM)
    fit[[i]] <- Update_mBeta
    npar <- c(npar, dim(Update_mBeta$vcov)[1] + 1)
    }
  cat((proc.time() - ptm)[["elapsed"]], "s\n")
  if (i==1) model_n <- Update_mBeta$nobs else stopifnot(Update_mBeta$nobs == model_n)
  logLik <- c(logLik, Update_mBeta$loglik)
}


AIC <- - 2 * logLik  + 2 * npar
AICc <- AIC + (2 * (npar)^2 + 2 * npar)/(model_n - npar - 1)
BIC <- - 2 * logLik + log(model_n) * npar

Model_name <- c(
  "M1: $\\beta_{r,k} = \\beta_{k}$, $\\beta_{r}^{(\\nu)} = \\beta^{(\\nu)}$, $\\beta_{r}^{(\\phi)} = \\beta^{(\\phi)}$, $\\gamma_r = \\gamma$",
  "M2: $\\beta_{r}^{(\\nu)} = \\beta^{(\\nu)}$, $\\beta^{(\\phi)} = \\beta^{(\\phi)}$, $\\gamma_r = \\gamma$",
  "M3: $\\gamma_r = 0$",
  "M4: $\\gamma_r = \\gamma$",
  "M5: full model"
)
comp <- data.frame(Model = Model_name,
                   logLik,
                   AIC,
                   AICc,
                   BIC,
                  npar)
save(list = c("comp", "d", "hessian", "fit"), 
     file = here::here("./Results/mBeta_fit_PW.RData"))

d5 <- log(d[5])
h5 <- sqrt(hessian[5])
CIupper <- exp(d5 + 1.96 * 1/h5)
CIlower <- exp(d5 - 1.96 * 1/h5)


printformat1 <- function(x) {
  paste0(formatC(round(x, digits = 0), format='f', digits=0 ), " (",rank(-x), ")")
}
printformat <- function(x) {
  paste0(formatC(round(x, digits = 0), format='f', digits=0 ), " (",rank(x), ")")
}
comp$logLik <- printformat1(comp$logLik)
comp[, 3:5] <- sapply(comp[, 3:5], printformat)

print(xtable(comp, align = "ll|rrrrr",
             caption = 'Summaries of model fit.
             Models are ordered by complexity (number of estimated parameters, \"npar\").
             Ranks are shown in parantheses.
             The log-likelihood (LL) is ranked descending,
             and AIC, AICc, BIC are ranked ascending.',
             label = "tab:mBetafit",
             digits = 0),
      type = "latex",
      include.colnames = TRUE,
      include.rownames = FALSE,
      hline.after = c(-1, 0, 5),
      sanitize.text.function=function(x){x},
      file=here::here("./Results/mBetafit_PW.tex"),
      size="\\fontsize{9pt}{10pt}\\selectfont",
      comment = FALSE)
#----------------------------------------
#
#-------------------------------------------------------
# One-step-ahead forecast

## the design is based on the "ili_national" example in
## https://github.com/reichlab/article-disease-pred-with-kcde/blob/master/inst/code/prediction/sarima-prediction.R

for(i in 1 : length(beta_formulas)){
  
  ptm <- proc.time()
  beta_formula <- beta_formulas[[i]]
  data_set_results <- data.frame(data_set = data[data$train == FALSE,],
                                 model = paste0("mBeta"),
                                 prediction_horizon = 1,
                                 prediction_time = rep(NA, num_rows),
                                 log_score = rep(NA_real_, num_rows),
                                 pt_pred = rep(NA_real_, num_rows),
                                 AE = rep(NA_real_, num_rows),
                                 precision = rep(NA_real_, num_rows),
                                 shape1 = rep(NA_real_, num_rows),
                                 shape2 = rep(NA_real_, num_rows),
                                 DS_score = rep(NA_real_, num_rows),
                                 vari = rep(NA_real_, num_rows),
                                 stringsAsFactors = FALSE,
                                 row.names = NULL)
  
  class(data_set_results$prediction_time) <- class(data$time)
  quantile_matrix_name <- data.frame(time = data[data$train == FALSE,]$time,
                                     region = data[data$train == FALSE,]$region)
  quantile_matrix <- matrix(nrow = nrow(data[data$train == FALSE,]), ncol = 99)
  # fix decay parameter during forecast
  # model 3 doesn't contain neighborhood effect
  if(i != 3){
    di <- d[i]
    AM <- zetaweights(OMat, di)
  }else AM <- NULL 
  
  for(time_ind in 1 : length(test_index)) {
    prediction_time_ind <- test_index[time_ind]
    cat("M", i, " @ ", format(prediction_time_ind), "\n", sep = "")
    results_row_ind <- which(data_set_results$data_set.time == prediction_time_ind)
    ## Set values describing case in data_set_results
    data_set_results$prediction_time[results_row_ind] <-
      prediction_time_ind
    
    ## Observed value at prediction time -- used in calculating log
    ## score and absolute error
    observed_prediction_target <-
      data[data$time == prediction_time_ind, weighted_ili_org]
    
    newdata <- data[
      time < prediction_time_ind, ]
    newdata <- as_tsibble(newdata, key = region, index = time, regular = TRUE)
    
    Update_mBeta <- fit_mBeta(beta_formula,
                              tsiObj = newdata,
                              AM = AM)
    
    foredata <- as_tsibble( data[
      time == prediction_time_ind, ], index = time, key = region)
    
    predict_result <-
      forecast_mBeta(Update_mBeta, foredata, type = "response")
    data_set_results$pt_pred[results_row_ind] <- predict_result
    data_set_results$precision[results_row_ind] <- precision <-
      forecast_mBeta(Update_mBeta, foredata, type = "precision")
    
    data_set_results$shape1[results_row_ind] <- shape1 <- predict_result * precision
    data_set_results$shape2[results_row_ind] <- shape2 <- precision - shape1
    
    ## Compute log score of distribution prediction
    
    data_set_results$log_score[results_row_ind] <-
      dbeta(observed_prediction_target,
            shape1 = shape1,
            shape2 = shape2,
            ncp = 0,
            log = TRUE)
    
    
    
    ## Compute absolute error of point prediction
    data_set_results$AE[results_row_ind] <-
      abs(predict_result -
            observed_prediction_target)
    
    quantile_matrix[results_row_ind,] <- mapply(qbeta,
                                                p = (1:99)/100,
                                                MoreArgs = list(shape1 = shape1,
                                                                shape2 = shape2,
                                                                ncp = 0))
    
    
    
    vari <- predict_result * (1 - predict_result)/(1 + precision)
    data_set_results$DS_score[results_row_ind] <- log(vari) +
      (predict_result - observed_prediction_target)^2/vari
    data_set_results$vari[results_row_ind] <- vari
    
  } # prediction_time_ind
  
  run_time <- proc.time() - ptm
  npar <- length(Update_mBeta$coefficients$mean) + length(Update_mBeta$coefficients$precision)
  mBeta <- list(result = data_set_results,
                run_time = run_time,
                npar = npar,
                model = beta_formula,
                quantile_matrix = quantile_matrix)
  saveRDS(mBeta,
          file = here::here(paste0("./Results/Forecast_mBeta_pw_", i,".rds")))
  
} # beta_formula




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
# profil likelihood
pll <- function(logd, OMat, beta_formula, train_data){
  d <- exp(logd)
  neW <- zetaweights(OMat, d, maxlag = max(OMat), normalize = TRUE)
  Beta_fit <- fit_mBeta(beta_formula,
            tsiObj = train_data,
            AM = neW)
  ll <- -logLik(Beta_fit)
  return(ll)
}

# optim_beta1 <- optim(par = 1, pll, method = "BFGS", OMat = OMat, beta_formula = beta_formula,
#       train_data = train_data)
# 
# beta_formula <- weighted_ili_org ~ region + x:region + y:region + Har(3,frac = InPeriod):region +
#   AR(4):region + NB(1):region | region + (SIndex + Har(4, frac = InPeriod)):region
# 
# optim_beta5 <- optim(par = 1, pll, method = "BFGS", OMat = OMat, beta_formula = beta_formula,
#                      train_data = train_data)


## Beware that the initial fitting on the training data is relatively fast
## but the one-step-ahead (rolling) forecasts for all five models
## take approx. 5+10+3*45 minutes = 2.5 hours


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
hessian <- list()
conv <- numeric()
for(i in 1 : length(beta_formulas)){
  cat("Fitting model", i, "... ")
  beta_formula <- beta_formulas[[i]]
  ptm <- proc.time()
  optim_beta <- optim(par = 0.6, fn = pll, method = "BFGS", OMat = OMat, 
                      beta_formula = beta_formula,
                       train_data = train_data, hessian = TRUE)
  hessian[[i]] <- optim_beta$hessian
  conv[i] <- optim_beta$convergence
  di <- exp(optim_beta$par)
  d[i] <- di
  AM <- zetaweights(OMat, di, maxlag = max(OMat), normalize = TRUE)
  Update_mBeta <- fit_mBeta(beta_formula,
                            tsiObj = train_data,
                            AM = AM)
  cat((proc.time() - ptm)[["elapsed"]], "s\n")
  if (i==1) model_n <- Update_mBeta$nobs else stopifnot(Update_mBeta$nobs == model_n)
  logLik <- c(logLik, Update_mBeta$loglik)
  npar <- c(npar, dim(Update_mBeta$vcov)[1] + 1)
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

save(comp, file = here::here("./Results/mBeta_fit_PW.RData"))

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



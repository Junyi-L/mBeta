library(tsibble)
library(betareg)
library(data.table)
library(Formula)

forecast_mBeta <- function(model, newdata, ...){
  if(!inherits(model, "betareg") ||
     !is_tsibble(model$data))
      stop("Need betareg model from fit_mBeta() with tsibble data attached)!")
  # Prepare for AR and NB in fit_mBeta.

  if(!is_tsibble(newdata)) stop("Newdata must be a tsibble object!")
  if(!is_ordered(newdata)) stop("The newdata is not ordered!")

  new_index <- index_var(newdata)
  if(is.null(new_index)) stop("The index of newdata must be provided!")

  data <- model$data
  AM <- model$AM
  formula <- Formula(model$formula)

  key_tsi <- key_vars(data)
  index_tsi <- index_var(data)

  if(index_tsi != new_index) stop("The indices of newdata and training data must be the same!")

  myterms <- terms(formula, specials = c("AR", "NB"))
  AR_idx <- attr(myterms, "specials")$AR
  if(is.null(AR_idx)) nAR <- 0 else{
    nAR <- attr(myterms, "variables")[[1 + AR_idx]][[2]]
  }
  NB_idx <- attr(myterms, "specials")$NB
  if(is.null(NB_idx)) nNB <- 0 else{
    nNB <- attr(myterms, "variables")[[1 + NB_idx]][[2]]
  }
  max_lag <- max(nAR, nNB)

  # Keep the last several observations in training data for AR and NB.
  ntotal <- nrow(data)
  nkey <- length(unique(data[[key_tsi]]))

  nobs <- ntotal/nkey
  unwhich <- function(x, n) {
    out <- rep_len(FALSE, n)
    out[x] <- TRUE
    out
  }
  lags_index <- rep(unwhich((nobs - max_lag + 1) : nobs, nobs),
                    times = nkey)

  data_lag <- data[lags_index, ]
  train_index <- max(data_lag[[index_tsi]])
  data_lag <- rbind.data.frame(data_lag, newdata)

  tsiObj <- as_tsibble(data_lag, key = key_tsi, index = index_tsi, regular = TRUE)
  test_index <- tsiObj[[index_tsi]] > train_index

  predict_result <-
    predict(model, tsiObj,...)
  #browser()
  res <- predict_result[test_index]
  names(res) <- NULL
  return(res)
}



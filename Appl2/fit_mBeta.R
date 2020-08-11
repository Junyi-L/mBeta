library(tsibble)
library(betareg)
library(data.table)
library(tidyselect)
library(Formula)

fit_mBeta <- function(formula, tsiObj, AM = NULL){
  if(!is_regular(tsiObj)) stop("The tsibble object has to be regular!")
  if(!is_ordered(tsiObj)) stop("The tsibble object is not ordered!")
  # Ordered tsible object is first ordered by key, then by index.
  
  # AR and NB are only allowed in the mean model and can appear only once.
  myterms <- terms(Formula(formula), specials = c("AR", "NB"), rhs = 1)
  AR_idx <- attr(myterms, "specials")$AR
  NB_idx <- attr(myterms, "specials")$NB
  if(length(AR_idx) > 1) stop("Only one autoregressive part is allowed!")
  if(length(NB_idx) > 1) stop("Only one neighborhood effect part is allowed!")
   
  myterms <- terms(Formula(formula), specials = c("AR", "NB"), rhs = 2)
  AR_idx <- attr(myterms, "specials")$AR
  NB_idx <- attr(myterms, "specials")$NB
  if(length(AR_idx) > 0) stop("The autoregressive part is not allowed in the precision model!")
  if(length(NB_idx) > 0) stop("The neighborhood effect part is not allowed in the precision model!")
  
  index_tsi <- index_var(tsiObj)
  key_tsi <- key_vars(tsiObj)
  if(length(key_tsi) > 1) stop("The key of tsibble must be unique!")
  if(is.null(key_tsi)) stop("The key of tsibble object must be provided!")
  
  # Check for each key the time series has the same length.
  if(length(unique(tsiObj[[key_tsi]])) > 1){
    if (var(table(tsiObj[[key_tsi]])) != 0) stop("Time series should have the same length!")
    start_index <- aggregate(tsiObj[[index_tsi]], by = list(tsiObj[[key_tsi]]), min)
    if (var(start_index[, 2]) != 0) stop("Time series should have the same starting date!")
  }
  # Harmonic regression
  Har <- function(nhar, frac){
    if(!is.numeric(nhar) | length(nhar) != 1) stop("nhar should be numeric and of length 1!")
    if(is.null(frac)) stop("Fraction should be provided!")
    data <- parent.frame(5)$tsiObj
    ntotal <- nrow(data)
    key_tsi <- key_vars(data)
    nkey <- length(unique(data[[key_tsi]]))
    nobs <- ntotal/nkey
    nHar_matrix <- matrix(nrow = ntotal, ncol = 2 * nhar)
    
    for(j in 1:nhar){
      nHar_matrix[, (2 * j - 1)] <- sin(2 * j  * pi * frac)
      nHar_matrix[, 2 * j] <- cos(2 * j  * pi * frac)
    }
    
    colnames(nHar_matrix) <- paste0(c("sin", "cos"), rep(1 : nhar, each = 2))
    return(nHar_matrix)
  }
  
  # Autoregressive terms
  AR <- function(lags){
    if(!is.numeric(lags) | length(lags) != 1) stop("lags should be numeric and of length 1!")
    data <- parent.frame(5)$tsiObj
    key_tsi <- key_vars(data)
    data <- as.data.table(data)
    response <- all.vars(parent.frame(5)$formula)[1]
    lags_name <- paste0("p", 1:lags)
    for (i in 1 : lags){
      data[, paste0("p", i) := qlogis(shift(get(response),n = i, type = "lag")), by = key_tsi]
    }
    lag_matrix <- as.matrix(data[, ..lags_name])
    return(lag_matrix)
  }
  # Neighborhood effect.
  NB <- function(lags){
    if(!is.numeric(lags) | length(lags) != 1) stop("lags should be numeric and of length 1!")
    data <- parent.frame(5)$tsiObj
    key_tsi <- key_vars(data)
    index_tsi <- index_var(data)
    if(length(unique(data[[key_tsi]])) == 1) stop("Neighborhood effect is not allowed for univariate time series!")

    AM <- parent.frame(5)$AM
    if(is.null(AM)) stop("The adjacent matrix must be provided for neighborhood effect!")
    response <- all.vars(parent.frame(5)$formula)[1]
    # check AM having the same name with key, reorder.
    keys <- key_data(data)
    keys_data <- as.character(unlist(keys[,1]))
    keys_AM <- colnames(AM)
    if(!all.equal(keys_data,keys_AM)) stop("The adjacent matrix must have same keys with data!")
    data <- as.data.table(data)
    NB_name <- paste0("NB", 1:lags)
    for (i in 1 : lags){
      pname <- paste0("p", i)
      # lagged observations
      data[, paste0("p", i)  := qlogis(shift(get(response),n = i, type = "lag")), by = key_tsi]
      # lagged neighbor observations, averaged by the number of neighbors.
      data[, paste0("NB", i) := drop(crossprod(get(pname), AM))/colSums(AM), by = index_tsi]
    }
    NB_matrix <- as.matrix(data[, ..NB_name])
    return(NB_matrix)
  }
  environment(formula) <- environment()
  beta_fit <- betareg(formula, data = tsiObj)
  beta_fit$data <- tsiObj
  beta_fit$AM <- AM
  return(beta_fit)
}




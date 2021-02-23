library(betareg)
library(data.table)
library(ggplot2)


# prepare data
load(file = here::here("./Data/data_holidays.RData"))

logit_FUN <- function(x){
  qlogis(x/100)
}

logistic_FUN <- function(x){
  plogis(x) * 100
}

missing <- is.na(data$weighted_ili)
firstnonmiss <- head(which(!missing), 1)
data <- data[firstnonmiss : dim(data)[1], ]
data <- data.table(data)


## AICc-based model selection of number of harmonics and autoregressive lags
auto.beta <- function() {

  Try_BetaReg5 <- function(S_mean, S_Precision, lags, data){
    mean_covar1 <- paste0(c(rep("sin_InPeriod",S_mean),rep("cos_InPeriod",S_mean)),1 : S_mean)
    mean_covar2 <- c("x", "y")
    mean_covar <- c(mean_covar1, mean_covar2)
    precision_covar <- c(paste0(c(rep("sin_InPeriod",S_Precision),
                                  rep("cos_InPeriod",S_Precision)), 1 : S_Precision),
                         "SIndex")

    mean_AR <- paste("weighted_ili_org ~", paste(paste0("p", 1 : lags), collapse = " + "))
    mean_model <- paste(mean_AR, paste(mean_covar,collapse = " + "), sep = " + ")
    precision_model <- paste(precision_covar, collapse = " + ")
    full_model <- paste(mean_model, "|", precision_model)

    res <- try(betareg(full_model,
                       data = data))
    return(res)
  }

  train_data <- data
  lags <- 5
  for (i in 1 : lags){
    train_data[, paste0("p", i) := logit_FUN(shift(weighted_ili,n = i, type = "lag"))]
    train_data[, paste0("o", i) := shift(weighted_ili,n = i, type = "lag")]
  }

  # Ignore obserations with missing lagged values.
  train_data[is.na(p1 + p2 + p3 + p4 + p5), ] <- NA

  res <- list()
  i = 1
  combi <- character()
  LL <- numeric()
  AIC <- numeric()
  npar <- numeric()
  AICc <- numeric()

  for(p in 1:5){
    for(h in 1:5){
      for(k in 1:5){
        combi[i] <- paste0("S_v = ", h, "S_{\\phi} = ", k, "lag = ", p)
        model_hk_try <- Try_BetaReg5(S_mean = h, S_Precision = k, lags = p, data = train_data)
        if(is(model_hk_try,"try-error")){
          npar[i] <- 0
          LL[i] <- 0
          AIC[i] <- 0
          AICc[i] <- 0
          res[[i]] <- list(S_mean = h, S_Precision = k, 0)
        }else{
          model_hk <- model_hk_try
          npar[i] <- dim(model_hk_try$vcov)[1]
          LL[i] <- model_hk$loglik
          AIC[i] <- -2 * model_hk$loglik + 2 * npar[i]
          n <- nobs(model_hk_try)
          AICc[i] <- -2 * model_hk$loglik + 2 * npar[i] + 2 * npar[i] * ( npar[i] + 1)/(n -  npar[i] - 1)
          res[[i]] <- list(S_mean = h, S_Precision = k, model_hk)
        }
        i <- i + 1
      }
    }
  }

  Vali_table <- data.frame(npar,LL,AIC,AICrank = rank(AIC), AICc, AICcrank = rank(AICc))
  rownames(Vali_table) <- combi
  return(Vali_table)

}

if (FALSE) { # takes a few seconds and prints errors from failing fits
  Vali_table <- auto.beta()
  Vali_table[Vali_table$AICrank < 6,]
}
#                             npar       LL       AIC AICrank      AICc AICcrank
# S_v = 3S_{\\phi} = 3lag = 4   21 4467.583 -8893.166       2 -8892.082        2
# S_v = 3S_{\\phi} = 4lag = 4   23 4469.854 -8893.708       1 -8892.411        1
# S_v = 4S_{\\phi} = 3lag = 4   23 4469.053 -8892.105       4 -8890.808        4
# S_v = 4S_{\\phi} = 4lag = 4   25 4471.509 -8893.019       3 -8891.488        3
# S_v = 3S_{\\phi} = 4lag = 5   24 4469.862 -8891.724       5 -8890.312        5
################################################################################

lags <- 4
S_mean <- 3
S_Precision <- 4
for (i in 1 : lags){
  data[, paste0("p", i) := logit_FUN(shift(weighted_ili,n = i, type = "lag"))]
  data[, paste0("o", i) := shift(weighted_ili,n = i, type = "lag")]
}
data1 <- data
data1[is.na(p1 + p2 + p3 + p4), ] <- NA
data1 <- na.omit(data)


# Beta(p) model with trend in precision model, model (a)
mean_covar1 <- paste0(c(rep("sin_InPeriod",S_mean),rep("cos_InPeriod",S_mean)),1 : S_mean)
mean_covar2 <- c("x", "y")
mean_covar <- c(mean_covar1, mean_covar2)

precision_covar <- c(paste0(c(rep("sin_InPeriod",S_Precision),
                              rep("cos_InPeriod",S_Precision)), 1 : S_Precision),
                     "SIndex")
mean_AR <- paste("weighted_ili_org ~", paste(paste0("p", 1 : lags), collapse = " + "))
mean_model <- paste(mean_AR, paste(mean_covar,collapse = " + "), sep = " + ")
precision_model <- paste(precision_covar, collapse = " + ")
full_model <- paste(mean_model, "|", precision_model)
Beta_lags_fit_a <- betareg(full_model,
                           data = data1)
# use data because it contains NA value
mean <- predict(Beta_lags_fit_a, data, type = "response")
precision <- predict(Beta_lags_fit_a, data, type = "precision")
Beta_LL <- dbeta(data$weighted_ili_org,
                 shape1 = mean * precision,
                 shape2 =  precision - mean * precision,
                 log = TRUE)
Beta_lags_fit_a$LL <- Beta_LL
#-------------- --------------------------

Beta_resid <- residuals(Beta_lags_fit_a, type = "pearson")
Beta_resid_table <- data.frame(time_index = data1$time_index, resid = Beta_resid)
# prepare for acf plot
data_a <- merge(data,Beta_resid_table, by = "time_index", all.x = TRUE)


# seasonality plot
season_week <- 1:52
season_table <- data.table(season_week,
                           sin_InPeriod1 = sin(2 * pi * season_week/52),
                           sin_InPeriod2 = sin(4 * pi * season_week/52),
                           sin_InPeriod3 = sin(6 * pi * season_week/52),
                           sin_InPeriod4 = sin(8 * pi * season_week/52),
                           cos_InPeriod1 = cos(2 * pi * season_week/52),
                           cos_InPeriod2 = cos(4 * pi * season_week/52),
                           cos_InPeriod3 = cos(6 * pi * season_week/52),
                           cos_InPeriod4 = cos(8 * pi * season_week/52),
                           x =  (season_week == 22),
                           y =  (season_week == 23))

x <- Beta_lags_fit_a$coefficients$mean
season_table$nu <- (t(x[c("sin_InPeriod1", "sin_InPeriod2","sin_InPeriod3",
                          "cos_InPeriod1", "cos_InPeriod2","cos_InPeriod3",
                          "xTRUE", "yTRUE")] %*%
                        t(season_table[,.(sin_InPeriod1, sin_InPeriod2,sin_InPeriod3,
                                          cos_InPeriod1, cos_InPeriod2,cos_InPeriod3, x, y)]))
                    + x["(Intercept)"])

y <- Beta_lags_fit_a$coefficients$precision

season_table$phi <- exp(t(y[c("sin_InPeriod1", "sin_InPeriod2",
                              "sin_InPeriod3","sin_InPeriod4",
                              "cos_InPeriod1", "cos_InPeriod2",
                              "cos_InPeriod3","cos_InPeriod4")] %*%
                            t(season_table[,.(sin_InPeriod1, sin_InPeriod2,
                                              sin_InPeriod3,sin_InPeriod4,
                                              cos_InPeriod1, cos_InPeriod2,
                                              cos_InPeriod4, cos_InPeriod3)]))
                        + y["(Intercept)"])


library(grid)
pdf(file = here::here("./Plots/season.pdf"), width = 8, height = 3)
p1 <- ggplot(data = season_table, aes(x = season_week, y= nu)) +
  geom_line() +
  xlab("Season week") +
  ylab(expression(hat(v)[t])) +
  theme_bw() +
  theme(legend.position = "none")

p2 <- ggplot(data = season_table, aes(x = season_week, y= phi)) +
  geom_line() +
  xlab("Season week") +
  ylab(expression(hat(phi1)[t])) +
  theme_bw() +
  theme(legend.position = "none")

grid.draw(cbind(ggplotGrob(p1), ggplotGrob(p2), size = "last"))
dev.off()



# a full Beta(p) model (b)

mean_covar1 <- paste0(c(rep("sin_InPeriod",S_mean),rep("cos_InPeriod",S_mean)),1 : S_mean)
mean_covar2 <- c("x", "y")
mean_covar <- c(mean_covar1, mean_covar2)
precision_covar <- paste0(c(rep("sin_InPeriod",S_Precision), rep("cos_InPeriod",S_Precision)), 1 : S_Precision)

mean_AR <- paste("weighted_ili_org ~", paste(paste0("p", 1 : lags), collapse = " + "))
mean_model <- paste(mean_AR, paste(mean_covar,collapse = " + "), sep = " + ")
precision_model <- paste(precision_covar, collapse = " + ")
full_model <- paste(mean_model, "|", precision_model)
#--------------------------------------
ptm <- proc.time()

Beta_lags_fit <- betareg(full_model,
                         data = data1)
run_time1 <- proc.time() - ptm
#-------------- --------------------------
mean <- predict(Beta_lags_fit, data, type = "response")
precision <- predict(Beta_lags_fit, data, type = "precision")
Beta_LL <- dbeta(data$weighted_ili_org,
                 shape1 = mean * precision,
                 shape2 =  precision - mean * precision,
                 log = TRUE)
Beta_lags_fit$LL <- Beta_LL

Beta_resid <- residuals(Beta_lags_fit, type = "pearson")
Beta_resid_table <- data.frame(time_index = data1$time_index, resid = Beta_resid)
# prepare for acf plot
data_b <- merge(data,Beta_resid_table, by = "time_index", all.x = TRUE)


#-----------------------------------------------------------------------------------------------
# constant precision (c)

mean_covar1 <- paste0(c(rep("sin_InPeriod",S_mean),rep("cos_InPeriod",S_mean)),1 : S_mean)
mean_covar2 <- c("x", "y")
mean_covar <- c(mean_covar1, mean_covar2)

mean_AR <- paste("weighted_ili_org ~", paste(paste0("p", 1 : lags), collapse = " + "))
mean_model <- paste(mean_AR, paste(mean_covar,collapse = " + "), sep = " + ")
full_model <- mean_model
#--------------------------------------
ptm <- proc.time()

Beta_con_p_fit <- betareg(full_model,
                          data = data1)
run_time1 <- proc.time() - ptm
#-------------- --------------------------
mean <- predict(Beta_con_p_fit, data, type = "response")
precision <- predict(Beta_con_p_fit, data, type = "precision")
Beta_LL <- dbeta(data$weighted_ili_org,
                 shape1 = mean * precision,
                 shape2 =  precision - mean * precision,
                 log = TRUE)
Beta_con_p_fit$LL <- Beta_LL

Beta_resid <- residuals(Beta_con_p_fit, type = "pearson")
Beta_resid_table <- data.frame(time_index = data1$time_index, resid = Beta_resid)
data_c <- merge(data,Beta_resid_table, by = "time_index", all.x = TRUE)


#-----------------------------------------------------------------
# no lags (d)
mean_covar1 <- paste0(c(rep("sin_InPeriod",S_mean),rep("cos_InPeriod",S_mean)),1 : S_mean)
mean_covar2 <- c("x", "y")
mean_covar <- c(mean_covar1, mean_covar2)
precision_covar <- c(paste0(c(rep("sin_InPeriod",S_Precision),
                              rep("cos_InPeriod",S_Precision)), 1 : S_Precision),
                     "SIndex")

mean_model <- paste(mean_covar,collapse = " + ")
precision_model <- paste(precision_covar, collapse = " + ")
full_model <- paste("weighted_ili_org ~", paste(mean_model, "|", precision_model))

Beta_lags_fit_no_lag <- betareg(full_model,
                                data = data1)

mean <- predict(Beta_lags_fit_no_lag, data, type = "response")
precision <- predict(Beta_lags_fit_no_lag, data, type = "precision")
Beta_LL <- dbeta(data$weighted_ili_org,
                 shape1 = mean * precision,
                 shape2 =  precision - mean * precision,
                 log = TRUE)
Beta_lags_fit_no_lag$LL <- Beta_LL

Beta_resid <- residuals(Beta_lags_fit_no_lag, type = "pearson")
Beta_resid_table <- data.frame(time_index = data1$time_index, resid = Beta_resid)
data_d <- merge(data,Beta_resid_table, by = "time_index", all.x = TRUE)


# no logit transform (e)
mean_covar1 <- paste0(c(rep("sin_InPeriod",S_mean),rep("cos_InPeriod",S_mean)),1 : S_mean)
mean_covar2 <- c("x", "y")
mean_covar <- c(mean_covar1, mean_covar2)
precision_covar <- c(paste0(c(rep("sin_InPeriod",S_Precision),
                              rep("cos_InPeriod",S_Precision)), 1 : S_Precision),
                     "SIndex")

mean_AR <- paste("weighted_ili_org ~", paste(paste0("o", 1 : lags), collapse = " + "))
mean_model <- paste(mean_AR, paste(mean_covar,collapse = " + "), sep = " + ")
precision_model <- paste(precision_covar, collapse = " + ")
full_model <- paste(mean_model, "|", precision_model)

Beta_lags_nologit_fit <- betareg(full_model,
                                 data = data1)

mean <- predict(Beta_lags_nologit_fit, data, type = "response")
precision <- predict(Beta_lags_nologit_fit, data, type = "precision")
Beta_LL <- dbeta(data$weighted_ili_org,
                 shape1 = mean * precision,
                 shape2 =  precision - mean * precision,
                 log = TRUE)
Beta_lags_nologit_fit$LL <- Beta_LL

Beta_resid_nologit <-residuals(Beta_lags_nologit_fit, type = "pearson")
Beta_resid_table <- data.frame(time_index = data1$time_index, resid = Beta_resid_nologit)
data_e <- merge(data,Beta_resid_table, by = "time_index", all.x = TRUE)


#-----------------------------------------------------------------

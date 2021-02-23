#-------------------------------------------------
# Summary of models
source(file = here::here("./Appl1/Beta.R"))

load(file = here::here("./Results/sarima_fit.RData"))

model_name <- c(rep("Beta", 5), "SARIMA")
model_type <-
  c("(a) full model",
    "(b) no trend in $\\phi_t$",
    "(c) $\\phi_t = \\phi$",
    "(d) $\\beta_k = 0$",
    "(e) $g(X_{t-k}) = X_{t-k}$",
    sub("^ARIMA([^ ]+).*$", "\\1", forecast:::arima.string(sarima_fit)))
# a: whole model
# b: constant precision
# c: no lag
# d: no logit
SARIMA_idx <- !is.na(sarima_fit$LL)
Beta_idx <- !is.na(Beta_lags_fit_a$LL)
NNA_idx <- as.logical(SARIMA_idx * Beta_idx)

# > sum(NNA_idx)
# [1] 881
# > sum(!is.na(data$weighted_ili))
# [1] 905

model_logLik <- c(sum(Beta_lags_fit_a$LL[NNA_idx], na.rm = TRUE),
                  sum(Beta_lags_fit$LL[NNA_idx], na.rm = TRUE),
                  sum(Beta_con_p_fit$LL[NNA_idx], na.rm = TRUE),
                  sum(Beta_lags_fit_no_lag$LL[NNA_idx], na.rm = TRUE),
                  sum(Beta_lags_nologit_fit$LL[NNA_idx], na.rm = TRUE),
                  sum(sarima_fit$LL[NNA_idx], na.rm = TRUE))
model_n <- nobs(Beta_lags_fit_a)
model_npar <- c(dim(Beta_lags_fit_a$vcov)[1],
                dim(Beta_lags_fit$vcov)[1],
                dim(Beta_con_p_fit$vcov)[1],
                dim(Beta_lags_fit_no_lag$vcov)[1],
                dim(Beta_lags_nologit_fit$vcov)[1],
                (sarima_fit$aic + 2 * sarima_fit$loglik)/2)

AIC <- - 2 * model_logLik + 2 * model_npar
AICc <- AIC + (2 * (model_npar)^2 + 2 * model_npar)/(model_n - model_npar - 1)
BIC <- - 2 * model_logLik + log(model_n) * model_npar


AIC <- paste0(formatC(round(AIC, digits = 0), format='f', digits=0 ), " (",rank(AIC), ")")
AICc <- paste0(formatC(round(AICc, digits = 0), format='f', digits=0 ), " (",rank(AICc), ")")
BIC <- paste0(formatC(round(BIC, digits = 0), format='f', digits=0 ), " (",rank(BIC), ")")
model_logLik <-  paste0(formatC(round(model_logLik, digits = 0), format='f', digits=0 ), " (",rank(- model_logLik), ")")
#model_npar <- paste0(formatC(round(model_npar, digits = 0), format='f', digits=0 ), "(",rank(model_npar), ")")

summary_table <- data.frame(Model = model_name,
                            model_type,
                            LogLik = model_logLik,
                            npar = model_npar,
                            AIC = gsub("-","--",AIC),
                            AICc = gsub("-","--",AICc),
                            BIC = gsub("-","--",BIC))
colnames(summary_table) <- c("Model", "Variant", "LL", "npar", "AIC","AICc","BIC")

library(xtable)
print(xtable(summary_table, align = "ccl|rrrrr",
             caption = 'Summaries of model fit.
             Ranks are shown in parantheses.
             The log-likelihood (LL) is ranked descending,
             and AIC, AICc, BIC are ranked ascending.
             The \"npar\" column gives the number of estimated parameters.',
             label = "tab:wILIsum",
             digits = 0),
      type = "latex",
      include.colnames = TRUE,
      include.rownames = FALSE,
      hline.after = c(-1, 0, 5, 6),
      sanitize.text.function=function(x){x},
      file=here::here("Results", "flusummary.tex"),
      comment = FALSE
      )

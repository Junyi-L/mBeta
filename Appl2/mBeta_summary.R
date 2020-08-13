library(data.table)
result1 <- readRDS(file = here::here("./Results/Forecast_mBeta1.RData"))
result2 <- readRDS(file = here::here("./Results/Forecast_mBeta2.RData"))
result3 <- readRDS(file = here::here("./Results/Forecast_mBeta3.RData"))
result4 <- readRDS(file = here::here("./Results/Forecast_mBeta4.RData"))
result5 <- readRDS(file = here::here("./Results/Forecast_mBeta5.RData"))

result_list_old <- list(result1,
                        result2,
                        result3,
                        result4,
                        result5)
model_list <- c( "M1",
                 "M2",
                 "M3",
                 "M4",
                 "M5")
result_list <- lapply(result_list_old, function(x){return(x$result)})

npar_vector <- unlist(lapply(result_list_old, function(x){return(x$npar)}))

nrows <- nrow(result_list[[1]])
result_table <- data.table(time = rep(result_list[[1]]$data_set.time,
                                      times = length(result_list)),
                           region = rep(result_list[[1]]$data_set.region,
                                        times = length(result_list)),
                           season = rep(result_list[[1]]$data_set.season,
                                        times = length(result_list)),
                           Model = rep(model_list,
                                       each = nrow(result_list[[1]])),
                           Inseason = rep(result_list[[1]]$data_set.InSeason,
                                          times = length(result_list)),
                           LS = NA,
                           AE = NA,
                           DSS = NA)
LS <- numeric()
AE <- numeric()
DSS <- numeric()
npar <- numeric()
for(i in 1:length(result_list)){
  result <- result_list[[i]]
  LS <- c(LS, -result$log_score)
  DSS <- c(DSS, result$DS_score)
  AE <- c(AE, result$AE)
  #npar <- c(npar, result$npar)

}
result_table$LS <- LS
result_table$AE <- AE
result_table$DSS <- DSS

result_table_avgR <- setDT(result_table)[ , list(LS = sum(LS),
                                                 DSS = sum(DSS),
                                                 Inseason = head(Inseason, n = 1)) ,
                                          by = .(time, Model, season)]
sum_table <- setDT(result_table_avgR)[ , list(LS = mean(LS),
                                              maxLS = max(LS),
                                              DSS = mean(DSS),
                                              maxDSS = max(DSS)),
                                       by = .(season, Model)]
setkey(sum_table, season)

sum_table_sub <- setDT(result_table_avgR)[Inseason == TRUE ,
                                          list(LS = mean(LS),
                                               maxLS = max(LS),
                                               DSS = mean(DSS),
                                               maxDSS = max(DSS)),
                                          by = .(season, Model)]

setkey(sum_table_sub, season)

library(ggplot2)
pdf(file = here::here("./Plots/mBeta_LS.pdf"), height = 6, width = 8)
ggplot(data = sum_table_sub, aes(x = season, y = LS, color = Model)) +
  geom_point(aes(shape = Model), size = 3)
dev.off()

sum_table2 <- setDT(sum_table)[, list(LS = mean(LS),
                                      maxLS = max(LS),
                                      DSS = mean(DSS),
                                      maxDSS = max(DSS)
),
by = Model]

sum_table2_sub <- setDT(sum_table_sub)[, list(LS = mean(LS),
                                              maxLS = max(LS),
                                              DSS = mean(DSS),
                                              maxDSS = max(DSS)
),
by = Model]


sum_table2[, LS :=
             paste0(formatC(round(LS, digits = 2), format='f', digits=2 ), "(",rank(LS), ")")]
sum_table2_sub[, LS :=
                 paste0(formatC(round(LS, digits = 2), format='f', digits=2 ), "(",rank(LS), ")")]

sum_table2[, maxLS :=
             paste0(formatC(round(maxLS, digits = 2), format='f', digits=2 ), "(",rank(maxLS), ")")]
sum_table2_sub[, maxLS :=
                 paste0(formatC(round(maxLS, digits = 2), format='f', digits=2 ), "(",rank(maxLS), ")")]

sum_table2[, DSS :=
             paste0(formatC(round(DSS, digits = 2), format='f', digits=2 ), "(",rank(DSS), ")")]
sum_table2_sub[, DSS :=
                 paste0(formatC(round(DSS, digits = 2), format='f', digits=2 ), "(",rank(DSS), ")")]

sum_table2[, maxDSS :=
             paste0(formatC(round(maxDSS, digits = 2), format='f', digits=2 ), "(",rank(maxDSS), ")")]
sum_table2_sub[, maxDSS :=
                 paste0(formatC(round(maxDSS, digits = 2), format='f', digits=2 ), "(",rank(maxDSS), ")")]


RNGversion("3.6.2")
set.seed(20200812)

library(surveillance)
Per_43 <- permutationTest(result_table$LS[result_table$Model == "M4"],
                          result_table$LS[result_table$Model == "M3"],
                          nPermutation = 9999)
Per_41 <- permutationTest(result_table$LS[result_table$Model == "M4"],
                          result_table$LS[result_table$Model == "M1"],
                          nPermutation = 9999)
Per_42 <- permutationTest(result_table$LS[result_table$Model == "M4"],
                          result_table$LS[result_table$Model == "M2"],
                          nPermutation = 9999)
Per_45 <- permutationTest(result_table$LS[result_table$Model == "M2"],
                          result_table$LS[result_table$Model == "M5"],
                          nPermutation = 9999)

Per_43_sub <- permutationTest(result_table$LS[result_table$Model == "M4" & result_table$Inseason == TRUE],
                              result_table$LS[result_table$Model == "M3" & result_table$Inseason == TRUE],
                              nPermutation = 9999)
Per_41_sub <- permutationTest(result_table$LS[result_table$Model == "M4" & result_table$Inseason == TRUE],
                              result_table$LS[result_table$Model == "M1" & result_table$Inseason == TRUE],
                              nPermutation = 9999)
Per_45_sub <- permutationTest(result_table$LS[result_table$Model == "M4" & result_table$Inseason == TRUE],
                              result_table$LS[result_table$Model == "M5" & result_table$Inseason == TRUE],
                              nPermutation = 9999)
Per_42_sub <- permutationTest(result_table$LS[result_table$Model == "M2" & result_table$Inseason == TRUE],
                              result_table$LS[result_table$Model == "M4" & result_table$Inseason == TRUE],
                              nPermutation = 9999)


Per_list <- as.numeric(c(Per_41$pVal.permut,
                         Per_42$pVal.permut,
                         Per_43$pVal.permut,
                         NA,
                         Per_45$pVal.permut
))

Per_list2 <- as.numeric(c(Per_41_sub$pVal.permut,
                          Per_42_sub$pVal.permut,
                          Per_43_sub$pVal.permut,
                          NA,
                          Per_45_sub$pVal.permut
))
sum_table2$pvalue <- Per_list
sum_table2_sub$pvalue <- Per_list2

Model_name <- c(
  "M1: $\\beta_{r,k} = \\beta_{k}$, $\\beta_{r}^{(\\nu)} = \\beta^{(\\nu)}$, $\\beta^{(\\phi)} = \\beta^{(\\phi)}$, $\\gamma_r = \\gamma$",
  "M2: $\\beta_{r}^{(\\nu)} = \\beta^{(\\nu)}$, $\\beta^{(\\phi)} = \\beta^{(\\phi)}$, $\\gamma_r = \\gamma$",
  "M3: $\\gamma_r = 0$",
  "M4: $\\gamma_r = \\gamma$",
  "M5: full model"
)
sum_table2$Model <- Model_name
sum_table2_sub$Model <- Model_name



sum_table2 <- rbind(sum_table2,sum_table2_sub)
Subset <- c("All weeks", rep(NA, 4), "High Incidence",rep(NA, 4))

sum_table2$Subset <- Subset

sum_table2 <- sum_table2[, c("Model", "Subset", "LS", "pvalue", "maxLS","DSS")]
names(sum_table2)[4] <- "p-value"
sum_table2$LS <- gsub("-","--",sum_table2$LS)
sum_table2$maxLS <- gsub("-","--",sum_table2$maxLS)
sum_table2$DSS <- gsub("-","--",sum_table2$DSS)
print(xtable(sum_table2, align = "lll|rrrr",
             caption = 'Model performance in terms of mean log score (LS),
             mean Dawid-Sebastiani score (DSS), and maximum log score (maxLS)
             for one-week-ahead forecasts.
             Ranks are shown in bracket.
             The "all weeks" group shows average scores over the
             whole test period (208 weeks),
             whereas the "High Incidence" group shows
             averages over the high incidence periods only (132 weeks).
             Models are ordered by model complexity.
             The Monte Carlo p-values for differences in mean log scores
             are based on 9999 random permutations,
             comparing each model against the best model in each subset.',
             label = "tab:forecast",
             digits = 2),
      type = "latex",
      include.colnames = TRUE,
      include.rownames = FALSE,
      hline.after = c(-1, 0, 5, 10),
      sanitize.text.function=function(x){x},
      file=here::here("./Results/mBeta_sum.tex"),
      size="\\fontsize{9pt}{10pt}\\selectfont",
      comment = FALSE)

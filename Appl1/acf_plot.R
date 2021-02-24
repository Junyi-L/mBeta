# acf plot

source(file = here::here("./Appl1/Beta.R"))
load(file = here::here("./Results/sarima_fit.RData"))

pcex <- 0.8
pline <- -1.5
ylim1 <- c(-0.15,1)
pdf(file = here::here("./Plots/ACF.pdf"), height = 4, width = 6)

par(mfcol = c(3, 2))
par(oma = c(5, 3, 1, 3))

par(mar = c(0,2,0,0))
acf(data_a$resid, lag.max = 52, na.action = na.pass,
    ann = FALSE, xaxt = "n", ylim = ylim1)
mtext("Beta model (a)", side = 3, line = pline, cex = pcex)

acf(data_c$resid, lag.max = 52,  na.action = na.pass,
    ann = FALSE, xaxt = "n", ylim = ylim1)
mtext("Beta model (c)", side = 3, line= pline, cex = pcex)

acf(data_e$resid, lag.max = 52, main = "Beta e", na.action = na.pass,
    ann = FALSE, xaxt = "n", ylim = ylim1)
mtext("Beta model (e)", side = 3, line= pline, cex = pcex)

axis(1)


par(mar = c(0,0,0,2))

acf(data_b$resid, lag.max = 52, na.action = na.pass,
    ann = FALSE, xaxt = "n", yaxt = "n", ylim = ylim1)
mtext("Beta model (b)", side = 3, line = pline, cex = pcex)
axis(4)

acf(data_d$resid, lag.max = 52,  na.action = na.pass,
    ann = FALSE, xaxt = "n",yaxt = "n", ylim = ylim1)
mtext("Beta model (d)", side = 3, line = pline, cex = pcex)
axis(4)

acf(sarima_fit$ARIMA_resid, lag.max = 52, main = "Gaussian c", na.action = na.pass,
    ann = FALSE, xaxt = "n",yaxt = "n", ylim = ylim1)
mtext("SARIMA", side = 3, line= pline, cex = pcex)
axis(4)

axis(1)
title(xlab = "Lag", outer = TRUE)
title(ylab = "ACF", outer = TRUE, line = 1)

dev.off()

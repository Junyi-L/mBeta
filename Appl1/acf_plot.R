# acf plot

source(file = here::here("./Appl1/Beta.R"))
load(file = here::here("./Results/sarima_fit.RData"))

pcex <- 0.8
pline <- -1.5
ylim1 <- c(-0.15,1)
pdf(file = here::here("./Plots/ACF.pdf"), height = 4, width = 6)
layout(mat = matrix(c(1,2,3,4,5,6), 
                   nrow = 3, 
                   ncol = 2),
      heights = c(1, 1, 1.15),    # Heights of the two rows
      widths = c(1, 1))     # Widths of the two columns

par(mar = c(0,5,0,0))
acf(data_a$resid, lag.max = 52, na.action = na.pass,
    xaxt = "n", ylim = ylim1)
mtext("Beta a", side = 3, line = pline, cex = pcex)

acf(data_c$resid, lag.max = 52,  na.action = na.pass,
    xaxt = "n", ylim = ylim1)
mtext("Beta c", side = 3, line= pline, cex = pcex)

par(mar = c(2,5,0,0))

acf(data_e$resid, lag.max = 52, main = "Beta e", na.action = na.pass,
    xaxt = "n", ylim = ylim1)
mtext("Beta e", side = 3, line= pline, cex = pcex)
axis(1)


par(mar = c(0,0,0,5))

acf(data_b$resid, lag.max = 52, na.action = na.pass,
    xaxt = "n", yaxt = "n", ylim = ylim1)
mtext("Beta b", side = 3, line = pline, cex = pcex)
axis(4)

acf(data_d$resid, lag.max = 52,  na.action = na.pass,
    xaxt = "n",yaxt = "n", ylim = ylim1)
mtext("Beta d", side = 3, line = pline, cex = pcex)
axis(4)


par(mar = c(2,0,0,5))
acf(sarima_fit$ARIMA_resid, lag.max = 52, main = "Gaussian c", na.action = na.pass,
    xaxt = "n",yaxt = "n",
     ylim = ylim1)
mtext("SARIMA", side = 3, line= pline, cex = pcex)
axis(4)
axis(1)
dev.off()
library(tsibble)
library(betareg)
library(data.table)
library(tidyselect)
library(Formula)
load(file = here::here("./Results/mBeta_fit_PW.RData"))

mBeta <- fit[[5]]
#covariance matrix of estimated parameters.
vcov <- vcov(mBeta)
#x <- vcov[grepl("NB",rownames(vcov)),grepl("NB",rownames(vcov))]
coef_sigma <- diag(vcov)
mBeta_coef <- coef(mBeta)
all.equal(names(coef_sigma), names(mBeta_coef))
# The vcov and coef are in the same order.

# Parameters of harmonics are not compared.
mBeta_coef_r <- mBeta_coef[!grepl("Har", names(mBeta_coef)) ]
coef_sigma_r <- coef_sigma[!grepl("Har", names(coef_sigma)) ]

coef_table <- data.table(coefname = names(mBeta_coef_r), 
                         value = mBeta_coef_r,
                         sigma = coef_sigma_r, 
                         row.names = NULL) 
# the parameters belong to ether mean/precision model.
coef_table[, model:= ifelse(grepl("(phi)", coefname), "precision", "mean")]
# Find region information
region_func <- function(name){
  for(i in 10:1){
    if(grepl(paste("Region", i), name))   
      break
  }
  return(i)
}
coef_table[, region := ifelse(!grepl("region", coefname), 
                              1, 
                              sapply(coefname, region_func))]
coef_table$region <- as.factor(coef_table$region)
# find specific parameters.
term_func <- function(name, nAR){
  name_list <- c("xTRUE", "yTRUE", paste0("p", 1 : nAR), "NB", "SIndex")
  for(i in 1 : length(name_list)){
    if(grepl(name_list[i], name)) return(name_list[i])
  }
  return("Intercept")
}
coef_table[, term := sapply(coefname, term_func, nAR = 4)]
# order data by region, so that the first element 
# of a subset is always of region 1. 
setkey(coef_table, region)

# For region > 1, need to adjust the estimated value and 
# variance using the estimation of region 1. 
# All parameters follow asymptotic normal distribution.
# Find the corresponding coefficient name of region 1.
coef_table[, RefName := coefname[1], by = .(term, model)]
# Do nothing for estimation of region 1.
coef_table[, value1 := ifelse(region == 1, 0, value[1]),
           by = .(term, model)]
# for intercept
# E(Par_Region(i)) = E(Par_Region(1)) + E(Par0_Region(i))
coef_table[, newvalue := ifelse(term == "Intercept", 
                                value + value1,
                                value)
           ]
# Find the cov between Par0_Region(i) and Par_region(1)
coef_table[, cov := ifelse(region == 1, 0,
                           vcov[RefName, coefname])]
# Do nothing for estimation of region 1.
coef_table[, sigma1 := ifelse(region == 1, 0, sigma[1]),
           by = .(term, model)]
# for intercept
# var(Par_Region(i)) = var(Par_Region(1)) + var(Par0_Region(i)) + 
# cov(Par_Region(1),Par0_Region(i))
coef_table[, newsigma := ifelse(term == "Intercept", 
                                sigma + sigma1 + 2 * cov,
                                sigma)
             ]
# Calculate 95% CI
coef_table[, upperCI := newvalue + 1.96 * sqrt(newsigma)]
coef_table[, lowerCI := newvalue - 1.96 * sqrt(newsigma)]

coef_table1 <- coef_table
coef_table1$term[coef_table1$term == "xTRUE"] <- "Xmas"
coef_table1$term[coef_table1$term == "yTRUE"] <- "NY"
coef_table1$term[coef_table1$term == "Intercept" & 
                   coef_table1$model == "mean"] <- "Intercept (mean)"
coef_table1$term[coef_table1$term == "Intercept" & 
                   coef_table1$model == "precision"] <- "Intercept (precision)"
coef_table1$term <- factor(coef_table1$term,
                              levels = c("Intercept (mean)",
                                         "NB",
                                         "p1",
                                         "p2",
                                         "p3",
                                         "p4",
                                         "Xmas",
                                         "NY",
                                         "Intercept (precision)",
                                         "SIndex"),
                              labels = c("alpha^(nu)",
                                         "gamma[r]",
                                         "beta['r,1']",
                                         "beta['r,2']",
                                         "beta['r,3']",
                                         "beta['r,4']",
                                         "beta['r,Xmas']^(nu)",
                                         "beta['r,NY']^(nu)",
                                         "alpha^(phi)",
                                         "beta['r,T']^(phi)"))


library(ggplot2)
library(gridExtra)
pdf(file = here::here("./Plots/par_CI.pdf"), 
    height = 6, width = 8)
p1 <- ggplot(data = coef_table1, aes(x = region, y = newvalue)) +
  geom_pointrange(aes(
    ymin  = lowerCI,
    ymax  = upperCI
  ),
  fatten = 1) +
  ylab("Estimated value") +
  xlab("Region") +
  theme_bw()

p2 <- p1 +  facet_wrap(~ term + model,scales = "free", ncol = 5,
                       labeller=label_parsed)

p2
dev.off()




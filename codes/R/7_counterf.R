#######################################################################################################
## ---- COUNTERF
library(Counterfactual)

# numeric cohort
scores2plot$cohn <- as.numeric(scores2plot$cohort)-1

# decide how many quantiles
quants <- seq(.1,.9,.025)

# Logit regression
logitcf.ext.m <- counterfactual(form.ext, data = subset(scores2plot, sex=='M'),
                                group = cohn, treatment=TRUE, decomposition=TRUE, 
                                method = "logit", quantiles = quants,
                                nreg=100, weightedboot = TRUE)
logitcf.ext.f <- counterfactual(form.ext, data = subset(scores2plot, sex=='F'),
                                group = cohn, treatment=TRUE, decomposition=TRUE, 
                                method = "logit", quantiles = quants,
                                nreg=100, weightedboot = TRUE)
logitcf.int.m <- counterfactual(form.int, data = subset(scores2plot, sex=='M'),
                                group = cohn, treatment=TRUE, decomposition=TRUE, 
                                method = "logit", quantiles = quants,
                                nreg=100, weightedboot = TRUE)
logitcf.int.f <- counterfactual(form.int, data = subset(scores2plot, sex=='F'),
                                group = cohn, treatment=TRUE, decomposition=TRUE, 
                                method = "logit", quantiles = quants,
                                nreg=100, weightedboot = TRUE)

## ---- COUNTERF2
# location shift (JMP)
# locres.m <- counterfactual(EXT ~ as.factor(incq) + ysch_moth + ysch_fath, data = subset(scores2plot, sex=='M'),
#                            group = cohn, treatment=TRUE, decomposition=TRUE, 
#                            method = "loc", noboot=T)
#nreg=100, weightedboot = TRUE)
#######################################################################################################
## ---- DEC_VARS
# select variables for decomposition
decvars <- c("region","faminc10_infl","scl10b","mysch5","mothageb","numch5")

# formulas
form.ext <- as.formula(paste("EXT ~ ", paste0(decvars, collapse = '+')))
form.int <- as.formula(paste("INT ~ ", paste0(decvars, collapse = '+')))


## ---- COUNTERF
library(Counterfactual)

# numeric cohort
scores2plot$cohn <- as.numeric(scores2plot$cohort)-1

# decide how many quantiles
quants <- seq(.1,.9,.025)

# remove region from formulas
form.ext.cf <- as.formula(paste("EXT ~ ", paste0(decvars[decvars!="region"], collapse = '+')))
form.int.cf <- as.formula(paste("INT ~ ", paste0(decvars[decvars!="region"], collapse = '+')))

# Logit regression
logitcf.ext.m <- counterfactual(form.ext.cf, data = subset(scores2plot, sex=='M'),
                                group = cohn, treatment=TRUE, decomposition=TRUE, 
                                method = "logit", quantiles = quants,
                                nreg=100, weightedboot = TRUE)
logitcf.ext.f <- counterfactual(form.ext.cf, data = subset(scores2plot, sex=='F'),
                                group = cohn, treatment=TRUE, decomposition=TRUE, 
                                method = "logit", quantiles = quants,
                                nreg=100, weightedboot = TRUE)
logitcf.int.m <- counterfactual(form.int.cf, data = subset(scores2plot, sex=='M'),
                                group = cohn, treatment=TRUE, decomposition=TRUE, 
                                method = "logit", quantiles = quants,
                                nreg=100, weightedboot = TRUE)
logitcf.int.f <- counterfactual(form.int.cf, data = subset(scores2plot, sex=='F'),
                                group = cohn, treatment=TRUE, decomposition=TRUE, 
                                method = "logit", quantiles = quants,
                                nreg=100, weightedboot = TRUE)

## ---- COUNTERF2
# location shift (JMP)
# locres.m <- counterfactual(EXT ~ as.factor(incq) + ysch_moth + ysch_fath, data = subset(scores2plot, sex=='M'),
#                            group = cohn, treatment=TRUE, decomposition=TRUE, 
#                            method = "loc", noboot=T)
#nreg=100, weightedboot = TRUE)
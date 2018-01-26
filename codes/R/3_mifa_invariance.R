

############################################################################################
## ---- FA_INVARIANCE

# WE CONFIGURAL - THETA PARAMETERISATION (Condition 8)
# free loadings and thresholds
# lv means = 0
# lv variances = 1
# intercepts = 0
# unique variances = 1
config.we.th <- collapse(readLines(paste0(dir_syntax, "config_we_th.lav")), sep=" \n  ")
config.we.thb <- collapse(readLines(paste0(dir_syntax, "config_we_thb.lav")), sep=" \n  ")

# WE CONFIGURAL - DELTA PARAMETERISATION (Condition 7)
# free loadings and thresholds
# lv means = 0
# lv variances = 1
# intercepts = 0
# scales = 1
config.we.d <- collapse(readLines(paste0(dir_syntax, "config_we_d.lav")), sep=" \n  ")


# WE THRESHOLD invariance, theta par (Condition 15)
# --> equivalent to configural for K=2
# thresholds constrained across groups
# lv means = 0
# lv variances = 1
# unique variances free in group 2 (except binary items)
# intercepts free in group 2
thr.we.th <- collapse(readLines(paste0(dir_syntax, "thr_we_th.lav")), sep=" \n  ")
thr.we.thb <- collapse(readLines(paste0(dir_syntax, "thr_we_thb.lav")), sep=" \n  ")


# WE THRESHOLD+LOADINGS invariance, theta par (Condition 19)
# thresholds constrained across groups (specified in cfa call)
# loadings constrained across groups
# lv means = 0
# lv variances free in group 2
# unique variances free in group 2 (except binary items)
# intercepts free in group 2
thr.load.we.th <- collapse(readLines(paste0(dir_syntax, "thr_load_we_th.lav")), sep=" \n  ")
thr.load.we.thb <- collapse(readLines(paste0(dir_syntax, "thr_load_we_thb.lav")), sep=" \n  ")


# WE THRESHOLD+INTERCEPT invariance, delta par (Condition 21)
# thresholds constrained across groups (specified in cfa call)
# intercepts constrained to zero
# lv means free
# lv variances constrained to 1
# unique variances free in group 2 (except binary items)
# intercepts free in group 2
thr.int.we.d <- collapse(readLines(paste0(dir_syntax, "thr_int_we_d.lav")), sep=" \n  ")


# WE THRESHOLD+LOADINGS+INTERCEPT invariance, theta par (Condition 27)
# thresholds constrained across groups (specified in cfa call)
# loadings constrained across groups
# lv means free in group 2
# lv variances free in group 2
# unique variances free in group 2 (except binary items)
# intercepts constrained to zero for both groups
thr.load.int.we.th <- collapse(readLines(paste0(dir_syntax, "thr_load_int_we_th.lav")), sep=" \n  ")
thr.load.int.we.thb <- collapse(readLines(paste0(dir_syntax, "thr_load_int_we_thb.lav")), sep=" \n  ")

# MODELS -------------------------------------------------------
# equivalent models (MT does not converge!)
# fa.config.mt.th <- cfa(config.mt.th, data = items.c, group = "cohort", estimator="wlsmv", parameterization="theta")
# printfit(fa.config.mt.th)
# fa.config.mt.d<- cfa(config.mt.d, data = items.c, group = "cohort", estimator="wlsmv", parameterization="delta")
# printfit(fa.config.mt.d)
# fa.config.we.d <- cfa(config.we.d, data = items.cb, group = "cohort", estimator="wlsmv", parameterization="delta")
# printfit(fa.config.we.d)
# # THRESHOLDS + INTERCEPTS (does not converge)
# fa.thr.int.we.d <- cfa(thr.int.we.d, data = items.c, group = "cohort", estimator="wlsmv", parameterization="delta")
# printfit(fa.thr.int.we.d)

# WHOLE SAMPLE
fa.c.a <- cfa(config.we.th, data = items.cb, group = "cohort", estimator="wlsmv", parameterization="theta")
printfit(fa.c.a)
fa.t.a <- cfa(thr.we.th, data = items.cb, group = "cohort", estimator="wlsmv", parameterization="theta")
printfit(fa.t.a)
# first restrictive model: THRESHOLDS + LOADINGS
fa.tl.a <- cfa(thr.load.we.th, data = items.cb, group = "cohort", estimator="wlsmv", parameterization="theta", group.equal='loadings')
printfit(fa.tl.a)
# second restrictive model: THRESHOLDS + LOADINGS + INTERCEPTS
fa.tli.a <- cfa(thr.load.int.we.th, data = items.cb, group = "cohort", estimator="wlsmv", parameterization="theta", group.equal='loadings')
printfit(fa.tli.a)


# MALES
fa.c.m   <- cfa(config.we.th,        data = subset(items.cb, sex=="M"), group = "cohort", estimator="wlsmv", parameterization="theta")
fa.t.m   <- cfa(thr.we.th,           data = subset(items.cb, sex=="M"), group = "cohort", estimator="wlsmv", parameterization="theta")
fa.tl.m  <- cfa(thr.load.we.th,      data = subset(items.cb, sex=="M"), group = "cohort", estimator="wlsmv", parameterization="theta", group.equal='loadings')
fa.tli.m <- cfa(thr.load.int.we.th,  data = subset(items.cb, sex=="M"), group = "cohort", estimator="wlsmv", parameterization="theta", group.equal='loadings')

# FEMALES
fa.c.f   <- cfa(config.we.th,        data = subset(items.cb, sex=="F"), group = "cohort", estimator="wlsmv", parameterization="theta")
fa.t.f   <- cfa(thr.we.th,           data = subset(items.cb, sex=="F"), group = "cohort", estimator="wlsmv", parameterization="theta")
fa.tl.f  <- cfa(thr.load.we.th,      data = subset(items.cb, sex=="F"), group = "cohort", estimator="wlsmv", parameterization="theta", group.equal='loadings')
fa.tli.f <- cfa(thr.load.int.we.th,  data = subset(items.cb, sex=="F"), group = "cohort", estimator="wlsmv", parameterization="theta", group.equal='loadings')

# JOINT
fa.c.b   <- cfa(config.we.thb,        data = items.cc, group = "cohortsex", estimator="wlsmv", parameterization="theta")
fa.t.b   <- cfa(thr.we.thb,           data = items.cc, group = "cohortsex", estimator="wlsmv", parameterization="theta")
fa.tl.b  <- cfa(thr.load.we.thb,      data = items.cc, group = "cohortsex", estimator="wlsmv", parameterization="theta", group.equal='loadings')
fa.tli.b <- cfa(thr.load.int.we.thb,  data = items.cc, group = "cohortsex", estimator="wlsmv", parameterization="theta", group.equal='loadings')

# collect models
fa.c <- list(fa.c.a, fa.c.m, fa.c.f, fa.c.b)
fa.t <- list(fa.t.a, fa.t.m, fa.t.f, fa.t.b)
fa.tl <- list(fa.tl.a, fa.tl.m, fa.tl.f, fa.tl.b)
fa.tli <- list(fa.tli.a, fa.tli.m, fa.tli.f, fa.tli.b)


## ---- FA_FIT
# assemble AFIs indices for table
indsel <- c("npar", "chisq", "rmsea", "mfi", "cfi.scaled")
mnames <- c("Configural", "Threshold Invariance", "Threshold + Loading Invariance", "Threshold, Loading, + Intercept Invariance")

afitab <- list()
for (i in 1:4) {
  afitab[[i]] <- data.frame(rbind(
    round(c( fitMeasures(fa.c[[i]], indsel), moreFitIndices(fa.c[[i]])["gammaHat"] ),5),
    round(c( fitMeasures(fa.t[[i]], indsel), moreFitIndices(fa.t[[i]])["gammaHat"] ),5),
    round(c( fitMeasures(fa.tl[[i]], indsel), moreFitIndices(fa.tl[[i]])["gammaHat"] ),5),
    round(c( fitMeasures(fa.tli[[i]], indsel), moreFitIndices(fa.tli[[i]])["gammaHat"] ),5)
  ))
  afitab[[i]]$npar  <- as.character(afitab[[i]]$npar)
  afitab[[i]]$chisq <- as.character(round(afitab[[i]]$chisq,1))
  
  # add deltas
  dmfi <- NA
  dcfi <- NA
  dgam <- NA
  for (r in 2:nrow(afitab[[i]])) {
    dmfi <- c(dmfi, afitab[[i]][r,"mfi"] -  afitab[[i]][1,"mfi"])
    dcfi <- c(dcfi, afitab[[i]][r,"cfi.scaled"] -  afitab[[i]][1,"cfi.scaled"])
    dgam <- c(dgam, afitab[[i]][r,"gammaHat"] -  afitab[[i]][1,"gammaHat"])
  }
  afitab[[i]]$dmfi <- as.matrix(dmfi)
  afitab[[i]]$dcfi <- as.matrix(dcfi)
  afitab[[i]]$dgam <- as.matrix(dgam)
  
  # add names
  afitab[[i]] <- data.frame(cbind(mnames = as.matrix(mnames), afitab[[i]]))
}









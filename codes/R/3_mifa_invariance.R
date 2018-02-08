

############################################################################################
## ---- FA_INVARIANCE

# WE CONFIGURAL - THETA PARAMETERISATION (Condition 8)
# free loadings and thresholds
# lv means = 0
# lv variances = 1
# intercepts = 0
# unique variances = 1
c.th.2a <- glue::collapse(readLines(paste0(dir_syntax, "c_th_2a.lav")), sep=" \n  ")
c.th.2ac <- glue::collapse(readLines(paste0(dir_syntax, "c_th_2ac.lav")), sep=" \n  ")
c.th.2 <- glue::collapse(readLines(paste0(dir_syntax, "c_th_2.lav")), sep=" \n  ")
c.th.4a <- glue::collapse(readLines(paste0(dir_syntax, "c_th_4a.lav")), sep=" \n  ")
c.th.4ac <- glue::collapse(readLines(paste0(dir_syntax, "c_th_4ac.lav")), sep=" \n  ")
c.th.4 <- glue::collapse(readLines(paste0(dir_syntax, "c_th_4.lav")), sep=" \n  ")
c.mimic <- paste(c.th.4, "EXT ~ age", "INT ~ age",  sep=" \n  ")

# WE CONFIGURAL - DELTA PARAMETERISATION (Condition 7)
# free loadings and thresholds
# lv means = 0
# lv variances = 1
# intercepts = 0
# scales = 1
c.dl.2a <- glue::collapse(readLines(paste0(dir_syntax, "c_dl_2a.lav")), sep=" \n  ")


# WE THRESHOLD invariance, theta par (Condition 15)
# --> equivalent to configural for K=2
# thresholds constrained across groups
# lv means = 0
# lv variances = 1
# unique variances free in group 2 (except binary items)
# intercepts free in group 2
t.th.2a <- glue::collapse(readLines(paste0(dir_syntax, "t_th_2a.lav")), sep=" \n  ")
t.th.2ac <- glue::collapse(readLines(paste0(dir_syntax, "t_th_2ac.lav")), sep=" \n  ")
t.th.2 <- glue::collapse(readLines(paste0(dir_syntax, "t_th_2.lav")), sep=" \n  ")
t.th.4a <- glue::collapse(readLines(paste0(dir_syntax, "t_th_4a.lav")), sep=" \n  ")
t.th.4ac <- glue::collapse(readLines(paste0(dir_syntax, "t_th_4ac.lav")), sep=" \n  ")
t.th.4 <- glue::collapse(readLines(paste0(dir_syntax, "t_th_4.lav")), sep=" \n  ")
t.mimic <- paste(t.th.4, "EXT ~ age", "INT ~ age",  sep=" \n  ")


# WE THRESHOLD+LOADINGS invariance, theta par (Condition 19)
# thresholds constrained across groups (specified in cfa call)
# loadings constrained across groups
# lv means = 0
# lv variances free in group 2
# unique variances free in group 2 (except binary items)
# intercepts free in group 2
tl.th.2a <- glue::collapse(readLines(paste0(dir_syntax, "tl_th_2a.lav")), sep=" \n  ")
tl.th.2ac <- glue::collapse(readLines(paste0(dir_syntax, "tl_th_2ac.lav")), sep=" \n  ")
tl.th.2 <- glue::collapse(readLines(paste0(dir_syntax, "tl_th_2.lav")), sep=" \n  ")
tl.th.4a <- glue::collapse(readLines(paste0(dir_syntax, "tl_th_4a.lav")), sep=" \n  ")
tl.th.4ac <- glue::collapse(readLines(paste0(dir_syntax, "tl_th_4ac.lav")), sep=" \n  ")
tl.th.4 <- glue::collapse(readLines(paste0(dir_syntax, "tl_th_4.lav")), sep=" \n  ")
tl.mimic <- paste(tl.th.4, "EXT ~ age", "INT ~ age",  sep=" \n  ")


# WE THRESHOLD+LOADINGS+INTERCEPT invariance, theta par (Condition 27)
# thresholds constrained across groups (specified in cfa call)
# loadings constrained across groups
# lv means free in group 2
# lv variances free in group 2
# unique variances free in group 2 (except binary items)
# intercepts constrained to zero for both groups
tli.th.2a <- glue::collapse(readLines(paste0(dir_syntax, "tli_th_2a.lav")), sep=" \n  ")
tli.th.2ac <- glue::collapse(readLines(paste0(dir_syntax, "tli_th_2ac.lav")), sep=" \n  ")
tli.th.2 <- glue::collapse(readLines(paste0(dir_syntax, "tli_th_2.lav")), sep=" \n  ")
tli.th.4a <- glue::collapse(readLines(paste0(dir_syntax, "tli_th_4a.lav")), sep=" \n  ")
tli.th.4ac <- glue::collapse(readLines(paste0(dir_syntax, "tli_th_4ac.lav")), sep=" \n  ")
tli.th.4 <- glue::collapse(readLines(paste0(dir_syntax, "tli_th_4.lav")), sep=" \n  ")
tli.mimic <- paste(tli.th.4, "EXT ~ age", "INT ~ age",  sep=" \n  ")

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


# INITIALISE LISTS
fa.c <- list()
fa.t <- list()
fa.tl <- list()
fa.tli <- list()

# MODEL 1: WHOLE SAMPLE, no gender split, not age adjusted
fa.c[[1]]   <- cfa(c.th.2, data = items[[1]], group = "cohort", estimator="wlsmv", parameterization="theta")
fa.tl[[1]]  <- cfa(tl.th.2, data = items[[1]], group = "cohort", estimator="wlsmv", parameterization="theta", group.equal='loadings')
fa.tli[[1]] <- cfa(tli.th.2, data = items[[1]], group = "cohort", estimator="wlsmv", parameterization="theta", group.equal='loadings')

# MODEL 2: WHOLE SAMPLE, no gender split, age adjusted
fa.c[[2]]   <- cfa(c.th.2a, data = items[[2]], group = "cohort", estimator="wlsmv", parameterization="theta")
fa.tl[[2]]  <- cfa(tl.th.2a, data = items[[2]], group = "cohort", estimator="wlsmv", parameterization="theta", group.equal='loadings')
fa.tli[[2]] <- cfa(tli.th.2a, data = items[[2]], group = "cohort", estimator="wlsmv", parameterization="theta", group.equal='loadings')

# MODEL 3: MALES ONLY, not age adjusted
fa.c[[3]]   <- cfa(c.th.2, data = items[[3]], group = "cohort", estimator="wlsmv", parameterization="theta")
fa.tl[[3]]  <- cfa(tl.th.2, data = items[[3]], group = "cohort", estimator="wlsmv", parameterization="theta", group.equal='loadings')
fa.tli[[3]] <- cfa(tli.th.2, data = items[[3]], group = "cohort", estimator="wlsmv", parameterization="theta", group.equal='loadings')

# MODEL 7: FEMALES ONLY, not age adjusted
fa.c[[4]]   <- cfa(c.th.2, data = items[[4]], group = "cohort", estimator="wlsmv", parameterization="theta")
fa.tl[[4]]  <- cfa(tl.th.2, data = items[[4]], group = "cohort", estimator="wlsmv", parameterization="theta", group.equal='loadings')
fa.tli[[4]] <- cfa(tli.th.2, data = items[[4]], group = "cohort", estimator="wlsmv", parameterization="theta", group.equal='loadings')

# MODEL 5: MALES ONLY, age adjusted
fa.c[[5]]   <- cfa(c.th.2a, data = items[[5]], group = "cohort", estimator="wlsmv", parameterization="theta")
fa.tl[[5]]  <- cfa(tl.th.2a, data = items[[5]], group = "cohort", estimator="wlsmv", parameterization="theta", group.equal='loadings')
fa.tli[[5]] <- cfa(tli.th.2a, data = items[[5]], group = "cohort", estimator="wlsmv", parameterization="theta", group.equal='loadings')

# MODEL 6: FEMALES ONLY, age adjusted
fa.c[[6]]   <- cfa(c.th.2a, data = items[[6]], group = "cohort", estimator="wlsmv", parameterization="theta")
fa.tl[[6]]  <- cfa(tl.th.2a, data = items[[6]], group = "cohort", estimator="wlsmv", parameterization="theta", group.equal='loadings')
fa.tli[[6]] <- cfa(tli.th.2a, data = items[[6]], group = "cohort", estimator="wlsmv", parameterization="theta", group.equal='loadings')

# MODEL 7: separate gender groups, not age adjusted
fa.c[[7]]   <- cfa(c.th.4, data = items[[7]], group = "cohortsex", estimator="wlsmv", parameterization="theta")
fa.tl[[7]]  <- cfa(tl.th.4, data = items[[7]], group = "cohortsex", estimator="wlsmv", parameterization="theta", group.equal='loadings')
fa.tli[[7]] <- cfa(tli.th.4, data = items[[7]], group = "cohortsex", estimator="wlsmv", parameterization="theta", group.equal='loadings')

# MODEL 8: separate gender groups, age adjusted
fa.c[[8]]   <- cfa(c.th.4a, data = items[[8]], group = "cohortsex", estimator="wlsmv", parameterization="theta")
fa.tl[[8]]  <- cfa(tl.th.4a, data = items[[8]], group = "cohortsex", estimator="wlsmv", parameterization="theta", group.equal='loadings')
fa.tli[[8]] <- cfa(tli.th.4a, data = items[[8]], group = "cohortsex", estimator="wlsmv", parameterization="theta", group.equal='loadings')

# MODEL 9: separate gender groups, overlapping ages, no age adjustment
fa.c[[9]]   <- cfa(c.th.4, data = items[[9]], group = "cohortsex", estimator="wlsmv", parameterization="theta")
fa.tl[[9]]  <- cfa(tl.th.4, data = items[[9]], group = "cohortsex", estimator="wlsmv", parameterization="theta", group.equal='loadings')
fa.tli[[9]] <- cfa(tli.th.4, data = items[[9]], group = "cohortsex", estimator="wlsmv", parameterization="theta", group.equal='loadings')

# MODEL 10: separate gender groups, age adjusted (constrained)
fa.c[[10]]   <- cfa(c.th.4ac, data = items[[10]], group = "cohortsex", estimator="wlsmv", parameterization="theta")
fa.tl[[10]]  <- cfa(tl.th.4ac, data = items[[10]], group = "cohortsex", estimator="wlsmv", parameterization="theta", group.equal='loadings')
fa.tli[[10]] <- cfa(tli.th.4ac, data = items[[10]], group = "cohortsex", estimator="wlsmv", parameterization="theta", group.equal='loadings')

# MODEL 11: MIMIC
fa.c[[11]]   <- cfa(c.mimic, data = items[[11]], group = "cohortsex", estimator="wlsmv", parameterization="theta")
fa.tl[[11]]  <- cfa(tl.mimic, data = items[[11]], group = "cohortsex", estimator="wlsmv", parameterization="theta", group.equal='loadings')
fa.tli[[11]] <- cfa(tli.mimic, data = items[[11]], group = "cohortsex", estimator="wlsmv", parameterization="theta", group.equal='loadings')


## ---- FA_FIT
# assemble AFIs indices for table
indsel <- c("npar", "chisq", "rmsea", "mfi", "cfi.scaled")
mnames <- c("Configural", "Threshold + Loading Invariance", "Threshold, Loading, + Intercept Invariance")

afitab <- list()
for (i in 1:length(items)) {
  afitab[[i]] <- data.frame(rbind(
    round(c( fitMeasures(fa.c[[i]], indsel), moreFitIndices(fa.c[[i]])["gammaHat"] ),5),
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









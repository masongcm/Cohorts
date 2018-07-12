############################################################################################
## ---- FA_INV_MAIN
# import syntax and estimate core models

# WE CONFIGURAL - THETA PARAMETERISATION (Condition 8)
c.th.4 <- glue::collapse(readLines(paste0(dir_syntax, "c_th_4.lav")), sep=" \n  ")

# WE CONFIGURAL - DELTA PARAMETERISATION (Condition 7)
c.dl.2a <- glue::collapse(readLines(paste0(dir_syntax, "c_dl_2a.lav")), sep=" \n  ")

# WE THRESHOLD invariance, theta par (Condition 15)
t.th.4 <- glue::collapse(readLines(paste0(dir_syntax, "t_th_4.lav")), sep=" \n  ")

# WE THRESHOLD+LOADINGS invariance, theta par (Condition 19)
tl.th.4 <- glue::collapse(readLines(paste0(dir_syntax, "tl_th_4.lav")), sep=" \n  ")

# WE THRESHOLD+LOADINGS+INTERCEPT invariance, theta par (Condition 27)
tli.th.4 <- glue::collapse(readLines(paste0(dir_syntax, "tli_th_4.lav")), sep=" \n  ")

# PARTIAL INTERCEPT INVARIANCE
tlip.th.4 <- glue::collapse(readLines(paste0(dir_syntax, "tlip_th_4.lav")), sep=" \n  ")

# allocate empty lists
fa.c <- list()
fa.t <- list()
fa.tl <- list()
fa.tli <- list()
fa.tlip <- list()

# MODEL 1: separate gender groups, not age adjusted
fa.c[[1]]   <- cfa(c.th.4, data = items[[1]], group = "cohortsex", estimator="wlsmv", parameterization="theta")
fa.tl[[1]]  <- cfa(tl.th.4, data = items[[1]], group = "cohortsex", estimator="wlsmv", parameterization="theta", group.equal='loadings')
fa.tli[[1]] <- cfa(tli.th.4, data = items[[1]], group = "cohortsex", estimator="wlsmv", parameterization="theta", group.equal='loadings')
fa.tlip[[1]] <- cfa(tlip.th.4, data = items[[1]], group = "cohortsex", estimator="wlsmv", parameterization="theta", group.equal='loadings')

# MODEL 2: separate gender groups, overlapping ages, no age adjustment
fa.c[[2]]   <- cfa(c.th.4, data = items[[2]], group = "cohortsex", estimator="wlsmv", parameterization="theta")
fa.tl[[2]]  <- cfa(tl.th.4, data = items[[2]], group = "cohortsex", estimator="wlsmv", parameterization="theta", group.equal='loadings')
fa.tli[[2]] <- cfa(tli.th.4, data = items[[2]], group = "cohortsex", estimator="wlsmv", parameterization="theta", group.equal='loadings')
fa.tlip[[2]] <- cfa(tlip.th.4, data = items[[2]], group = "cohortsex", estimator="wlsmv", parameterization="theta", group.equal='loadings')

# select final model
finalmod <- fa.tl[[1]]


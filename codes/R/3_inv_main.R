

############################################################################################
## ---- FA_INV_MAIN
# import syntax and estimate core models

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


# allocate empty lists
fa.c <- list()
fa.t <- list()
fa.tl <- list()
fa.tli <- list()

# MODEL 1: separate gender groups, not age adjusted
fa.c[[1]]   <- cfa(c.th.4, data = items[[1]], group = "cohortsex", estimator="wlsmv", parameterization="theta")
fa.tl[[1]]  <- cfa(tl.th.4, data = items[[1]], group = "cohortsex", estimator="wlsmv", parameterization="theta", group.equal='loadings')
fa.tli[[1]] <- cfa(tli.th.4, data = items[[1]], group = "cohortsex", estimator="wlsmv", parameterization="theta", group.equal='loadings')

# MODEL 2: separate gender groups, overlapping ages, no age adjustment
fa.c[[2]]   <- cfa(c.th.4, data = items[[2]], group = "cohortsex", estimator="wlsmv", parameterization="theta")
fa.tl[[2]]  <- cfa(tl.th.4, data = items[[2]], group = "cohortsex", estimator="wlsmv", parameterization="theta", group.equal='loadings')
fa.tli[[2]] <- cfa(tli.th.4, data = items[[2]], group = "cohortsex", estimator="wlsmv", parameterization="theta", group.equal='loadings')

# select final model
finalmod <- fa.tl[[1]]


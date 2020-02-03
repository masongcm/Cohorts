## ---- FA_INV_AUX_1FAC
# robustness checks with a single factor
c.th.4.1f <- glue::glue_collapse(readLines(paste0(dir_syntax, "c_th_4_1f.lav")), sep=" \n  ")
tl.th.4.1f <- glue::glue_collapse(readLines(paste0(dir_syntax, "tl_th_4_1f.lav")), sep=" \n  ")
tli.th.4.1f <- glue::glue_collapse(readLines(paste0(dir_syntax, "tli_th_4_1f.lav")), sep=" \n  ")

fa.c.1f   <- cfa(c.th.4.1f, data = fadata[[1]], group = "cohortsex", estimator="wlsmv", parameterization="theta")
fa.tl.1f   <- cfa(tl.th.4.1f, data = fadata[[1]], group = "cohortsex", estimator="wlsmv", parameterization="theta",group.equal='loadings')
fa.tli.1f <- cfa(tli.th.4.1f, data = fadata[[1]], group = "cohortsex", estimator="wlsmv", parameterization="theta", group.equal='loadings')

## ---- FA_INV_AUX_SYN
# syntax for robustness checks

# CONFIGURAL
c.th.2a <- glue::collapse(readLines(paste0(dir_syntax, "c_th_2a.lav")), sep=" \n  ")
c.th.2ac <- glue::collapse(readLines(paste0(dir_syntax, "c_th_2ac.lav")), sep=" \n  ")
c.th.2 <- glue::collapse(readLines(paste0(dir_syntax, "c_th_2.lav")), sep=" \n  ")
c.th.4a <- glue::collapse(readLines(paste0(dir_syntax, "c_th_4a.lav")), sep=" \n  ")
c.th.4ac <- glue::collapse(readLines(paste0(dir_syntax, "c_th_4ac.lav")), sep=" \n  ")
c.mimic <- paste(c.th.4, "EXT ~ age", "INT ~ age",  sep=" \n  ")

# THRESHOLD
t.th.2a <- glue::collapse(readLines(paste0(dir_syntax, "t_th_2a.lav")), sep=" \n  ")
t.th.2ac <- glue::collapse(readLines(paste0(dir_syntax, "t_th_2ac.lav")), sep=" \n  ")
t.th.2 <- glue::collapse(readLines(paste0(dir_syntax, "t_th_2.lav")), sep=" \n  ")
t.th.4a <- glue::collapse(readLines(paste0(dir_syntax, "t_th_4a.lav")), sep=" \n  ")
t.th.4ac <- glue::collapse(readLines(paste0(dir_syntax, "t_th_4ac.lav")), sep=" \n  ")
t.mimic <- paste(t.th.4, "EXT ~ age", "INT ~ age",  sep=" \n  ")

# THRESHOLD+LOADING
tl.th.2a <- glue::collapse(readLines(paste0(dir_syntax, "tl_th_2a.lav")), sep=" \n  ")
tl.th.2ac <- glue::collapse(readLines(paste0(dir_syntax, "tl_th_2ac.lav")), sep=" \n  ")
tl.th.2 <- glue::collapse(readLines(paste0(dir_syntax, "tl_th_2.lav")), sep=" \n  ")
tl.th.4a <- glue::collapse(readLines(paste0(dir_syntax, "tl_th_4a.lav")), sep=" \n  ")
tl.th.4ac <- glue::collapse(readLines(paste0(dir_syntax, "tl_th_4ac.lav")), sep=" \n  ")
tl.mimic <- paste(tl.th.4, "EXT ~ age", "INT ~ age",  sep=" \n  ")

# THRESHOLD+LOADING+INTERCEPT
tli.th.2a <- glue::collapse(readLines(paste0(dir_syntax, "tli_th_2a.lav")), sep=" \n  ")
tli.th.2ac <- glue::collapse(readLines(paste0(dir_syntax, "tli_th_2ac.lav")), sep=" \n  ")
tli.th.2 <- glue::collapse(readLines(paste0(dir_syntax, "tli_th_2.lav")), sep=" \n  ")
tli.th.4a <- glue::collapse(readLines(paste0(dir_syntax, "tli_th_4a.lav")), sep=" \n  ")
tli.th.4ac <- glue::collapse(readLines(paste0(dir_syntax, "tli_th_4ac.lav")), sep=" \n  ")
tli.mimic <- paste(tli.th.4, "EXT ~ age", "INT ~ age",  sep=" \n  ")


## ---- FA_INV_AUX_GEND
# robustness checks with gender

# MODEL 3: WHOLE SAMPLE, no gender split, not age adjusted
fa.c[[3]]   <- cfa(c.th.2, data = fadata[[3]], group = "cohort", estimator="wlsmv", parameterization="theta")
fa.tl[[3]]  <- cfa(tl.th.2, data = fadata[[3]], group = "cohort", estimator="wlsmv", parameterization="theta", group.equal='loadings')
fa.tli[[3]] <- cfa(tli.th.2, data = fadata[[3]], group = "cohort", estimator="wlsmv", parameterization="theta", group.equal='loadings')

# MODEL 4: MALES ONLY, not age adjusted
fa.c[[4]]   <- cfa(c.th.2, data = fadata[[4]], group = "cohort", estimator="wlsmv", parameterization="theta")
fa.tl[[4]]  <- cfa(tl.th.2, data = fadata[[4]], group = "cohort", estimator="wlsmv", parameterization="theta", group.equal='loadings')
fa.tli[[4]] <- cfa(tli.th.2, data = fadata[[4]], group = "cohort", estimator="wlsmv", parameterization="theta", group.equal='loadings')

# MODEL 5: FEMALES ONLY, not age adjusted
fa.c[[5]]   <- cfa(c.th.2, data = fadata[[5]], group = "cohort", estimator="wlsmv", parameterization="theta")
fa.tl[[5]]  <- cfa(tl.th.2, data = fadata[[5]], group = "cohort", estimator="wlsmv", parameterization="theta", group.equal='loadings')
fa.tli[[5]] <- cfa(tli.th.2, data = fadata[[5]], group = "cohort", estimator="wlsmv", parameterization="theta", group.equal='loadings')


## ---- FA_INV_AUX_AGE
# robustness checks with age adjustments

# MODEL 6: WHOLE SAMPLE, no gender split, age adjusted
fa.c[[6]]   <- cfa(c.th.2a, data = fadata[[6]], group = "cohort", estimator="wlsmv", parameterization="theta")
fa.tl[[6]]  <- cfa(tl.th.2a, data = fadata[[6]], group = "cohort", estimator="wlsmv", parameterization="theta", group.equal='loadings')
fa.tli[[6]] <- cfa(tli.th.2a, data = fadata[[6]], group = "cohort", estimator="wlsmv", parameterization="theta", group.equal='loadings')

# MODEL 7: MALES ONLY, age adjusted
fa.c[[7]]   <- cfa(c.th.2a, data = fadata[[7]], group = "cohort", estimator="wlsmv", parameterization="theta")
fa.tl[[7]]  <- cfa(tl.th.2a, data = fadata[[7]], group = "cohort", estimator="wlsmv", parameterization="theta", group.equal='loadings')
fa.tli[[7]] <- cfa(tli.th.2a, data = fadata[[7]], group = "cohort", estimator="wlsmv", parameterization="theta", group.equal='loadings')

# MODEL 8: FEMALES ONLY, age adjusted
fa.c[[8]]   <- cfa(c.th.2a, data = fadata[[8]], group = "cohort", estimator="wlsmv", parameterization="theta")
fa.tl[[8]]  <- cfa(tl.th.2a, data = fadata[[8]], group = "cohort", estimator="wlsmv", parameterization="theta", group.equal='loadings')
fa.tli[[8]] <- cfa(tli.th.2a, data = fadata[[8]], group = "cohort", estimator="wlsmv", parameterization="theta", group.equal='loadings')

# MODEL 9: separate gender groups, age adjusted
fa.c[[9]]   <- cfa(c.th.4a, data = fadata[[9]], group = "cohortsex", estimator="wlsmv", parameterization="theta")
fa.tl[[9]]  <- cfa(tl.th.4a, data = fadata[[9]], group = "cohortsex", estimator="wlsmv", parameterization="theta", group.equal='loadings')
fa.tli[[9]] <- cfa(tli.th.4a, data = fadata[[9]], group = "cohortsex", estimator="wlsmv", parameterization="theta", group.equal='loadings')

# MODEL 10: separate gender groups, age adjusted (constrained)
fa.c[[10]]   <- cfa(c.th.4ac, data = fadata[[10]], group = "cohortsex", estimator="wlsmv", parameterization="theta")
fa.tl[[10]]  <- cfa(tl.th.4ac, data = fadata[[10]], group = "cohortsex", estimator="wlsmv", parameterization="theta", group.equal='loadings')
fa.tli[[10]] <- cfa(tli.th.4ac, data = fadata[[10]], group = "cohortsex", estimator="wlsmv", parameterization="theta", group.equal='loadings')

# MODEL 11: MIMIC
fa.c[[11]]   <- cfa(c.mimic, data = fadata[[11]], group = "cohortsex", estimator="wlsmv", parameterization="theta")
fa.tl[[11]]  <- cfa(tl.mimic, data = fadata[[11]], group = "cohortsex", estimator="wlsmv", parameterization="theta", group.equal='loadings')
fa.tli[[11]] <- cfa(tli.mimic, data = fadata[[11]], group = "cohortsex", estimator="wlsmv", parameterization="theta", group.equal='loadings')


# equivalent models (MT does not converge!)
# fa.config.mt.th <- cfa(config.mt.th, data = fadata.c, group = "cohort", estimator="wlsmv", parameterization="theta")
# printfit(fa.config.mt.th)
# fa.config.mt.d<- cfa(config.mt.d, data = fadata.c, group = "cohort", estimator="wlsmv", parameterization="delta")
# printfit(fa.config.mt.d)
# fa.config.we.d <- cfa(config.we.d, data = fadata.cb, group = "cohort", estimator="wlsmv", parameterization="delta")
# printfit(fa.config.we.d)
# # THRESHOLDS + INTERCEPTS (does not converge)
# fa.thr.int.we.d <- cfa(thr.int.we.d, data = fadata.c, group = "cohort", estimator="wlsmv", parameterization="delta")
# printfit(fa.thr.int.we.d)




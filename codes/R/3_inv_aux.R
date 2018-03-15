
## ---- FA_INV_AUX
# auxiliary models for robustness

# MODEL 3: WHOLE SAMPLE, no gender split, not age adjusted
fa.c[[3]]   <- cfa(c.th.2, data = items[[3]], group = "cohort", estimator="wlsmv", parameterization="theta")
fa.tl[[3]]  <- cfa(tl.th.2, data = items[[3]], group = "cohort", estimator="wlsmv", parameterization="theta", group.equal='loadings')
fa.tli[[3]] <- cfa(tli.th.2, data = items[[3]], group = "cohort", estimator="wlsmv", parameterization="theta", group.equal='loadings')

# MODEL 4: MALES ONLY, not age adjusted
fa.c[[4]]   <- cfa(c.th.2, data = items[[4]], group = "cohort", estimator="wlsmv", parameterization="theta")
fa.tl[[4]]  <- cfa(tl.th.2, data = items[[4]], group = "cohort", estimator="wlsmv", parameterization="theta", group.equal='loadings')
fa.tli[[4]] <- cfa(tli.th.2, data = items[[4]], group = "cohort", estimator="wlsmv", parameterization="theta", group.equal='loadings')

# MODEL 5: FEMALES ONLY, not age adjusted
fa.c[[5]]   <- cfa(c.th.2, data = items[[5]], group = "cohort", estimator="wlsmv", parameterization="theta")
fa.tl[[5]]  <- cfa(tl.th.2, data = items[[5]], group = "cohort", estimator="wlsmv", parameterization="theta", group.equal='loadings')
fa.tli[[5]] <- cfa(tli.th.2, data = items[[5]], group = "cohort", estimator="wlsmv", parameterization="theta", group.equal='loadings')

# MODEL 6: WHOLE SAMPLE, no gender split, age adjusted
fa.c[[6]]   <- cfa(c.th.2a, data = items[[6]], group = "cohort", estimator="wlsmv", parameterization="theta")
fa.tl[[6]]  <- cfa(tl.th.2a, data = items[[6]], group = "cohort", estimator="wlsmv", parameterization="theta", group.equal='loadings')
fa.tli[[6]] <- cfa(tli.th.2a, data = items[[6]], group = "cohort", estimator="wlsmv", parameterization="theta", group.equal='loadings')

# MODEL 7: MALES ONLY, age adjusted
fa.c[[7]]   <- cfa(c.th.2a, data = items[[7]], group = "cohort", estimator="wlsmv", parameterization="theta")
fa.tl[[7]]  <- cfa(tl.th.2a, data = items[[7]], group = "cohort", estimator="wlsmv", parameterization="theta", group.equal='loadings')
fa.tli[[7]] <- cfa(tli.th.2a, data = items[[7]], group = "cohort", estimator="wlsmv", parameterization="theta", group.equal='loadings')

# MODEL 8: FEMALES ONLY, age adjusted
fa.c[[8]]   <- cfa(c.th.2a, data = items[[8]], group = "cohort", estimator="wlsmv", parameterization="theta")
fa.tl[[8]]  <- cfa(tl.th.2a, data = items[[8]], group = "cohort", estimator="wlsmv", parameterization="theta", group.equal='loadings')
fa.tli[[8]] <- cfa(tli.th.2a, data = items[[8]], group = "cohort", estimator="wlsmv", parameterization="theta", group.equal='loadings')

# MODEL 9: separate gender groups, age adjusted
fa.c[[9]]   <- cfa(c.th.4a, data = items[[9]], group = "cohortsex", estimator="wlsmv", parameterization="theta")
fa.tl[[9]]  <- cfa(tl.th.4a, data = items[[9]], group = "cohortsex", estimator="wlsmv", parameterization="theta", group.equal='loadings')
fa.tli[[9]] <- cfa(tli.th.4a, data = items[[9]], group = "cohortsex", estimator="wlsmv", parameterization="theta", group.equal='loadings')

# MODEL 10: separate gender groups, age adjusted (constrained)
fa.c[[10]]   <- cfa(c.th.4ac, data = items[[10]], group = "cohortsex", estimator="wlsmv", parameterization="theta")
fa.tl[[10]]  <- cfa(tl.th.4ac, data = items[[10]], group = "cohortsex", estimator="wlsmv", parameterization="theta", group.equal='loadings')
fa.tli[[10]] <- cfa(tli.th.4ac, data = items[[10]], group = "cohortsex", estimator="wlsmv", parameterization="theta", group.equal='loadings')

# MODEL 11: MIMIC
fa.c[[11]]   <- cfa(c.mimic, data = items[[11]], group = "cohortsex", estimator="wlsmv", parameterization="theta")
fa.tl[[11]]  <- cfa(tl.mimic, data = items[[11]], group = "cohortsex", estimator="wlsmv", parameterization="theta", group.equal='loadings')
fa.tli[[11]] <- cfa(tli.mimic, data = items[[11]], group = "cohortsex", estimator="wlsmv", parameterization="theta", group.equal='loadings')


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




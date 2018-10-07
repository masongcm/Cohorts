
############################################################################################
## ---- FA_INVARIANCE

# MT CONFIGURAL - THETA PARAMETERISATION
# - one anchor loadings = 1 for each factor
# - all intercepts = 0
# - factor means = 0 in group 1 only
# - thresholds: tau1 restricted equal across groups, tau2 invariant only for anchors

config.mt.th <- "
EXT =~ c(1, 1)*X2 + X1 + X3 + X4 + X5 + X6
INT =~ c(1, 1)*X8 + X7 + X9 + X10 + X11
X2 | c(t1_2, t1_2)*t1 + c(t2_2, t2_2)*t2
X1 | c(t1_1, t1_1)*t1 + t2
X3 | c(t1_3, t1_3)*t1 + t2
X4 | c(t1_4, t1_4)*t1 + t2
X5 | c(t1_5, t1_5)*t1
X6 | c(t1_6, t1_6)*t1
X8 | c(t1_8, t1_8)*t1 + c(t2_8, t2_8)*t2
X7 | c(t1_7, t1_7)*t1 + t2
X10 | c(t1_10, t1_10)*t1 + t2
X11 | c(t1_11, t1_11)*t1
X1  ~ c(0, 0)*1
X2  ~ c(0, 0)*1
X3  ~ c(0, 0)*1
X4  ~ c(0, 0)*1
X5  ~ c(0, 0)*1
X6  ~ c(0, 0)*1
X7  ~ c(0, 0)*1
X8  ~ c(0, 0)*1
X9  ~ c(0, 0)*1
X10  ~ c(0, 0)*1
X11  ~ c(0, 0)*1
X1 ~~ c(1,NA)*X1
X2 ~~ c(1,NA)*X2
X3 ~~ c(1,NA)*X3
X4 ~~ c(1,NA)*X4
X5 ~~ c(1,NA)*X5
X6 ~~ c(1,NA)*X6
X7 ~~ c(1,NA)*X7
X8 ~~ c(1,NA)*X8
X9 ~~ c(1,NA)*X9
X10 ~~ c(1,NA)*X10
X11 ~~ c(1,NA)*X11
EXT ~~ NA*EXT
INT ~~ NA*INT
EXT ~~ NA*INT
EXT ~ c(0, NA)*1
INT ~ c(0, NA)*1
"

# MT CONFIGURAL - DELTA PARAMETERISATION
config.mt.d <- "
EXT =~ c(1, 1)*X2 + X1 + X3 + X4 + X5 + X6
INT =~ c(1, 1)*X8 + X7 + X9 + X10 + X11
X2 | c(t1_2, t1_2)*t1 + c(t2_2, t2_2)*t2
X1 | c(t1_1, t1_1)*t1 + t2
X3 | c(t1_3, t1_3)*t1 + t2
X4 | c(t1_4, t1_4)*t1 + t2
X5 | c(t1_5, t1_5)*t1
X6 | c(t1_6, t1_6)*t1
X8 | c(t1_8, t1_8)*t1 + c(t2_8, t2_8)*t2
X7 | c(t1_7, t1_7)*t1 + t2
X10 | c(t1_10, t1_10)*t1 + t2
X11 | c(t1_11, t1_11)*t1
X1  ~ c(0, 0)*1
X2  ~ c(0, 0)*1
X3  ~ c(0, 0)*1
X4  ~ c(0, 0)*1
X5  ~ c(0, 0)*1
X6  ~ c(0, 0)*1
X7  ~ c(0, 0)*1
X8  ~ c(0, 0)*1
X9  ~ c(0, 0)*1
X10  ~ c(0, 0)*1
X11  ~ c(0, 0)*1
X1 ~*~ c(1,NA)*X1
X2 ~*~ c(1,NA)*X2
X3 ~*~ c(1,NA)*X3
X4 ~*~ c(1,NA)*X4
X5 ~*~ c(1,NA)*X5
X6 ~*~ c(1,NA)*X6
X7 ~*~ c(1,NA)*X7
X8 ~*~ c(1,NA)*X8
X9 ~*~ c(1,NA)*X9
X10 ~*~ c(1,NA)*X10
X11 ~*~ c(1,NA)*X11
EXT ~~ NA*EXT
INT ~~ NA*INT
EXT ~~ NA*INT
EXT ~ c(0,NA)*1
INT ~ c(0,NA)*1
"

# WE CONFIGURAL - THETA PARAMETERISATION (Condition 8)
# free loadings and thresholds
# lv means = 0
# lv variances = 1
# intercepts = 0
# unique variances = 1
config.we.th <- "
EXT =~ c(NA, NA)*X2 + X1 + X3 + X4 + X5 + X6
INT =~ c(NA, NA)*X8 + X7 + X9 + X10 + X11
X1 | t1 + t2
X2 | t1 + t2
X3 | t1 + t2
X4 | t1 + t2
X5 | t1
X6 | t1
X7 | t1 + t2
X8 | t1 + t2
X9 | t1 + t2
X10 | t1 + t2
X11 | t1
X1  ~ c(0, 0)*1
X2  ~ c(0, 0)*1
X3  ~ c(0, 0)*1
X4  ~ c(0, 0)*1
X5  ~ c(0, 0)*1
X6  ~ c(0, 0)*1
X7  ~ c(0, 0)*1
X8  ~ c(0, 0)*1
X9  ~ c(0, 0)*1
X10  ~ c(0, 0)*1
X11  ~ c(0, 0)*1
X1 ~~ c(1,1)*X1
X2 ~~ c(1,1)*X2
X3 ~~ c(1,1)*X3
X4 ~~ c(1,1)*X4
X5 ~~ c(1,1)*X5
X6 ~~ c(1,1)*X6
X7 ~~ c(1,1)*X7
X8 ~~ c(1,1)*X8
X9 ~~ c(1,1)*X9
X10 ~~ c(1,1)*X10
X11 ~~ c(1,1)*X11
EXT ~~ c(1,1)*EXT
INT ~~ c(1,1)*INT
EXT ~~ NA*INT
EXT ~ c(0, 0)*1
INT ~ c(0, 0)*1
"


# WE CONFIGURAL - DELTA PARAMETERISATION (Condition 7)
# free loadings and thresholds
# lv means = 0
# lv variances = 1
# intercepts = 0
# scales = 1
config.we.d <- "
EXT =~ c(NA, NA)*X2 + X1 + X3 + X4 + X5 + X6
INT =~ c(NA, NA)*X8 + X7 + X9 + X10 + X11
X1 | t1 + t2
X2 | t1 + t2
X3 | t1 + t2
X4 | t1 + t2
X5 | t1
X6 | t1
X7 | t1 + t2
X8 | t1 + t2
X9 | t1 + t2
X10 | t1 + t2
X11 | t1
X1  ~ c(0, 0)*1
X2  ~ c(0, 0)*1
X3  ~ c(0, 0)*1
X4  ~ c(0, 0)*1
X5  ~ c(0, 0)*1
X6  ~ c(0, 0)*1
X7  ~ c(0, 0)*1
X8  ~ c(0, 0)*1
X9  ~ c(0, 0)*1
X10  ~ c(0, 0)*1
X11  ~ c(0, 0)*1
X1 ~*~ c(1,1)*X1
X2 ~*~ c(1,1)*X2
X3 ~*~ c(1,1)*X3
X4 ~*~ c(1,1)*X4
X5 ~*~ c(1,1)*X5
X6 ~*~ c(1,1)*X6
X7 ~*~ c(1,1)*X7
X8 ~*~ c(1,1)*X8
X9 ~*~ c(1,1)*X9
X10 ~*~ c(1,1)*X10
X11 ~*~ c(1,1)*X11
EXT ~~ c(1,1)*EXT
INT ~~ c(1,1)*INT
EXT ~~ NA*INT
EXT ~ c(0, 0)*1
INT ~ c(0, 0)*1
"

# WE THRESHOLD invariance, theta par (Condition 15)
# --> equivalent to configural for K=2
# thresholds constrained across groups
# lv means = 0
# lv variances = 1
# unique variances free in group 2 (except binary items)
# intercepts free in group 2
thr.we.th <- "
EXT =~ c(NA, NA)*X2 + X1 + X3 + X4 + X5 + X6
INT =~ c(NA, NA)*X8 + X7 + X9 + X10 + X11
X2 | c(t1_2, t1_2)*t1 + c(t2_2, t2_2)*t2
X1 | c(t1_1, t1_1)*t1 + c(t2_1, t2_1)*t2
X3 | c(t1_3, t1_3)*t1 + c(t2_3, t2_3)*t2
X4 | c(t1_4, t1_4)*t1 + c(t2_4, t2_4)*t2
X5 | c(t1_5, t1_5)*t1
X6 | c(t1_6, t1_6)*t1
X8 | c(t1_8, t1_8)*t1 + c(t2_8, t2_8)*t2
X7 | c(t1_7, t1_7)*t1 + c(t2_7, t2_7)*t2
X9 | c(t1_9, t1_9)*t1 + c(t2_9, t2_9)*t2
X10 | c(t1_10, t1_10)*t1 + c(t2_10, t2_10)*t2
X11 | c(t1_11, t1_11)*t1
X1  ~ c(0, NA)*1
X2  ~ c(0, NA)*1
X3  ~ c(0, NA)*1
X4  ~ c(0, NA)*1
X5  ~ c(0, NA)*1
X6  ~ c(0, NA)*1
X7  ~ c(0, NA)*1
X8  ~ c(0, NA)*1
X9  ~ c(0, NA)*1
X10  ~ c(0, NA)*1
X11  ~ c(0, NA)*1
X1 ~~ c(1,NA)*X1
X2 ~~ c(1,NA)*X2
X3 ~~ c(1,NA)*X3
X4 ~~ c(1,NA)*X4
X5 ~~ c(1,1)*X5
X6 ~~ c(1,1)*X6
X7 ~~ c(1,NA)*X7
X8 ~~ c(1,NA)*X8
X9 ~~ c(1,NA)*X9
X10 ~~ c(1,NA)*X10
X11 ~~ c(1,1)*X11
EXT ~~ c(1,1)*EXT
INT ~~ c(1,1)*INT
EXT ~~ NA*INT
EXT ~ c(0, 0)*1
INT ~ c(0, 0)*1
"

# WE THRESHOLD+LOADINGS invariance, theta par (Condition 19)
# thresholds constrained across groups (specified in cfa call)
# loadings constrained across groups
# lv means = 0
# lv variances free in group 2
# unique variances free in group 2 (except binary items)
# intercepts free in group 2
thr.load.we.th <- "
EXT =~ c(NA, NA)*X2 + X1 + X3 + X4 + X5 + X6
INT =~ c(NA, NA)*X8 + X7 + X9 + X10 + X11
X2 | c(t1_2, t1_2)*t1 + c(t2_2, t2_2)*t2
X1 | c(t1_1, t1_1)*t1 + c(t2_1, t2_1)*t2
X3 | c(t1_3, t1_3)*t1 + c(t2_3, t2_3)*t2
X4 | c(t1_4, t1_4)*t1 + c(t2_4, t2_4)*t2
X5 | c(t1_5, t1_5)*t1
X6 | c(t1_6, t1_6)*t1
X8 | c(t1_8, t1_8)*t1 + c(t2_8, t2_8)*t2
X7 | c(t1_7, t1_7)*t1 + c(t2_7, t2_7)*t2
X9 | c(t1_9, t1_9)*t1 + c(t2_9, t2_9)*t2
X10 | c(t1_10, t1_10)*t1 + c(t2_10, t2_10)*t2
X11 | c(t1_11, t1_11)*t1
X1  ~ c(0, NA)*1
X2  ~ c(0, NA)*1
X3  ~ c(0, NA)*1
X4  ~ c(0, NA)*1
X5  ~ c(0, NA)*1
X6  ~ c(0, NA)*1
X7  ~ c(0, NA)*1
X8  ~ c(0, NA)*1
X9  ~ c(0, NA)*1
X10  ~ c(0, NA)*1
X11  ~ c(0, NA)*1
X1 ~~ c(1,NA)*X1
X2 ~~ c(1,NA)*X2
X3 ~~ c(1,NA)*X3
X4 ~~ c(1,NA)*X4
X5 ~~ c(1,1)*X5
X6 ~~ c(1,1)*X6
X7 ~~ c(1,NA)*X7
X8 ~~ c(1,NA)*X8
X9 ~~ c(1,NA)*X9
X10 ~~ c(1,NA)*X10
X11 ~~ c(1,1)*X11
EXT ~~ c(1,NA)*EXT
INT ~~ c(1,NA)*INT
EXT ~~ NA*INT
EXT ~ c(0, 0)*1
INT ~ c(0, 0)*1
"

# WE THRESHOLD+INTERCEPT invariance, delta par (Condition 21)
# thresholds constrained across groups (specified in cfa call)
# intercepts constrained to zero
# lv means free
# lv variances constrained to 1
# unique variances free in group 2 (except binary items)
# intercepts free in group 2
thr.int.we.d <- "
EXT =~ c(NA, NA)*X2 + X1 + X3 + X4 + X5 + X6
INT =~ c(NA, NA)*X8 + X7 + X9 + X10 + X11
X2 | c(t1_2, t1_2)*t1 + c(t2_2, t2_2)*t2
X1 | c(t1_1, t1_1)*t1 + c(t2_1, t2_1)*t2
X3 | c(t1_3, t1_3)*t1 + c(t2_3, t2_3)*t2
X4 | c(t1_4, t1_4)*t1 + c(t2_4, t2_4)*t2
X5 | c(t1_5, t1_5)*t1
X6 | c(t1_6, t1_6)*t1
X8 | c(t1_8, t1_8)*t1 + c(t2_8, t2_8)*t2
X7 | c(t1_7, t1_7)*t1 + c(t2_7, t2_7)*t2
X9 | c(t1_9, t1_9)*t1 + c(t2_9, t2_9)*t2
X10 | c(t1_10, t1_10)*t1 + c(t2_10, t2_10)*t2
X11 | c(t1_11, t1_11)*t1
X1  ~ c(0, 0)*1
X2  ~ c(0, 0)*1
X3  ~ c(0, 0)*1
X4  ~ c(0, 0)*1
X5  ~ c(0, 0)*1
X6  ~ c(0, 0)*1
X7  ~ c(0, 0)*1
X8  ~ c(0, 0)*1
X9  ~ c(0, 0)*1
X10  ~ c(0, 0)*1
X11  ~ c(0, 0)*1
X1 ~*~ c(1,NA)*X1
X2 ~*~ c(1,NA)*X2
X3 ~*~ c(1,NA)*X3
X4 ~*~ c(1,NA)*X4
X5 ~*~ c(1,1)*X5
X6 ~*~ c(1,1)*X6
X7 ~*~ c(1,NA)*X7
X8 ~*~ c(1,NA)*X8
X9 ~*~ c(1,NA)*X9
X10 ~*~ c(1,NA)*X10
X11 ~*~ c(1,1)*X11
EXT ~~ c(1,1)*EXT
INT ~~ c(1,1)*INT
EXT ~~ NA*INT
EXT ~ c(NA, NA)*1
INT ~ c(NA, NA)*1
"


# WE THRESHOLD+LOADINGS+INTERCEPT invariance, theta par (Condition 27)
# thresholds constrained across groups (specified in cfa call)
# loadings constrained across groups
# lv means free in group 2
# lv variances free in group 2
# unique variances free in group 2 (except binary items)
# intercepts constrained to zero for both groups
thr.load.int.we.th <- "
EXT =~ c(NA, NA)*X2 + X1 + X3 + X4 + X5 + X6
INT =~ c(NA, NA)*X8 + X7 + X9 + X10 + X11
X2 | c(t1_2, t1_2)*t1 + c(t2_2, t2_2)*t2
X1 | c(t1_1, t1_1)*t1 + c(t2_1, t2_1)*t2
X3 | c(t1_3, t1_3)*t1 + c(t2_3, t2_3)*t2
X4 | c(t1_4, t1_4)*t1 + c(t2_4, t2_4)*t2
X5 | c(t1_5, t1_5)*t1
X6 | c(t1_6, t1_6)*t1
X8 | c(t1_8, t1_8)*t1 + c(t2_8, t2_8)*t2
X7 | c(t1_7, t1_7)*t1 + c(t2_7, t2_7)*t2
X9 | c(t1_9, t1_9)*t1 + c(t2_9, t2_9)*t2
X10 | c(t1_10, t1_10)*t1 + c(t2_10, t2_10)*t2
X11 | c(t1_11, t1_11)*t1
X1  ~ c(0, 0)*1
X2  ~ c(0, 0)*1
X3  ~ c(0, 0)*1
X4  ~ c(0, 0)*1
X5  ~ c(0, 0)*1
X6  ~ c(0, 0)*1
X7  ~ c(0, 0)*1
X8  ~ c(0, 0)*1
X9  ~ c(0, 0)*1
X10  ~ c(0, 0)*1
X11  ~ c(0, 0)*1
X1 ~~ c(1,NA)*X1
X2 ~~ c(1,NA)*X2
X3 ~~ c(1,NA)*X3
X4 ~~ c(1,NA)*X4
X5 ~~ c(1,1)*X5
X6 ~~ c(1,1)*X6
X7 ~~ c(1,NA)*X7
X8 ~~ c(1,NA)*X8
X9 ~~ c(1,NA)*X9
X10 ~~ c(1,NA)*X10
X11 ~~ c(1,1)*X11
EXT ~~ c(1,NA)*EXT
INT ~~ c(1,NA)*INT
EXT ~~ NA*INT
EXT ~ c(0, NA)*1
INT ~ c(0, NA)*1
"

# equivalent models (MT does not converge!)
# fa.config.mt.th <- cfa(config.mt.th, data = items.c, group = "cohort", estimator="wlsmv", parameterization="theta")
# printfit(fa.config.mt.th)
# fa.config.mt.d<- cfa(config.mt.d, data = items.c, group = "cohort", estimator="wlsmv", parameterization="delta")
# printfit(fa.config.mt.d)

fa.config.we.th <- cfa(config.we.th, data = items.c, group = "cohort", estimator="wlsmv", parameterization="theta")
printfit(fa.config.we.th)
fa.config.we.d <- cfa(config.we.d, data = items.c, group = "cohort", estimator="wlsmv", parameterization="delta")
printfit(fa.config.we.d)
fa.thr.we.th <- cfa(thr.we.th, data = items.c, group = "cohort", estimator="wlsmv", parameterization="theta")
printfit(fa.thr.we.th)

# first restrictive model: THRESHOLDS + LOADINGS
fa.thr.load.we.th <- cfa(thr.load.we.th, data = items.c, group = "cohort", estimator="wlsmv", parameterization="theta", group.equal='loadings')
printfit(fa.thr.load.we.th)

# # THRESHOLDS + INTERCEPTS (does not converge)
# fa.thr.int.we.d <- cfa(thr.int.we.d, data = items.c, group = "cohort", estimator="wlsmv", parameterization="delta")
# printfit(fa.thr.int.we.d)

# second restrictive model: THRESHOLDS + LOADINGS + INTERCEPTS
fa.thr.load.int.we.th <- cfa(thr.load.int.we.th, data = items.c, group = "cohort", estimator="wlsmv", parameterization="theta", group.equal='loadings')
printfit(fa.thr.load.int.we.th)


## ---- FA_FIT
# assemble AFIs indices for table
indsel <- c("npar", "chisq", "rmsea", "mfi", "cfi.scaled")

# start from configural and full scalar
afitab <- data.frame(rbind(
  round(c( fitMeasures(fa.config.we.d, indsel), moreFitIndices(fa.config.we.d)["gammaHat"] ),5),
  round(c( fitMeasures(fa.thr.we.th, indsel), moreFitIndices(fa.thr.we.th)["gammaHat"] ),5),
  round(c( fitMeasures(fa.thr.load.we.th, indsel), moreFitIndices(fa.thr.load.we.th)["gammaHat"] ),5),
  # round(c( fitMeasures(fa.thr.int.we.th, indsel), moreFitIndices(fa.thr.int.we.th)["gammaHat"] ),5),
  round(c( fitMeasures(fa.thr.load.int.we.th, indsel), moreFitIndices(fa.thr.load.int.we.th)["gammaHat"] ),5)
))
mnames <- c("Configural", "Threshold Invariance", "Threshold + Loading Invariance", "Threshold, Loading, + Intercept Invariance")

afitab$npar  <- as.character(afitab$npar)
afitab$chisq <- as.character(round(afitab$chisq,1))

# add deltas
dmfi <- NA
dcfi <- NA
dgam <- NA
for (r in 2:nrow(afitab)) {
  dmfi <- c(dmfi, afitab[r,"mfi"] -  afitab[1,"mfi"])
  dcfi <- c(dcfi, afitab[r,"cfi.scaled"] -  afitab[1,"cfi.scaled"])
  dgam <- c(dgam, afitab[r,"gammaHat"] -  afitab[1,"gammaHat"])
}
afitab$dmfi <- as.matrix(dmfi)
afitab$dcfi <- as.matrix(dcfi)
afitab$dgam <- as.matrix(dgam)

# add names
afitab <- data.frame(cbind(mnames = as.matrix(mnames), afitab))



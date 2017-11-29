# ############################################################################################
## ---- FA_FINAL
# select final model

# select final model
finalmod <- fa.thr.load.we.th
pta <- partable(finalmod)

##################################################################################
## ---- FA_FINAL_PARS
# for model with constrained loadings and thresholds

# item parameters ----------------------------------------------------------
# extract loadings
BCS.lambda = pta[(pta$op == "=~" & pta$group == 1),c("lhs", "rhs","est")]
MCS.lambda = pta[(pta$op == "=~" & pta$group == 2),c("lhs", "rhs","est")]
lambdas <- BCS.lambda
lambdas$measure <- lambdas$rhs   # column for merging
lambdas <- lambdas[ , -which(names(lambdas) %in% c("rhs","lhs"))] # drop useless columns

# extract tau1
BCS.tau1 = pta[(pta$op == "|" & pta$rhs == "t1" & pta$group == 1),c("lhs", "rhs","est")]
MCS.tau1 = pta[(pta$op == "|" & pta$rhs == "t1" & pta$group == 2),c("lhs", "rhs","est")]
tau1 <- BCS.tau1
tau1$measure <- tau1$lhs
tau1 <- tau1[ , -which(names(tau1) %in% c("rhs","lhs"))] # drop useless columns

# extract tau2
BCS.tau2 = pta[(pta$op == "|" & pta$rhs == "t2" & pta$group == 1),c("lhs", "rhs","est")]
MCS.tau2 = pta[(pta$op == "|" & pta$rhs == "t2" & pta$group == 2),c("lhs", "rhs","est")]
tau2 <- BCS.tau2
tau2$measure <- tau2$lhs
tau2 <- tau2[ , -which(names(tau2) %in% c("rhs","lhs"))] # drop useless columns

# extract intercepts (group 2 only)
MCS.nu = pta[(pta$op == "~1" & pta$rhs == "" & pta$group == 2),c("lhs", "rhs","est")]
nu <- MCS.nu
nu$measure <- nu$lhs
nu <- nu[ , -which(names(nu) %in% c("rhs","lhs"))] # drop useless columns


# merge parameters together
allpar <- merge(lambdas,tau1,by="measure", all.x = T)
allpar <- merge(allpar,tau2,by="measure", all.x = T)
allpar <- merge(allpar,nu,by="measure", all.x = T)
allpar <- allpar[mixedorder(allpar$measure),]

# name variables
colnames(allpar) <- c("measure", 
                      "$\\lambda_{BCS} = \\lambda_{MCS}$",
                      "$\\tau_{1,BCS} = \\tau_{1_MCS}$",
                      "$\\tau_{2,BCS} = \\tau_{2,MCS}$",
                      "$\\nu_{MCS}$ ($\\nu_{BCS}=0$)"
)

# add latent factors
allpar <- cbind(allpar, 
                factor = as.matrix(c(rep("EXT",6), rep("INT",5)))
)

# latent variable parameters ----------------------------------------------------------

# model with threshold and loading invariance
lv.means <- inspect(finalmod, what="mean.lv") # means
lv.covs <- inspect(finalmod, what="cov.lv")   # covariances
for (i in 1:2) upperTriangle(lv.covs[[i]]) <- NA

lvpars <- data.frame(rbind(
  cbind(as.matrix(lv.means[[1]]), lv.covs[[1]]),
  cbind(as.matrix(lv.means[[2]]), lv.covs[[2]])
))

lvpars <- cbind( Measure = as.matrix( 
  c("$\\theta^{EXT}$", "$\\theta^{INT}$",
    "$\\theta^{EXT}$", "$\\theta^{INT}$")),
  lvpars)

# model with threshold, loading and intercept invariance
lv.means <- inspect(fa.thr.load.int.we.th, what="mean.lv") # means
lv.covs <- inspect(fa.thr.load.int.we.th, what="cov.lv")   # covariances
for (i in 1:2) upperTriangle(lv.covs[[i]]) <- NA

lvpars2 <- data.frame(rbind(
  cbind(as.matrix(lv.means[[1]]), lv.covs[[1]]),
  cbind(as.matrix(lv.means[[2]]), lv.covs[[2]])
))

lvpars2 <- cbind( Measure = as.matrix( 
  c("$\\theta^{EXT}$", "$\\theta^{INT}$",
    "$\\theta^{EXT}$", "$\\theta^{INT}$")),
  lvpars2)



## ---- CLEANUP
rm(BCS.lambda, BCS.tau1, BCS.tau2, MCS.lambda, MCS.tau1, MCS.tau2, 
   lambdas, means, pta, tau1, tau2, 
   Xtemp.bcs, Xtemp.mcs, bcs_rutb419, bcs_rutbAB, bcs_rutc419,
   cohort, dmfi, dcfi, dgam, indsel, mcskeepb, mnames, ncats, ncomm, nn,
   t, t2)




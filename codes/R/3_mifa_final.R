# ############################################################################################
## ---- FA_FINAL

# select final model
finalmod <- fa.thr.load.int.we.th

##################################################################################
## ---- FA_FINAL_PARS
# extract parameters and make table for final model estimates
pta <- partable(finalmod)

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

# extract variances (group 2 only)
MCS.eps = pta[(pta$op == "~~" & substr(pta$rhs,1,1) == "X" & pta$group == 2),c("lhs", "rhs","est")]
eps <- MCS.eps
eps$measure <- eps$lhs
eps <- eps[ , -which(names(eps) %in% c("rhs","lhs"))] # drop useless columns


# merge parameters together
allpar <- merge(lambdas,tau1,by="measure", all.x = T)
allpar <- merge(allpar,tau2,by="measure", all.x = T)
allpar <- merge(allpar,eps,by="measure", all.x = T)
allpar <- allpar[mixedorder(allpar$measure),]

# name variables
colnames(allpar) <- c("measure", 
                      "$\\lambda_{BCS} = \\lambda_{MCS}$",
                      "$\\tau_{1,BCS} = \\tau_{1_MCS}$",
                      "$\\tau_{2,BCS} = \\tau_{2,MCS}$",
                      "$\\varepsilon_{MCS}$ ($\\varepsilon_{BCS}=1$)"
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
lv.corrs <- lapply(lv.covs,cov2cor)


lvpars <- data.frame(
  cbind(as.matrix(lv.means[[1]]), lv.covs[[1]], as.matrix(c(NA,lv.corrs[[1]][2,1])), as.matrix(lv.means[[2]]), lv.covs[[2]], as.matrix(c(NA,lv.corrs[[2]][2,1])))
)

lvpars <- cbind( Measure = as.matrix( 
  c("$\\theta^{EXT}$", "$\\theta^{INT}$")),
  lvpars)


## ---- CLEANUP
rm(BCS.lambda, BCS.tau1, BCS.tau2, MCS.lambda, MCS.tau1, MCS.tau2, MCS.eps,
   lambdas, means, pta, tau1, tau2, eps,
   Xtemp.bcs, Xtemp.mcs, bcs5_rutb419, bcs5_rutbAB, bcs5_rutc419,
   cohort, dmfi, dcfi, dgam, indsel, mcskeepb, mnames, ncats, ncomm, nn,
   t, t2)




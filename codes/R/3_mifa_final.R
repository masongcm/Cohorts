# ############################################################################################
## ---- FA_FINAL

# select final model
finalmod <- list(fa.tli.a, fa.tli.m, fa.tli.f, fa.tli.b, fa.tli.ol)

##################################################################################
## ---- FA_FINAL_PARS
# extract parameters and make table for final model estimates

allpar <- list()
for (i in 1:5) {
  pta <- partable(finalmod[[i]])

  # item parameters ----------------------------------------------------------
  # extract loadings
  lambdas <- pta[(pta$op == "=~" & pta$group == 1),c("lhs", "rhs","est")]
  lambdas$measure <- lambdas$rhs   # column for merging
  lambdas <- lambdas[ , -which(names(lambdas) %in% c("rhs","lhs"))] # drop useless columns
  
  # extract tau1
  tau1 <- pta[(pta$op == "|" & pta$rhs == "t1" & pta$group == 1),c("lhs", "rhs","est")]
  tau1$measure <- tau1$lhs
  tau1 <- tau1[ , -which(names(tau1) %in% c("rhs","lhs"))] # drop useless columns
  
  # extract tau2
  tau2 <- pta[(pta$op == "|" & pta$rhs == "t2" & pta$group == 1),c("lhs", "rhs","est")]
  tau2$measure <- tau2$lhs
  tau2 <- tau2[ , -which(names(tau2) %in% c("rhs","lhs"))] # drop useless columns
  
  # extract variances (group 2 only)
  eps <- pta[(pta$op == "~~" & substr(pta$rhs,1,1) == "X" & pta$group == 2),c("lhs", "rhs","est")]
  eps$measure <- eps$lhs
  eps <- eps[ , -which(names(eps) %in% c("rhs","lhs"))] # drop useless columns
  
  # merge parameters together
  allpar[[i]] <- merge(lambdas,tau1,by="measure", all.x = T)
  allpar[[i]] <- merge(allpar[[i]],tau2,by="measure", all.x = T)
  allpar[[i]] <- merge(allpar[[i]],eps,by="measure", all.x = T)
  allpar[[i]] <- allpar[[i]][mixedorder(allpar[[i]]$measure),]
  
  # name variables
  colnames(allpar[[i]]) <- c("measure", 
                        "lambda",
                        "tau1",
                        "tau2$",
                        "eps"
  )
  
  # add latent factors
  allpar[[i]]$factor <- as.matrix(c(rep("EXT",6), rep("INT",5)))
}

# additional variances for 4-group models
for (i in 4:5) {
  pta <- partable(finalmod[[i]])
  eps3 <- pta[(pta$op == "~~" & substr(pta$rhs,1,1) == "X" & pta$group == 3),c("lhs", "rhs","est")]
  eps3$measure <- eps3$lhs
  eps3 <- eps3[ , -which(names(eps3) %in% c("rhs","lhs"))] # drop useless columns
  eps4 <- pta[(pta$op == "~~" & substr(pta$rhs,1,1) == "X" & pta$group == 4),c("lhs", "rhs","est")]
  eps4$measure <- eps4$lhs
  eps4 <- eps4[ , -which(names(eps4) %in% c("rhs","lhs"))] # drop useless columns
  allpar[[i]] <- merge(allpar[[i]],eps3,by="measure", all.x = T)
  allpar[[i]] <- merge(allpar[[i]],eps4,by="measure", all.x = T)
  colnames(allpar[[i]]) <- c("measure", "lambda", "tau1", "tau2$", "eps2", "factor", "eps3", "eps4")
  allpar[[i]] <- allpar[[i]][, c(setdiff(names(allpar[[i]]), "factor"), "factor")]
  allpar[[i]] <- allpar[[i]][mixedorder(allpar[[i]]$measure),]
}


# latent variable parameters ----------------------------------------------------------

lvpars <- list()
for (i in 1:3) {
  # model with threshold and loading invariance
  lv.means <- inspect(finalmod[[i]], what="mean.lv") # means
  lv.covs <- inspect(finalmod[[i]], what="cov.lv")   # covariances
  for (j in 1:2) upperTriangle(lv.covs[[j]]) <- NA
  lv.corrs <- lapply(lv.covs,cov2cor)
  
  lvpars[[i]] <- data.frame(
    cbind(as.matrix(lv.means[[1]]), lv.covs[[1]], as.matrix(c(NA,lv.corrs[[1]][2,1])), as.matrix(lv.means[[2]]), lv.covs[[2]], as.matrix(c(NA,lv.corrs[[2]][2,1])))
  )
  
}


# 4-GROUP MODEL
lv.means4 <- inspect(finalmod[[4]], what="mean.lv")
lv.covs4 <- inspect(finalmod[[4]], what="cov.lv")
for (j in 1:2) upperTriangle(lv.covs4[[j]]) <- NA
lv.corrs4 <- lapply(lv.covs4,cov2cor)

# females
lvpars[[5]] <- data.frame(cbind(
  as.matrix(lv.means4$BCS.F),
  lv.covs4$BCS.F,
  as.matrix(c(NA,lv.corrs4$BCS.F[2,1])),
  as.matrix(lv.means4$MCS.F),
  lv.covs4$MCS.F,
  as.matrix(c(NA,lv.corrs4$MCS.F[2,1]))
))
# males
lvpars[[4]] <- data.frame(cbind(
  as.matrix(lv.means4$BCS.M),
  lv.covs4$BCS.M,
  as.matrix(c(NA,lv.corrs4$BCS.M[2,1])),
  as.matrix(lv.means4$MCS.M),
  lv.covs4$MCS.M,
  as.matrix(c(NA,lv.corrs4$MCS.M[2,1]))
))


for (i in 1:5) {
  lvpars[[i]] <- cbind(as.matrix(c("$\\theta^{EXT}$", "$\\theta^{INT}$")),lvpars[[i]])
  names(lvpars[[i]]) <- c("measure", 
                          "mean_BCS", "covext_BCS", "covint_BCS", "corr_BCS", 
                          "mean_MCS", "covext_MCS", "covint_MCS", "corr_MCS")
}
  
  
## ---- CLEANUP
rm(
   lambdas, means, pta, tau1, tau2, eps,
   Xtemp.bcs, Xtemp.mcs, bcs5_rutb419, bcs5_rutbAB, bcs5_rutc419,
   dmfi, dcfi, dgam, indsel, mcskeepb, mnames, ncats, ncomm, nn,
   t, t2)




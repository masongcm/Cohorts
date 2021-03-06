##---- AUXFUN
# auxiliary functions


########################################################################
# GETMEASPARS
# Function to extract measurement parameters from lavaan parameter table

getmeaspars <- function(fit, groups = 2, mode = "tli") {
  
  if (groups!=2 & groups!=4) {
    stop("Number of groups is 2 or 4")
  }
  
  if (!mode %in% c("tl", "tli")) {
    stop("\"mode\" is either \"tl\" or \"tli\" ")
  }
  
  pta <- partable(fit)
  
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
  
  # extract intercepts (group 2 only)
  nus <- pta[(pta$op == "~1" & substr(pta$lhs,1,1) == "X" & pta$group == 2),c("lhs", "rhs","est")]
  nus$measure <- nus$lhs   # column for merging
  nus <- nus[ , -which(names(nus) %in% c("rhs","lhs"))] # drop useless columns
  
  # merge parameters together
  allparout <- merge(lambdas,tau1,by="measure", all.x = T)
  allparout <- merge(allparout,tau2,by="measure", all.x = T)
  
  # ADD INTERCEPTS
  if (mode == "tl") {
    # intercepts for 2-group models
    if (groups == 2) allparout <- merge(allparout,nus,by="measure", all.x = T)
    # intercepts for 4-group models
    if (groups == 4) {
      for (i in 2:4) {
        addnu <- pta[(pta$op == "~1" & substr(pta$lhs,1,1) == "X" & pta$group == i),c("lhs", "rhs","est")]
        addnu$measure <- addnu$lhs
        addnu <- addnu[ , -which(names(addnu) %in% c("rhs","lhs"))] # drop useless columns
        allparout <- merge(allparout,addnu,by="measure", all.x = T)
      }
    }
  }
  
  # ADD VARIANCES
  if (groups == 2) allparout <- merge(allparout,eps,by="measure", all.x = T)
  # additional variances for 4-group models
  if (groups == 4) { 
    for (i in 2:4) {
      addeps <- pta[(pta$op == "~~" & substr(pta$rhs,1,1) == "X" & pta$group == i),c("lhs", "rhs","est")]
      addeps$measure <- addeps$lhs
      addeps <- addeps[ , -which(names(addeps) %in% c("rhs","lhs"))] # drop useless columns
      allparout <- merge(allparout,addeps,by="measure", all.x = T)
    }
  }
  
  # reorder
  allparout <- allparout[mixedorder(allparout$measure),]
  
  
  # name variables
  if (mode=="tl" & groups==2) colnames(allparout) <- c("measure", "lambda", "tau1", "tau2", "nu", "eps")
  if (mode=="tli" & groups==2) colnames(allparout) <- c("measure", "lambda", "tau1", "tau2", "eps")
  if (mode=="tl" & groups==4) colnames(allparout) <- c("measure", "lambda", "tau1", "tau2", "nu2", "nu3", "nu4", "eps2", "eps3", "eps4")
  if (mode=="tli" & groups==4) colnames(allparout) <- c("measure", "lambda", "tau1", "tau2", "eps2", "eps3", "eps4")
  
  # add latent factor and reorder
  allparout$factor <- as.matrix(c(rep("EXT",6), rep("INT",5)))
  allparout <- cbind(allparout[,c("measure", "factor")], allparout[,!names(allparout) %in% c("measure", "factor")])
  allparout[,"measure"] <- str_remove(allparout[,"measure"], "X") # remove X
  return(allparout)
}



########################################################################
# GETLVPARS
# Function to extract measurement parameters from lavaan parameter table

getlvpars <- function(fit, groups = 2) {
  
  if (groups!=2 & groups!=4) {
    stop("Number of groups is 2 or 4")
  }
  
  lv.means <- inspect(fit, what="mean.lv") # means
  lv.covs <- inspect(fit, what="cov.lv")   # covariances
  for (j in 1:groups) gdata::upperTriangle(lv.covs[[j]]) <- NA
  lv.corrs <- lapply(lv.covs,cov2cor)
  
  if (groups == 2) {
    lvparsout <- data.frame(
      cbind(as.matrix(lv.means[[1]]), lv.covs[[1]], as.matrix(c(NA,lv.corrs[[1]][2,1])), as.matrix(lv.means[[2]]), lv.covs[[2]], as.matrix(c(NA,lv.corrs[[2]][2,1])))
    )
    
    lvparsout <- cbind(as.matrix(c("$\\theta^{EXT}$", "$\\theta^{INT}$")),lvparsout)
    names(lvparsout) <- c("measure", 
                          "mean_BCS", "covext_BCS", "covint_BCS", "corr_BCS", 
                          "mean_MCS", "covext_MCS", "covint_MCS", "corr_MCS")
  }
  
  # 4-GROUP MODELS
  if (groups == 4) {
    lvparsout <- list()
    # males
    lvparsout$Males <- data.frame(cbind(
      as.matrix(lv.means$BCS.M),
      lv.covs$BCS.M,
      as.matrix(c(NA,lv.corrs$BCS.M[2,1])),
      as.matrix(lv.means$MCS.M),
      lv.covs$MCS.M,
      as.matrix(c(NA,lv.corrs$MCS.M[2,1]))
    ))
    # females
    lvparsout$Females <- data.frame(cbind(
      as.matrix(lv.means$BCS.F),
      lv.covs$BCS.F,
      as.matrix(c(NA,lv.corrs$BCS.F[2,1])),
      as.matrix(lv.means$MCS.F),
      lv.covs$MCS.F,
      as.matrix(c(NA,lv.corrs$MCS.F[2,1]))
    ))
    
    for (j in 1:2) {
      lvparsout[[j]] <- cbind(as.matrix(c("$\\theta^{EXT}$", "$\\theta^{INT}$")),lvparsout[[j]])
      names(lvparsout[[j]]) <- c("measure", 
                                 "mean_BCS", "covext_BCS", "covint_BCS", "corr_BCS", 
                                 "mean_MCS", "covext_MCS", "covint_MCS", "corr_MCS")
    }
  }
  return(lvparsout)
}


########################################################################
# printing for outcomes tables

# generic prn function
prn <- function(obj, dig=3) {
  if (!is.na(obj)) sprintf(paste0("%.",dig,"f"), round(obj, digits = dig)) 
  else ""
}
prn <- Vectorize(prn, vectorize.args = "obj")

# function to print coefficients in correct format
prcoef <- function(x) {
  if (abs(x)<10) out <- sub("^(-?)0.", "\\1.", sprintf("%.3f", x))
  else out <- sprintf("%.1f", x)
  return(out)
}
# function to put stars
stars <- function(t) {
  if (abs(t) > 2.58) return("^{***}")
  else if (abs(t) <= 2.58 & abs(t) > 1.96) return("^{**}")
  else if (abs(t) <= 1.96 & abs(t) > 1.64) return("^{*}")
  else return("")
}

# function to extract estimate, put stars and robust SE
cellpr2 <- function(mode, vr) {
  # mod: estimates from ML model
  # vrs: variable to extract
  coefs <- lmtest::coeftest(mod, vcov = sandwich::vcovHC(mod, type = "HC1"))
  coefs2 <- coefs[vr,]
  return(
    paste0("$", prcoef(round(coefs2[1],3)), # estimate
           stars(coefs2[3]), "$", # stars
           " \\newline ($", prcoef(round(coefs2[2],3)), "$)") # SE (on new line)
  ) 
}

# function to extract estimate, put stars and (bootstrap) SE
cellpr <- function(estimtab, vr) {
  # estimtab: table with estimate, SE, tstat
  # vrs: variable to extract
  coefs2 <- estimtab[vr,]
  return(
    paste0("$", prcoef(round(coefs2[1],3)), # estimate
           stars(coefs2[3]), "$", # stars
           " \\newline ($", prcoef(round(coefs2[2],3)), "$)")
  ) # SE (on new line)
}

# print R2 (with padding)
prr2 <- function(mod) {
  return(paste0("\\newline ", sprintf("%.3f", summary(mod)$adj.r.squared)))
}

# function to print pvals as strings in square brackets
prnpval <- function(obj, dig=3) {
  if (!is.na(obj)) paste0("[", sprintf(paste0("%.",dig,"f"), round(obj, digits = dig)), "]")
  else ""
}
prnpval <- Vectorize(prnpval, vectorize.args = "obj")

# function to collapse regressors in formula
pplus <- function(x) paste0(x, collapse="+")

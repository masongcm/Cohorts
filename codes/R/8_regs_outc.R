
###################################################################
## ---- REGS_OUTC
# REGRESSIONS OF ADOLESCENT OUTCOMES ON EXT/INT & CHILDHOOD VARIABLES

nboot <- length(bootscores)

# estimate BCS models
bcsoutcmod <- list()
bcsoutcsum <- list()
for (o in 1:length(bcsoutcvars)) { # length(bcsoutcvars)
  outc <- bcsoutcvars[o]
  
  # formulas
  form_outc <- list()
  form_outc$b    <- as.formula(paste0(outc, " ~ EXT + INT + age16 + region +", pplus(decvars)))
  form_outc$bc   <- as.formula(paste0(outc, " ~ EXT + INT + age16 + region + cogscore +", pplus(decvars)))
  form_outc$bcr  <- as.formula(paste0(outc, " ~ EXT_RAWs + INT_RAWs + age16 + region + cogscore +", pplus(decvars)))
  
  for (s in c("M", "F")) {
    # bootstrap SEs
    cat("\nInference for", bcsoutcvars[o], "BCS", s, "\n")
    bootcoefs <- list(b = NULL, bc = NULL, bcr = NULL)
    # bootstrap
    for (b in 1:nboot) {
      cat("\r Bootstrap sample", b, "of", nboot)
      # fetch scores from bootstrap samples
      bsamp <- bcsoutc %>%
        select(-EXT, -INT) %>%
        right_join(bootscores[[b]], by = "id")
      # compute coefficients
      for (t in c("b", "bc")) bootcoefs[[t]] <- cbind(bootcoefs[[t]], as.matrix(lm(form_outc[[t]], data = subset(bsamp, sex==s))$coefficients))
    }
    # assemble estimates
    for (t in c("b", "bc")) {
      # compute main models
      bcsoutcmod[[s]][[outc]][[t]]   <- lm(form_outc[[t]], data = subset(bcsoutc, sex==s))
      bcsoutcsum[[s]][[outc]][[t]] <- summary(bcsoutcmod[[s]][[outc]][[t]])$coefficients[,c(1,2)]
      # substitute SEs in summary of models
      bootse <- apply(bootcoefs[[t]], MARGIN = 1, FUN = sd, na.rm=TRUE)
      bcsoutcsum[[s]][[outc]][[t]][,2] <- bootse[!is.na(bootse)]
      bcsoutcsum[[s]][[outc]][[t]] <- cbind(bcsoutcsum[[s]][[outc]][[t]], bcsoutcsum[[s]][[outc]][[t]][,1]/bcsoutcsum[[s]][[outc]][[t]][,2])
      colnames(bcsoutcsum[[s]][[outc]][[t]]) <- c("Estimate","SE","tstat")
    }
    # this does not require bootstrapping
    bcsoutcmod[[s]][[outc]][["bcr"]]   <- lm(form_outc[["bcr"]], data = subset(bcsoutc, sex==s))
    bcsoutcsum[[s]][[outc]][["bcr"]]   <- summary(bcsoutcmod[[s]][[outc]][["bcr"]])$coefficients[,c(1,2,3)]
  }
}

# estimate MCS models
mcsoutcmod <- list()
mcsoutcsum <- list()
for (o in 1:length(mcsoutcvars)) { # length(bcsoutcvars)
  outc <- mcsoutcvars[o]
  
  # formulas
  form_outc <- list()
  form_outc$b    <- as.formula(paste0(outc," ~ EXT + INT + age14 + region +", pplus(decvars)))
  form_outc$bc   <- as.formula(paste0(mcsoutcvars[o]," ~ EXT + INT + age14 + region + cogscore +", pplus(decvars)))
  form_outc$bcr  <- as.formula(paste0(mcsoutcvars[o]," ~ EXT_RAWs + INT_RAWs + age14 + region + cogscore +", pplus(decvars)))
  bootcoefs <- list()
  
  for (s in c("M", "F")) {
    # bootstrap SEs
    cat("\nInference for", mcsoutcvars[o], "MCS", s, "\n")
    bootcoefs <- list(b = NULL, bc = NULL, bcr = NULL)
    # bootstrap
    for (b in 1:nboot) {
      cat("\r Bootstrap sample", b, "of", nboot)
      # fetch scores from bootstrap samples
      bsamp <- mcsoutc %>%
        select(-EXT, -INT) %>%
        right_join(bootscores[[b]], by = "id")
      # compute coefficients
      for (t in c("b", "bc")) bootcoefs[[t]] <- cbind(bootcoefs[[t]], as.matrix(lm(form_outc[[t]], data = subset(bsamp, sex==s))$coefficients))
    }
    # assemble estimates
    for (t in c("b", "bc")) {
      # compute main models
      mcsoutcmod[[s]][[outc]][[t]]   <- lm(form_outc[[t]], data = subset(mcsoutc, sex==s))
      mcsoutcsum[[s]][[outc]][[t]]   <- summary(mcsoutcmod[[s]][[outc]][[t]])$coefficients[,c(1,2)]
      # substitute SEs in summary of models
      bootse <- apply(bootcoefs[[t]], MARGIN = 1, FUN = sd, na.rm=TRUE)
      mcsoutcsum[[s]][[outc]][[t]][,2] <- bootse[!is.na(bootse)]
      mcsoutcsum[[s]][[outc]][[t]]   <- cbind(mcsoutcsum[[s]][[outc]][[t]], mcsoutcsum[[s]][[outc]][[t]][,1]/mcsoutcsum[[s]][[outc]][[t]][,2])
      colnames(mcsoutcsum[[s]][[outc]][[t]]) <- c("Estimate","SE","tstat")
    }
    # this does not require bootstrapping
    mcsoutcmod[[s]][[outc]][["bcr"]]   <- lm(form_outc[["bcr"]], data = subset(mcsoutc, sex==s))
    mcsoutcsum[[s]][[outc]][["bcr"]]   <- summary(mcsoutcmod[[s]][[outc]][["bcr"]])$coefficients[,c(1,2,3)]
  }
}



###################################################################
## ---- REGS_OUTC_TAB
# TABLES from outcome regressions

# BCS
# mean outcomes
dvmeans_bcs_m <- sapply(colMeans(bcsoutc[bcsoutc$sex=="M",bcsoutcvars], na.rm = T), prcoef)
dvmeans_bcs_f <- sapply(colMeans(bcsoutc[bcsoutc$sex=="F",bcsoutcvars], na.rm = T), prcoef)

bcs_outctab <- list()
for (i in 1:length(bcsoutcvars)) {
  lab <- paste0("\\textit{\\textbf{", bcsoutclabs[i], "}}")
  bcs_outctab[[bcsoutcvars[i]]] <- matrix(c(lab, dvmeans_bcs_m[i], "", "", "", dvmeans_bcs_f[i], "", "", "",   # outcome and means
                                            "Externalising skills (5)", # row label
                                            "", cellpr(bcsoutcsum$M[[i]]$b, "EXT"), cellpr(bcsoutcsum$M[[i]]$bc, "EXT"), "", # males EXT
                                            "", cellpr(bcsoutcsum$F[[i]]$b, "EXT"), cellpr(bcsoutcsum$M[[i]]$bc, "EXT"), "", # females EXT
                                            "Internalising skills (5)", # row label
                                            "", cellpr(bcsoutcsum$M[[i]]$b, "INT"), cellpr(bcsoutcsum$M[[i]]$bc, "INT"), "", # males EXT
                                            "", cellpr(bcsoutcsum$F[[i]]$b, "INT"), cellpr(bcsoutcsum$F[[i]]$bc, "INT"), "", # females EXT
                                            "Externalising (sum score)", # row label
                                            "", "", "", cellpr(bcsoutcsum$M[[i]]$bcr, "EXT_RAWs"), # males EXT
                                            "", "", "", cellpr(bcsoutcsum$F[[i]]$bcr, "EXT_RAWs"), # females EXT
                                            "Internalising (sum score)", # row label
                                            "", "", "", cellpr(bcsoutcsum$M[[i]]$bcr, "INT_RAWs"), # males EXT
                                            "", "", "", cellpr(bcsoutcsum$F[[i]]$bcr, "INT_RAWs"), # females EXT
                                            "Cognitive skills (5)", # row label
                                            "", "", cellpr(bcsoutcsum$M[[i]]$bc, "cogscore"), cellpr(bcsoutcsum$M[[i]]$bcr, "cogscore"), # males COG
                                            "", "", cellpr(bcsoutcsum$F[[i]]$bc, "cogscore"), cellpr(bcsoutcsum$F[[i]]$bcr, "cogscore"), # females COG
                                            "\\newline Adj. R$^2$", # row label
                                            "", prr2(bcsoutcmod$M[[i]]$b), prr2(bcsoutcmod$M[[i]]$bc), prr2(bcsoutcmod$M[[i]]$bcr),
                                            "", prr2(bcsoutcmod$F[[i]]$b), prr2(bcsoutcmod$F[[i]]$bc), prr2(bcsoutcmod$F[[i]]$bcr),
                                            "Observations", # row label
                                            "", nobs(bcsoutcmod$M[[i]]$b), nobs(bcsoutcmod$M[[i]]$bc), nobs(bcsoutcmod$M[[i]]$bcr),
                                            "", nobs(bcsoutcmod$F[[i]]$b), nobs(bcsoutcmod$F[[i]]$bc), nobs(bcsoutcmod$F[[i]]$bcr)
                                            
  ), nrow=8, byrow=TRUE
  )
}

# MCS
# mean outcomes
dvmeans_mcs_m <- sapply(colMeans(mcsoutc[mcsoutc$sex=="M",mcsoutcvars], na.rm = T), prcoef)
dvmeans_mcs_f <- sapply(colMeans(mcsoutc[mcsoutc$sex=="F",mcsoutcvars], na.rm = T), prcoef)

mcs_outctab <- list()
for (i in 1:length(mcsoutcvars)) {
  lab <- paste0("\\textit{\\textbf{", mcsoutclabs[i], "}}")
  mcs_outctab[[mcsoutcvars[i]]] <- matrix(c(lab, dvmeans_mcs_m[i], "", "", "", dvmeans_mcs_f[i], "", "", "",   # outcome and means
                                            "Externalising skills (5)", # row label
                                            "", cellpr(mcsoutcsum$M[[i]]$b, "EXT"), cellpr(mcsoutcsum$M[[i]]$bc, "EXT"), "", # males EXT
                                            "", cellpr(mcsoutcsum$F[[i]]$b, "EXT"), cellpr(mcsoutcsum$M[[i]]$bc, "EXT"), "", # females EXT
                                            "Internalising skills (5)", # row label
                                            "", cellpr(mcsoutcsum$M[[i]]$b, "INT"), cellpr(mcsoutcsum$M[[i]]$bc, "INT"), "", # males EXT
                                            "", cellpr(mcsoutcsum$F[[i]]$b, "INT"), cellpr(mcsoutcsum$F[[i]]$bc, "INT"), "", # females EXT
                                            "Externalising (sum score)", # row label
                                            "", "", "", cellpr(mcsoutcsum$M[[i]]$bcr, "EXT_RAWs"), # males EXT
                                            "", "", "", cellpr(mcsoutcsum$F[[i]]$bcr, "EXT_RAWs"), # females EXT
                                            "Internalising (sum score)", # row label
                                            "", "", "", cellpr(mcsoutcsum$M[[i]]$bcr, "INT_RAWs"), # males EXT
                                            "", "", "", cellpr(mcsoutcsum$F[[i]]$bcr, "INT_RAWs"), # females EXT
                                            "Cognitive skills (5)", # row label
                                            "", "", cellpr(mcsoutcsum$M[[i]]$bc, "cogscore"), cellpr(mcsoutcsum$M[[i]]$bcr, "cogscore"), # males COG
                                            "", "", cellpr(mcsoutcsum$F[[i]]$bc, "cogscore"), cellpr(mcsoutcsum$F[[i]]$bcr, "cogscore"), # females COG
                                            "\\newline Adj. R$^2$", # row label
                                            "", prr2(mcsoutcmod$M[[i]]$b), prr2(mcsoutcmod$M[[i]]$bc), prr2(mcsoutcmod$M[[i]]$bcr),
                                            "", prr2(mcsoutcmod$F[[i]]$b), prr2(mcsoutcmod$F[[i]]$bc), prr2(mcsoutcmod$F[[i]]$bcr),
                                            "Observations", # row label
                                            "", nobs(mcsoutcmod$M[[i]]$b), nobs(mcsoutcmod$M[[i]]$bc), nobs(mcsoutcmod$M[[i]]$bcr),
                                            "", nobs(mcsoutcmod$F[[i]]$b), nobs(mcsoutcmod$F[[i]]$bc), nobs(mcsoutcmod$F[[i]]$bcr)
                                            
  ), nrow=8, byrow=TRUE
  )
}


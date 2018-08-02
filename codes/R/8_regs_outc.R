
###################################################################
## ---- LOAD_DATA_OUTC
# LOAD OUTCOMES DATA

# assemble outcomes data (BCS)
bcs16outc <- read.dta(paste0(dir_data, "bcsoutc.dta"), convert.factors = F) # all BCS data
bcs16outc$id <- bcs16outc$bcsid
bcs16outc$age16 <- factor(bcs16outc$age16)
bcsoutc <- merge(regdata[regdata$cohort=="BCS",], bcs16outc, by = "id", all.x = TRUE)
bcsoutc$cogscore <- factor.scores(bcsoutc[,c("C1", "C2", "C3")], fa(bcsoutc[,c("C1", "C2", "C3")], factors=1), method = "Bartlett")$scores

# assemble outcomes data (MCS)
mcs14outc <- read.dta(paste0(dir_data, "mcsoutc.dta"), convert.factors = F) # all BCS data
mcs14outc$id <- mcs14outc$mcsid
mcs14outc$age14 <- factor(mcs14outc$age14)
mcsoutc <- merge(regdata[regdata$cohort=="MCS",], mcs14outc, by="id", all.x = TRUE)
mcsoutc$cogscore <- factor.scores(mcsoutc[,c("C1", "C2", "C3")], fa(mcsoutc[,c("C1", "C2", "C3")], factors=1), method = "Bartlett")$scores

# recode
bcsoutc$hscl42 <- NA # high social class
bcsoutc$hscl42[bcsoutc$scl42 %in% c(1,2)] <- 1
bcsoutc$hscl42[bcsoutc$scl42 %in% 3:7] <- 0
bcsoutc$hnvq30 <- NA # high NVQ
bcsoutc$hnvq30[bcsoutc$nvq30 %in% c(4,5)] <- 1
bcsoutc$hnvq30[bcsoutc$nvq30 %in% 0:3] <- 0

# select variables
bcsoutclabs <- c(smktry16 = "Tried smoking (16)",
                 alcoh16 = "Alcohol last week (16)",
                 canntry16 = "Tried cannabis (16)",
                 bmi16 = "BMI (16)",
                 hnvq30 = "Higher education (30)",
                 lhgrpay38 = "(log) Gross hr. pay (38)",
                 hscl42 = "Social class I/II (42)",
                 bmi42 = "BMI (42)"
)
bcsoutcvars <- names(bcsoutclabs)

# select variables
mcsoutclabs <- c(smktry14 = "Tried smoking (14)",
                 alctry14 = "Tried alcohol (14)",
                 canntry14 = "Tried cannabis (14)",
                 selfharm14 = "Self-harmed in past year (14)",
                 bmi14 = "BMI (14)"
)
mcsoutcvars <- names(mcsoutclabs)


###################################################################
## ---- REGS_OUTC
# REGRESSIONS OF ADOLESCENT OUTCOMES ON EXT/INT & CHILDHOOD VARIABLES

# estimate BCS models
bcsoutcmod_b <- list()
bcsoutcmod_bc <- list()
bcsoutcmod_bcr <- list()
for (o in 1:length(bcsoutcvars)) {
  form_outc_b    <- as.formula(paste0(bcsoutcvars[o]," ~ EXT + INT + age16 + region +", pplus(decvars)))
  form_outc_bc   <- as.formula(paste0(bcsoutcvars[o]," ~ EXT + INT + age16 + region + cogscore +", pplus(decvars)))
  form_outc_bcr  <- as.formula(paste0(bcsoutcvars[o]," ~ EXT_RAW + INT_RAW + age16 + region + cogscore +", pplus(decvars)))
  for (s in c("M", "F")) {
    bcsoutcmod_b[[s]][[bcsoutcvars[o]]]   <- lm(form_outc_b, data = subset(bcsoutc, sex==s))
    bcsoutcmod_bc[[s]][[bcsoutcvars[o]]]   <- lm(form_outc_bc, data = subset(bcsoutc, sex==s))
    bcsoutcmod_bcr[[s]][[bcsoutcvars[o]]]   <- lm(form_outc_bcr, data = subset(bcsoutc, sex==s))
  }
}

# estimate BCS models
mcsoutcmod_b <- list()
mcsoutcmod_bc <- list()
mcsoutcmod_bcr <- list()
for (o in 1:length(mcsoutcvars)) {
  form_outc_b    <- as.formula(paste0(mcsoutcvars[o]," ~ EXT + INT + age14 + region +", pplus(decvars)))
  form_outc_bc   <- as.formula(paste0(mcsoutcvars[o]," ~ EXT + INT + age14 + region + cogscore +", pplus(decvars)))
  form_outc_bcr  <- as.formula(paste0(mcsoutcvars[o]," ~ EXT_RAW + INT_RAW + age14 + region + cogscore +", pplus(decvars)))
  for (s in c("M", "F")) {
    mcsoutcmod_b[[s]][[mcsoutcvars[o]]]   <- lm(form_outc_b, data = subset(mcsoutc, sex==s))
    mcsoutcmod_bc[[s]][[mcsoutcvars[o]]]   <- lm(form_outc_bc, data = subset(mcsoutc, sex==s))
    mcsoutcmod_bcr[[s]][[mcsoutcvars[o]]]   <- lm(form_outc_bcr, data = subset(mcsoutc, sex==s))
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
                                            "Externalising (factor)", # row label
                                            "", cellpr(bcsoutcmod_b$M[[i]], "EXT"), cellpr(bcsoutcmod_bc$M[[i]], "EXT"), "", # males EXT
                                            "", cellpr(bcsoutcmod_b$F[[i]], "EXT"), cellpr(bcsoutcmod_bc$F[[i]], "EXT"), "", # females EXT
                                            "Internalising (factor)", # row label
                                            "", cellpr(bcsoutcmod_b$M[[i]], "INT"), cellpr(bcsoutcmod_bc$M[[i]], "INT"), "", # males EXT
                                            "", cellpr(bcsoutcmod_b$F[[i]], "INT"), cellpr(bcsoutcmod_bc$F[[i]], "INT"), "", # females EXT
                                            "Externalising (raw score)", # row label
                                            "", "", "", cellpr(bcsoutcmod_bcr$M[[i]], "EXT_RAW"), # males EXT
                                            "", "", "", cellpr(bcsoutcmod_bcr$F[[i]], "EXT_RAW"), # females EXT
                                            "Internalising (raw score)", # row label
                                            "", "", "", cellpr(bcsoutcmod_bcr$M[[i]], "INT_RAW"), # males EXT
                                            "", "", "", cellpr(bcsoutcmod_bcr$F[[i]], "INT_RAW"), # females EXT
                                            "Cognition (factor)", # row label
                                            "", "", cellpr(bcsoutcmod_bc$M[[i]], "cogscore"), cellpr(bcsoutcmod_bcr$M[[i]], "cogscore"), # males COG
                                            "", "", cellpr(bcsoutcmod_bc$F[[i]], "cogscore"), cellpr(bcsoutcmod_bcr$F[[i]], "cogscore"), # females COG
                                            "\\newline Adj. R$^2$", # row label
                                            "", prr2(bcsoutcmod_b$M[[i]]), prr2(bcsoutcmod_bc$M[[i]]), prr2(bcsoutcmod_bcr$M[[i]]),
                                            "", prr2(bcsoutcmod_b$F[[i]]), prr2(bcsoutcmod_bc$F[[i]]), prr2(bcsoutcmod_bcr$F[[i]]),
                                            "Observations", # row label
                                            "", nobs(bcsoutcmod_b$M[[i]]), nobs(bcsoutcmod_bc$M[[i]]), nobs(bcsoutcmod_bcr$M[[i]]),
                                            "", nobs(bcsoutcmod_b$F[[i]]), nobs(bcsoutcmod_bc$F[[i]]), nobs(bcsoutcmod_bcr$F[[i]])
                                            
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
                                            "Externalising (factor)", # row label
                                            "", cellpr(mcsoutcmod_b$M[[i]], "EXT"), cellpr(mcsoutcmod_bc$M[[i]], "EXT"), "", # males EXT
                                            "", cellpr(mcsoutcmod_b$F[[i]], "EXT"), cellpr(mcsoutcmod_bc$F[[i]], "EXT"), "", # females EXT
                                            "Internalising (factor)", # row label
                                            "", cellpr(mcsoutcmod_b$M[[i]], "INT"), cellpr(mcsoutcmod_bc$M[[i]], "INT"), "", # males EXT
                                            "", cellpr(mcsoutcmod_b$F[[i]], "INT"), cellpr(mcsoutcmod_bc$F[[i]], "INT"), "", # females EXT
                                            "Externalising (raw score)", # row label
                                            "", "", "", cellpr(mcsoutcmod_bcr$M[[i]], "EXT_RAW"), # males EXT
                                            "", "", "", cellpr(mcsoutcmod_bcr$F[[i]], "EXT_RAW"), # females EXT
                                            "Internalising (raw score)", # row label
                                            "", "", "", cellpr(mcsoutcmod_bcr$M[[i]], "INT_RAW"), # males EXT
                                            "", "", "", cellpr(mcsoutcmod_bcr$F[[i]], "INT_RAW"), # females EXT
                                            "Cognition (factor)", # row label
                                            "", "", cellpr(mcsoutcmod_bc$M[[i]], "cogscore"), cellpr(mcsoutcmod_bcr$M[[i]], "cogscore"), # males COG
                                            "", "", cellpr(mcsoutcmod_bc$F[[i]], "cogscore"), cellpr(mcsoutcmod_bcr$F[[i]], "cogscore"), # females COG
                                            "\\newline Adj. R$^2$", # row label
                                            "", prr2(mcsoutcmod_b$M[[i]]), prr2(mcsoutcmod_bc$M[[i]]), prr2(mcsoutcmod_bcr$M[[i]]),
                                            "", prr2(mcsoutcmod_b$F[[i]]), prr2(mcsoutcmod_bc$F[[i]]), prr2(mcsoutcmod_bcr$F[[i]]),
                                            "Observations", # row label
                                            "", nobs(mcsoutcmod_b$M[[i]]), nobs(mcsoutcmod_bc$M[[i]]), nobs(mcsoutcmod_bcr$M[[i]]),
                                            "", nobs(mcsoutcmod_b$F[[i]]), nobs(mcsoutcmod_bc$F[[i]]), nobs(mcsoutcmod_bcr$F[[i]])
                                            
  ), nrow=8, byrow=TRUE
  )
}



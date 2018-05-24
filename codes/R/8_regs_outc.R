
###################################################################
## ---- LOAD_DATA_OUTC
# LOAD OUTCOMES DATA

# assemble outcomes data (BCS)
bcs16outc <- read.dta(paste0(dir_data, "bcsoutc.dta"), convert.factors = F) # all BCS data
bcs16outc$id <- bcs16outc$bcsid
bcs16outc$age16 <- factor(bcs16outc$age16)
bcsoutc <- merge(finaldata, bcs16outc, by = "id", all.x = TRUE)
# merge cognitive scores
cogbcs <- bcs5data[,c("bcsid", "epvt_z", "hfd_z", "copy_z")]
colnames(cogbcs)[1] <- "id"
bcsoutc <- merge(bcsoutc, cogbcs, by = "id", all.x = TRUE)
# make factor
bcsoutc$cogscore <- fa(bcsoutc[,c("epvt_z", "hfd_z", "copy_z")], factors=1)$scores

# assemble outcomes data (MCS)
mcs14outc <- read.dta(paste0(dir_data, "mcsoutc.dta"), convert.factors = F) # all BCS data
mcs14outc$id <- mcs14outc$mcsid
mcs14outc$age14 <- factor(mcs14outc$age14)
mcsoutc <- merge(finaldata, mcs14outc, by="id")
# merge cognitive scores
cogmcs <- mcs5data[,c("mcsid", "nvoc_bastz", "psim_bastz", "patc_bastz")]
colnames(cogmcs)[1] <- "id"
mcsoutc <- merge(mcsoutc, cogmcs, by = "id", all.x = TRUE)
# make factor
mcsoutc$cogscore <- fa(mcsoutc[,c("nvoc_bastz", "psim_bastz", "patc_bastz")], factors=1)$scores

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
                 #canntry14 = "Tried cannabis (14)",
                 selfharm14 = "Self-harmed in past year (14)",
                 #bmi11 = "BMI (11)",
                 bmi14 = "BMI (14)"
)
mcsoutcvars <- names(mcsoutclabs)


###################################################################
## ---- REGS_OUTC
# REGRESSIONS OF ADOLESCENT OUTCOMES ON EXT/INT & CHILDHOOD VARIABLES

# estimate BCS models
bcsoutcmod_m_ext <- list()
bcsoutcmod_m_int <- list()
bcsoutcmod_m_b <- list()
bcsoutcmod_m_bc <- list()
bcsoutcmod_f_ext <- list()
bcsoutcmod_f_int <- list()
bcsoutcmod_f_b <- list()
bcsoutcmod_f_bc <- list()
for (o in 1:length(bcsoutcvars)) {
  form_outc_ext  <- as.formula(paste0(bcsoutcvars[o]," ~ EXT + age16 + region + ", pplus(decvars)))
  form_outc_int  <- as.formula(paste0(bcsoutcvars[o]," ~ INT + age16 + region + ", pplus(decvars)))
  form_outc_b    <- as.formula(paste0(bcsoutcvars[o]," ~ EXT + INT + age16 + region +", pplus(decvars)))
  form_outc_bc   <- as.formula(paste0(bcsoutcvars[o]," ~ EXT + INT + age16 + region + cogscore +", pplus(decvars)))
  bcsoutcmod_m_ext[[bcsoutcvars[o]]] <- lm(form_outc_ext, data = subset(bcsoutc, sex=="M"))
  bcsoutcmod_m_int[[bcsoutcvars[o]]] <- lm(form_outc_int, data = subset(bcsoutc, sex=="M"))
  bcsoutcmod_m_b[[bcsoutcvars[o]]]   <- lm(form_outc_b, data = subset(bcsoutc, sex=="M"))
  bcsoutcmod_m_bc[[bcsoutcvars[o]]]   <- lm(form_outc_bc, data = subset(bcsoutc, sex=="M"))
  bcsoutcmod_f_ext[[bcsoutcvars[o]]] <- lm(form_outc_ext, data = subset(bcsoutc, sex=="F"))
  bcsoutcmod_f_int[[bcsoutcvars[o]]] <- lm(form_outc_int, data = subset(bcsoutc, sex=="F"))
  bcsoutcmod_f_b[[bcsoutcvars[o]]]   <- lm(form_outc_b, data = subset(bcsoutc, sex=="F"))
  bcsoutcmod_f_bc[[bcsoutcvars[o]]]   <- lm(form_outc_bc, data = subset(bcsoutc, sex=="F"))
}

# estimate MCS models
mcsoutcmod_m_ext <- list()
mcsoutcmod_m_int <- list()
mcsoutcmod_m_b <- list()
mcsoutcmod_m_bc <- list()
mcsoutcmod_f_ext <- list()
mcsoutcmod_f_int <- list()
mcsoutcmod_f_b <- list()
mcsoutcmod_f_bc <- list()
for (o in 1:length(mcsoutcvars)) {
  form_outc_ext  <- as.formula(paste0(mcsoutcvars[o]," ~ EXT + age14 + region + ", pplus(decvars)))
  form_outc_int  <- as.formula(paste0(mcsoutcvars[o]," ~ INT + age14 + region + ", pplus(decvars)))
  form_outc_b    <- as.formula(paste0(mcsoutcvars[o]," ~ EXT + INT + age14 + region +", pplus(decvars)))
  form_outc_bc   <- as.formula(paste0(mcsoutcvars[o]," ~ EXT + INT + age14 + region + cogscore + ", pplus(decvars)))
  mcsoutcmod_m_ext[[mcsoutcvars[o]]] <- lm(form_outc_ext, data = subset(mcsoutc, sex=="M"))
  mcsoutcmod_m_int[[mcsoutcvars[o]]] <- lm(form_outc_int, data = subset(mcsoutc, sex=="M"))
  mcsoutcmod_m_b[[mcsoutcvars[o]]]   <- lm(form_outc_b, data = subset(mcsoutc, sex=="M"))
  mcsoutcmod_m_bc[[mcsoutcvars[o]]]  <- lm(form_outc_bc, data = subset(mcsoutc, sex=="M"))
  mcsoutcmod_f_ext[[mcsoutcvars[o]]] <- lm(form_outc_ext, data = subset(mcsoutc, sex=="F"))
  mcsoutcmod_f_int[[mcsoutcvars[o]]] <- lm(form_outc_int, data = subset(mcsoutc, sex=="F"))
  mcsoutcmod_f_b[[mcsoutcvars[o]]]   <- lm(form_outc_b, data = subset(mcsoutc, sex=="F"))
  mcsoutcmod_f_bc[[mcsoutcvars[o]]]  <- lm(form_outc_bc, data = subset(mcsoutc, sex=="F"))
}

###################################################################
## ---- REGS_OUTC_TAB
# TABLES from outcome regressions

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
cellpr <- function(mod, vr) {
  # mod: lm model
  # vrs: variable to extract
  coefs <- lmtest::coeftest(mod, vcov = sandwich::vcovHC(mod, type = "HC1"))
  coefs2 <- coefs[vr,]
  return(
    paste0("$", prcoef(round(coefs2[1],3)), # estimate
           stars(coefs2[3]), "$", # stars
           " \\newline ($", prcoef(round(coefs2[2],3)), "$)")
  ) # SE (on new line)
}

# BCS
# mean outcomes
dvmeans_bcs_m <- sapply(colMeans(bcsoutc[bcsoutc$sex=="M",bcsoutcvars], na.rm = T), prcoef)
dvmeans_bcs_f <- sapply(colMeans(bcsoutc[bcsoutc$sex=="F",bcsoutcvars], na.rm = T), prcoef)

bcs_outctab <- list()
for (i in 1:length(bcsoutcvars)) {
  lab <- paste0("\\textit{\\textbf{", bcsoutclabs[i], "}}")
  bcs_outctab[[bcsoutcvars[i]]] <- matrix(c(lab, dvmeans_bcs_m[i], "", "", "", "", dvmeans_bcs_f[i], "", "", "", "",   # outcome and means
                                            "EXT", # row label
                                            "", cellpr(bcsoutcmod_m_ext[[i]], "EXT"), "", cellpr(bcsoutcmod_m_b[[i]], "EXT"), cellpr(bcsoutcmod_m_bc[[i]], "EXT"), # males EXT
                                            "", cellpr(bcsoutcmod_f_ext[[i]], "EXT"), "", cellpr(bcsoutcmod_f_b[[i]], "EXT"), cellpr(bcsoutcmod_f_bc[[i]], "EXT"),# females EXT
                                            "INT", # row label
                                            "", "", cellpr(bcsoutcmod_m_int[[i]], "INT"), cellpr(bcsoutcmod_m_b[[i]], "INT"), cellpr(bcsoutcmod_m_bc[[i]], "INT"),# males INT
                                            "", "", cellpr(bcsoutcmod_f_int[[i]], "INT"), cellpr(bcsoutcmod_f_b[[i]], "INT"), cellpr(bcsoutcmod_f_bc[[i]], "INT"),# females INT
                                            "COG", # row label
                                            "", "", "", "", cellpr(bcsoutcmod_m_bc[[i]], "cogscore"), # males COG
                                            "", "", "", "", cellpr(bcsoutcmod_f_bc[[i]], "cogscore") # females COG
  ), nrow=4, byrow=TRUE
  )
}

# MCS
# mean outcomes
dvmeans_mcs_m <- sapply(colMeans(mcsoutc[mcsoutc$sex=="M",mcsoutcvars], na.rm = T), prcoef)
dvmeans_mcs_f <- sapply(colMeans(mcsoutc[mcsoutc$sex=="F",mcsoutcvars], na.rm = T), prcoef)

mcs_outctab <- list()
for (i in 1:length(mcsoutcvars)) {
  lab <- paste0("\\textit{\\textbf{", mcsoutclabs[i], "}}")
  mcs_outctab[[mcsoutcvars[i]]] <- matrix(c(lab, dvmeans_mcs_m[i], "", "", "", "", dvmeans_mcs_f[i], "", "", "", "",   # outcome and means
                                            "EXT", # row label
                                            "", cellpr(mcsoutcmod_m_ext[[i]], "EXT"), "", cellpr(mcsoutcmod_m_b[[i]], "EXT"), cellpr(mcsoutcmod_m_bc[[i]], "EXT"), # males EXT
                                            "", cellpr(mcsoutcmod_f_ext[[i]], "EXT"), "", cellpr(mcsoutcmod_f_b[[i]], "EXT"), cellpr(mcsoutcmod_f_bc[[i]], "EXT"),# females EXT
                                            "INT", # row label
                                            "", "", cellpr(mcsoutcmod_m_int[[i]], "INT"), cellpr(mcsoutcmod_m_b[[i]], "INT"), cellpr(mcsoutcmod_m_bc[[i]], "INT"),# males INT
                                            "", "", cellpr(mcsoutcmod_f_int[[i]], "INT"), cellpr(mcsoutcmod_f_b[[i]], "INT"), cellpr(mcsoutcmod_f_bc[[i]], "INT"),# females INT
                                            "COG", # row label
                                            "", "", "", "", cellpr(mcsoutcmod_m_bc[[i]], "cogscore"), # males COG
                                            "", "", "", "", cellpr(mcsoutcmod_f_bc[[i]], "cogscore") # females COG
  ), nrow=4, byrow=TRUE
  )
}



## ---- REGRESSIONS

# function to collapse regressors in formula
pplus <- function(x) paste0(x, collapse="+")

###################################################################
# REGRESSIONS OF EXT/INT ON CHILDHOOD VARIABLES
# add region to vector of regressors
decvars_reg <- c(decvars, c("region"))
decvars_int_reg <- c(decvars_int, c("region"))

# formulas
form_ext <- list()
form_ext[[1]] <- as.formula("EXT ~ mpsla5")
form_ext[[2]] <- as.formula(paste("EXT ~ ", pplus(decvars_reg)))
#form_ext[[3]] <- as.formula(paste("EXT ~ ", pplus(decvars_int_reg)))
form_int <- list()
form_int[[1]] <- as.formula("INT ~ mpsla5")
form_int[[2]] <- as.formula(paste("INT ~ ", pplus(decvars_reg)))
#form_int[[3]] <- as.formula(paste("INT ~ ", pplus(decvars_int_reg)))

# regressions
r.ext <- list()
r.int <- list()
for (cs in c("BCS.M", "MCS.M", "BCS.F", "MCS.F")) {
  r.ext[[cs]] <- list()
  r.int[[cs]] <- list()
  for (i in 1:2) {
    r.ext[[cs]][[i]]  <- lm(form_ext[[i]],  data=subset(scores2plot, cohortsex==cs))
    r.int[[cs]][[i]]  <- lm(form_int[[i]],  data=subset(scores2plot, cohortsex==cs))
  }
}


###################################################################
# REGRESSIONS OF ADOLESCENT OUTCOMES ON EXT/INT & CHILDHOOD VARIABLES

# assemble outcomes data
bcs16outc <- read.dta(paste0(dir_data, "bcs16outc.dta"), convert.factors = F) # all BCS data
bcs16outc$id <- bcs16outc$bcsid
bcs16outc$age16 <- factor(bcs16outc$age16)
bcsoutc <- merge(finaldata, bcs16outc, by="id")

mcs14outc <- read.dta(paste0(dir_data, "mcs14outc.dta"), convert.factors = F) # all BCS data
mcs14outc$id <- mcs14outc$mcsid
mcs14outc$age14 <- factor(mcs14outc$age14)
mcsoutc <- merge(finaldata, mcs14outc, by="id")

# recode
bcsoutc$hscl42 <- NA # high social class
bcsoutc$hscl42[bcsoutc$scl42 %in% c(1,2)] <- 1
bcsoutc$hscl42[bcsoutc$scl42 %in% 3:7] <- 0

bcsoutc$hnvq30 <- NA # high NVQ
bcsoutc$hnvq30[bcsoutc$nvq30 %in% c(4,5)] <- 1
bcsoutc$hnvq30[bcsoutc$nvq30 %in% 0:3] <- 0

# BCS models
bcsoutcvars <- c("smktry16", "alcoh16", "drugtry16", "hnvq30", "lhgrpay38", "hscl42")
bcsoutcmod_m_uc <- list()
bcsoutcmod_m_c <- list()
bcsoutcmod_f_uc <- list()
bcsoutcmod_f_c <- list()
for (o in 1:length(bcsoutcvars)) {
  form_outc_uc <- as.formula(paste0(bcsoutcvars[o]," ~ EXT + INT"))
  form_outc_c <- as.formula(paste0(bcsoutcvars[o]," ~ EXT + INT + age16 + region +", pplus(decvars)))
  bcsoutcmod_m_uc[[bcsoutcvars[o]]] <- lm(form_outc_uc, data = subset(bcsoutc, sex=="M"))
  bcsoutcmod_m_c[[bcsoutcvars[o]]] <- lm(form_outc_c, data = subset(bcsoutc, sex=="M"))
  bcsoutcmod_f_uc[[bcsoutcvars[o]]] <- lm(form_outc_uc, data = subset(bcsoutc, sex=="F"))
  bcsoutcmod_f_c[[bcsoutcvars[o]]] <- lm(form_outc_c, data = subset(bcsoutc, sex=="F"))
}

# MCS models
mcsoutcvars <- c("smktry14", "alctry14", "drugtry14", "hadsex14", "selfharm14")
mcsoutcmod_m_uc <- list()
mcsoutcmod_m_c <- list()
mcsoutcmod_f_uc <- list()
mcsoutcmod_f_c <- list()
for (o in 1:length(mcsoutcvars)) {
  form_outc_uc <- as.formula(paste0(mcsoutcvars[o]," ~ EXT + INT"))
  form_outc_c <- as.formula(paste0(mcsoutcvars[o]," ~ EXT + INT + age14 + region +", pplus(decvars)))
  mcsoutcmod_m_uc[[mcsoutcvars[o]]] <- lm(form_outc_uc, data = subset(mcsoutc, sex=="M"))
  mcsoutcmod_m_c[[mcsoutcvars[o]]] <- lm(form_outc_c, data = subset(mcsoutc, sex=="M"))
  mcsoutcmod_f_uc[[mcsoutcvars[o]]] <- lm(form_outc_uc, data = subset(mcsoutc, sex=="F"))
  mcsoutcmod_f_c[[mcsoutcvars[o]]] <- lm(form_outc_c, data = subset(mcsoutc, sex=="F"))
}


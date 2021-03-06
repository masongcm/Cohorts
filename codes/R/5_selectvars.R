#######################################################################################################
## ---- SELECTDATA

# select variables for decomposition, in groups
decvarslist <- list()
decvarslist$med <- c("mpsla5")
decvarslist$mem <- c("mempl5b")
decvarslist$ses <- c("fscl5wb")
decvarslist$mch <- c("mothageb", "mheight", "singlem", "ethn", "numch5")
decvarslist$prg <- c("firstb", "nprevst", "smkpr", "preterm", "lbwt")

# interaction between mother characteristics and initial conditions
mchxbth <-  list(apply(
  expand.grid(decvarslist$mch, decvarslist$bth),
  MARGIN=1, FUN = function(x) paste0(x,collapse = "*")))
decvarslist_int <- c(decvarslist, mchxbth)

# names of the groups
decvarsgroups <- c("Maternal education (5)", "Maternal employment (5)", "Father occ. (5) - White Collar = 0","Maternal background (0)", "Pregnancy")

# make into vector
decvars <- unname(unlist(decvarslist))
decvars_int <- unname(unlist(decvarslist_int))

# decomposition sequence (without interactions)
decseq <- lapply(decvarslist, FUN = function(x) paste0(x, collapse="+"))

# final data for analysis and decomposition
finaldata_all <- merge(cohdata_all, fascores[,c("INT","EXT","id")], by = "id", all.x = TRUE)
finaldata <- merge(cohdata, fascores[,c("INT","EXT","id")], by = "id")
regdata <- finaldata[complete.cases(finaldata[,decvars]),]

# export to stata for Gelbach
export(finaldata, paste0(dir_data, "finaldata.dta"))
export(finaldata_all, paste0(dir_data, "finaldata_all.dta"))
export(regdata, paste0(dir_data, "regdata.dta"))

###################################################################
## ---- SELECTDATA_OUTC
# LOAD OUTCOMES DATA

# assemble outcomes data (BCS)
bcs16outc <- read.dta(paste0(dir_data, "bcsoutc.dta"), convert.factors = F) # all BCS data
bcs16outc$id <- bcs16outc$bcsid
bcsoutc <- merge(regdata[regdata$cohort=="BCS",], bcs16outc, by = "id", all.x = TRUE)
bcsoutc$cogscore <- as.vector(factor.scores(bcsoutc[,c("C1", "C2", "C3")], fa(bcsoutc[,c("C1", "C2", "C3")], factors=1), method = "Bartlett")$scores)

# assemble outcomes data (MCS)
mcs14outc <- read.dta(paste0(dir_data, "mcsoutc.dta"), convert.factors = F) # all BCS data
mcs14outc$id <- mcs14outc$mcsid
mcsoutc <- merge(regdata[regdata$cohort=="MCS",], mcs14outc, by="id", all.x = TRUE)
mcsoutc$cogscore <- as.vector(factor.scores(mcsoutc[,c("C1", "C2", "C3")], fa(mcsoutc[,c("C1", "C2", "C3")], factors=1), method = "Bartlett")$scores)

# recode NVQ and employment
bcsoutc <- bcsoutc %>%
  mutate(hnvq30 = ifelse(nvq30 %in% c(4,5), 1, ifelse(nvq30 %in% c(0,1,2,3), 0, NA))) %>%
  mutate(hnvq34 = ifelse(nvq34 %in% c(4,5), 1, ifelse(nvq34 %in% c(0,1,2,3), 0, NA))) %>%
  mutate(emp34 = ifelse(empst34 %in% c(1,2), 1, ifelse(empst34==0, 0, NA))) %>%
  mutate(emp42 = ifelse(empst42 %in% c(1,2), 1, ifelse(empst42==0, 0, NA)))

# BMI splits
# BMI for age in adolescence http://www.who.int/growthref/who2007_bmi_for_age/en/
library(AGD)
bcsoutc$bmi16z <- AGD::y2z(y=bcsoutc$bmi16, x=bcsoutc$agebmi16, sex=as.character(bcsoutc$sex), ref=get("who.bmi"))
mcsoutc$bmi14z <- AGD::y2z(y=mcsoutc$bmi14, x=mcsoutc$agebmi14, sex=as.character(mcsoutc$sex), ref=get("who.bmi"))
# overweight/thin
bcsoutc$owt16 <- ifelse(bcsoutc$bmi16z > 1, 1, 0)
bcsoutc$thn16 <- ifelse(bcsoutc$bmi16z > -2, 0, 1)
mcsoutc$owt14 <- ifelse(mcsoutc$bmi14z > 1, 1, 0)
mcsoutc$thn14 <- ifelse(mcsoutc$bmi14z > -2, 0, 1)
# obese (adult)
bcsoutc$obs42 <- ifelse(bcsoutc$bmi42 > 30, 1, 0)
bcsoutc$owt42 <- ifelse(bcsoutc$bmi42 > 25, 1, 0)
# log BMI
bcsoutc$lbmi16 <- log(bcsoutc$bmi16)
bcsoutc$lbmi42 <- log(bcsoutc$bmi42)
mcsoutc$lbmi14 <- log(mcsoutc$bmi14)


# select variables
bcsoutclabs <- c(smktry16 = "Tried smoking (BCS - 16)",
                 bmi16 = "BMI (BCS - 16)",
                 #hialc16 = ">10 alcohol U last week (16)",
                 #alcoh16 = "Alcohol last week (16)",
                 #canntry16 = "Tried cannabis (16)",
                 #lbmi16 = "log BMI (16)",
                 #owt16 = "Overweight (16)",
                 #thn16 = "Thin (16)",
                 #hnvq30 = "Higher education (30)",
                 hnvq34 = "Higher education (34)",
                 lgpay34 = "(log) Gross weekly pay (34)",
                 #emp34 = "Employed (34)",
                 lgpay42 = "(log) Gross weekly pay (42)",
                 emp42 = "Employed (42)",
                 #lhgrpay38 = "(log) Gross hr. pay (38)",
                 #hscl42 = "Social class I/II (42)",
                 #smoke38 = "Daily smoker (38)",
                 smoke42 = "Daily smoker (42)",
                 bmi42 = "BMI (42)"
                 #lbmi42 = "BMI (42)",
                 #lbmi42 = "(log) BMI (42)",
                 #owt42 = "Overweight (42)",
                 #obs42 = "Obese (42)"
)
bcsoutcvars <- names(bcsoutclabs)

# select variables
mcsoutclabs <- c(smktry14 = "Tried smoking (MCS - 14)",
                 #alctry14 = "Tried alcohol (14)",
                 #canntry14 = "Tried cannabis (14)",
                 #selfharm14 = "Self-harmed in past year (14)",
                 bmi14 = "BMI (MCS - 14)"
                 #lbmi14 = "log BMI (14)",
                 #owt14 = "Overweight (14)",
                 #thn14 = "Thin (14)"
)
mcsoutcvars <- names(mcsoutclabs)

#######################################################################################################
## ---- SUMSTATS
# summary statistics by cohort and gender

# separate factors from numeric
finaldata$ethn <- as.factor(finaldata$ethn)
varnumeric <- decvars[unlist(lapply(finaldata[,decvars], is.numeric))]
varfac <- decvars[unlist(lapply(finaldata[,decvars],  is.factor))]
varfac_detect <- paste0(varfac, collapse = "|")

# ESTIMATION SAMPLE
# expand factors with model.matrix
sumdata <- cbind(finaldata[,c("cohort", "weight1", varnumeric)],
                 model.matrix(as.formula(paste0(c("~ 0 ", decvars[decvars %in% varfac]), collapse = "+")), model.frame(~ ., finaldata, na.action=na.pass)))
# replace birthweight
sumdata$bwt <- finaldata$bwt
sumdata$lbwt <- NULL

# WHOLE SAMPLE
# expand factors with model.matrix
finaldata_all$ethn <- as.factor(finaldata_all$ethn)
sumdata_all <- cbind(finaldata_all[,c("cohort", "weight1", varnumeric)],
                 model.matrix(as.formula(paste0(c("~ 0 ", decvars[decvars %in% varfac]), collapse = "+")), model.frame(~ ., finaldata_all, na.action=na.pass)))
# replace birthweight
sumdata_all$bwt <- finaldata_all$bwt
sumdata_all$lbwt <- NULL

sumlabs <- c(
             "Mother age", "Mother height (m)", "Unmarried", "Nonwhite child",
             "Firstborn child", "Number previous stillbirths", "Mother smoked in pregnancy", "Preterm birth",
             "Missing gest. age", "Birthweight (kg)",
             "Number of children in HH",
             "Mother has post-compulsory education", "Mother is employed",
             "Father occupation: blue collar", "No father figure" 
)

# make table
makeSumTab <- function(data, useweight=FALSE) {
  
  if (useweight) {
    summtab <- data %>%
      group_by(cohort) %>%
      summarise_all(funs(mean = weighted.mean(., weight1, na.rm = TRUE), sd(., na.rm = TRUE)))
  } else {
    summtab <- data %>%
      group_by(cohort) %>%
      summarise_all(funs(mean(., na.rm = TRUE), sd(., na.rm = TRUE)))
  }
  summtab <- summtab %>%
    gather(var, value, -cohort) %>%
    separate(var,c("var", "stat"), sep="_") %>%
    mutate(value = sprintf("%.2f", value))
  summtab <- reshape2::dcast(summtab, var ~ cohort + stat) %>%
    mutate(BCS_sd = ifelse(stringr::str_detect(var, varfac_detect), "", BCS_sd)) %>%
    mutate(MCS_sd = ifelse(stringr::str_detect(var, varfac_detect), "", MCS_sd))
  summtab <- summtab %>%
    mutate(
      BCS = ifelse(BCS_sd == "", paste0(as.character(BCS_mean)),
                   paste0(as.character(BCS_mean)," (", as.character(BCS_sd), ")" )),
      MCS = ifelse(MCS_sd == "", paste0(as.character(MCS_mean)),
                   paste0(as.character(MCS_mean)," (", as.character(MCS_sd), ")" ))
    ) %>%
    select(var, BCS, MCS) %>%
    filter(var != "mpsla5Compulsory") %>%
    filter(var != "weight1") %>%
    mutate(var = factor(var, 
                        levels = c("mothageb", "mheight", "singlemNot married", "ethn1",
                                   "firstb1", "nprevst", "smkprSmoker", "pretermPreterm", "pretermMissing", "bwt",
                                   "numch5", "mpsla5Post-Compulsory", "mempl5bEmployed",
                                   "fscl5wbBlue collar", "fscl5wbNo father fig."
                        ))) %>%
    arrange(var)
  
  summtab$var <- sumlabs
  return(summtab)
}
summtab_estim <- makeSumTab(sumdata)
summtab_all <- makeSumTab(sumdata_all, useweight = TRUE)

summtab_estimall <- merge(summtab_estim, summtab_all, by="var", sort = FALSE)

rm(sumdata, sumdata_all, varfac, varfac_detect, varnumeric, summtab_estim, summtab_all, sumlabs)

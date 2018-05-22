#######################################################################################################
## ---- SELECTVARS

# select variables for decomposition, in groups
decvarslist <- list()
decvarslist$med <- c("mpsla5")
decvarslist$mem <- c("mempl5")
decvarslist$mch <- c("mothageb", "mheight", "singlem", "ethn")
decvarslist$prg <- c("parity", "firstb", "nprevst", "smkpr", "preterm", "lbwt")

# interaction between mother characteristics and initial conditions
mchxbth <-  list(apply(
  expand.grid(decvarslist$mch, decvarslist$bth),
  MARGIN=1, FUN = function(x) paste0(x,collapse = "*")))
decvarslist_int <- c(decvarslist, mchxbth)

# names of the groups
decvarsgroups <- c("Mat. Education", "Mat. Employment", "Mat. Charact.", "Pregnancy/Birth")

# make into vector
decvars <- unname(unlist(decvarslist))
decvars_int <- unname(unlist(decvarslist_int))

# decomposition sequence (without interactions)
decseq <- lapply(decvarslist, FUN = function(x) paste0(x, collapse="+"))

# final data for analysis and decomposition
finaldata <- scores2plot[complete.cases(scores2plot[,decvars]),]

# export to stata for Gelbach
export(finaldata, paste0(dir_data, "finaldata.dta"))

# labels
declabs <- c(scl10b = "Parental social class (10)",
             mpsla5 = "Mother post-compuls. education (5)",
             mempl5 = "Mother employment status (5)",
             mothageb = "Mother age (0)",
             teenm = "Teen mother (0)",
             mheight = "Mother height (0)",
             singlem = "Unmarried mother (0)",
             ethn = "Nonwhite ethnicity (0)",
             parity = "Parity (0)",
             firstb = "Firstborn child (0)",
             nprevst = "Num previous stillbirths (0)",
             smkpr = "Mother smoked in pregnancy (0)",
             caesbirth = "Caesarean birth (0)",
             preterm = "Preterm birth (0)",
             lbwt = "(log) Birthweight (0)"
)


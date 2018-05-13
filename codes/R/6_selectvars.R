#######################################################################################################
## ---- SELECTVARS

# select variables for decomposition, in groups
decvarslist <- list()
decvarslist$ses <- c("scl10b")
decvarslist$med <- c("mpsla5")
decvarslist$mem <- c("mempl5")
decvarslist$mch <- c("mothageb", "teenm", "mheight", "singlem", "ethn")
decvarslist$prg <- c("parity", "firstb", "nprevst", "smkpr", "caesbirth", "gestaw", "preterm", "bwt", "lowbwt")

# interaction between mother characteristics and initial conditions
mchxbth <-  list(apply(
  expand.grid(decvarslist$mch, decvarslist$bth),
  MARGIN=1, FUN = function(x) paste0(x,collapse = "*")))
decvarslist_int <- c(decvarslist, mchxbth)

# names of the groups
decvarsgroups <- c("Social Class", "Mat. Education", "Mat. Employment", "Mat. Charact.", "Pregnancy/Birth")

# make into vector
decvars <- unname(unlist(decvarslist))
decvars_int <- unname(unlist(decvarslist_int))

# decomposition sequence (without interactions)
decseq <- lapply(decvarslist, FUN = function(x) paste0(x, collapse="+"))

# final data for analysis and decomposition
finaldata <- scores2plot[complete.cases(scores2plot[,decvars]),]

# export to stata for Gelbach
export(finaldata, paste0(dir_data, "finaldata.dta"))

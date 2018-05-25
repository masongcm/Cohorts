#######################################################################################################
## ---- SELECTVARS

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
decvarsgroups <- c("Maternal education (5)", "Maternal employment (5)", "Father occupation (5)","Maternal background (0)", "Pregnancy")

# make into vector
decvars <- unname(unlist(decvarslist))
decvars_int <- unname(unlist(decvarslist_int))

# decomposition sequence (without interactions)
decseq <- lapply(decvarslist, FUN = function(x) paste0(x, collapse="+"))

# final data for analysis and decomposition
finaldata <- scores2plot[complete.cases(scores2plot[,decvars]),]

# export to stata for Gelbach
export(finaldata, paste0(dir_data, "finaldata.dta"))

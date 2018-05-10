#######################################################################################################
## ---- FORMULAS

# select variables for decomposition, in groups
decvarslist <- list()
decvarslist$ses <- c("scl10b")
decvarslist$med <- c("mysch5b")
decvarslist$mch <- c("mothageb", "teenm", "mheight", "singlem", "mempl", "ethn")
decvarslist$prg <- c("parity", "firstb", "nprevst", "smkpr")
decvarslist$bth <- c("caesbirth", "gestaw", "preterm", "bwt", "lowbwt")

# interaction between mother characteristics and initial conditions
mchxbth <-  list(apply(
  expand.grid(decvarslist$mch, decvarslist$bth),
  MARGIN=1, FUN = function(x) paste0(x,collapse = "*")))
decvarslist_int <- c(decvarslist, mchxbth)

# names of the groups
decvarsgroups <- c("Social Class", "Mat. Educ.", "Mat. Charact.", "Pregnancy", "Birth", "Birth * Mat. Char.")

# make into vector
decvars <- unname(unlist(decvarslist))
decvars_int <- unname(unlist(decvarslist_int))

# decomposition sequence
decseq <- lapply(decvarslist_int, FUN = function(x) paste0(x, collapse="+"))

# final data for analysis and decomposition
finaldata <- scores2plot[complete.cases(scores2plot[,decvars]),]


#######################################################################################################
## ---- FORMULAS

# select variables for decomposition, in groups
decvarslist <- list()
decvarslist$ses <- c("scl10b")
decvarslist$med <- c("mysch5b")
decvarslist$mch <- c("mothageb", "teenm", "mheight", "singlem", "mempl", "ethn")
decvarslist$prg <- c("parity", "firstb", "nprevst", "smkpr")
decvarslist$bth <- c("caesbirth", "gestaw", "preterm", "bwt", "lowbwt")

# names of the groups
decvarsgroups <- c("Social Class", "Maternal Education", "Maternal Charact.", "Pregnancy", "Birth")

# make into vector
decvars <- unname(unlist(decvarslist))

# decomposition sequence
decseq <- lapply(decvarslist, FUN = function(x) paste0(x, collapse="+"))

# final data for analysis and decomposition
finaldata <- scores2plot[complete.cases(scores2plot[,decvars]),]


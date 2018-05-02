#######################################################################################################
## ---- FORMULAS

# select variables for decomposition, in groups
decvarslist <- list()
decvarslist$ses <- c("scl10b")
decvarslist$med <- c("mysch5", "mhied5")
decvarslist$mch <- c("mothageb", "teenm", "mheight", "singlem", "mempl")
decvarslist$prg <- c("parity", "firstb", "nprevst", "smkpr")
decvarslist$bth <- c("caesbirth", "gestaw", "preterm", "bwt", "lowbwt")

# make into vector
decvars <- unname(unlist(decvarslist))

# decomposition sequence
decseq <- lapply(decvarslist, FUN = function(x) paste0(x, collapse="+"))

# formulas
form.ext <- as.formula(paste("EXT ~ ", paste0(decvars, collapse = '+')))
form.int <- as.formula(paste("INT ~ ", paste0(decvars, collapse = '+')))

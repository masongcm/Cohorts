## ---- FA_FIT

# assemble AFIs indices for table
indsel <- c("npar", "chisq", "rmsea", "mfi", "cfi.scaled")
mnames <- c("Configural", "Threshold + Loading Invariance", "Threshold, Loading, + Intercept Invariance")

afitab <- list()
for (i in 1:length(fa.c)) {
  afitab[[i]] <- data.frame(rbind(
    round(c( fitMeasures(fa.c[[i]], indsel), moreFitIndices(fa.c[[i]])["gammaHat"] ),5),
    round(c( fitMeasures(fa.tl[[i]], indsel), moreFitIndices(fa.tl[[i]])["gammaHat"] ),5),
    round(c( fitMeasures(fa.tli[[i]], indsel), moreFitIndices(fa.tli[[i]])["gammaHat"] ),5)
  ))
  afitab[[i]]$npar  <- as.character(afitab[[i]]$npar)
  afitab[[i]]$chisq <- as.character(round(afitab[[i]]$chisq,1))
  
  # add deltas
  dmfi <- NA
  dcfi <- NA
  dgam <- NA
  for (r in 2:nrow(afitab[[i]])) {
    dmfi <- c(dmfi, afitab[[i]][r,"mfi"] -  afitab[[i]][1,"mfi"])
    dcfi <- c(dcfi, afitab[[i]][r,"cfi.scaled"] -  afitab[[i]][1,"cfi.scaled"])
    dgam <- c(dgam, afitab[[i]][r,"gammaHat"] -  afitab[[i]][1,"gammaHat"])
  }
  afitab[[i]]$dmfi <- as.matrix(dmfi)
  afitab[[i]]$dcfi <- as.matrix(dcfi)
  afitab[[i]]$dgam <- as.matrix(dgam)
  
  # add names
  afitab[[i]] <- data.frame(cbind(mnames = as.matrix(mnames), afitab[[i]]))
}
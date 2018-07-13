## ---- FA_FIT

# function to produce table
# modlist: list of nested models (first model is configural)
# mlabels: label for each model

afitab <- function(modlist, mlabels) {
  
  # AFIs to select
  indsel <- c("npar", "chisq", "rmsea", "srmr", "mfi", "cfi.scaled")

  # assemble AFIs indices for table
  tabout <- matrix(NA, length(modlist), 7)
  for (i in 1:length(modlist)) {
    tabout[i,] <- round(c( 
      fitMeasures(modlist[[i]], indsel), 
      moreFitIndices(modlist[[i]])["gammaHat"] ),5)
  }
  tabout <- data.frame(tabout)
  colnames(tabout) <- c(indsel, "gammaHat")
  tabout$npar  <- as.character(tabout$npar)
  tabout$chisq <- as.character(round(tabout$chisq,1))
  
  # anova to get pval of chisquare
  pval <- NA
  for (i in 2:length(modlist)) {
    pval <- c(pval, anova(modlist[[1]], modlist[[i]])$`Pr(>Chisq)`[2])
  }
  
  # add deltas
  drmsea <- NA
  dsrmr <- NA
  dmfi <- NA
  dcfi <- NA
  dgam <- NA
  for (r in 2:length(modlist)) {
    drmsea <- c(drmsea, tabout[r,"rmsea"] -  tabout[1,"rmsea"])
    dsrmr <- c(dsrmr, tabout[r,"srmr"] -  tabout[1,"srmr"])
    dmfi <- c(dmfi, tabout[r,"mfi"] -  tabout[1,"mfi"])
    dcfi <- c(dcfi, tabout[r,"cfi.scaled"] -  tabout[1,"cfi.scaled"])
    dgam <- c(dgam, tabout[r,"gammaHat"] -  tabout[1,"gammaHat"])
  }
  tabout$pchisq <- as.matrix(pval)
  tabout$drmsea <- as.matrix(drmsea)
  tabout$dsrmr <- as.matrix(dsrmr)
  tabout$dmfi <- as.matrix(dmfi)
  tabout$dcfi <- as.matrix(dcfi)
  tabout$dgam <- as.matrix(dgam)
  
  # add names
  tabout <- data.frame(cbind(mlabels = as.matrix(mlabels), tabout))
  return(tabout)

}

# MAKE FIT TABLES
fit_main <- afitab(list(fa.c[[1]], fa.tl[[1]], fa.tli[[1]], fa.tlip[[1]]), 
       c("Configural", 
         "Threshold + Loading Inv", 
         "Threshold, Loading, + Intercept Inv",
         "Threshold, Loading, + Partial Int Inv")
)

fit_agesub <- afitab(list(fa.c[[2]], fa.tl[[2]], fa.tli[[2]], fa.tlip[[2]]), 
                   c("Configural", 
                     "Threshold + Loading Inv", 
                     "Threshold, Loading, + Intercept Inv",
                     "Threshold, Loading, + Partial Int Inv")
)

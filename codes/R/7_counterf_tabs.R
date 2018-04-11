## ---- COUNTERF_PREP_TAB
# prepare tables with summary inequality statistics

iqdecomp <- function(cffit) {
  cfd <- getcfdat(cffit) # get output from counterfactual
  
  # matrix of quantile differences of interest
  qdiffmat <- matrix(c(.9,.1,
                       .5,.1,
                       .9,.5,
                       .75,.25), nrow=4, byrow=T)
  difftab <- matrix(NA, nrow=4, ncol=3)
  rownames(difftab) <- c("90-10", "50-10", "90-50", "75-25")
  colnames(difftab) <- c("Total Diff", "Structure", "Composition")
  for (r in 1:nrow(qdiffmat)) {
    attach(cfd)
    
    changes <- c(
      tot = (qf.ref1[quant==qdiffmat[r,1]] - qf.ref1[quant==qdiffmat[r,2]]) - (qf.ref0[quant==qdiffmat[r,1]] - qf.ref0[quant==qdiffmat[r,2]]),
      str = (qf.ref1[quant==qdiffmat[r,1]] - qf.ref1[quant==qdiffmat[r,2]]) - (qf.cf[quant==qdiffmat[r,1]] - qf.cf[quant==qdiffmat[r,2]]),
      com = (qf.cf[quant==qdiffmat[r,1]] - qf.cf[quant==qdiffmat[r,2]]) - (qf.ref0[quant==qdiffmat[r,1]] - qf.ref0[quant==qdiffmat[r,2]])
    )
    detach(cfd)
    difftab[r,] <- changes
  }
  return(difftab)
}

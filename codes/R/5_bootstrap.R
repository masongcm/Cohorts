
############################################################################################
## ---- FA_BOOTSTRAP
# bootstrap scores and save samples

nboot <- 1                    # number of bootstraps
bootscores <- list()          # store samples
for (b in 1:nboot) {
  bootdata <- items[[1]]
  cat("\r Bootstrap number",b, "of", nboot)
  bootsamp <- bootdata[sort(sample(nrow(bootdata), replace = TRUE)),]
  bootmod <- lavaan::cfa(tl.th.4, data = bootsamp, group = "cohortsex", estimator="wlsmv", parameterization="theta", group.equal='loadings')
  bootscores[[b]] <- cbind(bootsamp, do.call(rbind, lavaan::lavPredict(finalmod, newdata = bootdata)))
}
saveRDS(bootscores, file = paste0(dir_data, "bootscores.RData"))


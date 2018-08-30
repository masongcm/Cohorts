
############################################################################################
## ---- FA_BOOTSCORES
# bootstrap scores and save samples

nboot <- 5                    # number of bootstraps
bootscores <- list()          # store samples
for (b in 1:nboot) {
  bootdata <- fadata[[1]]
  cat("\r Bootstrap number",b, "of", nboot)
  bootsamp <- bootdata[sort(sample(nrow(bootdata), replace = TRUE)),]
  bootmod <- lavaan::cfa(tl.th.4, data = bootsamp, group = "cohortsex", estimator="wlsmv", parameterization="theta", group.equal='loadings')
  bootscores[[b]] <- cbind(bootsamp, do.call(rbind, lavaan::lavPredict(finalmod, newdata = bootdata)))
  bootscores[[b]] <- bootscores[[b]] %>% select(id, EXT, INT)
}
saveRDS(bootscores, file = paste0(dir_data, "bootscores.rds"))
rm(bootdata, bootsamp, bootmod, nboot)

############################################################################################
## ---- FA_BOOTSCORES_READ
# bootstrap scores and save samples
bootscores <- readRDS(file = paste0(dir_data, "bootscores.rds"))

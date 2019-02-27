## ---- BCSEFA_ALL
# full sample
bcs5data$bcs5_rutbAB <- bcs5data$bcs5_rutbA
bcs5data$bcs5_rutbAB[which(bcs5data$bcs5_rutbB == 0)] <- 0
items_bcs_all <- bcs5data[,
                          c( 
                            grep("bcs5_rutc", names(bcs5data), value=T),
                            "bcs5_rutbAB", "bcs5_rutbD"
                          )] %>%
  mutate_all(as.factor)

nbcs_all <- dim(items_bcs_all)[1]
bcscor_all <- hetcor(items_bcs_all, ML=TRUE, std.err = F)$correlations
bcsfa_all <- fa(r=bcscor_all, nfactors=5, rotate = "oblimin", fm="wls")

# names
rutnames1 <- c("Restless*", "Squirmy/Fidgety*", 
               "Destroys Belongings", "Fights**", "Not Liked",
               "Worried*", "Solitary*", "Irritable", "Unhappy*",
               "Steals", "Twitches", "Sucks thumbs", "Bites Nails",
               "Disobedient*", "Distracted*", "Fearful*", "Fussy",
               "Lies", "Bullies**")
names(rutnames1) <- paste0("bcs5_rutc", seq(1:19))
# rutnames2 <- c("Headaches**", "Stomachaches**", "Bilious", "Tantrums")
# names(rutnames2) <- paste0("bcs5_rutb", LETTERS[1:4])
rutnames2 <- c("Headaches, stomachaches**", "Tantrums**")
names(rutnames2) <- c("bcs5_rutbAB", "bcs5_rutbD")

rutnames <- c(rutnames1, rutnames2)
ruttitle <- c(as.character(seq(1:19)), "A+B", "D")

bcs_loadtab_all <- data.frame(cbind(ruttitle, rutnames, bcsfa_all$loadings))
colnames(bcs_loadtab_all) <- c("item", "title", "f1", "f2", "f3", "f4", "f5")

## ---- MCSEFA_ALL
to_na <- function(column) {
  ifelse(column %in% c(-9,-8,-1,4), NA, column)
}
rotation <- "oblimin"
items_mcs_all <- mcs5data[,
                          c( 
                            grep("mcs5_sdq([1-9]|[1-9][0-9])", names(mcs5data), value=T)
                          )] %>%
  mutate_all(to_na) %>%
  mutate_all(as.factor)

nmcs_all <- dim(items_mcs_all)[1]
mcscor_all <- hetcor(items_mcs_all, ML=TRUE, std.err = F)$correlations


mcsfa_all <- fa(r=mcscor_all, nfactors=5, rotate = rotation, fm="wls")

# names
sdqnames <- c("Considerate", "Restless*", "Headaches/stomachaches*",
              "Shares", "Tantrums", "Solitary*", "Obedient*",
              "Worried*", "Helpful", "Squirmy/Fidgety*", 
              "Good friend", "Fights/Bullies*", "Unhappy*",
              "Liked", "Distracted*", "Clingy", "Kind", "Lies",
              "Bullied", "Volunteers", "Thinks out", "Steals",
              "Adults", "Fearful*", "Sees through"
)
names(sdqnames) <- paste0("mcs5_sdq", seq(1:25))
sdqtitle <- seq(1:25)
mcs_loadtab_all <- data.frame(cbind(sdqtitle, sdqnames, mcsfa_all$loadings))
colnames(mcs_loadtab_all) <- c("item", "title", "f1", "f2", "f3", "f4", "f5")


## ---- NUMFAC_ALL
# Scree
ns_bcs_all <- nScree(bcscor_all)$Components
ns_mcs_all <- nScree(mcscor_all)$Components
vss_bcs_all <- VSS(bcscor_all, plot=F, rotate=rotation, n.obs = nbcs_all)
vss_mcs_all <- VSS(mcscor_all, plot=F, rotate=rotation, n.obs = nmcs_all)

#combine
efatab_all <- data.frame(cbind(
  rbind(t(ns_bcs_all), which.max(vss_bcs_all$cfit.1), which.max(vss_bcs_all$cfit.2), which.min(vss_bcs_all$map)),
  rbind(t(ns_mcs_all), which.max(vss_mcs_all$cfit.1), which.max(vss_mcs_all$cfit.2), which.min(vss_mcs_all$map))
))
rownames(efatab_all) <- c("Optimal Coordinates", "Acceleration Factor", "Parallel Analysis", "Kaiser", 
                          "VSS Compl. 1", "VSS Compl. 2", "Velicer MAP")


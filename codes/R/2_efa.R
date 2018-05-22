# FUNCTIONS TO PERFORM EFA

############################################################################################
## ---- BCSEFA
# choose rotation
rotation <- "oblimin"

# EFA with 11 item scale
items.bcs <- items.c[items.c$cohort=="BCS",c(grep("X[0-9]", names(items.c), value=T))]
nbcs <- dim(items.bcs)[1]
bcscor <- hetcor(items.bcs, ML=TRUE, std.err = F)$correlations
bcsfa <- fa(r=bcscor, nfactors=2, rotate = rotation, fm="wls")
#bcsfa3 <- fa(r=bcscor, nfactors=3, rotate = rotation, fm="wls")

items.bcs.m <- items.c[items.c$cohort=="BCS" & items.c$sex=="M",c(grep("X[0-9]", names(items.c), value=T))]
nbcs.m <- dim(items.bcs.m)[1]
bcscor.m <- hetcor(items.bcs.m, ML=TRUE, std.err = F)$correlations
bcsfa.m <- fa(r=bcscor.m, nfactors=2, rotate = rotation, fm="wls")

items.bcs.f <- items.c[items.c$cohort=="BCS" & items.c$sex=="F",c(grep("X[0-9]", names(items.c), value=T))]
nbcs.f <- dim(items.bcs.f)[1]
bcscor.f <- hetcor(items.bcs.f, ML=TRUE, std.err = F)$correlations
bcsfa.f <- fa(r=bcscor.f, nfactors=2, rotate = rotation, fm="wls")



## ---- BCSEFARES
# Scree
ns.bcs <- nScree(bcscor)$Components
ns.bcs.m <- nScree(bcscor.m)$Components
ns.bcs.f <- nScree(bcscor.f)$Components

# VSS for number of factors
vss.bcs <- VSS(bcscor, plot=F, rotate=rotation, n.obs = nbcs)
vss.bcs.m <- VSS(bcscor.m, plot=F, rotate=rotation, n.obs = nbcs.m)
vss.bcs.f <- VSS(bcscor.f, plot=F, rotate=rotation, n.obs = nbcs.f)
#combine
efatab.bcs <- cbind(
  rbind(t(ns.bcs), which.max(vss.bcs$cfit.1), which.max(vss.bcs$cfit.2), which.min(vss.bcs$map)),
  rbind(t(ns.bcs.m), which.max(vss.bcs.m$cfit.1), which.max(vss.bcs.m$cfit.2), which.min(vss.bcs.m$map)),
  rbind(t(ns.bcs.f), which.max(vss.bcs.f$cfit.1), which.max(vss.bcs.f$cfit.2), which.min(vss.bcs.f$map))
)
rownames(efatab.bcs) <- c("Optimal Coordinates", "Acceleration Factor", "Parallel Analysis", "Kaiser", 
                          "VSS Compl. 1", "VSS Compl. 2", "Velicer MAP")

# FACTOR ANALYSIS loadings
cat("\n\n ----------------------- \n BCS Loadings - Males and Females")
bcsfa$loadings
cat("\n\n ----------------------- \n BCS Loadings - Males")
bcsfa.m$loadings
cat("\n\n ----------------------- \n BCS Loadings - Females")
bcsfa.f$loadings

## ---- BCSREL
# reliability 
fz.bcs <- fisherz(bcscor)
fz.bcs[fz.bcs==Inf] <- NA 
fz.bcs.ext <- fz.bcs[1:6,1:6]
mr.bcs.ext <- fisherz2r(mean(fz.bcs.ext, na.rm=T))
fz.bcs.int <- fz.bcs[7:11,7:11]
mr.bcs.int <- fisherz2r(mean(fz.bcs.int, na.rm=T))

## ---- MCSEFA

# EFA with 11 item scale
items.mcs <- items.c[items.c$cohort=="MCS",c(grep("X[0-9]", names(items.c), value=T))]
nmcs <- dim(items.mcs)[1]
mcscor <- hetcor(items.mcs, ML=TRUE, std.err = F)$correlations
mcsfa <- fa(r=mcscor, nfactors=2, rotate = rotation, fm="wls")
#mcsfa3 <- fa(r=mcscor, nfactors=3, rotate = rotation, fm="wls")

items.mcs.m <- items.c[items.c$cohort=="MCS" & items.c$sex=="M",c(grep("X[0-9]", names(items.c), value=T))]
nmcs.m <- dim(items.mcs.m)[1]
mcscor.m <- hetcor(items.mcs.m, ML=TRUE, std.err = F)$correlations
mcsfa.m <- fa(r=mcscor.m, nfactors=2, rotate = rotation, fm="wls")

items.mcs.f <- items.c[items.c$cohort=="MCS" & items.c$sex=="F",c(grep("X[0-9]", names(items.c), value=T))]
nmcs.f <- dim(items.mcs.f)[1]
mcscor.f <- hetcor(items.mcs.f, ML=TRUE, std.err = F)$correlations
mcsfa.f <- fa(r=mcscor.f, nfactors=2, rotate = rotation, fm="wls")

## ---- MCSEFARES
# VSS for number of factors
ns.mcs <- nScree(mcscor)$Components
ns.mcs.m <- nScree(mcscor.m)$Components
ns.mcs.f <- nScree(mcscor.f)$Components

# VSS for number of factors
vss.mcs <- VSS(mcscor, plot=F, rotate=rotation, n.obs = nmcs)
vss.mcs.m <- VSS(mcscor.m, plot=F, rotate=rotation, n.obs = nmcs)
vss.mcs.f <- VSS(mcscor.f, plot=F, rotate=rotation, n.obs = nmcs)

#combine
efatab.mcs <- cbind(
  rbind(t(ns.mcs), which.max(vss.mcs$cfit.1), which.max(vss.mcs$cfit.2), which.min(vss.mcs$map)),
  rbind(t(ns.mcs.m), which.max(vss.mcs.m$cfit.1), which.max(vss.mcs.m$cfit.2), which.min(vss.mcs.m$map)),
  rbind(t(ns.mcs.f), which.max(vss.mcs.f$cfit.1), which.max(vss.mcs.f$cfit.2), which.min(vss.mcs.f$map))
)
  
rownames(efatab.mcs) <- c("Optimal Coordinates", "Acceleration Factor", "Parallel Analysis", "Kaiser", 
                          "VSS Compl. 1", "VSS Compl. 2", "Velicer MAP")

# FACTOR ANALYSIS
cat("\n\n ----------------------- \n MCS Loadings - Males and Females")
mcsfa$loadings
cat("\n\n ----------------------- \n MCS Loadings - Males")
mcsfa.m$loadings
cat("\n\n ----------------------- \n MCS Loadings - Females")
mcsfa.f$loadings

## ---- MCSREL
# reliability 
fz.mcs <- fisherz(mcscor)
fz.mcs[fz.mcs==Inf] <- NA 
fz.mcs.ext <- fz.mcs[1:6,1:6]
mr.mcs.ext <- fisherz2r(mean(fz.mcs.ext, na.rm=T))
fz.mcs.int <- fz.mcs[7:11,7:11]
mr.mcs.int <- fisherz2r(mean(fz.mcs.int, na.rm=T))


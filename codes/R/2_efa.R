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


## ---- BCSEFARES
# Scree
ns.bcs <- nScree(bcscor)$Components
# VSS for number of factors
vss.bcs <- VSS(bcscor, plot=F, rotate=rotation, n.obs = nbcs)
#combine
efatab.bcs <- rbind(t(ns.bcs), which.max(vss.bcs$cfit.1), which.max(vss.bcs$cfit.2))
rownames(efatab.bcs) <- c("Optimal Coordinates", "Acceleration Factor", "Parallel Analysis", "Kaiser", "VSS Compl. 1", "VSS Compl. 2")
efatab.bcs

# FACTOR ANALYSIS loadings
bcsfa$loadings

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

## ---- MCSEFARES
# VSS for number of factors
ns.mcs <- nScree(mcscor)$Components
ns.mcs
# VSS for number of factors
vss.mcs <- VSS(mcscor, plot=F, rotate=rotation, n.obs = nmcs)

#combine
efatab.mcs <- rbind(t(ns.mcs), which.max(vss.mcs$cfit.1), which.max(vss.mcs$cfit.2))
rownames(efatab.mcs) <- c("Optimal Coordinates", "Acceleration Factor", "Parallel Analysis", "Kaiser", "VSS Compl. 1", "VSS Compl. 2")
efatab.mcs

# FACTOR ANALYSIS
mcsfa$loadings

## ---- MCSREL
# reliability 
fz.mcs <- fisherz(mcscor)
fz.mcs[fz.mcs==Inf] <- NA 
fz.mcs.ext <- fz.mcs[1:6,1:6]
mr.mcs.ext <- fisherz2r(mean(fz.mcs.ext, na.rm=T))
fz.mcs.int <- fz.mcs[7:11,7:11]
mr.mcs.int <- fisherz2r(mean(fz.mcs.int, na.rm=T))


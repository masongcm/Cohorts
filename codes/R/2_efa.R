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
# VSS for number of factors
nScree(bcscor)
VSS(bcscor, plot=F, rotate=rotation, n.obs = nbcs)
# FACTOR ANALYSIS loadings
bcsfa$loadings


## ---- MCSEFA

# EFA with 11 item scale
items.mcs <- items.c[items.c$cohort=="MCS",c(grep("X[0-9]", names(items.c), value=T))]
nmcs <- dim(items.mcs)[1]
mcscor <- hetcor(items.mcs, ML=TRUE, std.err = F)$correlations
mcsfa <- fa(r=mcscor, nfactors=2, rotate = rotation, fm="wls")

## ---- MCSEFARES
# VSS for number of factors
nScree(mcscor)
VSS(mcscor, plot=F, rotate=rotation, n.obs = nmcs)
# FACTOR ANALYSIS
mcsfa$loadings



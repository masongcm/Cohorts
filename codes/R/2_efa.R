# FUNCTIONS TO PERFORM EFA

############################################################################################
## ---- BCSEFA
# choose rotation
rotation <- "oblimin"

# EFA with 11 item scale
items.c.bcs <- items.c[cohort=="BCS",!(names(items.c) %in% c("cohort"))]
nbcs <- dim(items.c.bcs)[1]
bcscor <- hetcor(items.c.bcs, ML=TRUE, std.err = F)$correlations
bcsfa <- fa(r=bcscor, nfactors=2, rotate = rotation, fm="wls")


## ---- BCSEFARES
# VSS for number of factors
nScree(bcscor)
VSS(bcscor, plot=F, rotate=rotation, n.obs = nbcs)
# FACTOR ANALYSIS loadings
bcsfa$loadings


## ---- MCSEFA

# EFA with 11 item scale
items.c.mcs <- items.c[cohort=="MCS",!(names(items.c) %in% c("cohort"))]
nmcs <- dim(items.c.mcs)[1]
mcscor <- hetcor(items.c.mcs, ML=TRUE, std.err = F)$correlations
mcsfa <- fa(r=mcscor, nfactors=2, rotate = rotation, fm="wls")

## ---- MCSEFARES
# VSS for number of factors
nScree(mcscor)
VSS(mcscor, plot=F, rotate=rotation, n.obs = nmcs)
# FACTOR ANALYSIS
mcsfa$loadings



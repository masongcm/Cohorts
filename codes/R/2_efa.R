# FUNCTIONS TO PERFORM EFA

############################################################################################
## ---- BCSEFA
# choose rotation
rotation <- "oblimin"

# EFA with 11 item scale
items.bcs <- cohdata[cohdata$cohort=="BCS",c(grep("X[0-9]", names(cohdata), value=T))]
nbcs <- dim(items.bcs)[1]
bcscor <- hetcor(items.bcs, ML=TRUE, std.err = F)$correlations
bcsfa <- fa(r=bcscor, nfactors=2, rotate = rotation, fm="wls")

items.bcs.m <- cohdata[cohdata$cohort=="BCS" & cohdata$sex=="M",c(grep("X[0-9]", names(cohdata), value=T))]
nbcs.m <- dim(items.bcs.m)[1]
bcscor.m <- hetcor(items.bcs.m, ML=TRUE, std.err = F)$correlations
bcsfa.m <- fa(r=bcscor.m, nfactors=2, rotate = rotation, fm="wls")

items.bcs.f <- cohdata[cohdata$cohort=="BCS" & cohdata$sex=="F",c(grep("X[0-9]", names(cohdata), value=T))]
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
items.mcs <- cohdata[cohdata$cohort=="MCS",c(grep("X[0-9]", names(cohdata), value=T))]
nmcs <- dim(items.mcs)[1]
mcscor <- hetcor(items.mcs, ML=TRUE, std.err = F)$correlations
mcsfa <- fa(r=mcscor, nfactors=2, rotate = rotation, fm="wls")

items.mcs.m <- cohdata[cohdata$cohort=="MCS" & cohdata$sex=="M",c(grep("X[0-9]", names(cohdata), value=T))]
nmcs.m <- dim(items.mcs.m)[1]
mcscor.m <- hetcor(items.mcs.m, ML=TRUE, std.err = F)$correlations
mcsfa.m <- fa(r=mcscor.m, nfactors=2, rotate = rotation, fm="wls")

items.mcs.f <- cohdata[cohdata$cohort=="MCS" & cohdata$sex=="F",c(grep("X[0-9]", names(cohdata), value=T))]
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

## ---- MCSREL
# reliability 
fz.mcs <- fisherz(mcscor)
fz.mcs[fz.mcs==Inf] <- NA 
fz.mcs.ext <- fz.mcs[1:6,1:6]
mr.mcs.ext <- fisherz2r(mean(fz.mcs.ext, na.rm=T))
fz.mcs.int <- fz.mcs[7:11,7:11]
mr.mcs.int <- fisherz2r(mean(fz.mcs.int, na.rm=T))


## ---- BCSEFA_ADD
# use only 1 factor
bcsfa1 <- fa(r=bcscor, nfactors=1, rotate = rotation, fm="wls")
bcsfa1.m <- fa(r=bcscor.m, nfactors=1, rotate = rotation, fm="wls")
bcsfa1.f <- fa(r=bcscor.f, nfactors=1, rotate = rotation, fm="wls")
# 3 factors for females
bcsfa3.f <- fa(r=bcscor.f, nfactors=3, rotate = rotation, fm="wls")

## ---- MCSEFA_ADD
# use only 1 factor
mcsfa1 <- fa(r=mcscor, nfactors=1, rotate = rotation, fm="wls")
mcsfa1.m <- fa(r=mcscor.m, nfactors=1, rotate = rotation, fm="wls")
mcsfa1.f <- fa(r=mcscor.f, nfactors=1, rotate = rotation, fm="wls")
# 3 factors for females
mcsfa3.f <- fa(r=mcscor.f, nfactors=3, rotate = rotation, fm="wls")

loadtab <- cbind(meantab$num, meantab$title, 
                 data.frame(cbind(
                   bcsfa1.m$loadings,
                   bcsfa1.f$loadings,
                   bcsfa3.f$loadings,
                   mcsfa1.m$loadings,
                   mcsfa1.f$loadings,
                   mcsfa3.f$loadings
                 ))
                 )
colnames(loadtab) <- c("item", "title", 
                       "bcs1m", "bcs1f", "bcs3f1", "bcs3f2", "bcs3f3",
                       "mcs1m", "mcs1f", "mcs3f1", "mcs3f2", "mcs3f3"
)

loadtab %>%
  mutate_at(vars(matches("cs")), round, 3) %>%
  mutate_at(
    vars(matches("cs")),
    function(x) cell_spec(x, "latex", bold = ifelse(abs(x) > .3, TRUE, FALSE))
    ) %>%
  kable("latex", escape = F, booktabs = T, linesep = "", align=c('c','l','c','c','c','c','c','c','c','c','c','c'),
        col.names = c("Item", "Title", 
                      "Males", "Females", "Females Factor 1", "Females Factor 2", "Females Factor 3",
                      "Males", "Females", "Females Factor 1", "Females Factor 2", "Females Factor 3"
                      )) %>%
  add_header_above(c(" " = 2,  
                     "BCS (1970) - 1 factor" = 2, "BCS (1970) - 3 factors" = 3, 
                     "MCS (2000/1) - 1 factor" = 2, "MCS (2000/1) - 3 factors" = 3))


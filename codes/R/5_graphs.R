
############################################################################################
## ---- AGEHIST
# histogram of age (months) by cohort

# make data
aged <- aggregate(scores2plot$cohortsex, list(age = scores2plot$age, cohort = scores2plot$cohort), FUN = length)
# convert to densities
aged$dens[aged$cohort == "BCS"] <-  aged[aged$cohort == "BCS", "x"]/ sum(aged[aged$cohort == "BCS", "x"])
aged$dens[aged$cohort == "MCS"] <-  aged[aged$cohort == "MCS", "x"]/ sum(aged[aged$cohort == "MCS", "x"])
aged[aged$cohort=="MCS", "dens"] <- - aged[aged$cohort=="MCS", "dens"] # flip MCS

ggplot(aged, aes(x=age, y=dens, fill=cohort)) + 
  ylab("Density") + scale_y_continuous(limits = c(-.5,.75)) +
  xlab("Age (months)") +
  geom_bar(stat="identity", position="identity") +
  theme(legend.position = c(0.9, 0.2)) +
  geom_hline(aes(yintercept=0))

## ---- FACDENS
# densities of factor scores
pdext.ebm.m <- ggplot(subset(scores2plot, sex=="M"), aes(x=EXT, group=cohort, fill=cohort, colour=cohort)) + geom_density(alpha = 0.1) + ggtitle("EXT Scores (Males)")
pdint.ebm.m <- ggplot(subset(scores2plot, sex=="M"), aes(x=INT, group=cohort, fill=cohort, colour=cohort)) + geom_density(alpha = 0.1) + ggtitle("INT Scores (Males)")
pdext.ebm.f <- ggplot(subset(scores2plot, sex=="F"), aes(x=EXT, group=cohort, fill=cohort, colour=cohort)) + geom_density(alpha = 0.1) + ggtitle("EXT Scores (Females)")
pdint.ebm.f <- ggplot(subset(scores2plot, sex=="F"), aes(x=INT, group=cohort, fill=cohort, colour=cohort)) + geom_density(alpha = 0.1) + ggtitle("INT Scores (Females)")
plot_grid(pdext.ebm.m, pdext.ebm.f, pdint.ebm.m, pdint.ebm.f, ncol=2, align="h")

## ---- FACLOESS_AGE
# loess plot of scores on age
loess.ext.m <- ggplot(data=subset(scores2plot, sex=="M"), aes(x=age, y=EXT, group=cohort, fill=cohort, colour=cohort) ) + geom_smooth(method = "loess") + xlab("Age (months)") + ylab("EXT") + ggtitle("Males Externalising")
loess.ext.f <- ggplot(data=subset(scores2plot, sex=="F"), aes(x=age, y=EXT, group=cohort, fill=cohort, colour=cohort) ) + geom_smooth(method = "loess") + xlab("Age (months)") + ylab("EXT") + ggtitle("Females Externalising") 
loess.int.m <- ggplot(data=subset(scores2plot, sex=="M"), aes(x=age, y=INT, group=cohort, fill=cohort, colour=cohort) ) + geom_smooth(method = "loess") + xlab("Age (months)") + ylab("INT") + ggtitle("Males Internalising")
loess.int.f <- ggplot(data=subset(scores2plot, sex=="F"), aes(x=age, y=INT, group=cohort, fill=cohort, colour=cohort) ) + geom_smooth(method = "loess") + xlab("Age (months)") + ylab("INT") + ggtitle("Females Internalising")

pcol <- plot_grid( loess.ext.m + theme(legend.position="none"),
                   loess.ext.f + theme(legend.position="none"),
                   loess.int.m + theme(legend.position="none"),
                   loess.int.f + theme(legend.position="none"),
                   align = 'vh',
                   hjust = -1,
                   nrow = 2
)
# add the legend underneath the row we made earlier. Give it 10% of the height of one plot (via rel_heights).
legend_b <- get_legend(loess.ext.m + theme(legend.position="bottom"))
p <- plot_grid( pcol, legend_b, ncol = 1, rel_heights = c(1, .1))
p

## ---- FACRAW
# see how scored factors compare with raw scores
box.ext.bcs <- ggplot(data=subset(scores2plot, cohort=="BCS"), aes(x=as.factor(EXT.RAW), y=EXT)) + geom_boxplot() + ggtitle("BCS EXT")
box.int.bcs <- ggplot(data=subset(scores2plot, cohort=="BCS"), aes(x=as.factor(INT.RAW), y=INT)) + geom_boxplot() + ggtitle("BCS INT")
box.ext.mcs <- ggplot(data=subset(scores2plot, cohort=="MCS"), aes(x=as.factor(EXT.RAW), y=EXT)) + geom_boxplot() + ggtitle("MCS EXT")
box.int.mcs <- ggplot(data=subset(scores2plot, cohort=="MCS"), aes(x=as.factor(INT.RAW), y=INT)) + geom_boxplot() + ggtitle("MCS INT")
plot_grid(box.ext.bcs, box.int.bcs, box.ext.mcs, box.int.mcs, ncol=2, align="h")

## ---- FACSCAT
scat.bcs.m <- ggplot(data=subset(scores2plot, cohort=="BCS" & sex=="M"), aes(EXT, INT)) + stat_binhex(bins=75) + ggtitle("BCS Males") + xlim(-3, 3) + ylim(-3, 3)
scat.mcs.m <- ggplot(data=subset(scores2plot, cohort=="MCS" & sex=="M"), aes(EXT, INT)) + stat_binhex(bins=75) + ggtitle("MCS Males") + xlim(-3, 3) + ylim(-3, 3)
scat.bcs.f <- ggplot(data=subset(scores2plot, cohort=="BCS" & sex=="F"), aes(EXT, INT)) + stat_binhex(bins=75) + ggtitle("BCS Females") + xlim(-3, 3) + ylim(-3, 3)
scat.mcs.f <- ggplot(data=subset(scores2plot, cohort=="MCS" & sex=="F"), aes(EXT, INT)) + stat_binhex(bins=75) + ggtitle("MCS Females") + xlim(-3, 3) + ylim(-3, 3)
plot_grid(scat.bcs.m + theme(legend.position="none") + coord_fixed(),
          scat.mcs.m + theme(legend.position="none") + coord_fixed(), 
          scat.bcs.f + theme(legend.position="none") + coord_fixed(), 
          scat.mcs.f + theme(legend.position="none") + coord_fixed(), 
          ncol=2, align="h")


## ---- FACINEQ2
ineq.ext.bcs <- ggplot(data=subset(facandraw, cohort=="BCS" & !is.na(facandraw$incq)), aes(x=as.factor(incq), y=EXT)) + ggtitle("BCS EXT") +
                geom_boxplot() + scale_x_discrete("Family Income Quintile at 10") + scale_y_continuous(limits = c(-3, 2))
ineq.int.bcs <- ggplot(data=subset(facandraw, cohort=="BCS" & !is.na(facandraw$incq)), aes(x=as.factor(incq), y=INT)) + ggtitle("BCS INT") +
                geom_boxplot() + scale_x_discrete("Family Income Quintile at 10") + scale_y_continuous(limits = c(-3, 2))
ineq.ext.mcs <- ggplot(data=subset(facandraw, cohort=="MCS" & !is.na(facandraw$incq)), aes(x=as.factor(incq), y=EXT)) + ggtitle("MCS EXT") +
                geom_boxplot() + scale_x_discrete("Family Income Quintile at 10") + scale_y_continuous(limits = c(-3, 2))
ineq.int.mcs <- ggplot(data=subset(facandraw, cohort=="MCS" & !is.na(facandraw$incq)), aes(x=as.factor(incq), y=INT)) + ggtitle("MCS INT") +
                geom_boxplot() + scale_x_discrete("Family Income Quintile at 10") + scale_y_continuous(limits = c(-3, 2))

plot_grid(ineq.ext.bcs, ineq.ext.mcs, ineq.int.bcs, ineq.int.mcs, ncol=2, align="h")

## ---- FACINEQ
# boxplots of scores by gender and cohort

#common options
addopts <- function(x) {
  x <- x + geom_boxplot() + scale_x_discrete("Family Income Quintile at 10") + scale_y_continuous(name = "Factor score", limits = c(-3.5, 2.2)) +
      stat_summary(fun.y=mean, geom="point", size=4, fill="white", color="black", aes(shape=cohort),position=position_dodge(.8))+
      labs(list(fill="", shape="")) + theme(legend.position="none")
  return(x)
} 
ineq.ext.m <- ggplot(data=subset(scores2plot, sex=="M" & !is.na(scores2plot$incq)), aes(x=as.factor(incq), y=EXT, fill=cohort)) + ggtitle("Males Externalising")
ineq.ext.f <- ggplot(data=subset(scores2plot, sex=="F" & !is.na(scores2plot$incq)), aes(x=as.factor(incq), y=EXT, fill=cohort)) + ggtitle("Females Externalising")
ineq.int.m <- ggplot(data=subset(scores2plot, sex=="M" & !is.na(scores2plot$incq)), aes(x=as.factor(incq), y=INT, fill=cohort)) + ggtitle("Males Internalising")
ineq.int.f <- ggplot(data=subset(scores2plot, sex=="F" & !is.na(scores2plot$incq)), aes(x=as.factor(incq), y=INT, fill=cohort)) + ggtitle("Females Internalising")

ineqlist <- list(ineq.ext.m, ineq.ext.f, ineq.int.m, ineq.int.f) 
ineqlist <- lapply(ineqlist, addopts) # apply options to all graphs

# arrange the plots in a single column
pcol <- plot_grid( ineqlist[[1]],ineqlist[[2]],ineqlist[[3]],ineqlist[[4]],
                   align = 'vh',
                   hjust = -1,
                   nrow = 2
)
# add the legend underneath the row we made earlier. Give it 10% of the height of one plot (via rel_heights).
legend_b <- get_legend(ineqlist[[1]] + theme(legend.position="bottom"))
p <- plot_grid( pcol, legend_b, ncol = 1, rel_heights = c(1, .1))
p

## ---- FACLOESS_INC

# loess plot of scores on income
l.inc.ext.m <- ggplot(data=subset(scores2plot, sex=="M"), aes(x=faminc_real, y=EXT, group=cohort, fill=cohort, colour=cohort) ) + ylab("EXT") + ggtitle("Males Externalising")
l.inc.ext.f <- ggplot(data=subset(scores2plot, sex=="F"), aes(x=faminc_real, y=EXT, group=cohort, fill=cohort, colour=cohort) ) + ylab("EXT") + ggtitle("Females Externalising")
l.inc.int.m <- ggplot(data=subset(scores2plot, sex=="M"), aes(x=faminc_real, y=INT, group=cohort, fill=cohort, colour=cohort) ) + ylab("INT") + ggtitle("Males Internalising")
l.inc.int.f <- ggplot(data=subset(scores2plot, sex=="F"), aes(x=faminc_real, y=INT, group=cohort, fill=cohort, colour=cohort) ) + ylab("INT") + ggtitle("Females Internalising")
l.inc <- list(l.inc.ext.m, l.inc.ext.f, l.inc.int.m, l.inc.int.f)
l.inc <- lapply(l.inc, 
                 function(x) x + theme(legend.position="none") +
                   geom_smooth(method = "loess") + 
                   xlab("Weekly Family Income at 10 (,000£ 2015)")
                 ) # apply options to all graphs 

pcol <- plot_grid( l.inc[[1]], l.inc[[2]], l.inc[[3]], l.inc[[4]],
                   align = 'vh',
                   hjust = -1,
                   nrow = 2
)
legend_b <- get_legend(l.inc[[1]] + theme(legend.position="bottom"))
p <- plot_grid( pcol, legend_b, ncol = 1, rel_heights = c(1, .1))


pcol

## ---- FACNPREG
# npreg plot of scores on income
npr.ext.bcs <- npreg(EXT~faminc, data = subset(items.scored, cohort=="BCS"), regtype = "ll")
npr.ext.mcs <- npreg(EXT~faminc, data = subset(items.scored, cohort=="MCS"), regtype = "ll")
npr.int.bcs <- npreg(INT~faminc, data = subset(items.scored, cohort=="BCS"), regtype = "ll")
npr.int.mcs <- npreg(INT~faminc, data = subset(items.scored, cohort=="MCS"), regtype = "ll")

# extract fitted data
dat.ext.bcs <- plot(npr.ext.bcs, plot.errors.method="bootstrap", plot.behavior = "data", neval=200)
dat.int.bcs <- plot(npr.int.bcs, plot.errors.method="bootstrap", plot.behavior = "data", neval=200)
dat.ext.mcs <- plot(npr.ext.mcs, plot.errors.method="bootstrap", plot.behavior = "data", neval=200)
dat.int.mcs <- plot(npr.int.mcs, plot.errors.method="bootstrap", plot.behavior = "data", neval=200)

# assemble fitted data
pdat.bcs <- data.frame(cbind(
  eval = unlist(dat.ext.bcs$r1$eval,use.names = F),   # evaluation points (same for EXT and INT, checked)
  mEXT = dat.ext.bcs$r1$mean,
  ciuEXT = dat.ext.bcs$r1$mean+as.numeric(dat.ext.bcs$r1$merr[,2]),
  cilEXT = dat.ext.bcs$r1$mean+as.numeric(dat.ext.bcs$r1$merr[,1]),
  mINT = dat.int.bcs$r1$mean,
  ciuINT = dat.int.bcs$r1$mean+as.numeric(dat.int.bcs$r1$merr[,2]),
  cilINT = dat.int.bcs$r1$mean+as.numeric(dat.int.bcs$r1$merr[,1])
  ),
cohort= "BCS"
)

pdat.mcs <- data.frame(cbind(
  eval = unlist(dat.ext.mcs$r1$eval,use.names = F),   # evaluation points (same for EXT and INT, checked)
  mEXT = dat.ext.mcs$r1$mean,
  ciuEXT = dat.ext.mcs$r1$mean+as.numeric(dat.ext.mcs$r1$merr[,2]),
  cilEXT = dat.ext.mcs$r1$mean+as.numeric(dat.ext.mcs$r1$merr[,1]),
  mINT = dat.int.mcs$r1$mean,
  ciuINT = dat.int.mcs$r1$mean+as.numeric(dat.int.mcs$r1$merr[,2]),
  cilINT = dat.int.mcs$r1$mean+as.numeric(dat.int.mcs$r1$merr[,1])
  ),
  cohort= "MCS"
)

pdat <- rbind(pdat.bcs,pdat.mcs)
pdat$eval <- as.numeric(as.character(pdat$eval))
pdat <- pdat[with(pdat, order(pdat$eval)), ]      # sort by income

            
npp.ext <- ggplot(pdat, aes(x=eval, y=mEXT, group=cohort, fill=cohort, colour=cohort)) + geom_line() 
npp.ext <- npp.ext + geom_ribbon(data=pdat, aes(ymin=cilEXT,ymax=ciuEXT) , alpha=0.3)
npp.ext <- npp.ext + xlab("Weekly Family Income at 10 (,000£ 2010)") + ylab("EXT") + coord_cartesian(ylim = c(-1, 1)) 

npp.int <- ggplot(pdat, aes(x=eval, y=mINT, group=cohort, fill=cohort, colour=cohort)) + geom_line() 
npp.int <- npp.int + geom_ribbon(data=pdat, aes(ymin=cilINT,ymax=ciuINT) , alpha=0.3)
npp.int <- npp.int + xlab("Weekly Family Income at 10 (,000£ 2010)") + ylab("INT") + coord_cartesian(ylim = c(-1, 1)) 

plot_grid(npp.ext, npp.int, ncol=2, align="h")



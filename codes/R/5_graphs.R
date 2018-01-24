
############################################################################################
## ---- FACDENS
# densities of factor scores (residuals)
pdext.ebm <- ggplot(items.scored, aes(x=EXTr, group=cohort, fill=cohort, colour=cohort)) + geom_density(alpha = 0.1) + ggtitle("EXT (EBM Scores)")
pdint.ebm <- ggplot(items.scored, aes(x=INTr, group=cohort, fill=cohort, colour=cohort)) + geom_density(alpha = 0.1) + ggtitle("INT (EBM Scores)")
plot_grid(pdext.ebm, pdint.ebm, ncol=2, align="h")

## ---- FACRAW
# see how scored factors compare with raw scores
box.ext.bcs <- ggplot(data=subset(items.scored, cohort=="BCS"), aes(x=as.factor(EXT.RAW), y=EXTr)) + geom_boxplot() + ggtitle("BCS EXT")
box.int.bcs <- ggplot(data=subset(items.scored, cohort=="BCS"), aes(x=as.factor(INT.RAW), y=INTr)) + geom_boxplot() + ggtitle("BCS INT")
box.ext.mcs <- ggplot(data=subset(items.scored, cohort=="MCS"), aes(x=as.factor(EXT.RAW), y=EXTr)) + geom_boxplot() + ggtitle("MCS EXT")
box.int.mcs <- ggplot(data=subset(items.scored, cohort=="MCS"), aes(x=as.factor(INT.RAW), y=INTr)) + geom_boxplot() + ggtitle("MCS INT")
plot_grid(box.ext.bcs, box.int.bcs, box.ext.mcs, box.int.mcs, ncol=2, align="h")

## ---- FACSCAT
scat.bcs <- ggplot(data=subset(items.scored, cohort=="BCS"), aes(EXTr, INTr)) + stat_binhex(bins=75) + ggtitle("BCS") + xlim(-3, 1.9) + ylim(-3.5, 1.4)
scat.mcs <- ggplot(data=subset(items.scored, cohort=="MCS"), aes(EXTr, INTr)) + stat_binhex(bins=75) + ggtitle("MCS") + xlim(-3, 1.9) + ylim(-3.5, 1.4)
plot_grid(scat.bcs, scat.mcs, ncol=2, align="h")

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
ineq.ext.m <- ggplot(data=subset(items.scored, sex==1 & !is.na(items.scored$incq)), aes(x=as.factor(incq), y=EXTr, fill=cohort)) + ggtitle("Males Externalising")
ineq.ext.f <- ggplot(data=subset(items.scored, sex==2 & !is.na(items.scored$incq)), aes(x=as.factor(incq), y=EXTr, fill=cohort)) + ggtitle("Females Externalising")
ineq.int.m <- ggplot(data=subset(items.scored, sex==1 & !is.na(items.scored$incq)), aes(x=as.factor(incq), y=INTr, fill=cohort)) + ggtitle("Males Internalising")
ineq.int.f <- ggplot(data=subset(items.scored, sex==2 & !is.na(items.scored$incq)), aes(x=as.factor(incq), y=INTr, fill=cohort)) + ggtitle("Females Internalising")

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


## ---- FACLOESS
# loess plot of scores on income
loess.ext <- ggplot(data=items.scored, aes(x=faminc, y=EXTr, group=cohort, fill=cohort, colour=cohort) ) + geom_smooth(method = "loess") + xlab("Weekly Family Income at 10 (,000£ 2015)") + ylab("EXT")
loess.int <- ggplot(data=items.scored, aes(x=faminc, y=INTr, group=cohort, fill=cohort, colour=cohort) ) + geom_smooth(method = "loess") + xlab("Weekly Family Income at 10 (,000£ 2015)") + ylab("INT")
plot_grid(loess.ext, loess.int, ncol=2, align="h")

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



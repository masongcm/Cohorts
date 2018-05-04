
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


############################################################################################
## ---- RAWHIST
# histograms for raw scores

# common options
addopts.raw <- function(x) {
  x <- x +
    theme(
      axis.title.y=element_blank(),
      legend.position="none"
    ) +
    scale_x_continuous(name = "Raw score", breaks = seq(0,10,1)) +
    scale_fill_discrete("") + # remove fill guide title
    scale_colour_discrete(guide=FALSE) +  # remove colour legend
    geom_bar(position="dodge", aes(y = ..prop..)) # PLOTS
  return(x)
}

rawext.m <- ggplot(subset(scores2plot, sex=="M"),aes(x=EXT.RAW, fill=cohort)) + ggtitle("EXT Scores (Males)") + coord_cartesian(ylim = c(0,.25))
rawint.m <- ggplot(subset(scores2plot, sex=="M"),aes(x=INT.RAW, fill=cohort)) + ggtitle("INT Scores (Males)") + coord_cartesian(ylim = c(0,.4))
rawext.f <- ggplot(subset(scores2plot, sex=="F"),aes(x=EXT.RAW, fill=cohort)) + ggtitle("EXT Scores (Females)") + coord_cartesian(ylim = c(0,.25))
rawint.f <- ggplot(subset(scores2plot, sex=="F"),aes(x=INT.RAW, fill=cohort)) + ggtitle("INT Scores (Females)") + coord_cartesian(ylim = c(0,.4))
rawlist <- list(rawext.m, rawint.m, rawext.f, rawint.f) 
rawlist <- lapply(rawlist, addopts.raw) # apply options to all graphs

pcol <- plot_grid( rawlist[[1]], rawlist[[2]], rawlist[[3]], rawlist[[4]],
                   align = 'vh',
                   hjust = -1,
                   nrow = 2
)

# add legend
legend_b <- get_legend(rawlist[[1]] + theme(legend.position="bottom"))
p <- plot_grid( pcol, legend_b, ncol = 1, rel_heights = c(1, .1))
p


############################################################################################
## ---- FACDENS
# densities of scored factors

# Kolmogorov-Smirnov test
ksp <- list()
for (g in c("M", "F")) {
  for (f in c("EXT","INT")) {
    cs1 <- paste0("BCS.",g)
    cs2 <- paste0("MCS.",g)
    pval <- round(ks.test(scores2plot[scores2plot$cohortsex==cs1,f],
                                  scores2plot[scores2plot$cohortsex==cs2,f]
    )$p.value,3)
    if (pval>.0001) ksp[[paste0(f,".",g)]] <- toString(pval)
    else ksp[[paste0(f,".",g)]] <- "<0.0001"
  }
}
# graph x axis boundaries
maxx <- max(scores2plot[names(scores2plot) %in% c("EXT","INT")]) +.05
minx <- min(scores2plot[names(scores2plot) %in% c("EXT","INT")]) -.05

# common options
addopts.dens <- function(x) {
  x <- x +
    theme(
      axis.title.y=element_blank(),
      legend.position="none"
    ) +
    scale_x_continuous(name = "Quantile", breaks = seq(-3.5,1.5,.5)) +
    coord_cartesian(xlim = c(minx, maxx), ylim = c(0,.62)) +
    scale_fill_discrete("") + # remove fill guide title
    scale_colour_discrete(guide=FALSE) +  # remove colour legend
    geom_density(alpha = 0.1) # PLOTS
  return(x)
}

# densities of factor scores
pdext.ebm.m <- ggplot(subset(scores2plot, sex=="M"), aes(x=EXT, group=cohort, fill=cohort, colour=cohort)) + ggtitle("EXT Scores (Males)")
pdint.ebm.m <- ggplot(subset(scores2plot, sex=="M"), aes(x=INT, group=cohort, fill=cohort, colour=cohort)) + ggtitle("INT Scores (Males)")
pdext.ebm.f <- ggplot(subset(scores2plot, sex=="F"), aes(x=EXT, group=cohort, fill=cohort, colour=cohort)) + ggtitle("EXT Scores (Females)")
pdint.ebm.f <- ggplot(subset(scores2plot, sex=="F"), aes(x=INT, group=cohort, fill=cohort, colour=cohort)) + ggtitle("INT Scores (Females)")
denslist <- list(pdext.ebm.m, pdint.ebm.m, pdext.ebm.f, pdint.ebm.f) 
denslist <- lapply(denslist, addopts.dens) # apply options to all graphs
# add KS pvalue
for (p in 1:4) {
  denslist[[p]] <- denslist[[p]] + 
    annotate("text", x = minx+.5, y = .5, label = "KS p-value") + 
    annotate("text", x = minx+.5, y = .45, label = ksp[[p]])
}
pcol <- plot_grid( denslist[[1]], denslist[[2]], denslist[[3]], denslist[[4]],
                   align = 'vh',
                   hjust = -1,
                   nrow = 2
)
# add legend
legend_b <- get_legend(denslist[[1]] + theme(legend.position="bottom"))
p <- plot_grid( pcol, legend_b, ncol = 1, rel_heights = c(1, .1))
p

############################################################################################
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

############################################################################################
## ---- FACRAW
# see how scored factors compare with raw scores
box.ext.bcs <- ggplot(data=subset(scores2plot, cohort=="BCS"), aes(x=as.factor(EXT.RAW), y=EXT)) + geom_boxplot() + ggtitle("BCS EXT") + xlab("Sum score") + ylab("Factor score")
box.int.bcs <- ggplot(data=subset(scores2plot, cohort=="BCS"), aes(x=as.factor(INT.RAW), y=INT)) + geom_boxplot() + ggtitle("BCS INT") + xlab("Sum score") + ylab("Factor score")
box.ext.mcs <- ggplot(data=subset(scores2plot, cohort=="MCS"), aes(x=as.factor(EXT.RAW), y=EXT)) + geom_boxplot() + ggtitle("MCS EXT") + xlab("Sum score") + ylab("Factor score")
box.int.mcs <- ggplot(data=subset(scores2plot, cohort=="MCS"), aes(x=as.factor(INT.RAW), y=INT)) + geom_boxplot() + ggtitle("MCS INT") + xlab("Sum score") + ylab("Factor score")
plot_grid(box.ext.bcs, box.int.bcs, box.ext.mcs, box.int.mcs, ncol=2, align="h")


############################################################################################
## ---- INCINEQ
# MEAN/CI plot of income by income quintile

# aggregate mean income
semean <- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))
meaninc <- data.frame(
  aggregate(scores2plot$faminc10_real, 
            by = list(scores2plot$cohort, scores2plot$incq), 
            FUN=function(x) c(m = mean(x, na.rm=T), se = semean(x) )))
meaninc <- cbind(meaninc[1:2], as.matrix(meaninc$x))
colnames(meaninc) <- c("cohort", "quant", "mean", "semean")

# restandardise
q1.bcs <- meaninc[meaninc$cohort=="BCS" & meaninc$quant=="1", "mean"]
q1.mcs <- meaninc[meaninc$cohort=="MCS" & meaninc$quant=="1", "mean"]
meaninc[meaninc$cohort=="BCS", "mean"] <- (meaninc[meaninc$cohort=="BCS", "mean"] - q1.bcs)/q1.bcs
meaninc[meaninc$cohort=="MCS", "mean"] <- (meaninc[meaninc$cohort=="MCS", "mean"] - q1.mcs)/q1.mcs
meaninc$ciu <- meaninc$mean + 1.96*meaninc$semean
meaninc$cil <- meaninc$mean - 1.96*meaninc$semean

ineq.inc <- ggplot(data=meaninc, aes(x=as.factor(quant), y=mean, colour=cohort)) +
  geom_point(size=3) + 
  scale_y_continuous(name = "Mean Family Income", breaks = seq(0,11,2)) +
  xlab("Family Income Quintile at 10") +
  theme(legend.justification=c(0,0), legend.position=c(0,.8), legend.title = element_blank())

ineq.inc


############################################################################################
## ---- FACINEQ
# MEAN/CI plot of scores by income quintile

#common options
addopts <- function(x) {
  x <- x + xlab("Family Income Quintile at 10") + ylab("Factor score") +
    coord_cartesian(ylim = c(-.4, .5)) +
    stat_summary(geom="errorbar", fun.data=mean_cl_normal, width=.2, position=position_dodge(.5)) +
    stat_summary(fun.y=mean, geom="point", size=4, aes(colour=cohort), position=position_dodge(.5)) +
    labs(list(colour="")) + theme(legend.position="none")
  return(x)
}

ineq.ext.m <- ggplot(data=subset(scores2plot, sex=="M" & !is.na(scores2plot$incq10)), aes(x=as.factor(incq10), y=EXT, colour=cohort)) + ggtitle("Males Externalising")
ineq.ext.f <- ggplot(data=subset(scores2plot, sex=="F" & !is.na(scores2plot$incq10)), aes(x=as.factor(incq10), y=EXT, colour=cohort)) + ggtitle("Females Externalising")
ineq.int.m <- ggplot(data=subset(scores2plot, sex=="M" & !is.na(scores2plot$incq10)), aes(x=as.factor(incq10), y=INT, colour=cohort)) + ggtitle("Males Internalising")
ineq.int.f <- ggplot(data=subset(scores2plot, sex=="F" & !is.na(scores2plot$incq10)), aes(x=as.factor(incq10), y=INT, colour=cohort)) + ggtitle("Females Internalising")

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


############################################################################################
## ---- FACINEQ_SC
# MEAN/CI plot of scores by social class

# get mean by cohortsex to rescale
scoreslowclass <- scores2plot[scores2plot$scl10b=="IV V",]
meansfac <- aggregate(scoreslowclass[,c("EXT","INT")], by=list(scoreslowclass$cohortsex), FUN = function(x) mean(x, na.rm=T))
rownames(meansfac) <- meansfac$Group.1
meansfac <- meansfac[,-1]

# rescale scores so that mean in lowest level is 0
scores2plot$EXTdsc <- NA
scores2plot$INTdsc <- NA
for (c in c("BCS","MCS")) {
  for (f in c("EXT","INT")) {
    for (g in c("M","F")) {
      cs <- paste0(c,".",g)
      fd <- paste0(f,"dsc")
      scores2plot[scores2plot$cohortsex==cs,fd] <- scores2plot[scores2plot$cohortsex==cs,f]-meansfac[cs,f]
    }
  }
}

#common options
addopts <- function(x) {
  x <- x + xlab("Parental Social Class at 10") + ylab("Factor score (IV V = 0)") +
    coord_cartesian(ylim = c(-.4, .5)) +
    stat_summary(geom="errorbar", fun.data=mean_cl_normal, width=.2, position=position_dodge(.5)) +
    stat_summary(fun.y=mean, geom="point", size=4, aes(colour=cohort), position=position_dodge(.5)) +
    labs(list(colour="")) + theme(legend.position="none")
  return(x)
}

ineq.ext.m <- ggplot(data=subset(scores2plot, sex=="M" & !is.na(scores2plot$scl10b)), aes(x=as.factor(scl10b), y=EXTdsc, colour=cohort)) + ggtitle("Males Externalising")
ineq.ext.f <- ggplot(data=subset(scores2plot, sex=="F" & !is.na(scores2plot$scl10b)), aes(x=as.factor(scl10b), y=EXTdsc, colour=cohort)) + ggtitle("Females Externalising")
ineq.int.m <- ggplot(data=subset(scores2plot, sex=="M" & !is.na(scores2plot$scl10b)), aes(x=as.factor(scl10b), y=INTdsc, colour=cohort)) + ggtitle("Males Internalising")
ineq.int.f <- ggplot(data=subset(scores2plot, sex=="F" & !is.na(scores2plot$scl10b)), aes(x=as.factor(scl10b), y=INTdsc, colour=cohort)) + ggtitle("Females Internalising")

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

############################################################################################
## ---- FACINEQ_YS
# MEAN/CI plot of scores by maternal years of schooling

scores2plot$mysch5b <- cut(scores2plot$mysch5, c(-1,0,2,5,15))
levels(scores2plot$mysch5b) <- c("Compuls.", "C - C+2", "C+3 - C+5", ">C+5")

# get mean by cohortsex to rescale
scoreslowschool <- scores2plot[scores2plot$mysch5b=="Compuls.",]
meansfac <- aggregate(scoreslowschool[,c("EXT","INT")], by=list(scoreslowschool$cohortsex), FUN = function(x) mean(x, na.rm=T))
rownames(meansfac) <- meansfac$Group.1
meansfac <- meansfac[,-1]

# rescale scores so that mean in lowest level is 0
scores2plot$EXTdys <- NA
scores2plot$INTdys <- NA
for (c in c("BCS","MCS")) {
  for (f in c("EXT","INT")) {
    for (g in c("M","F")) {
      cs <- paste0(c,".",g)
      fd <- paste0(f,"dys")
      scores2plot[scores2plot$cohortsex==cs,fd] <- scores2plot[scores2plot$cohortsex==cs,f]-meansfac[cs,f]
    }
  }
}


#common options
addopts <- function(x) {
  x <- x + xlab("Maternal years of schooling at 5") +
    scale_y_continuous(name = "Factor score (Compuls.=0)", breaks = seq(-.2,.8,.2)) +
    coord_cartesian(ylim = c(-.2, .8)) +
    stat_summary(geom="errorbar", fun.data=mean_cl_normal, width=.2, position=position_dodge(.5)) +
    stat_summary(fun.y=mean, geom="point", size=4, aes(colour=cohort), position=position_dodge(.5)) +
    labs(list(colour="")) + theme(legend.position="none")
  return(x)
}

ineq.ext.m <- ggplot(data=subset(scores2plot, sex=="M" & !is.na(scores2plot$mysch5b)), aes(x=as.factor(mysch5b), y=EXTdys, colour=cohort)) + ggtitle("Males Externalising")
ineq.ext.f <- ggplot(data=subset(scores2plot, sex=="F" & !is.na(scores2plot$mysch5b)), aes(x=as.factor(mysch5b), y=EXTdys, colour=cohort)) + ggtitle("Females Externalising")
ineq.int.m <- ggplot(data=subset(scores2plot, sex=="M" & !is.na(scores2plot$mysch5b)), aes(x=as.factor(mysch5b), y=INTdys, colour=cohort)) + ggtitle("Males Internalising")
ineq.int.f <- ggplot(data=subset(scores2plot, sex=="F" & !is.na(scores2plot$mysch5b)), aes(x=as.factor(mysch5b), y=INTdys, colour=cohort)) + ggtitle("Females Internalising")

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

############################################################################################
## ---- LOESS_REAL
require(grid)
require(gridExtra)

# limits for plots
inc.lims <- c(0, 2.0)

# loess plot of scores on income
l.inc.ext.m <- ggplot(data=subset(scores2plot, sex=="M"), aes(x=faminc10_real, y=EXT, group=cohort, fill=cohort, colour=cohort) ) + ylab("EXT") + ggtitle("Males Externalising")
l.inc.ext.f <- ggplot(data=subset(scores2plot, sex=="F"), aes(x=faminc10_real, y=EXT, group=cohort, fill=cohort, colour=cohort) ) + ylab("EXT") + ggtitle("Females Externalising")
l.inc.int.m <- ggplot(data=subset(scores2plot, sex=="M"), aes(x=faminc10_real, y=INT, group=cohort, fill=cohort, colour=cohort) ) + ylab("INT") + ggtitle("Males Internalising")
l.inc.int.f <- ggplot(data=subset(scores2plot, sex=="F"), aes(x=faminc10_real, y=INT, group=cohort, fill=cohort, colour=cohort) ) + ylab("INT") + ggtitle("Females Internalising")

l.inc <- list(l.inc.ext.m, l.inc.ext.f, l.inc.int.m, l.inc.int.f)
l.inc <- lapply(l.inc, 
                 function(x) x + theme(legend.position="none") + labs(list(fill="", colour = "")) +
                   geom_smooth(method = "loess") + 
                   coord_cartesian(xlim = inc.lims, ylim = c(-.6, .6)) + #axis limits withouth affecting sample
                   xlab("Weekly Family Income at 10 (,000£ 2015 CPI+GDP)") +
                   ylab("Factor Score")
                 ) # apply options to all graphs 

# densities of income
theme_dens <- theme(axis.line.x=element_blank(),axis.text.x=element_blank(),
                    axis.text.y=element_text(colour="white"), axis.title.y=element_text(colour="white"),
                    axis.ticks=element_blank(),
                    legend.position = "none")
addopts_dens <- function(x) x + ylab("Density") + xlab("") + scale_y_reverse() + coord_cartesian(xlim = inc.lims) + theme_dens

dens.m <- ggplot(subset(scores2plot, sex=="M"), aes(x=faminc10_real, group=cohort, fill=cohort, colour=cohort)) + geom_density(alpha = 0.1)
dens.m <- addopts_dens(dens.m)
dens.f <- ggplot(subset(scores2plot, sex=="F"), aes(x=faminc10_real, group=cohort, fill=cohort, colour=cohort)) + geom_density(alpha = 0.1)
dens.f <- addopts_dens(dens.f)

# EXT plots
pcol <- plot_grid(l.inc[[1]], l.inc[[2]], dens.m, dens.f, ncol=2, nrow=2, rel_heights=c(4, 1))
legend_b <- get_legend(l.inc[[1]] + theme(legend.position="bottom"))
ploess.real.ext <- plot_grid( pcol, legend_b, ncol = 1, rel_heights = c(1, .1))

# INT plots
pcol <- plot_grid(l.inc[[3]], l.inc[[4]], dens.m, dens.f, ncol=2, nrow=2, rel_heights=c(4, 1))
legend_b <- get_legend(l.inc[[3]] + theme(legend.position="bottom"))
ploess.real.int <- plot_grid( pcol, legend_b, ncol = 1, rel_heights = c(1, .1))



############################################################################################
## ---- LOESS_INFL

# limits for plots
inc.lims <- c(0, 2.0)

# loess plot of scores on income
l.inc.ext.m <- ggplot(data=subset(scores2plot, sex=="M"), aes(x=faminc10_infl, y=EXT, group=cohort, fill=cohort, colour=cohort) ) + ylab("EXT") + ggtitle("Males Externalising")
l.inc.ext.f <- ggplot(data=subset(scores2plot, sex=="F"), aes(x=faminc10_infl, y=EXT, group=cohort, fill=cohort, colour=cohort) ) + ylab("EXT") + ggtitle("Females Externalising")
l.inc.int.m <- ggplot(data=subset(scores2plot, sex=="M"), aes(x=faminc10_infl, y=INT, group=cohort, fill=cohort, colour=cohort) ) + ylab("INT") + ggtitle("Males Internalising")
l.inc.int.f <- ggplot(data=subset(scores2plot, sex=="F"), aes(x=faminc10_infl, y=INT, group=cohort, fill=cohort, colour=cohort) ) + ylab("INT") + ggtitle("Females Internalising")

l.inc <- list(l.inc.ext.m, l.inc.ext.f, l.inc.int.m, l.inc.int.f)
l.inc <- lapply(l.inc, 
                function(x) x + theme(legend.position="none") + labs(list(fill="", colour = "")) +
                  geom_smooth(method = "loess") + 
                  coord_cartesian(xlim = inc.lims, ylim = c(-.6, .6)) + #axis limits withouth affecting sample
                  xlab("Weekly Family Income at 10 (,000£ 2015 CPI)") +
                  ylab("Factor Score")
) # apply options to all graphs 

# densities of income
theme_dens <- theme(axis.line.x=element_blank(),axis.text.x=element_blank(),
                    axis.text.y=element_text(colour="white"), axis.title.y=element_text(colour="white"),
                    axis.ticks=element_blank(),
                    legend.position = "none")
addopts_dens <- function(x) x + ylab("Density") + xlab("") + scale_y_reverse() + coord_cartesian(xlim = inc.lims) + theme_dens

dens.m <- ggplot(subset(scores2plot, sex=="M"), aes(x=faminc10_infl, group=cohort, fill=cohort, colour=cohort)) + geom_density(alpha = 0.1)
dens.m <- addopts_dens(dens.m)
dens.f <- ggplot(subset(scores2plot, sex=="F"), aes(x=faminc10_infl, group=cohort, fill=cohort, colour=cohort)) + geom_density(alpha = 0.1)
dens.f <- addopts_dens(dens.f)

# EXT plots
pcol <- plot_grid(l.inc[[1]], l.inc[[2]], dens.m, dens.f, ncol=2, nrow=2, rel_heights=c(4, 1))
legend_b <- get_legend(l.inc[[1]] + theme(legend.position="bottom"))
ploess.infl.ext <- plot_grid( pcol, legend_b, ncol = 1, rel_heights = c(1, .1))

# INT plots
pcol <- plot_grid(l.inc[[3]], l.inc[[4]], dens.m, dens.f, ncol=2, nrow=2, rel_heights=c(4, 1))
legend_b <- get_legend(l.inc[[3]] + theme(legend.position="bottom"))
ploess.infl.int <- plot_grid( pcol, legend_b, ncol = 1, rel_heights = c(1, .1))


############################################################################################
## ---- FACLOESS_INC_INFL

# loess plot of scores on income
l.inc.ext.m <- ggplot(data=subset(scores2plot, sex=="M"), aes(x=faminc10_infl, y=EXT, group=cohort, fill=cohort, colour=cohort) ) + ylab("EXT") + ggtitle("Males Externalising")
l.inc.ext.f <- ggplot(data=subset(scores2plot, sex=="F"), aes(x=faminc10_infl, y=EXT, group=cohort, fill=cohort, colour=cohort) ) + ylab("EXT") + ggtitle("Females Externalising")
l.inc.int.m <- ggplot(data=subset(scores2plot, sex=="M"), aes(x=faminc10_infl, y=INT, group=cohort, fill=cohort, colour=cohort) ) + ylab("INT") + ggtitle("Males Internalising")
l.inc.int.f <- ggplot(data=subset(scores2plot, sex=="F"), aes(x=faminc10_infl, y=INT, group=cohort, fill=cohort, colour=cohort) ) + ylab("INT") + ggtitle("Females Internalising")
l.inc <- list(l.inc.ext.m, l.inc.ext.f, l.inc.int.m, l.inc.int.f)
l.inc <- lapply(l.inc, 
                function(x) x + theme(legend.position="none") + labs(list(fill="", colour = "")) +
                  geom_smooth(method = "loess") + 
                  coord_cartesian(xlim = c(0.0, 2.5), ylim = c(-.6, .6)) + #axis limits withouth affecting sample
                  xlab("Weekly Family Income at 10 (,000£ 2015 CPI)") +
                  ylab("Factor Score")
) # apply options to all graphs 

pcol <- plot_grid( l.inc[[1]], l.inc[[2]], l.inc[[3]], l.inc[[4]],
                   align = 'vh',
                   hjust = -1,
                   nrow = 2
)
legend_b <- get_legend(l.inc[[1]] + theme(legend.position="bottom"))
p <- plot_grid( pcol, legend_b, ncol = 1, rel_heights = c(1, .1))
p



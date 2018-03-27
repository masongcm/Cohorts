
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
box.ext.bcs <- ggplot(data=subset(scores2plot, cohort=="BCS"), aes(x=as.factor(EXT.RAW), y=EXT)) + geom_boxplot() + ggtitle("BCS EXT") + xlab("Sum score") + ylab("Factor score")
box.int.bcs <- ggplot(data=subset(scores2plot, cohort=="BCS"), aes(x=as.factor(INT.RAW), y=INT)) + geom_boxplot() + ggtitle("BCS INT") + xlab("Sum score") + ylab("Factor score")
box.ext.mcs <- ggplot(data=subset(scores2plot, cohort=="MCS"), aes(x=as.factor(EXT.RAW), y=EXT)) + geom_boxplot() + ggtitle("MCS EXT") + xlab("Sum score") + ylab("Factor score")
box.int.mcs <- ggplot(data=subset(scores2plot, cohort=="MCS"), aes(x=as.factor(INT.RAW), y=INT)) + geom_boxplot() + ggtitle("MCS INT") + xlab("Sum score") + ylab("Factor score")
plot_grid(box.ext.bcs, box.int.bcs, box.ext.mcs, box.int.mcs, ncol=2, align="h")

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

ineq.ext.m <- ggplot(data=subset(scores2plot, sex=="M" & !is.na(scores2plot$incq)), aes(x=as.factor(incq), y=EXT, colour=cohort)) + ggtitle("Males Externalising")
ineq.ext.f <- ggplot(data=subset(scores2plot, sex=="F" & !is.na(scores2plot$incq)), aes(x=as.factor(incq), y=EXT, colour=cohort)) + ggtitle("Females Externalising")
ineq.int.m <- ggplot(data=subset(scores2plot, sex=="M" & !is.na(scores2plot$incq)), aes(x=as.factor(incq), y=INT, colour=cohort)) + ggtitle("Males Internalising")
ineq.int.f <- ggplot(data=subset(scores2plot, sex=="F" & !is.na(scores2plot$incq)), aes(x=as.factor(incq), y=INT, colour=cohort)) + ggtitle("Females Internalising")

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

## ---- FACINEQ_SC
# MEAN/CI plot of scores by social class

scores2plot$scl10b <- NA
scores2plot$scl10b[scores2plot$scl10 %in% c("I", "II")] <- 5
scores2plot$scl10b[scores2plot$scl10 %in% c("IIINM")] <- 4
scores2plot$scl10b[scores2plot$scl10 %in% c("IIIM")] <- 3
scores2plot$scl10b[scores2plot$scl10 %in% c("IV", "V")] <- 2
scores2plot$scl10b[scores2plot$scl10 %in% c("other")] <- 1
scores2plot$scl10b <- factor(scores2plot$scl10b, labels = c("oth","IV-V","IIIM","IIINM","I-II"))

#common options
addopts <- function(x) {
  x <- x + xlab("Parental Social Class at 10") + ylab("Factor score") +
    coord_cartesian(ylim = c(-.4, .5)) +
    stat_summary(geom="errorbar", fun.data=mean_cl_normal, width=.2, position=position_dodge(.5)) +
    stat_summary(fun.y=mean, geom="point", size=4, aes(colour=cohort), position=position_dodge(.5)) +
    labs(list(colour="")) + theme(legend.position="none")
  return(x)
}

ineq.ext.m <- ggplot(data=subset(scores2plot, sex=="M" & !is.na(scores2plot$scl10b)), aes(x=as.factor(scl10b), y=EXT, colour=cohort)) + ggtitle("Males Externalising")
ineq.ext.f <- ggplot(data=subset(scores2plot, sex=="F" & !is.na(scores2plot$scl10b)), aes(x=as.factor(scl10b), y=EXT, colour=cohort)) + ggtitle("Females Externalising")
ineq.int.m <- ggplot(data=subset(scores2plot, sex=="M" & !is.na(scores2plot$scl10b)), aes(x=as.factor(scl10b), y=INT, colour=cohort)) + ggtitle("Males Internalising")
ineq.int.f <- ggplot(data=subset(scores2plot, sex=="F" & !is.na(scores2plot$scl10b)), aes(x=as.factor(scl10b), y=INT, colour=cohort)) + ggtitle("Females Internalising")

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


## ---- LOESS_REAL
require(grid)
require(gridExtra)

# limits for plots
inc.lims <- c(0, 2.0)

# loess plot of scores on income
l.inc.ext.m <- ggplot(data=subset(scores2plot, sex=="M"), aes(x=faminc_real, y=EXT, group=cohort, fill=cohort, colour=cohort) ) + ylab("EXT") + ggtitle("Males Externalising")
l.inc.ext.f <- ggplot(data=subset(scores2plot, sex=="F"), aes(x=faminc_real, y=EXT, group=cohort, fill=cohort, colour=cohort) ) + ylab("EXT") + ggtitle("Females Externalising")
l.inc.int.m <- ggplot(data=subset(scores2plot, sex=="M"), aes(x=faminc_real, y=INT, group=cohort, fill=cohort, colour=cohort) ) + ylab("INT") + ggtitle("Males Internalising")
l.inc.int.f <- ggplot(data=subset(scores2plot, sex=="F"), aes(x=faminc_real, y=INT, group=cohort, fill=cohort, colour=cohort) ) + ylab("INT") + ggtitle("Females Internalising")

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

dens.m <- ggplot(subset(scores2plot, sex=="M"), aes(x=faminc_real, group=cohort, fill=cohort, colour=cohort)) + geom_density(alpha = 0.1)
dens.m <- addopts_dens(dens.m)
dens.f <- ggplot(subset(scores2plot, sex=="F"), aes(x=faminc_real, group=cohort, fill=cohort, colour=cohort)) + geom_density(alpha = 0.1)
dens.f <- addopts_dens(dens.f)

# EXT plots
pcol <- plot_grid(l.inc[[1]], l.inc[[2]], dens.m, dens.f, ncol=2, nrow=2, rel_heights=c(4, 1))
legend_b <- get_legend(l.inc[[1]] + theme(legend.position="bottom"))
ploess.real.ext <- plot_grid( pcol, legend_b, ncol = 1, rel_heights = c(1, .1))

# INT plots
pcol <- plot_grid(l.inc[[3]], l.inc[[4]], dens.m, dens.f, ncol=2, nrow=2, rel_heights=c(4, 1))
legend_b <- get_legend(l.inc[[3]] + theme(legend.position="bottom"))
ploess.real.int <- plot_grid( pcol, legend_b, ncol = 1, rel_heights = c(1, .1))


## ---- LOESS_INFL

# limits for plots
inc.lims <- c(0, 2.0)

# loess plot of scores on income
l.inc.ext.m <- ggplot(data=subset(scores2plot, sex=="M"), aes(x=faminc_infl, y=EXT, group=cohort, fill=cohort, colour=cohort) ) + ylab("EXT") + ggtitle("Males Externalising")
l.inc.ext.f <- ggplot(data=subset(scores2plot, sex=="F"), aes(x=faminc_infl, y=EXT, group=cohort, fill=cohort, colour=cohort) ) + ylab("EXT") + ggtitle("Females Externalising")
l.inc.int.m <- ggplot(data=subset(scores2plot, sex=="M"), aes(x=faminc_infl, y=INT, group=cohort, fill=cohort, colour=cohort) ) + ylab("INT") + ggtitle("Males Internalising")
l.inc.int.f <- ggplot(data=subset(scores2plot, sex=="F"), aes(x=faminc_infl, y=INT, group=cohort, fill=cohort, colour=cohort) ) + ylab("INT") + ggtitle("Females Internalising")

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

dens.m <- ggplot(subset(scores2plot, sex=="M"), aes(x=faminc_infl, group=cohort, fill=cohort, colour=cohort)) + geom_density(alpha = 0.1)
dens.m <- addopts_dens(dens.m)
dens.f <- ggplot(subset(scores2plot, sex=="F"), aes(x=faminc_infl, group=cohort, fill=cohort, colour=cohort)) + geom_density(alpha = 0.1)
dens.f <- addopts_dens(dens.f)

# EXT plots
pcol <- plot_grid(l.inc[[1]], l.inc[[2]], dens.m, dens.f, ncol=2, nrow=2, rel_heights=c(4, 1))
legend_b <- get_legend(l.inc[[1]] + theme(legend.position="bottom"))
ploess.infl.ext <- plot_grid( pcol, legend_b, ncol = 1, rel_heights = c(1, .1))

# INT plots
pcol <- plot_grid(l.inc[[3]], l.inc[[4]], dens.m, dens.f, ncol=2, nrow=2, rel_heights=c(4, 1))
legend_b <- get_legend(l.inc[[3]] + theme(legend.position="bottom"))
ploess.infl.int <- plot_grid( pcol, legend_b, ncol = 1, rel_heights = c(1, .1))

## ---- FACLOESS_INC_INFL

# loess plot of scores on income
l.inc.ext.m <- ggplot(data=subset(scores2plot, sex=="M"), aes(x=faminc_infl, y=EXT, group=cohort, fill=cohort, colour=cohort) ) + ylab("EXT") + ggtitle("Males Externalising")
l.inc.ext.f <- ggplot(data=subset(scores2plot, sex=="F"), aes(x=faminc_infl, y=EXT, group=cohort, fill=cohort, colour=cohort) ) + ylab("EXT") + ggtitle("Females Externalising")
l.inc.int.m <- ggplot(data=subset(scores2plot, sex=="M"), aes(x=faminc_infl, y=INT, group=cohort, fill=cohort, colour=cohort) ) + ylab("INT") + ggtitle("Males Internalising")
l.inc.int.f <- ggplot(data=subset(scores2plot, sex=="F"), aes(x=faminc_infl, y=INT, group=cohort, fill=cohort, colour=cohort) ) + ylab("INT") + ggtitle("Females Internalising")
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

